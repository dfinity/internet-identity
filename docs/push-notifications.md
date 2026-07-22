# Push notifications — design

Internet Identity hosts a single Web Push pipeline that any dApp can send
through. The user installs **one** PWA (II), subscribes **once** per device,
and consents per dApp; every consented dApp then delivers notifications via
II rather than running its own service worker, VAPID keypair, and
subscription. This doc captures the design across the two paths that decide
scalability — **dApp → II** and **II → device** — plus the security model
and the open items.

## Status

Built today (PoC on `feat/push-notifications-poc`):

- Device subscribe/unsubscribe, `/authorize` opt-in, per-dApp consent.
- Single `notify_user(principal, alert)` update call.
- RFC 8291 payload encryption + RFC 8292 VAPID JWT signing, both in-canister.
- VAPID private key generated via `raw_rand` and persisted in stable memory.
- Notification click routes through the consent-gated `/notify` redirect.

Proposed (this doc):

- The broadcast endpoint, sender registry, cycles charging, per-origin rate
  limiting, the enqueue + drain queue, and the optional web2 delivery gateway.

## Architecture

```
dApp canister ──push_notify(alert, delivery, [principals]) + cycles──▶ II
                                                                        │ auth origin, dedup,
                                                                        │ rate-limit, consent-check,
                                                                        │ charge, ENQUEUE
                                                                        ▼
                                                                stable outbox ◀── timer drains
                                                                        │        (encrypt + sign per device)
                                                        direct: per-device outcall ─┐
                                                        gateway: sealed bundles ────┤
                                                                                    ▼
                                                              FCM / Mozilla / APNs relays
                                                                                    ▼
                                                        device SW decrypts → showNotification
                                                                                    │ tap
                                                                                    ▼
                                                        II /notify?to=origin → consent-gated redirect → dApp
```

The cost driver is **outcalls**, never the crypto (encryption and signing are
microseconds of local compute). Everything below optimizes outcall count and
keeps the crypto in-canister.

## dApp → II

### One endpoint, one uniform path

There is a single public send endpoint taking a vector of targets. There is
no separate single-target endpoint to misuse. Internally every send — one
target or 50,000 — takes the identical **enqueue → drain** path; there is no
size-based branching (a size branch would create a behavioral cliff, two code
paths to keep consistent, and an inconsistent "small feels synchronous /
large feels queued" contract).

The primary shape is a **broadcast**: one alert, many targets. This is what
makes 10k users a single ~300 KB call. Personalized-per-target bodies, if ever
needed, are a separate endpoint added later — never a size branch.

### Interface (Candid sketch)

```candid
type PushAlert = record {        // end-to-end encrypted; the relay cannot read this
  title : text;                  // ≤ 64 bytes
  body : text;                   // ≤ 256 bytes
  url : opt text;                // must be same-origin as the sender's origin
};

type PushUrgency = variant { VeryLow; Low; Normal; High };

type PushDelivery = record {     // RFC 8030 relay headers; plaintext, relay-visible
  urgency : opt PushUrgency;     // default Normal; also orders II's drain
  ttl_seconds : opt nat32;       // default ~4h; 0 = only if online now; clamped to a max
  topic : opt text;              // ≤ 32 chars base64url; collapse key
};

type PushRejection = variant {
  NoConsent;                     // unknown target OR not consented to *your* origin (merged on purpose)
  AlertInvalid : text;
  RateLimited;
  QueueFull;
};

type PushBatchResult = record {
  accepted : nat32;              // = queued, NOT delivered
  rejected : vec record { index : nat32; reason : PushRejection };
};

service : {
  push_register_sender : (origin : text) -> (variant { Ok; Err : text });
  push_deregister_sender : (origin : text) -> (variant { Ok; Err : text });
  push_fee_per_target : () -> (nat) query;

  push_notify : (
    batch_id : blob,             // idempotency key, ~24h dedup window per origin
    alert : PushAlert,
    delivery : PushDelivery,     // one set for the whole broadcast
    targets : vec principal,     // in-app principals; ≤ ~50k per call (message-size bound)
  ) -> (PushBatchResult);
}
```

The `PushAlert` / `PushDelivery` split mirrors the trust boundary: everything
in `PushAlert` is encrypted end-to-end (the relay cannot read it); everything
in `PushDelivery` is a plaintext header the relay sees.

### Targets are in-app principals

The dApp only knows its users by their in-app principal (II's privacy model).
`PRINCIPAL_INDEX` resolves `principal → (anchor, origin_hash)`, and the index
entry **pins the origin** — a principal belonging to another dApp's consent
resolves to a different `origin_hash` and is rejected. dApp A physically
cannot target dApp B's users even with stolen principals. `>50k` audiences are
chunked across calls by a thin client SDK, not by server-side routing.

### Sender authentication

One call has one `caller()`, so the per-user `caller == in_app_principal`
model does not batch. Senders authenticate at the **origin** level:

1. The dApp serves `/.well-known/ii-push-senders` listing its backend
   canister principal(s).
2. The backend calls `push_register_sender(origin)`; II fetches the file via
   HTTPS outcall (with a transform for consensus) and verifies
   `caller ∈ senders`, storing `origin_hash → {principals, verified_at}`.
3. II re-verifies on a TTL (~weekly, lazy) and deregisters when the file
   disappears. This reuses the existing outcall / DoH machinery; IC custom
   domains also carry a `_canister-id` DNS TXT record as a second path.

### Cycles

Only canister-to-canister calls can carry cycles, so **paid senders are
canisters** (`msg_cycles_available128` / `_accept128` return 0 for ingress).

- The dApp attaches cycles; II accepts `accepted × fee_per_target + base_fee`
  and the over-attached remainder auto-refunds. A shortfall rejects the whole
  batch (predictable) rather than partial-filling.
- Charged at **accept**, spent on outcalls at **drain**; delivery is
  best-effort, so no per-outcall reconciliation.
- `fee_per_target` is a configurable rate in `PersistentState`, sized at
  ~`expected_devices × outcall_cost × margin`. The margin funds overhead and
  makes spam self-limiting.
- A per-call `base_fee` makes looping single-target calls strictly more
  expensive than one broadcast, aligning incentives without special
  enforcement.
- A prepaid per-origin balance (to support off-chain senders via ingress) is
  an additive future option, not v1.

### Rate limiting

A per-origin token bucket denominated in **notifications (accepted targets)
per window**, not calls. Whether a dApp loops the endpoint or sends one big
broadcast, it drains the same budget, so II's safety is independent of how the
send is sliced. Over budget returns `RateLimited` with a `retry_after` hint.

## II internals — enqueue then drain

The send call is cheap: authenticate, dedup `batch_id`, rate-limit,
per-target resolve + consent-check, charge, **enqueue**, return counts. It
never does delivery work — one message cannot do 10k × devices encryptions and
outcalls (per-message instruction limit + outcall-concurrency cap).

A timer (`ic_cdk_timers`, ~1s) drains the outbox:

- Pops a bounded chunk **round-robin across origins** (fairness — one origin's
  50k backlog must not starve another's single send). The outbox is keyed
  `(origin_hash, seq)`.
- One `raw_rand` per tick; ChaCha20 derives per-message ephemeral seeds.
- Resolves each anchor's devices **now** (they change between enqueue and
  drain) and re-checks consent (may have been revoked).
- Forces `alert.hostname = origin` (unspoofable attribution).
- RFC 8291 encrypts per device; attaches the per-audience VAPID JWT
  (cached, ≤ 12h).
- Fires delivery, capped at a bounded in-flight count.
- `410 Gone` deletes the subscription. No retries in v1.

`PushDelivery` integrates into the queue, not just the headers:

- **topic** — on enqueue, key by `(origin_hash, anchor, topic)`; an undrained
  entry with the same key is **replaced**, collapsing rapid updates into
  fewer outcalls (savings scale with backlog). The relay collapses in-flight
  duplicates too.
- **ttl_seconds** — at drain, if `enqueued_at + ttl` has passed, **skip** the
  outcall entirely (the relay would drop it anyway).
- **urgency** — sets the header *and* orders the drain (`High` first).

New stable-memory regions follow the existing storable patterns: outbox
`(origin_hash, seq) → entry`, sender registry `OriginSha256 → sender`, and a
heap batch-dedup map (loss on upgrade acceptable). Timers do not survive
upgrades — re-arm in `post_upgrade` and `init`; the queue is stable so nothing
is lost.

## II → device delivery

The relay API is one POST per subscription endpoint (RFC 8030) — there is no
multi-recipient send — so the drain has two sink modes behind a deploy flag
(`push_delivery_mode : direct | gateway`), invisible to dApps.

### Direct mode

One IC HTTPS outcall per device. Fully on-chain. Cost ~13k outcalls for a 10k
broadcast (~1T cycles, ~$1–2/blast); delivery finishes in minutes at a bounded
in-flight rate.

### Gateway mode

II encrypts and signs as usual, then ships **fully-sealed, ready-to-POST
bundles** to a trusted web2 instance, which fans them out over free off-chain
HTTP:

```
[ { endpoint, headers, authorization:<vapid jwt+pubkey>, body:<ciphertext> }, … ]
```

- **The VAPID key and encryption stay on II** — moving either would require
  giving the gateway the VAPID private key (push to all users) or the device
  keys + plaintext (read all notifications). RFC 8291 has no encrypt-once mode,
  so II produces the N ciphertexts regardless; the gateway saves **outcall
  count, not crypto**.
- The gateway is **stateless** — no keys, no subscriptions, no plaintext at
  rest; it only sees in-transit ciphertext + endpoints + timing.
- II batches sealed bundles into ~2 MB chunks: ~25 outcalls instead of ~13k
  (~500× fewer; cycles drop to cents).
- II authenticates to the gateway with a bearer token (IC outcalls have no
  stable source IP). The gateway must return a **deterministic ack** or the
  outcall's response-consensus fails; per-device results cannot return through
  this channel.

Trust delta vs direct mode: the gateway can **drop, delay, or replay**
captured bundles, but cannot read content or forge new sends. Confidentiality
and authenticity stay on-chain; only **liveness** moves off — a real, narrow
concession for a trust-minimizing service. Its replay capability is why
message-level replay protection (below) becomes a should-have in gateway mode.

## Device & click

- **Subscribe** (Settings): request permission, `pushManager.subscribe` with
  II's VAPID public key, store `(anchor, endpoint_hash) → {p256dh, auth}`.
- **Consent** (`/authorize`): `push_grant_consent(anchor, origin)`.
- **Click**: the service worker opens `/notify?to=<origin>`; the page
  validates the origin against the anchor's consent list (via a
  session-delegation actor, no ceremony) across signed-in identities and
  redirects only on a match — otherwise it fails closed. This prevents the
  redirect endpoint from being abused as an open redirect.

## Security model

- **Origin pinning** — a sender can only target anchors that consented to
  *its* origin; cross-dApp targeting is impossible even with leaked principals.
- **Attribution** — II forces `alert.hostname` to the consented origin; a
  dApp cannot spoof who sent a notification.
- **Content is dApp-controlled** — `title`/`body` are free-form, delivered
  under II's service worker. Attribution is shown and lengths are capped, but
  a compromised dApp could send misleading text within its own attribution.
  This is inherent to any notification relay and is a documented posture, not
  a bug.
- **Confidentiality** — payloads are encrypted end-to-end to each device; no
  relay or gateway can read them.
- **Coarse rejections** — the dApp learns only about its own relationship with
  a target (`NoConsent`), never device counts or II state.
- **VAPID key** — in stable memory today (same posture as the anchor salt;
  node operators could extract it, blast radius is spam, not identity). P-256
  threshold ECDSA would remove it from storage but is not yet available on the
  IC (only secp256k1 is).

## Open items

Must-fix before a real deployment:

- **`pushsubscriptionchange`** — browsers rotate/invalidate subscriptions; the
  service worker must re-subscribe and re-register with II, or delivery
  silently erodes over weeks. Invisible in short-lived testing.
- **Sender deregistration & re-verification TTL** — bound the window after a
  sender principal is compromised or a domain changes hands.
- **Queue bounds + backpressure** — a finite outbox with a `QueueFull`
  eviction policy.
- **Observability** — queue depth (canary for stuck timers / backlog), drain
  lag, per-origin outcall success / 410 / drop counts, cycles burn.

Must-decide (design, not code):

- **Shared / multi-anchor devices** — one browser has one push endpoint, but a
  user may have multiple identities and devices are shared. Does a device bind
  to one identity, or carry notifications for all identities that enabled it?
- **iOS reality** — Web Push on iOS Safari needs the installed PWA and is
  flakier and more throttled; "best-effort" is weakest there. Set expectations
  in dApp docs.

Known-deferred:

- **Message-level replay / `msg_id`** — no per-message device dedup, so
  retries (or a malicious gateway) would produce duplicate banners. Must land
  before adding retries; graduates to should-have in gateway mode.
- **VAPID rotation** — rotating invalidates every subscription at once; needs
  a "re-enable on your devices" UX if ever required.
- **Stale-subscription GC**, and cleanup hooks on device/anchor removal.

Explicitly rejected:

- **On-device `tag`-based collapsing by hostname** — a dApp sends distinct
  notifications that must not replace each other on the device. Collapsing is
  opt-in per send via `topic`, never automatic per origin.

## dApp developer UX

One-time setup (~15 min): serve `.well-known/ii-push-senders`, call
`push_register_sender(origin)` from the backend. Steady state is one call:

```rust
let result = ii.push_notify(
    batch_id(),
    PushAlert { title, body, url: Some("https://yourapp.com/…".into()) },
    PushDelivery { urgency: Some(High), ttl_seconds: Some(3600), topic: Some("balance".into()) },
    my_user_principals,   // the in-app principals the dApp already holds
).await?;
```

No keys, no crypto, no Web Push knowledge, no per-user state beyond the
principals from auth. Delivery is best-effort with no receipts; the only
per-user signal is `NoConsent`. A thin SDK (Rust crate + agent-js) hides
chunking for large audiences, attaches the `batch_id`, and merges rejections.

## Feasibility summary

| Metric | Direct | Gateway |
| --- | --- | --- |
| App → II for 10k users | 1 call (~300 KB) | 1 call |
| Outcalls per 10k blast | ~13k | ~25 |
| Cycles per blast | ~1T (~$1–2) | ~cents |
| Full-delivery latency | minutes | seconds |
| Crypto cost | negligible | negligible |

App → II is feasible with a single call. II → device is feasible in direct
mode and cheap in gateway mode; the gateway is the lever if per-outcall cost
becomes the ceiling.
