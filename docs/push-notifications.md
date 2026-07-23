# Push notifications — design

Internet Identity hosts a single Web Push pipeline that any dApp can send
through. The user grants notification permission to id.ai **once**, subscribes
per device, and consents per dApp; every consented dApp then delivers
notifications via II rather than running its own service worker, VAPID
keypair, and subscription.

The value is a **single permission + delivery hub**, not an install: the user
allows notifications for their identity provider once, and every consented
dApp reaches them through it — instead of each dApp prompting for its own
notification permission (which browsers increasingly bury) and running its own
push stack. Installing II as a PWA is **optional** on Android and desktop
(Web Push works in a plain tab there) and only **required on iOS Safari**,
where it also earns its keep as "install one app, get every dApp's
notifications." This doc captures the design across the two paths that decide
scalability — **dApp → II** and **II → device** — plus the security model and
the open items.

## What does the user experience?

### How does a user turn notifications on? (first time signing into a dApp)

1. On the dApp (e.g. `oisy.com`), the user clicks **Sign in with Internet
   Identity**. II opens at `/authorize`.
2. The user authenticates as usual (passkey, or an existing session for a
   returning user).
3. **Continue screen** (`ContinueView`): "Continue to `<dApp>`" with the
   account picker and a **Continue** button. There is no notification toggle
   here — the account choice is the only decision.
4. The user taps **Continue**.
5. **Notifications opt-in screen** (`NotifOptInView`): a preview illustration
   of what a notification looks like (an "Example" lock-screen stack), the
   heading "Let `<dApp>` notify you", two short reasons (instant alerts /
   reachable anytime), and two buttons — **Enable notifications** and **Maybe
   later**.
6a. **Enable** → the browser shows its **native permission prompt**
    ("`id.ai` wants to show notifications — Allow / Block"). This dialog is
    the browser's own and cannot be restyled. On **Allow**, II subscribes the
    device and records consent for this dApp, then redirects to the dApp. No
    app install is required (Android/desktop); iOS Safari is the exception.
6b. **Maybe later** → II redirects straight to the dApp; nothing is enabled.

The permission prompt appears only the first time the user enables
notifications on this browser; for later dApps the opt-in screen simply
records consent (no second prompt).

### What happens when a notification is sent?

7. The dApp's backend/frontend calls `notify_user` (later, a batch endpoint).
8. The notification arrives on every device the user enabled — **even with the
   tab closed / browser not running** (on Android). It shows the dApp's origin
   as the source and the dApp's title/body as the text.
9. The user **taps** it. II's `/notify` screen appears briefly — "Opening
   `<dApp>`" with the app's logo — then forwards to the dApp (the specific
   page if the dApp deep-linked, otherwise its home).

### How does a user manage or turn them off?

10. In **II → Settings**, the user sees **Notifications on this device**
    (a toggle to turn the whole device on/off) and **Allowed apps** — every
    dApp that can notify them, each with a remove button. Revoking an app
    stops its notifications immediately.

## Status

Built today (PoC on `feat/push-notifications-poc`):

- Device subscribe/unsubscribe, `/authorize` opt-in, per-dApp consent.
- Single `notify_user(principal, alert)` update call.
- RFC 8291 payload encryption + RFC 8292 VAPID JWT signing, both in-canister.
- VAPID private key generated via `raw_rand` and persisted in stable memory.
  This is not the ideal custody model, and we do it only because the ideal
  one is unavailable: VAPID/Web Push mandate ECDSA on the **P-256
  (secp256r1)** curve, but the IC's threshold ECDSA currently supports **only
  secp256k1** — so we cannot let the subnet hold the key via `sign_with_ecdsa`
  and must generate and store it ourselves. It follows the same posture as the
  anchor salt (node operators could extract it; the blast radius is spam, not
  identity). If P-256 threshold ECDSA lands on the IC, the key moves off
  storage entirely — see the security and open-items sections below.
- Notification click routes through the consent-gated `/notify` redirect.

Proposed (this doc):

- Chunked `push_send` with two-layer flow control (II admission + client
  pacing), sender registry, a stateless-for-campaigns II (transient heap
  buffer, storage O(users)), a durable client library, and delivery through a
  trusted web2 gateway (with direct per-device outcalls as the documented
  alternative/fallback). No cycles charging in v1 (the real costs — storage and
  outcalls — are controlled by design, not billing) — parked as a future
  exploration.

## How does it work, in plain terms?

Read this first; the sections after it just add detail.

- **A dApp never talks to phones directly.** It tells II "notify these users,"
  and II handles the actual delivery. The dApp only knows its users by an
  opaque per-app id, so it can't reach anyone it wasn't given.
- **II already knows how to reach each user.** When a user turns notifications
  on, their browser hands II the keys needed to deliver to that device; when
  they sign into a dApp and opt in, II records that the dApp is allowed. So II
  keeps two small per-user facts: *which devices* and *which apps are allowed*.
- **II seals every message.** It encrypts (using device key-pair generated during subscription. Standard web-push api.) the text so only the user's own
  device can read it (Google/Apple/Mozilla just forward sealed bytes, the browser makes sure to decrypt it), and it
  signs the request so those push services trust that it really came from II (VAPID JWT which is per audience and can be cached so that we don't have to generate it every time).
- **Big sends are streamed, not dumped.** To notify 10,000 users, the dApp uses
  a small helper **library** that feeds II the list in bite-size batches at a
  pace II can handle. II refuses more than it can take (so nobody can flood
  it), and the library slows down when asked. The big list lives with the
  dApp — **II stores almost nothing per send**, which matters because storage
  is II's resource we want to protect.
- **How II sends the final messages.** II hands the sealed messages to a small
  **trusted helper server (the "gateway")** that makes the many little
  per-device sends on II's behalf over ordinary internet — far cheaper and
  faster than the canister making each call itself. (Having the canister call
  each push service directly is the documented fallback, but one network call
  per device gets expensive at scale, so it isn't the default.) Either way II
  does the sealing; the helper only forwards messages it can't read.
- **Tapping a notification** opens II briefly ("Opening \<app\>…") and forwards
  the user to the app — only ever to the app that sent it.

Everything below is the same story with the exact mechanisms and edge cases.

## Architecture

```
┌─ dApp side ────────────────────────┐        ┌─ II ────────────────────────────────┐
│ client library (durable):          │        │ Layer 1 — admission control         │
│  owns the campaign + status         │ chunk  │  per-origin bucket + global cap,    │
│  chunks (≤1000, ≤2MB)               │──────▶ │  O(1) reject → protects II          │
│  templating / personalization       │        │        │ admit                       │
│  prioritization                     │ ◀──────│        ▼                            │
│  Layer 2 — pacing (cooperative):    │ ready/ │  transient HEAP buffer (small)      │
│  send on `ready`, back off,         │ retry  │        │ seal: encrypt + VAPID sign  │
│  retry, track per-target status     │        │        ▼ fast drain                 │
└────────────────────────────────────┘        │  few batched outcalls ──▶ gateway   │
                                               │  (direct per-device = alt/fallback) │
                                               └─────────────────────────────────────┘
                                                        │ (gateway fans out off-chain)
                                                        ▼
                                              FCM / Mozilla / APNs relays
                                                        ▼
                                    device SW decrypts → showNotification
                                                        │ tap
                                                        ▼
                          II /notify?origin=…&to=… → consent-gated redirect → dApp
```

Two costs drive the architecture, and it is built to control **both**:

- **Stable storage** — the hard constraint. It is limited and must not grow
  with how many notifications are sent. So the durable campaign lives in the
  dApp's client library, and II keeps only user-scoped data (subscriptions,
  consent) plus a small transient working set — its storage stays flat with
  volume.
- **HTTPS outcalls** — genuinely expensive and rate-bounded (one call per
  device adds up fast in both cycles and throughput). So delivery is batched
  through the gateway, turning thousands of per-device outcalls into a handful.

Raw compute is the cheap part (the crypto is microseconds of local work and
stays in-canister); storage and outcalls are the two things worth engineering
around.

## dApp → II

### Sending to thousands of users: chunked send + two-layer flow control

> *In short: the dApp streams the audience to II in small batches ("chunks");
> II refuses more than it can handle, and the dApp slows down when told to.
> Two separate safeguards — one on each side — keep this safe and smooth.*

A campaign of any size is delivered as a series of **bounded chunks** — a chunk
is just a bite-size batch of targets (≤ ~1000 targets, ≤ 2 MB per call). This is deliberate: II must never be asked to hold a
whole campaign, because that is the storage that scales with volume. Two
independent control layers make this safe and smooth — and they must not be
confused:

- **Layer 1 — II admission control (mandatory, adversarial).** This is the
  security boundary and assumes a hostile client. II enforces a per-origin
  token bucket (notifications-per-window) plus a global cap on its transient
  buffer. Over capacity, `push_send` **rejects the chunk** (`ready = false` +
  `retry_after_ms`). The reject is O(1). The guarantee: **II never holds more
  than its bounded buffer, no matter what the client does** — a client that
  ignores backpressure just collects cheap rejections. This is the anti-spam
  property, and it lives entirely on II.
- **Layer 2 — client pacing (cooperative, efficiency only).** The client
  library reads the same `ready` / `retry_after_ms` signal and paces so it
  doesn't waste calls getting rejected: send the next chunk when II is ready,
  back off when it isn't, retry, track status. This is a good-citizen layer,
  **not** a security layer. If a client skips it, nothing breaks for II
  (Layer 1 holds); the client just delivers slower.

The rule to remember: **II protects itself; the client optimizes itself.**
Never rely on client pacing to prevent spam — that is always Layer 1's job.

Both "same text for everyone" and "different text per user" go through the
**one** `push_send` call: it takes a shared `default_alert` plus a list of
recipients, each able to override the default with its own alert. Broadcast =
set the default and leave every override empty (the content isn't repeated, so
~1000 recipients fit well under 2 MB); personalized = give each recipient its
own alert, rendered client-side by the library's templating (II never holds a
template); a mix of the two works too.

### The Candid interface

The exact API a dApp's backend calls, written in Candid (the IC's interface
language). There is a single send entry point, `push_send`: it carries a
shared `default_alert` plus a list of recipients, each of which may override
that alert with its own — so the same call does both "same text for everyone"
and "different text per user." The rest are the types it uses; skim the
comments — each explains what the field is for.

```candid
type PushCategory = variant { Message; Transfer; Update; Generic };

// The content variant is what makes end-to-end encryption possible later
// (see the end-to-end-encryption section below). Display content is read by II
// and shown verbatim — fine for non-sensitive notifications, but II sees it.
// Hidden content is never sent to II at all: the payload carries no message
// text, so an E2E sender structurally cannot leak content. The service
// worker renders an II-controlled generic string keyed by category, and the
// real message is revealed on tap-through when the app decrypts it.
type PushContent = variant {
  Display : record {             // II-visible; transport-encrypted only, NOT E2E
    title : text;                // ≤ 64 bytes
    body : text;                 // ≤ 256 bytes
  };
  Hidden : record {              // content-free; E2E-safe by construction
    category : opt PushCategory; // maps to II-controlled copy ("New message", …)
  };
  Dismiss;                       // close the shown notification named by notification_id; renders nothing
};

type PushAlert = record {
  content : PushContent;
  url : opt text;                // tap-through target; must be same-origin as sender
  notification_id : opt text;    // dApp's id for this notification (maps to the Web Notification `tag`):
                                 // reuse it to UPDATE the shown one, or pair with Dismiss to close it
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
};

type PushResult = record {
  admitted : nat32;              // accepted into the in-flight buffer for delivery (NOT delivered)
  rejected : vec record { index : nat32; reason : PushRejection };
  ready : bool;                  // false → II is at capacity; stop and retry after retry_after_ms
  retry_after_ms : opt nat32;    // Layer-1 backpressure hint
};

// One recipient. `alert` is an optional per-recipient override; when null the
// recipient uses the chunk's shared `default_alert`. This is how one endpoint
// covers both cases: broadcast = shared default + all overrides null;
// personalized = per-recipient overrides. `null` costs ~1 byte, so broadcast
// stays compact (the content isn't repeated per recipient).
type PushRecipient = record {
  target : principal;            // in-app principal
  alert : opt PushAlert;         // override; falls back to default_alert
};

service : {
  push_register_sender : (origin : text) -> (variant { Ok; Err : text });
  push_deregister_sender : (origin : text) -> (variant { Ok; Err : text });

  // Submit ONE chunk (≤ ~1000 recipients, ≤ 2 MB). II admits what it has
  // capacity for (Layer 1) and returns per-recipient rejections plus a
  // backpressure signal. Each recipient's effective alert is its own override
  // or, if null, `default_alert`; if both are null it is rejected
  // (AlertInvalid). The client library owns the campaign, chunking, pacing,
  // retry, status, templating and prioritization — II holds no campaign state.
  push_send : (
    chunk_id : blob,             // per-chunk idempotency (short-lived heap dedup)
    delivery : PushDelivery,     // shared across the chunk (urgency / ttl / topic)
    default_alert : opt PushAlert, // shared alert for recipients that don't override
    recipients : vec PushRecipient,
  ) -> (PushResult);
}
```

The `content` / `PushDelivery` split mirrors the trust boundaries. `Display`
content is transport-encrypted (the relay can't read it) but **II can** —
acceptable for non-sensitive notifications. `Hidden` content is never sent to
II, which is what preserves end-to-end encryption. `PushDelivery` fields are
plaintext RFC 8030 headers the relay sees. A sender that wants E2E chooses the
`Hidden` variant and, by construction, has no field to put message text in.
`push_send` returns as soon as the chunk is admitted (or rejected) — it does
**not** wait for delivery; "admitted" means "accepted into II's in-flight
buffer", nothing more.

### How does II know which users to send to?

The dApp only knows its users by their in-app principal (II's privacy model).
`PRINCIPAL_INDEX` resolves `principal → (anchor, origin_hash)`, and the index
entry **pins the origin** — a principal belonging to another dApp's consent
resolves to a different `origin_hash` and is rejected. dApp A physically
cannot target dApp B's users even with stolen principals. Audiences larger
than one chunk are split across paced `push_send` calls by the client library,
not by server-side routing.

### How does II verify the sender is really that dApp?

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

### Admission control (Layer 1): stopping one dApp from flooding II

> *In short: II tracks how much each app is sending and simply says "not right
> now, try again in N ms" once an app goes over its share. This is what stops
> spam, and it works even if the sender ignores every hint.*

The mandatory guard, assuming a hostile client. Enforced on every `push_send`,
independent of client behavior:

- **Per-origin token bucket** in notifications-per-window — one origin can't
  spam and can't starve others. Denominated in notifications, not calls, so it
  can't be gamed by slicing a send into many small chunks.
- **Global cap** on the in-flight buffer — protects II/the subnet as a whole.
- **O(1) reject** when over capacity (`ready = false` + `retry_after_ms`), so
  absorbing a flood is itself cheap.

Because the queue lives on the client, the only II storage a sender can
pressure is the bounded in-flight buffer — which admission control caps
directly. That, plus the fast drain (below), is what lets II sustain high
throughput on a *small* buffer.

### What does this cost, and who pays?

Two real costs, controlled by the design; not charged to the dApp in v1:

- **Stable storage — the hard constraint.** Kept **O(users)** (subscriptions +
  consent), never O(notification volume), because the campaign state lives in
  the client library. This is the cost that would otherwise grow without bound,
  so it's the one the whole "stateless II" shape is built around.
- **HTTPS outcalls — expensive and bounded.** Minimized by batching delivery
  through the gateway (a handful of calls instead of one per device). Real
  enough to design against, which is why the gateway exists.
- **Compute (the crypto)** is the cheap part and stays in-canister.

On charging: **no cycles charging in v1.** Cycles can only be attached by
canister senders, and adding per-notification fees brings real complexity
(accept/refund, fee tuning) for a v1 whose abuse surface is already bounded by
admission control. Attached-cycle or prepaid-balance pricing is parked as a
future exploration — worth revisiting to offset the outcall/storage cost at
scale, or as an extra abuse lever. See Future exploration.

## II's state model: stateless for campaigns

> *In short: II accepts a chunk, briefly holds it in memory (not durable
> storage), seals and sends it, then forgets it. The durable list lives with
> the dApp, so II's storage doesn't grow as more notifications are sent.*

II holds **no campaign queue in stable (durable) memory**. `push_send` is
cheap:
authenticate the sender, dedup `chunk_id`, run admission control (Layer 1),
per-target resolve + consent-check, then admit the survivors into a small
**transient heap buffer** and return `{admitted, rejected, ready,
retry_after_ms}`. It does not wait for delivery — one message cannot do
1000 × devices encryptions and outcalls (per-message instruction limit +
outcall-concurrency cap).

A timer (`ic_cdk_timers`, ~1s) drains the heap buffer:

- One `raw_rand` per tick; ChaCha20 derives per-message ephemeral seeds.
- Resolves each anchor's devices **now** (they change between admit and drain)
  and re-checks consent (may have been revoked since).
- Forces `content` attribution to the sender origin (unspoofable).
- RFC 8291 encrypts per device; attaches the per-audience VAPID JWT
  (cached, ≤ 12h).
- Drains **fast** to the gateway in a few batched outcalls (see below). Fast
  drain is what lets the buffer stay small while admitting at high throughput.
- `410 Gone` deletes the subscription. No retries in v1 (retries are a client
  concern; see the client library).

`PushDelivery` integrates into the buffer, not just the headers:

- **topic** — key the buffer entry by `(origin_hash, anchor, topic)`; an
  un-drained entry with the same key is **replaced**, collapsing rapid updates
  (savings scale with buffer depth). The relay collapses in-flight duplicates
  too.
- **ttl_seconds** — at drain, if `admitted_at + ttl` has passed, **skip** the
  outcall (the relay would drop it anyway).
- **urgency** — sets the header *and* orders the drain (`High` first).

**Durability lives on the client, not in II.** The in-flight buffer is heap,
not stable memory — so it costs no persistent storage and is simply lost on
upgrade. That is fine: the client library is the source of truth and re-sends
any chunk it hasn't seen acknowledged. Consequently the only new **stable**
regions are user-scoped and volume-independent: the sender registry
(`OriginSha256 → sender`) alongside the existing subscription and consent maps.
`chunk_id` dedup is a short-lived heap set. Timers don't survive upgrades —
re-arm in `post_upgrade` and `init`.

## Delivering to devices

The relay API is one POST per subscription endpoint (RFC 8030) — there is no
multi-recipient send, so reaching N devices is fundamentally N sends. The
design routes those sends through a **trusted web2 gateway**. Doing them as
per-device outcalls straight from the canister ("direct") is the documented
**alternative** — kept for context and as a possible low-volume or
fully-on-chain fallback, but not the shipping path.

### The delivery path: a trusted web2 gateway

II encrypts and signs as usual, then hands the **fully-sealed, ready-to-send
messages** to a small trusted helper server, which makes the many little sends
over ordinary (free) internet instead of on-chain:

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
- **The gateway's cheapness sets II's admission capacity.** II's throughput is
  bounded by how fast it drains its (small) buffer; a fast, cheap batched
  drain empties the buffer quickly, so II recovers capacity and admits the next
  chunk sooner. Fast drain → high sustained throughput on a *small* buffer →
  flat storage. This is why the gateway hop is a scaling primitive, not just a
  cost optimization.

Trust delta — the deliberate cost of this path: the gateway can **drop, delay,
or replay** captured bundles, but cannot read content or forge new sends.
Confidentiality and authenticity stay on-chain; only **liveness** moves off — a
real, narrow concession for a trust-minimizing service. Its replay capability
is why message-level replay protection (below) is a should-have here.

### The alternative: direct per-device outcalls

II could instead make one HTTPS outcall per device straight to the push
service — fully on-chain, nothing extra to run or trust. Why it isn't the
default: a 10k-user broadcast is ~13k outcalls (~1T cycles, ~$1–2 per blast)
and takes minutes to drain at a safe in-flight rate; that per-device outcall
cost and the throughput ceiling are exactly what the gateway removes. It stays
viable for low volume, a fully-on-chain deployment, or as a fallback if the
gateway is unavailable.

## The dApp-side client library

> *In short: because II stores nothing per send, a small library on the dApp's
> side keeps the list, sends it to II in paced pieces, retries failures, tracks
> who got notified, and personalizes text. The dApp calls one simple
> "notify these users" method; the library handles the rest.*

Since II is stateless for campaigns, the durable coordination is a library the
dApp runs (in its own canister for on-chain durability, or its backend). This
is where the heavy list and its bookkeeping live — where the volume originates.
Responsibilities:

- **Campaign store** — the target list and per-target status (pending / sent /
  `NoConsent` / to-retry). This is the durable state that II deliberately does
  not hold.
- **Chunking** — split the audience into `push_send` chunks (≤ ~1000 targets,
  ≤ 2 MB), enforced client-side against the message-size bound.
- **Pacing (Layer 2)** — send on `ready`, back off on `retry_after_ms`,
  pipeline a few chunks within the caller's output-queue limit. Absorbs
  inter-canister latency so a large campaign completes over seconds-to-minutes
  without ever blocking II.
- **Retry** — resend chunks that were rejected for capacity or lost to an
  upgrade; `chunk_id` makes resends idempotent.
- **Status/reporting** — aggregate per-chunk results into campaign progress
  for the dApp. (Delivery itself is best-effort with no receipts; the only
  per-user signal is `NoConsent`.)
- **Templating / personalization** — expand `template + per-user data` into a
  per-recipient `alert` override, entirely client-side. II never sees a
  template.
- **Prioritization** — schedule which campaign/segment goes first. (Per-message
  `urgency` still rides the relay header; campaign ordering is the library's.)

Security is unaffected by moving this out: II re-validates sender-origin,
consent and origin-pinning on **every** chunk, so a buggy or malicious library
cannot fake consent, target another dApp's users, or exceed admission limits —
it can only mismanage its own campaign.

## On the device: rendering and tap-through

- **Subscribe** (Settings): request permission, `pushManager.subscribe` with
  II's VAPID public key, store `(anchor, endpoint_hash) → {p256dh, auth}`. No
  PWA install is needed on Android or desktop — this works in a plain tab;
  iOS Safari is the exception (it only allows Web Push for an installed
  home-screen app). The optional install adds an app icon / standalone window
  and slightly better attribution.
- **Consent** (`/authorize`): `push_grant_consent(anchor, origin)`.
- **Render**: the service worker branches on `content` — `Display` shows the
  supplied `title`/`body`; `Hidden` shows an II-controlled generic string
  keyed by `category` ("New message from `<origin>`"), never app-supplied text.
- **Click**: the service worker opens
  `/notify?origin=<sender>&to=<deep-link>`; the page validates and redirects
  (see below), otherwise it fails closed. This prevents the redirect endpoint
  from being abused as an open redirect. For `Hidden` (E2E) notifications this
  tap-through is also the content-reveal path: the app opens and decrypts the
  message in its own context.

### Can the app choose where the notification opens?

The app may set `alert.url` to send the user to a specific page rather than
the origin root. `/notify` honors it under two constraints, both enforced
client-side:

- **The target's origin must equal the sender's origin.** The sender origin
  is `alert.hostname`, which the backend forces to the consented origin and a
  dApp cannot spoof. So a notification can deep-link anywhere within the
  sender's own site (`https://app.com/thread/42`) but never to another
  origin — not `evil.com`, and not another consented dApp.
- **The sender origin must be in the anchor's consent list** (session-delegation
  check). A hand-crafted `/notify?origin=…&to=…` therefore also fails closed.

No target → redirect to the sender origin's root. Because the check keys off
the backend-forced `hostname`, this is entirely front-end; no backend or
Candid change is required. (A backend `alert.url`-origin validation at send
time is a nice defense-in-depth follow-up but not required for safety.)

### Can a notification be updated or dismissed after it's shown?

Yes, via a dApp-chosen `notification_id`, which the service worker maps to the
Web Notification `tag`:

- **Update / replace** — send again with the same `notification_id`; the device
  replaces the shown notification in place rather than stacking a second (e.g.
  "Order shipped" → "Order delivered", or a live score ticking up).
- **Dismiss** — send `content = Dismiss` with that `notification_id`; the SW
  finds notifications with the matching tag and calls `.close()`, showing
  nothing new.

This is complementary to the `topic` delivery header, and the two act at
different stages: `topic` collapses *undelivered* messages at the relay
(before they reach the device), while `notification_id` updates or dismisses an
*already-delivered* one on the device.

Caveats:

- **`userVisibleOnly` fights a silent dismiss.** Browsers require every push to
  show *something*; a pure "close and show nothing" push can trigger the
  browser's generic "site updated in background" notification and, if abused, a
  permission penalty. So **update-with-a-new-state is the robust pattern**;
  pure dismiss is best-effort.
- It only works if the device is online to receive the follow-up and the
  notification is still present (the user may have dismissed it already).
- This is **explicit, opt-in** grouping the dApp controls — the opposite of the
  automatic hostname-collapse we rejected. Notifications without a
  `notification_id` never replace each other.

## Security model

- **Origin pinning** — a sender can only target anchors that consented to
  *its* origin; cross-dApp targeting is impossible even with leaked principals.
- **Attribution** — II forces `alert.hostname` to the consented origin; a
  dApp cannot spoof who sent a notification.
- **Content is dApp-controlled (Display mode)** — `title`/`body` are free-form,
  delivered under II's service worker. Attribution is shown and lengths are
  capped, but a compromised dApp could send misleading text within its own
  attribution. Inherent to any notification relay; a documented posture, not a
  bug. `Hidden` mode avoids it entirely — no app text reaches the SW.
- **Transport confidentiality** — every payload is RFC 8291 encrypted to each
  device; no relay or gateway can read it. Note this is *transport*
  encryption: for `Display` content II itself sees plaintext (it does the
  encryption). Keeping content from II-the-service is the `Hidden` variant
  today (and, later, the vetKeys-sealed path); see the next section.
- **Coarse rejections** — the dApp learns only about its own relationship with
  a target (`NoConsent`), never device counts or II state.
- **Redirect-screen icon** — the `/notify` screen shows the app logo only from
  II's curated dApp registry, with a neutral globe fallback. Arbitrary or
  remote icons are deliberately not fetched: doing so would render
  attacker-controlled imagery inside II's trusted chrome and leak the fetch to
  the dApp. If arbitrary-app icons are ever wanted, the least-bad path is a
  vetted icon supplied at sender-registration, not a per-notification URL.
- **VAPID key** — in stable memory today (same posture as the anchor salt;
  node operators could extract it, blast radius is spam, not identity). P-256
  threshold ECDSA would remove it from storage but is not yet available on the
  IC (only secp256k1 is).

## End-to-end-encrypted apps

Some apps (e.g. a chat using vetKeys) encrypt content so that only the
recipient can read it — the app backend cannot. Our `Display` path is **not**
E2E: II receives plaintext `title`/`body` and briefly holds it. The `Hidden`
variant lets these apps use II-hosted push without ever handing content to II.

**Correcting an earlier overstatement.** It is tempting to say II can *never*
show decrypted content. That is too strong. The honest constraint is about
*who* decrypts and *where*, and it comes with a trust dial the user is already
turning:

- **II-the-service decrypts** — the canister and its node operators see
  plaintext. That is just `Display` relabeled: fine for non-sensitive content,
  not E2E.
- **II's service worker decrypts on the user's own device**, using a key it
  legitimately obtains and that II-the-service never sees in the clear. This
  **is** viable. It asks the user to trust II's *client code* running as them
  on their device — which is only a small step past the trust they already
  place in II by signing in with it (and exactly the trust the `Display` path
  already assumes). This is the basis for showing real content in an
  E2E-friendly way.

So there is a spectrum, from no trust to full trust in II for content:

| Approach | Who decrypts | In-notification | Trust in II for content |
| --- | --- | --- | --- |
| `Hidden` (ships first) | the app, after tap | generic ("New message") | none — II never touches content |
| Design A — vetKeys-sealed | II's **SW**, on device | full, real text | II's client code + IC vetKD threshold |
| Design B — dApp-fetch | II's **SW**, on device | full, real text | II's client code (+ the dApp) |
| `Display` | II-the-service | full | full — II reads it |
| App's own SW (not II-hosted) | the app's own SW | full | none — II isn't in the loop |

### Baseline that ships first: `Hidden` + tap-through

A content-hidden notification ("New message from `<origin>`") plus
**tap-through reveal** — the user taps, the app opens, the app decrypts and
shows the message in its own context. This is the industry-standard E2E
notification UX (Signal, iMessage with previews off), and the `/notify`
redirect we already have is exactly the reveal path. II never sees a byte of
content and stores no plaintext.

Framed honestly, `Hidden` is a **deliberate lesser experience that is what
enables E2E and push together at all.** The user gives up the lock-screen
preview (they see "New message," not the text) — a real downgrade versus
`Display`. But it needs zero extra trust in II and ships today, so it is the
right first step; the richer designs below are how the real text gets onto the
lock screen later.

### Design A — vetKeys-sealed content, decrypted in II's SW (preferred richer path)

II provides the encryption service so the app never hands plaintext to
II-the-service, yet the real text still reaches the lock screen:

- II derives a vetKeys (IBE) identity per `(user, origin)`. The dApp fetches
  II's master public key once and **encrypts the content to that identity
  offline** — no round-trip, recipient needn't be online. It sends only the
  *ciphertext* to II via `push_send`, as opaque bytes II cannot read.
- II delivers as usual (RFC 8291 wraps the opaque bytes). On the device, II's
  SW authenticates **as the user** to II's canister and requests the vetKD
  decryption key. vetKD returns it **encrypted to the SW's transport key**, so
  no node operator sees the key in the clear; the SW decrypts the content and
  shows the **real text**.
- **What II-the-service sees:** in the honest flow, nothing — not the content,
  not the key in clear. Plaintext exists only inside the user's SW, and II
  stores none of it.
- **The trust that remains:** II's canister *controls the vetKD
  authorization*, so a malicious/compromised II could authorize itself to
  derive a user's key and decrypt. That is the same class of trust the user
  already extends to II for their delegations — not a new kind. No single node
  can derive the key (threshold); II's client code is assumed honest (if it
  weren't, the user's identity is already lost). A small, coherent increment
  over `Hidden`.
- **Cost:** vetKeys integration in II and a derive-key call per render; the
  dApp does IBE encryption. No change to the send/admission model — II still
  just carries bytes it can't read.

### Design B — dApp keeps the keys, II's SW fetches + decrypts (alternative)

The push is a bare wake; on receipt II's SW calls an endpoint **on the dApp's
side** to obtain the content, then renders it. Genuinely E2E only if that
endpoint hands back *ciphertext* the SW can decrypt — which forces one of two
uncomfortable choices:

- **Endpoint returns plaintext.** Then the dApp *backend* can decrypt, so it is
  "trust the dApp backend," not strict E2E. Simple, no vetKeys; fine for apps
  that don't need backend-blind encryption.
- **Endpoint returns ciphertext + the SW holds the key.** The recipient's key
  lives in the dApp's client (cross-origin from II's SW), so the dApp must
  *provision a key into II's SW* — now II's SW holds dApp secrets, a real added
  surface (II's code could exfiltrate them).

Either way it also needs a way for the SW to authenticate to the dApp as the
user (it has no delegation to the dApp) and a **fetch per notification**
(latency, dApp serving load, offline-delivery gaps). More moving parts and a
murkier trust story than A; documented as the fallback for apps that prefer to
own the crypto or are content with backend-readable content.

### Recommendation

- **Ship `Hidden` first** — zero extra trust, works today.
- **Plan for Design A (vetKeys)** as the way to put real content on the lock
  screen while keeping II-the-service blind to it — the trust increment is
  small and of a kind the user already accepts.
- **Design B** stays the documented alternative for backend-trusted apps or
  teams that don't want vetKeys.
- **App runs its own SW** remains the strict, II-uninvolved option for teams
  that want rich previews with II entirely out of the loop.

Forward-compat is already in the interface: an E2E sender picks `Hidden` today
and has no field to leak content through. Design A slots in later as an opaque
**ciphertext arm** of `PushContent` (II still just carries bytes it can't
read) plus a vetKD derive-and-decrypt branch in the SW render path — no change
to send, admission, or storage.

## Open items

Must-fix before a real deployment:

- **`pushsubscriptionchange`** — browsers rotate/invalidate subscriptions; the
  service worker must re-subscribe and re-register with II, or delivery
  silently erodes over weeks. Invisible in short-lived testing.
- **Sender deregistration & re-verification TTL** — bound the window after a
  sender principal is compromised or a domain changes hands.
- **Buffer sizing & drain fairness** — tune the admission bucket + global cap,
  and drain the transient buffer **round-robin across origins** so one origin's
  chunk can't starve another's. (Admission control itself is designed, above;
  this is the tuning.)
- **Observability** — in-flight buffer depth (canary for stuck timers /
  backpressure), drain lag, admission reject rate per origin, per-origin
  outcall success / 410 / drop counts.

Must-decide (design, not code):

- **Shared / multi-anchor devices** — one browser has one push endpoint, but a
  user may have multiple identities and devices are shared. Does a device bind
  to one identity, or carry notifications for all identities that enabled it?
- **iOS reality** — iOS Safari is the one platform where a PWA install is
  *mandatory* for Web Push (Android/desktop work in a plain tab). It is also
  flakier and more throttled; "best-effort" is weakest there. Set expectations
  in dApp docs, and surface the "Add to Home Screen" hint only on iOS.

Known-deferred:

- **Device-level replay / `msg_id`** — note this internal `msg_id` is a
  *different* thing from the dApp-facing `notification_id` used to
  update/dismiss (above): `msg_id` is II-generated and suppresses exact
  duplicates (show at most once); `notification_id` is dApp-chosen and
  *replaces* the shown notification. Same-looking, opposite behavior — keep
  them as separate fields. There are two replay layers, handled separately.
  **dApp→II replay** (a resent/duplicated chunk) is solved by the `chunk_id`
  idempotency key. **relay/gateway→device replay** (a captured
  `(jwt, ciphertext, endpoint)` re-POSTed) is *not* solved: Web Push has no
  built-in protection — VAPID's JWT is a time-bounded bearer token and
  AES-GCM's freshness gives tamper-detection, not anti-replay, so a captured
  push re-decrypts and shows again. The fix is a `msg_id` plus a small SW
  dedup set, wired as follows (note it is **not** a Candid field — the dApp
  does not supply it):
  - **On send (in the canister):** II assigns a unique `msg_id` **once per
    admitted message** (per target) and includes it in the JSON *before* RFC
    8291 encryption (`alert_to_json` in `push/api.rs`). It must be **stable
    across delivery retries** — the same logical message always carries the
    same `msg_id` — so that both a retried send and a replayed captured
    ciphertext are recognized as duplicates, while genuinely distinct
    notifications get distinct ids.
  - **On receive (service worker):** the SW reads `msg_id` from the decrypted
    payload, keeps a bounded set of recently-seen ids (IndexedDB/Cache), and
    drops any it has already shown.
  It is a **hard prerequisite**, not a nicety, for two later features and must
  land before either:
  - **outcall retries** — a retry is itself a replay; without dedup, retry
    logic manufactures duplicate banners.
  - **the gateway** — a compromised gateway is inherently replay-capable, so
    `msg_id` + SW dedup should land together with the gateway (the chosen
    delivery path) to neutralize that power.
  Today's practical risk is low (TLS gates capture; blast radius is a
  duplicate banner, not credential theft).
- **VAPID key → P-256 threshold ECDSA** — the key is in stable memory only
  because the IC's threshold ECDSA is secp256k1-only while Web Push requires
  P-256 (secp256r1). When P-256 tECDSA ships, migrate to `sign_with_ecdsa` so
  the key never exists in storage. Migration invalidates existing
  subscriptions (the derived public key changes), so it pairs with the
  re-enable UX below.
- **VAPID rotation** — rotating invalidates every subscription at once; needs
  a "re-enable on your devices" UX if ever required.
- **Stale-subscription GC**, and cleanup hooks on device/anchor removal.

Explicitly rejected:

- **On-device `tag`-based collapsing by hostname** — a dApp sends distinct
  notifications that must not replace each other on the device. Collapsing is
  opt-in per send via `topic`, never automatic per origin.

## dApp developer integration

One-time setup (~15 min): serve `.well-known/ii-push-senders`, call
`push_register_sender(origin)` from the backend. Steady state: hand the client
library a campaign — it chunks, paces, retries and reports:

```rust
// Non-sensitive app: show the content.
push.broadcast(
    PushContent::Display { title, body },
    PushDelivery { urgency: High, ttl_seconds: 3600, topic: Some("balance") },
    &my_user_principals,        // library chunks + paces these
).await?;

// E2E app: send nothing sensitive; content is revealed on tap.
push.broadcast(
    PushContent::Hidden { category: Some(Message) },
    PushDelivery { urgency: High, ttl_seconds: 3600, topic: None },
    &my_user_principals,
).await?;
```

Under the hood the library splits the audience into ≤ ~1000-target chunks,
calls `push_send` per chunk, **paces on `ready` / `retry_after_ms`**, retries
with a stable `chunk_id`, and aggregates per-target results into campaign
status. The dApp sees a campaign API; II sees paced, bounded chunks. No keys,
no crypto, no Web Push knowledge, and no per-user state beyond the principals
from auth (the library owns campaign status). Delivery is best-effort with no
receipts; the only per-user signal is `NoConsent`.

## Feasibility and scale

| Metric | Gateway (chosen) | Direct (alternative) |
| --- | --- | --- |
| App → II for 10k users | ~10 paced chunks (client-driven) | ~10 paced chunks |
| Outcalls per 10k blast | ~25 | ~13k |
| II **stable storage** | O(users) — flat with volume | O(users) — flat with volume |
| II in-flight buffer | one chunk (heap, transient) | one chunk (heap, transient) |
| Full-delivery latency | seconds | minutes |
| Main costs | storage + ~25 outcalls | storage + ~13k outcalls |

App → II is feasible either way: the client library streams bounded chunks, II
admits what it has capacity for, and its **stable storage stays flat regardless
of volume**. The two rows above are why the **gateway is the chosen delivery
path** — a faster, cheaper drain recycles II's small buffer sooner, which is
what raises admission throughput. Direct delivery works and is fully on-chain,
but its per-device outcall cost and slower drain are exactly what the gateway
removes, so it stays as the alternative/fallback.

## Future exploration

Deliberately out of v1, kept here so the door stays open:

- **Cycles-based charging.** Attach cycles per notification (canister senders
  only — ingress calls can't carry cycles), or a prepaid per-origin balance
  (which would also let off-chain senders pay). Dropped from v1 because the
  design already controls the real costs (storage via the stateless model,
  outcalls via the gateway) and admission control bounds abuse — so billing
  adds complexity without solving an immediate problem. Worth revisiting at
  scale to make senders pay for the outcalls/storage they drive, or as an extra
  fairness/abuse lever. Retrofitting payment later is possible (the endpoint
  would check attached cycles or a prepaid balance) but easier to design in
  than bolt on, so keep the option visible.
- **Off-chain senders** (a web2 backend calling via ingress) — needs a
  self-auth challenge flow for sender identity and, if paid, the prepaid
  balance above.
- **Delivery receipts / analytics** — Web Push has none; any per-user delivery
  signal would have to come from the app itself, and per-origin aggregates are
  the most II can offer without a per-user tracking surface.
