# Push notifications — design

**How to read this.** It is long because it covers two independent scaling paths
and a security model. If you only read one section, read
[How it works, in plain terms](#how-it-works-in-plain-terms). Then:

- **Reviewing the design?** [Goals and non-goals](#goals-and-non-goals) →
  [Architecture](#architecture) →
  [Alternatives considered](#alternatives-considered) →
  [Security model](#security-model) → [Open items](#open-items).
- **Implementing it on II?** [dApp → II](#dapp--ii) →
  [II's state model](#iis-state-model-stateless-for-campaigns) →
  [Delivering to devices](#delivering-to-devices) →
  [Push must never degrade authentication](#push-must-never-degrade-authentication).
- **Integrating a dApp?**
  [dApp developer integration](#dapp-developer-integration) →
  [The Candid interface](#the-candid-interface) →
  [The dApp-side client library](#the-dapp-side-client-library).
- **Wondering what exists today?** [Status](#status), at the end.

## Context and scope



A dApp on the IC that wants to reach a user who is not currently looking at it
has no good option. Web Push is the browser-native answer, but doing it yourself
means prompting for your own notification permission, running your own service
worker, holding your own VAPID keypair, and storing your own subscription
endpoints — per dApp. Browsers increasingly bury that permission prompt, users
decline it more often the more times they see it, and on iOS Safari it requires
the site to be installed as a PWA at all. The result is that almost no dApp does
it, and users get nothing.

Internet Identity is already the one origin every user of these apps has a
relationship with, and already holds per-user state they have consented to. That
makes it the one place where the permission can be asked **once** and reused.

So: II hosts a single Web Push pipeline that any dApp can send through. The user
grants notification permission to `id.ai` once, subscribes per device, and
consents per dApp; a consented dApp then delivers through II rather than running
a push stack of its own. Installing II as a PWA stays **optional** on Android and
desktop, and is **required only on iOS Safari** — where "install one app, get
every dApp's notifications" is a better trade than installing each dApp.

This document covers the design of the two paths that decide whether it scales —
**dApp → II** and **II → device** — the security and privacy model, and the open
items. It is a design document, not an integration guide: the Candid interface
and the client-library internals are here to show the design is buildable, not to
serve as reference material.

## Goals and non-goals



### Goals

- **One permission, many apps.** A user allows notifications once, for their
  identity provider, and every consented dApp reaches them through it.
- **A dApp needs no push infrastructure.** No service worker, no VAPID keypair,
  no subscription storage, no relay accounts.
- **Consent is per dApp and per device, and revocable.** Revoking one dApp stops
  its notifications immediately and affects no other.
- **A 10k-user broadcast is a normal operation**, not an incident — with the
  explicit constraint that it must never degrade authentication, which is what
  this canister actually exists to do.
- **II's durable storage does not grow with volume.** Storage is user-scoped;
  sending more notifications must not make it larger.
- **A tap lands the user where the notification was about**, in the sending app,
  signed in.
- **There is a path for apps that cannot let II read their content** — a
  messenger should not hand message bodies to its identity provider.

### Non-goals

- **Not a messaging product.** No inbox, no history, no unread state, no
  read receipts. II forgets a notification once it is handed off.
- **Not exactly-once delivery.** Delivery is explicitly at-least-once, which is
  why device-side deduplication ships with it rather than after it.
- **Not guaranteed delivery.** The relays are best-effort and a device that never
  comes online never receives; no part of this promises otherwise.
- **Not per-notification billing, in v1.** Charging senders cycles is parked, not
  designed — which has consequences for abuse limits, covered below.
- **Not a way to reach "all II users".** There is no audience except users who
  consented to a specific dApp; a sender cannot discover or address anyone else.
- **Not universal browser support.** Web Push is browser-scoped. Browsers without
  it (some vendor forks ship none) are out of reach, and that is not something
  this design can fix.
- **Not a replacement for in-app notification UI.** This reaches users who are
  away; what an app shows a user who is present is the app's business.

## How it works, in plain terms



The whole design, in six bullets. Everything after this adds detail to one of
them.

- **A dApp never talks to phones directly.** It tells II "notify these users,"
  and II handles the actual delivery. The dApp only knows its users by an
  opaque per-app id, so it can't reach anyone it wasn't given.
- **II already knows how to reach each user.** When a user turns notifications
  on, their browser hands II the keys needed to deliver to that device; when
  they sign into a dApp and opt in, II records that the dApp is allowed. So II
  keeps two small per-user facts: _which devices_ and _which apps are allowed_.
- **II seals every message.** It encrypts the text with the device's own key
  (created when the device subscribed, using standard Web Push encryption) so
  only that device can read it — Google/Apple/Mozilla just forward the sealed
  bytes and the browser decrypts them. It also signs each request so those push
  services trust it really came from II (a VAPID token, cached per push service
  so II needn't re-sign every time).
- **Big sends are streamed, not dumped.** To notify 10,000 users, the dApp uses
  a small helper **library** that feeds II the list in bite-sized batches at a
  pace II can handle. II refuses more than it can take (so nobody can flood
  it), and the library slows down when asked. The big list lives with the
  dApp — **II stores almost nothing per send**.
- **How II sends the final messages.** II hands the sealed messages to a small
  **trusted helper server (the "gateway")** that makes the many little
  per-device sends on II's behalf over ordinary internet — far cheaper and
  faster than the canister making each call itself. (Having the canister call
  each push service directly is the documented fallback, but one network call
  per device gets expensive at scale, so it isn't the default.) Either way II
  does the sealing; the helper only forwards messages it can't read.
- **Tapping a notification** goes straight to the app's own deep link, which
  signs the user in on arrival if their session has lapsed. Only a notification
  without a deep link detours through II ("Opening \<app\>…"). Either way the
  destination is only ever the app that sent it.

Everything below is the same story with the exact mechanisms and edge cases.

## Architecture



```
┌─ dApp side ────────────────────────┐        ┌─ II ────────────────────────────────┐
│ client library (durable):          │        │ Layer 1 — admission control         │
│  owns the campaign + status         │ chunk  │  per-origin bucket + global cap,    │
│  chunks (≤1000 targets)             │──────▶ │  hard caps + reject → protects II   │
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
                    deep link → dApp (signs in on arrival if the session lapsed)
                          └── none → II /notify?origin=… → consent-gated → dApp home
```

Three resources drive the architecture, and it is built to control all three:

- **In-flight HTTPS outcalls** — the binding constraint, and it is _shared_.
  The subnet allows 3000 outcalls in flight **across every canister on it**,
  and II already spends from that budget on its own authentication paths (DoH
  for email recovery, JWKS and discovery for OIDC sign-in). One naive blast is
  ~13k outcalls, which would starve login. So delivery is batched through the
  gateway: ~25 outcalls instead of ~13k.
- **Stable storage** — limited on every deployment (500 GiB per canister;
  512 MiB touched per message, 2 GiB per upgrade) and it must not grow with how many
  notifications are sent. So the durable campaign lives in the dApp's client
  library and II keeps only user-scoped data plus a small transient working
  set.
- **Instructions per message and per round** — the crypto is _not_ free at
  chunk scale (see the sealing budget below), and II serves every login on the
  network from this same canister. So the drain works in bounded slices rather
  than whole chunks.

Cycles are a real cost too, but how much they matter depends on the deployment
— see [Deployment assumptions](#deployment-assumptions). The three limits above
bind everywhere regardless of who pays.

## Deployment assumptions



Numbers in this doc depend on where II runs, so state the deployment before
quoting a cost:

- **Canonical II** (`rdmx6-jaaaa-aaaaa-aaadq-cai`) runs on subnet `uzr34…`,
  which is a **system subnet with 34 nodes**. On a system subnet the IC waives
  execution, ingress, xnet, storage and HTTPS-outcall fees, so those are
  effectively free _for this deployment_. Chain-key fees (threshold ECDSA,
  vetKD) are **still charged**.
- **Any other deployment** — a self-hosted II, a fork, or a test network —
  runs on an application subnet where **every fee applies**. This is a
  first-class case: the design must be viable when cycles are really being
  spent, and a self-hoster needs the cost model to budget with.
- **Capacity limits apply everywhere**, priced or not: the 3000 in-flight
  outcall cap, per-message and per-round instruction limits, the 500-deep
  output queue per canister pair, 2 MB message ceilings, and the 30 s outcall
  timeout.
- **Outcall replication.** A replicated outcall is executed by **every node**,
  so the target receives _n_ requests, not one (n = 34 on II's subnet). This
  matters for both cost and correctness — see
  [Delivering to devices](#delivering-to-devices).

Rule of thumb for this doc: design to the **paying** case and treat the fee
waiver as a property of one deployment, not a premise of the design.

## The user experience



### Turning notifications on (first time signing into a dApp)

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
   6a. **Enable** → if this browser has not granted permission yet, it shows
   its **native permission prompt** ("`id.ai` wants to show notifications —
   Allow / Block"). This dialog is the browser's own and cannot be restyled.
   On **Allow** — or straight away, when permission was granted earlier — II
   subscribes the device and records consent for this dApp, then redirects to
   the dApp. No app install is required (Android/desktop); iOS Safari is the
   exception.
   6b. **Maybe later** → II redirects straight to the dApp; nothing is enabled.

Two separate things are being asked, and only one of them repeats.

II's opt-in screen **is** shown for each new dApp, because consent is recorded
per origin and no dApp inherits another's. It appears once per dApp per
device: a device that has not subscribed yet sees it again even for a dApp
already enabled on another device, with copy that asks about this device
rather than repeating the original pitch.

The **browser's** permission prompt is the part that does not come back. It is
granted to `id.ai`, not to the dApp, and every dApp's notifications are
delivered through that one origin — so once the user has allowed it, the
browser has nothing left to ask, and later opt-ins complete without a second
dialog.

### What happens when a notification is sent

7. The dApp's backend tells II to notify the user. At any real scale this runs
   through the dApp's client library and the chunked `push_send` endpoint (see
   below); the PoC has a simpler one-shot `notify_user` for a single recipient.
8. The notification arrives on every device the user enabled — **even with the
   tab closed / browser not running** (on Android). It shows the dApp's origin
   as the source and the dApp's title/body as the text.
9. The user **taps** it. When the dApp supplied a deep link — the normal case —
   the service worker opens that URL directly and II is not visited at all. The
   dApp lands the user on the page the notification was about, signing them in
   on the way with the ICRC-167 top-level redirect if the session has lapsed,
   which is the usual state when a notification is what brought them back.
   Only a notification with no deep link falls back to II's `/notify` screen
   ("Opening `<dApp>`" with the app's logo), which resolves the sender's origin
   behind a consent gate and forwards to the dApp's home.

### Managing them, and turning them off

10. Either from the browser or from II.

    In **II → Settings**, the user sees **Notifications on this device**
    (a toggle to turn the whole device on/off) and **Allowed apps** — every
    dApp that can notify them, each with a remove button. Revoking an app
    stops its notifications immediately.

    The **browser's own site settings** can also block notifications for
    `id.ai`, which silences every dApp at once and cannot be overridden from
    inside II — the permission belongs to the browser, not to us. II can only
    observe the result: a blocked permission makes the opt-in screen
    unofferable, so it is skipped rather than shown as a button that cannot
    work. Re-enabling has to happen in the browser too; that is the one path
    II cannot offer a control for.

## dApp → II



### Sending to thousands of users: chunked send + two-layer flow control

> _In short: the dApp streams the audience to II in small batches ("chunks");
> II refuses more than it can handle, and the dApp slows down when told to.
> Two separate safeguards — one on each side — keep this safe and smooth._

A campaign of any size is delivered as a series of **bounded chunks** — a chunk
is just a bite-sized batch of targets (≤ ~1000 targets, and under the message
size ceiling). This is deliberate: II must never be asked to hold a whole
campaign, because that is the storage that scales with volume. Two independent
control layers make this safe and smooth — and they must not be confused:

- **Layer 1 — II admission control (mandatory, adversarial).** This is the
  security boundary and assumes a hostile client. II enforces a hard
  `recipients.len()` ceiling (see below), a per-origin token bucket
  (notifications-per-window) plus a global cap on its transient buffer. Over
  capacity, `push_send` **rejects the chunk** (`ready = false` +
  `retry_after_ms`). The guarantee: **II never holds more than its bounded
  buffer, no matter what the client does**. This is the anti-spam property, and
  it lives entirely on II.

  Two honest caveats about how cheap that reject is:
  - The **policy decision** is O(1), but it happens _after_ the message has been
    admitted into a block and its payload Candid-decoded. A rejected 2 MB chunk
    still costs a consensus round and a 2 MB decode, so absorbing a flood is
    cheap _per decision_, not free. Cap the accepted message size well below
    2 MB so the pre-decision cost is bounded too.
  - **`inspect_message` cannot help here.** It is not invoked for
    inter-canister calls, and `push_send` is called by dApp _canisters_. It
    only ever filters ingress. Any claim that push is rate-limited via
    `canister_inspect_message` is wrong.

  **The ≤1000-recipient bound is enforced server-side**, before any
  per-recipient work. It is not a client obligation: 2 MB of `PushRecipient`
  with empty overrides is ~65,000 recipients, and doing per-recipient index and
  consent lookups for that many in one message would exceed the instruction
  limit and trap. The client-side chunking in the library is an _optimization
  over_ this hard limit, never a substitute for it.

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

The full interface — `push_send`, the alert and delivery types, the result and
rejection variants — is in [push-api.md](push-api.md#the-candid-interface). It is
reference material rather than design argument, so it lives beside the
integration guide. What matters here is the shape it implies, covered above and in
[II's state model](#iis-state-model-stateless-for-campaigns).

### How II knows which users to send to

The dApp only knows its users by their in-app principal (II's privacy model).
`PRINCIPAL_INDEX` resolves `principal → (anchor, origin_hash)`, and the index
entry **pins the origin** — a principal belonging to another dApp's consent
resolves to a different `origin_hash` and is rejected. dApp A physically
cannot target dApp B's users even with stolen principals. Audiences larger
than one chunk are split across paced `push_send` calls by the client library,
not by server-side routing.

### How II verifies the sender is really that dApp

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

> _In short: II tracks how much each app is sending and simply says "not right
> now, try again in N ms" once an app goes over its share. This is what stops
> spam, and it works even if the sender ignores every hint._

The mandatory guard, assuming a hostile client. Enforced on every `push_send`,
independent of client behavior:

- **Hard `recipients.len()` ceiling** (~1000), checked before any per-recipient
  work, and a hard accepted-payload cap well below 2 MB.
- **Per-origin token bucket** denominated in **device-messages, not
  recipients** — a recipient with five devices costs five sealed messages and
  five outcall payloads, so counting recipients under-charges the real work.
  Denominated per notification rather than per call, so it can't be gamed by
  slicing a send into many small chunks.
- **Bucket by registered operator (eTLD+1), not by bare origin.** An origin is
  scheme+host+port and registration only requires serving a `.well-known`
  file, so `a1.evil.com … a1000.evil.com` would otherwise be 1000 origins with
  1000 full buckets. Per-origin buckets bound one _origin_, not one _operator_.
- **Global cap** on the in-flight buffer — protects II/the subnet as a whole —
  **with per-origin reservations underneath it**, so a large sender at its own
  limit cannot consume the entire global budget and starve small senders. A
  first-come-first-served global cap does not deliver the "can't starve others"
  property on its own.
- **A push-specific outcall budget** well below the subnet's 3000 in-flight cap,
  yielding to II's own authentication outcalls. See
  [Push must never degrade authentication](#push-must-never-degrade-authentication).
- **Reject** when over capacity (`ready = false` + `retry_after_ms`, jittered
  per caller so rejected clients don't retry in lockstep).

Because the queue lives on the client, the only II storage a sender can
pressure is the bounded in-flight buffer — which admission control caps
directly. That, plus the fast drain (below), is what lets II sustain high
throughput on a _small_ buffer.

### What this costs, and who pays

Costs split into what is _always_ scarce and what depends on the deployment
(see [Deployment assumptions](#deployment-assumptions)).

**Always scarce, on every deployment:**

- **In-flight outcall slots** — 3000 subnet-wide, shared with II's own login
  outcalls. The reason the gateway exists.
- **Stable storage** — kept **O(users × origins)**: subscriptions are per
  `(anchor, endpoint)`, consent and the principal index per `(anchor, origin)`.
  Never O(notification volume), because campaign state lives in the client
  library. That is the invariant the "stateless II" shape buys; see the
  [bytes-per-user table](#what-this-actually-costs-per-user).
- **Instructions** — sealing is not free at chunk scale; the drain must work in
  bounded slices.

**Cycles — real, but deployment-dependent.** On canonical II (system subnet)
outcall, execution and storage fees are waived. On any application subnet
(self-hosted, fork, test net) they are fully charged, and the formula is:

```
replicated     = (3_000_000 + 60_000·n)·n              ← per-call base
               + (400·request_bytes + 800·max_response_bytes)·n

non-replicated =  3_000_000 + 60_000·n                 ← base, one node only
               +  400·request_bytes + 800·max_response_bytes
```

The whole `·n` disappears from the per-byte terms, which is where the money is.
At n = 34 replicated: base 171.4 M, 13,600 per request byte, 27,200 per reserved
response byte. Non-replicated: base 5.04 M, 400 and 800. Worked example, a
10k-user broadcast (~13k device-messages) on a 34-node **application** subnet:

| Path                                       | Outcalls | Cycles  | ≈ USD  |
| ------------------------------------------ | -------- | ------- | ------ |
| Direct, one per device, replicated         | ~13,000  | ~2.8 T  | ~$3.80 |
| Gateway, ~2 MB batches, replicated         | ~25      | ~0.7 T  | ~$0.95 |
| **Gateway, ~2 MB batches, non-replicated** | ~25      | ~0.02 T | ~$0.03 |

Three things to read off that table. Batching alone cuts outcall **count** ~500×
but **cycles only ~4×**, because the per-byte fee is charged on the same sealed
bytes either way — batching amortizes the base fee, not the payload. Dropping
replication is what actually cuts the payload cost, by the full factor of n
(~34×). And `max_response_bytes` is charged whether used or not, so always set it
tight.

Note what this does to the gateway's rationale. Once the batch handoff is
non-replicated the cycle saving is ~44× against direct-replicated, but the honest
justification is still **in-flight outcall slots and drain latency** — 13k
concurrent outcalls would starve login on a subnet capped at 3,000 in flight,
and that is a scheduling limit no pricing change touches.

One cost belongs to a variant not described yet, and it is large enough to
mention here rather than let it surprise anyone later. Everything above assumes
II can read a notification's text, because it does the sealing. Some apps cannot
accept that — a messenger should not hand its message bodies to II — so
[End-to-end-encrypted apps](#end-to-end-encrypted-apps) sets out a variant where
the content is encrypted such that only the user's own device can read it, and II
forwards bytes it cannot see. Doing that needs a per-user key, and the IC's way
to derive one is **vetKD**.

**vetKD is charged on every deployment**, system subnets included:
`vetkd_derive_key` is ~26.2 B cycles (~$0.036) per call. That is fine once per
user and ruinous per message: a derive _per notification render_ would be ~$357
per 10k blast. So that design must derive **once per `(user, origin)`** and cache
the result in the service worker, with a per-anchor rate limit. And the bill lands
on II, not the app — the service worker calls in over ingress, which carries no
cycles — so an uncached derive is a **user-triggerable drain**, not merely an
inefficiency.

On charging senders: **no cycles charging in v1.** Cycles can only be attached
by canister senders, and per-notification fees bring real complexity
(accept/refund, fee tuning). Prepaid-balance or attached-cycle pricing is parked
as a future exploration. But note what does _not_ follow from deferring
charging: a rate limit multiplied by time is unbounded spend, so a deployment
that pays fees needs a **cycle budget with a circuit breaker** independent of
the rate bucket — see
[Operating it](#operating-it-controls-alerts-and-rollout).

## II's state model: stateless for campaigns



> _In short: II accepts a chunk, briefly holds it in memory (not durable
> storage), seals and sends it, then forgets it. The durable list lives with
> the dApp, so II's storage doesn't grow as more notifications are sent._

II holds **no campaign queue in stable (durable) memory**. `push_send` is cheap:
check the recipient-count ceiling, authenticate the sender, dedup `chunk_id`, run
admission control (Layer 1), per-target resolve + consent-check, then admit the
survivors into a small **transient heap buffer** and return
`{admitted, rejected, ready, retry_after_ms, drain_epoch}`.

It does not wait for delivery, and cannot: sending a whole chunk inline would
need ~1000 × devices seals and outcalls in one message. The binding limits there
are the **500-deep output queue** to the management canister (which caps
concurrent outcalls per message) and the subnet's 3000 in-flight cap — with
instruction cost making a whole chunk a large fraction of an execution round on
a canister that also serves every login. Hence the bounded-slice drain below.

A timer (`ic_cdk_timers`, ~1s) drains the heap buffer:

- **Claims** the slice it is about to work on before the first `await`, marking
  those entries in-flight. Every `await` ends a message execution and lets the
  next tick interleave, so without a claim step two ticks send the same entries
  twice. A `draining` flag makes ticks non-reentrant. This is not optional:
  `ic-cdk-timers` documents that under load "timeouts may result in duplicate
  execution", and the drain is not idempotent.
- Works a **bounded slice** (target 100–300 device-messages, tuned from a real
  measurement), not a whole chunk.

  What costs instructions is **RFC 8291 encryption, and within it the elliptic
  curve**: per device-message II generates an ephemeral P-256 keypair and does an
  **ECDH** against that device's `p256dh`, so two scalar multiplications — one
  fixed-base for the ephemeral public key, one **variable-base** for the shared
  secret, which is the expensive one because the device's key differs every time
  and nothing precomputes. HKDF-SHA256 and AES-128-GCM beside it are noise. The
  RFC 8292 VAPID **signature** is not part of this budget at all: it is per
  audience and cached up to 12h, so it amortises to nothing however many messages
  are sent. Nor can the ECDH be amortised — RFC 8291 has no encrypt-once mode, so
  the cost is strictly per device.

  Two different ceilings follow, and only one of them is the platform's. The slice
  is kept to a small fraction of a message's instruction budget because this
  canister serves every login on the network, and spending most of a round sealing
  notifications surfaces as login latency — see
  [Push must never degrade authentication](#push-must-never-degrade-authentication).
  The per-message instruction limit sits well above that as a hard wall, and
  crossing it is worse than slow: the tick traps, the trap rolls back the tick's
  heap mutations, and the same slice is retried and traps again — a permanently
  wedged drain rather than a degraded one. That asymmetry is why the slice must be
  set from a measured per-seal cost rather than an estimate.
- One `raw_rand` per tick; ChaCha20 derives per-message ephemeral seeds. (Note
  the per-tick seed means the subnet's random tape, if observed, reveals that
  tick's ephemeral scalars — see the [security model](#security-model).)
- Resolves each anchor's devices **now** (they change between admit and drain)
  and re-checks consent (may have been revoked since). Device lookup must be a
  **prefix range scan** over `(anchor, endpoint_hash)`, never a filtered scan of
  the whole map — the latter is O(all users with push enabled) per notification
  and traps once the map is large.
- Forces attribution to the sender origin (unspoofable).
- RFC 8291 encrypts per device; attaches the per-audience VAPID JWT
  (cached, ≤ 12h).
- Assigns a `msg_id` per admitted message, stable across retries, for
  device-side replay/duplicate suppression. Not optional — see
  [Duplicate and replay suppression](#duplicate-and-replay-suppression-msg_id).
- Drains to the gateway in a few batched outcalls (see below). A fast drain is
  what lets the buffer stay small while admitting at high throughput.
- **Isolates per-entry failure.** A trap inside the drain rolls back the tick's
  heap mutations, leaving the buffer unchanged — so the next tick reprocesses
  the same poison entry and traps again, forever, with `ready = false` returned
  to _every_ origin. One malformed row would take the whole feature down until
  an upgrade (which then silently discards the buffer). Each entry needs its own
  failure boundary, an attempt counter with drop-and-count, and a watchdog that
  clears the buffer if depth stays pinned across N ticks.

`PushDelivery` integrates into the buffer, not just the headers:

- **topic** — key the buffer entry by `(origin_hash, anchor, topic)`; an
  un-drained entry with the same key is **replaced**, collapsing rapid updates
  (savings scale with buffer depth). The relay collapses in-flight duplicates
  too.
- **ttl_seconds** — at drain, if `admitted_at + ttl` has passed, **skip** the
  outcall (the relay would drop it anyway).
- **urgency** — sets the RFC 8030 header, and contributes to drain order as
  **one input among several**, never as the sole key. Ordering is: round-robin
  across origins first, then within an origin's slice by urgency combined with
  age and remaining TTL. Urgency is sender-supplied and unvalidated — every
  sender will set `High` (both examples in this doc do) — so if it ordered the
  whole buffer, one sender pinning `High` would starve every other origin's
  `Normal` traffic indefinitely. Origin fairness outranks it; age and TTL stop
  anything from sitting at the tail forever.

**Durability lives on the client — but the client needs a signal to act on.**
The in-flight buffer is heap, not stable memory, so it costs no persistent
storage and is lost on upgrade. The intent is that the client library re-sends
anything unacknowledged. **That requires an acknowledgment that today's
interface does not provide:** `admitted` means "accepted into the buffer", it is
returned _before_ any upgrade that would lose the buffer, and delivery itself
carries no receipts. A client that received `admitted: 1000` therefore has no
way to learn the messages were dropped and, by its own retry rule, never
re-sends — so an upgrade mid-campaign silently loses every in-flight chunk while
the campaign store marks them sent.

Close it with a **`drain_epoch`** in `PushResult` (a counter bumped in
`post_upgrade`) plus a `push_chunk_status(chunk_id)` query: a client that sees
the epoch move re-sends anything it hasn't confirmed drained. This makes
delivery explicitly **at-least-once**, which is why `msg_id` dedup ships with
it rather than after it — see
[Delivery semantics](#delivery-semantics-what-is-actually-guaranteed).

Consequently the only new **stable** regions are user-scoped and
volume-independent: the sender registry (`OriginSha256 → sender`) alongside the
subscription and consent maps. `chunk_id` dedup is a heap set — **bounded by
count with LRU eviction**, and `chunk_id` fixed at 16 bytes, so a sender cannot
grow it with attacker-chosen keys until the heap is exhausted (heap exhaustion
traps the canister, which takes authentication down with it). Timers don't
survive upgrades — re-arm in `post_upgrade` and `init`.

### Buffer size, and how many origins it serves at once

**Sizing one entry.** A buffer entry is per _recipient_, not per device — devices
are resolved at drain, not at admit. Recipient-scoped fields are small: anchor
(8 B), interned origin (4 B), `topic` (≤ 32 B), `msg_id` (16 B), `admitted_at`
(8 B), `ttl` (4 B), urgency and attempt counter (2 B). Call it **~200 B** with
Rust `String`/collection overhead.

The text is the variable. `PushTarget.alert` is an _override_ that falls back to
`default_alert`, so a broadcast stores one copy of title + body + url for the
whole chunk and entries point at it. Fully personalized sends cannot: `title`
(≤ 64 B) + `body` (≤ 256 B) + `url` (~256 B) plus string headers lands each entry
near **~1 KB**. That five-fold difference is a reason to keep `default_alert` the
common path, not merely a convenience.

**Memory is not the constraint — by three orders of magnitude.** The heap ceiling
is 4 GiB. A full 1,000-recipient chunk is ~200 KB broadcast, ~1 MB personalized.
Even a hundred concurrent full chunks — 100,000 recipients in flight — is ~20 MB
broadcast or ~100 MB personalized. Nothing about that is close to a limit.

**Drain rate is the constraint.** At the drain's own target of 100–300
device-messages per ~1s tick, take 200/s. With ~2 devices per user that is ~100
recipients/s, and everything downstream follows from it:

| Work                            | Device-messages | Time to drain at 200/s |
| ------------------------------- | --------------- | ---------------------- |
| One 1,000-recipient chunk       | ~2,000          | ~10 s                  |
| One 10k-user blast (10 chunks)  | ~20,000         | ~100 s                 |
| 5 origins blasting 10k at once  | ~100,000        | ~8 min                 |
| 10 origins blasting 10k at once | ~200,000        | ~17 min                |
| 50 origins blasting 10k at once | ~1,000,000      | ~83 min                |

The rate is a fixed pie shared by every origin: II drains 200/s in total, not per
sender. So concurrency does not degrade gracefully into slower delivery for the
noisy origin — it degrades into slower delivery for _everyone_, which is why
Layer 1 has to divide the rate rather than merely cap the buffer.

**Which fixes the buffer size.** Depth should be the drain rate multiplied by the
worst in-buffer latency worth accepting, not whatever the heap allows. Capping
queue latency at 60 s gives 200/s × 60 = 12,000 device-messages ≈ 6,000
recipients ≈ **~6 MB personalized, ~1.2 MB broadcast**. A buffer sized to memory
instead would happily accept _hours_ of backlog and report `admitted` for
messages whose `ttl` will expire before they are sent — the failure mode is a
lying success, not an out-of-memory.

So express the admission ceiling in **seconds of drain backlog**, with per-origin
fair share as `total_rate / active_origins`; and set `ttl_seconds` defaults with
the queue depth in mind, since a 60 s queue against the ~4 h default TTL is
comfortable while a 1 h queue is not.

**Everything above scales linearly with one unmeasured number.** The 200/s slice
is this design's own target, not an observation. What has to be measured before
these figures mean anything: instructions for one RFC 8291 seal (the P-256 ECDH
dominates), how large a slice fits in a safe fraction of an execution round on a
canister that also serves every login, and whether the ~1s timer holds under
load. Measure those three and every row in that table moves together.

### The way to lift that ceiling is a separate canister

Note what the 200/s is actually a limit on. It is not the crypto and it is not the
platform — it is **co-tenancy with authentication**. The slice is small because
this canister serves every login on the network, so the only reason push cannot
spend its whole execution round sealing is that logins are sharing that round.

Which suggests the structural fix: run push in its **own canister**. It could then
spend a full round on sealing — plausibly the same order as the 10–20× headroom
between the self-imposed budget and the instruction limit — while keeping the
trust boundary exactly where it is today. Still an IC canister under the same
controllers, still no plaintext leaving the platform, so this buys throughput
without the concession that moving sealing to the gateway would demand. It also
turns [Push must never degrade authentication](#push-must-never-degrade-authentication)
from a discipline that has to be maintained into a property that holds by
construction.

The real cost is state ownership. Subscriptions and consent are anchor-scoped, so
a push canister has to **own** those maps rather than calling II per send —
otherwise the load just moved back, with inter-canister latency added. Then
`/authorize`'s opt-in becomes one inter-canister write at consent time, which is
rare and cheap, and the service worker still lives on II's origin because that is
where the notification permission is granted. What is genuinely new is
two-canister upgrade coordination, and deciding where the VAPID key lives.

Not proposed as the shipping path here, because it changes which canister holds
user-scoped state and that deserves its own review. But it is the honest answer to
"the throughput number looks low", and it is a better answer than moving crypto
off-platform.

## Delivering to devices



The relay API is one POST per subscription endpoint (RFC 8030) — there is no
multi-recipient send, so reaching N devices is fundamentally N sends. The
design routes those sends through a **trusted web2 gateway**. Doing them as
per-device outcalls straight from the canister ("direct") is the documented
**alternative** — kept for context and as a possible low-volume or
fully-on-chain fallback, but not the shipping path.

### First, the constraint that shapes both paths: outcall replication

A replicated HTTPS outcall is executed by **every node in the subnet** — 34 on
II's. The target sees 34 requests, and the replicas' responses must be
_byte-identical_ or response-consensus fails (the call then errors while still
being charged). Three consequences the rest of this section is built around:

- **Duplicate delivery is the default, not an edge case.** RFC 8030 has no
  idempotency key, so 34 POSTs of the same push are 34 distinct messages to the
  relay and up to 34 banners on the device. `Topic` collapses _undelivered_
  duplicates, so an offline device may collapse to one — an online device does
  not. This is why `msg_id` + service-worker dedup is a v1 requirement rather
  than a later hardening step.
- **Per-device status cannot survive consensus.** The 34 POSTs legitimately get
  _different_ answers (the first may get `201`, later ones `429`, or `410` if the
  subscription just died). No transform can manufacture agreement about which
  POST was first; a transform can only collapse everything to one deterministic
  value. So `410`-driven subscription cleanup is **not implementable under
  replicated outcalls** — which is one of the reasons the next bullet is the
  design, not a future option. See
  [stale-subscription cleanup](#stale-subscription-cleanup).
- **So II's outcall to the gateway is non-replicated, and that is the design.**
  `is_replicated = false` (management-canister interface, on mainnet since
  2025-08-04) has **one** node make the request: no consensus on the response, no
  transform, no 34× fan-out, and roughly two orders of magnitude cheaper. The IC
  docs recommend it precisely for rate-limited APIs, which is what Web Push is.
  Applied to the one call II actually makes — the batch handoff to the gateway —
  it removes all three consequences above: the gateway receives one copy of each
  batch instead of 34, needs no idempotency window, and may return real per-device
  status instead of a deterministic ack.

  Two things this does not buy. The flag is marked **experimental** and its API
  may still change, so the interface should be isolated behind one call site that
  can be reverted to replicated with a transform. And a single node's reply is
  **not consensus-verified** — for a trusted gateway that is no new concession,
  since a dishonest gateway could always lie about delivery, but it does mean
  status coming back is evidence rather than proof. See
  [IC capabilities to re-evaluate](#ic-capabilities-to-re-evaluate).

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

  It is worth being precise about why offloading the sealing is not merely
  disallowed but useless, because the instinct to move expensive work to a server
  with no instruction limit is a good one. The cost splits in the wrong place: the
  **ECDH that derives the content key is the expensive half, and the AES-128-GCM
  that uses it is noise**. Deriving the key and being able to decrypt are the same
  capability, so the only part of the sealing that can be handed to the gateway is
  the part that was already free. It would also defeat
  [End-to-end-encrypted apps](#end-to-end-encrypted-apps) outright: that design
  exists so *II* cannot read content, and a web2 relay holding plaintext is
  strictly worse than the thing it was built to avoid.
- The gateway holds **no keys, no subscriptions, and no plaintext at rest** — it
  sees in-transit ciphertext, endpoints and timing. Because II sends each batch
  with a **non-replicated** outcall (below), the gateway receives one copy rather
  than 34 and needs no idempotency window to collapse them, so it is genuinely
  stateless. Under replicated outcalls it would have had to keep a short window
  keyed on batch id, and "stateless" would have been the wrong word.
- II batches sealed bundles into **~1.5 MB** chunks: ~25 outcalls instead of
  ~13k. The 2 MB outcall ceiling counts **headers too**, so targeting exactly
  2 MB leaves no room for the VAPID `Authorization` headers each bundle carries.
- II authenticates to the gateway with a bearer token (IC outcalls have no stable
  source IP — a non-replicated request comes from whichever node executed it, and
  IPv4 destinations go via a shared proxy pool). Because the response is not put
  to consensus, it does **not** have to be deterministic — so per-device results
  _can_ come back through this channel, which is what makes `410`-driven
  [stale-subscription cleanup](#stale-subscription-cleanup) implementable. Trust
  that status only as far as the gateway is already trusted: it is a single
  unverified reply, so treat a reported `410` as grounds to retire a subscription
  opportunistically, never as an authority to delete user state on one word.
- **The gateway sets II's admission capacity.** Throughput is bounded by how fast
  the small buffer drains; a batched drain empties it quickly, so II recovers
  capacity and admits the next chunk sooner. Fast drain → high sustained
  throughput on a _small_ buffer → flat storage.

**Why the gateway, stated correctly.** Its justification is **in-flight outcall
slots and drain latency — not cycles.** Direct delivery would spend ~13k of the
subnet's 3000 shared in-flight slots' worth of work per blast, starving II's own
authentication outcalls; the gateway needs ~25. On a paying deployment it also
saves cycles, but only ~4× (the per-byte fee applies to the same sealed bytes
either way) — not the "cents" a naive call-count comparison suggests. The slot
argument holds on _every_ deployment, including the fee-waived one, which is why
it is the real reason.

Trust delta — the deliberate cost of this path. The gateway can **drop, delay,
or replay** bundles. It **cannot read content**. But it **can forge sends**: each
bundle carries a VAPID `Authorization` token, relays authenticate on that token
alone and never validate the body, and the token is cached for up to 12 h. So a
compromised gateway can harvest `(endpoint, token)` pairs and POST arbitrary
bytes to those devices for the token's lifetime — including after being rotated
out of the path. Those bytes won't decrypt, so the browser shows its generic
"site updated in background" notification attributed to `id.ai` — which at volume
burns the shared notification permission (see
[shared fate](#shared-fate-one-permission-for-every-dapp)). Mitigations: scope
credentials **per batch** so they expire with it, cut the JWT cache to minutes,
and note that the bearer token in canister state is extractable by node
operators, so this replay capability is not the gateway operator's alone.

Only **liveness and this bounded forgery window** move off-chain;
confidentiality stays on. Which also means the gateway is a **single point of
failure**, and the doc must say what happens when it fails — see
[Operating it](#operating-it-controls-alerts-and-rollout).

### The alternative: direct per-device outcalls

II could instead make one HTTPS outcall per device straight to the push service
— fully on-chain, nothing extra to run or trust. Why it isn't the default:

- **It would consume the shared in-flight outcall budget.** ~13k outcalls per
  10k-user blast against a 3000-slot subnet-wide cap that II's own login paths
  draw on. This is the disqualifying reason.
- **Cost** on a paying deployment: ~2.8 T cycles (~$3.80) per blast at n = 34,
  versus ~0.7 T through the gateway.
- **Replication makes it worse, not just slower**: 34 POSTs per device, and no
  usable per-device status (above).

It stays viable for low volume or a deliberately fully-on-chain deployment. Note
that "fallback if the gateway is unavailable" is only true with caveats: under
replicated outcalls the direct path delivers duplicates and cannot observe
`410`, so failing over to it is a degraded mode, not a transparent one.
**`is_replicated = false` would change this assessment substantially** — it
removes the fan-out and restores per-device status, making direct delivery a
genuine peer of the gateway on correctness, still bounded by in-flight slots.

## The dApp-side client library

A dApp does not call `push_send` in a loop; it drives II through a small client
library that owns pacing, retry and durable campaign state — which is what keeps
II's own storage flat. Its design, the send loop, the state it must keep and the
failure modes it owns are specified in
[push-client-library.md](push-client-library.md).

The one point that belongs in this document: the library is where the durable list
lives, which is what lets II stay stateless per send. See
[II's state model](#iis-state-model-stateless-for-campaigns).

## On the device: rendering and tap-through



- **Subscribe** (Settings, or the `/authorize` opt-in): request permission,
  `pushManager.subscribe` with II's VAPID public key, store
  `(anchor, endpoint_hash) → {endpoint, p256dh, auth}`. A browser allows one
  subscription per service worker, permanently bound to the key it was created
  with, so `subscribe` **refuses a different key**: an existing subscription is
  reused when its key still matches and replaced when it doesn't. Otherwise
  every browser that subscribed under a previous VAPID key could never
  re-enable. No
  PWA install is needed on Android or desktop — this works in a plain tab;
  iOS Safari is the exception (it only allows Web Push for an installed
  home-screen app). The optional install adds an app icon / standalone window
  and slightly better attribution.
- **Consent** (`/authorize`): `push_grant_consent(anchor, origin)`. Asked once
  per `(identity, origin)` **per device** — the answer is remembered locally,
  which matches what it commits to, since a subscription belongs to one browser.
  Consent itself is shared by all of the identity's devices, so on a second
  device only the subscription is missing and the screen says so ("Also notify
  you on this device?") rather than repeating the first-run ask.
- **Render**: the service worker branches on `content` — `Display` shows the
  supplied `title`/`body`; `Hidden` shows an II-controlled generic string
  keyed by `category` ("New message from `<origin>`"), never app-supplied text.
- **Click**: with a deep link, the service worker opens it **directly**. II
  validated at send time that `alert.url` is on the consented origin (see
  [the deep-link question](#where-a-notification-opens)),
  so there is nothing left to check on the device — and routing through II
  would add a second visit in the middle of a journey the user expects to be
  one step. Without a deep link it opens `/notify?origin=<sender>`, which
  resolves the sender's own origin, shows which app is opening, and fails
  closed if it cannot verify the sender.

  For `Hidden` (E2E) notifications this tap-through is also the content-reveal
  path: the app opens and decrypts the message in its own context.

### Shared devices and multiple identities

A browser has **one** physical push endpoint, but a person may sign into
several identities on it and devices get shared. The rule is: **enabling and
consent are per identity, never per device.** Turning notifications on for one
identity never implies anything for another that happens to share the same
browser — each identity must separately enable notifications and grant its own
per-origin consent. We do not infer consent from a shared endpoint.

What that means in practice:

- **One endpoint, independent rows.** The subscription is stored as `(anchor,
endpoint_hash) → {endpoint, p256dh, auth, created_at}` — the endpoint URL
  itself is part of the value, since it is the POST target — so the same
  physical endpoint can appear under several anchors as separate rows. II
  delivers to an anchor's row only if _that_ anchor enabled it; identity B's
  senders reach the device only after B itself opted in.
- **Isolation is at the consent layer, not the transport.** Once two identities
  on the device have both enabled, the device physically receives pushes for
  both — there is only one endpoint, so they cannot be separated in transit.
  The service worker renders by **sender origin**, not by which II identity, and
  never reveals that the two identities live on the same device.
- **Disabling is per identity.** Turning notifications off for one identity
  removes only that anchor's rows; the browser subscription stays alive for the
  others. The SW calls `pushManager.unsubscribe()` only when **no** identity on
  the device still wants notifications.
- **Rotation re-registers lazily.** When the endpoint rotates
  (`pushsubscriptionchange`), every anchor that held the old endpoint needs its
  row updated, but the SW can only act for the currently-signed-in identity —
  so each identity re-registers the next time it authenticates.

### Where a notification opens

The app may set `alert.url` to send the user to a specific page rather than the
origin root. **II validates it at send time** and refuses the send otherwise:

- **The target must be on the sender's own application.** The sender origin is
  II-derived, not a field on `PushAlert`: the backend forces it to this sender's
  consented origin, so a dApp cannot set or spoof it. A notification can
  deep-link anywhere within the sender's own site
  (`https://app.com/thread/42`) but never to another origin — not `evil.com`,
  and not another consented dApp.
- **Both sides are canonicalised** before comparing, because consent is recorded
  against the _effective_ origin (already remapped to the legacy boundary
  domain) while a dApp's links use whichever domain the user is browsing.
  Without that, every real deep link is refused. It stays safe rather than
  loose: only the same subdomain collapses, and that subdomain is the canister
  id, so two different canisters can never normalise to one origin.
- **Only `https`, or `http` on loopback** — the secure-context rule browsers
  use. `javascript:` and `data:` URLs report their origin as the string
  `"null"`, so without a scheme check two of them compare equal, pass an
  origin test, and reach a navigation.

Validating on the send side rather than on the device is what lets the service
worker open the link directly, removing a hop from the tap-through. It is also
simply the better place: this is the only party that knows authoritatively which
origin the user consented to, whereas the device-side check sat on a publicly
craftable URL and every future consumer of `alert.url` would have had to repeat
it.

No target → the tap opens `/notify?origin=<sender>`, which resolves the sender's
origin from the anchor's consent list and fails closed if it cannot.

### Landing the user already signed in

Yes — and it needs **no II-side changes**, because it composes out of the
ICRC-167 URL transport (top-level redirect sign-in) that II already supports.

**The constraint that shapes it: II cannot push a signed-in session.** A
delegation is bound to a session public key the _dApp's client_ generates, and
II never sees the private half, so it cannot mint one unprompted. In the URL
transport the request always arrives _at_ II (`message` + `callback` + `state`
in the hash) and `channel.origin` is the callback's origin — the RP initiates,
II only answers. So the notification's job is not to carry a session; it is to
land the user on a page that _starts_ sign-in immediately.

The standard guarded-route pattern does exactly that:

1. The notification deep-links to a restricted page,
   `https://app.com/thread/42`.
2. That page checks `authClient.isAuthenticated()` on load; if not signed in it
   redirects to `/sign-in?next=/thread/42`.
3. `/sign-in`, **on page load** (not behind a click — the redirect unloads the
   page), memoizes `next` and calls `signIn({ transport: "redirect" })`, which
   top-level-redirects to II's `/authorize`.
4. The user already has an II session — they just tapped a notification from
   `id.ai` — and already consented to this origin at push opt-in, so this is the
   Continue screen: one tap.
5. II navigates back to the declared callback with the response in its fragment;
   the client replays its journaled state, completes the delegation, reads the
   memoized `next`, and forwards to `/thread/42` — signed in.

What the dApp has to provide:

- `transport: "redirect"` from a recent `@icp-sdk/auth`.
- **`/.well-known/ii-auth-callbacks`** declaring the exact callback URL. II's
  validation fails closed: redirects are not followed, no ambient credentials,
  never cached, must be `application/json` under a size cap, **exact** string
  match, same-origin, and no fragment — plus CORS headers so II can read it. So
  a push-enabled dApp ends up hosting **two** well-known files, this one and
  `ii-push-senders`.
- A callback that is **protocol + host + path only** — the transport rejects a
  callback carrying a query, fragment or credentials. That is why the
  destination rides in memoized flow state rather than as `?next=` on the
  callback itself.

One consistency requirement: push consent keys on the `effectiveOrigin` (see
[Which origin is authoritative](#which-origin-is-authoritative)), and this
sign-in derives the identity from the callback origin or its `derivationOrigin`.
They must agree, or the principal that was notified is not the principal the
user comes back with.

**Notifications should link straight at the sign-in route**, not at the app.
Sending them to the app means it boots, discovers it has no usable session and
bounces — a visible flash on the way to a redirect that was always going to
happen. Linking direct makes the journey tap → Internet Identity → the page.

Four things this cost to get right, all learned the hard way:

- **The sign-in route must be its own URL.** The flow journals state per route,
  so running it on the app root interleaves that journal with the app's whole
  boot path. The symptom is landing signed out with nothing explaining why.
- **Do not short-circuit on `isAuthenticated()` there.** A delegation can sit in
  storage while the app considers the session over — an app that ends its
  session when every tab closes is in exactly that state when a notification
  arrives. Forwarding on it returns the user to an app that then signs them out.
  Calling `signIn()` unconditionally is correct: it starts the redirect on the
  first pass and completes from journaled state on the return one.
- **A loopback callback needs II's `dev_csp`.** Establishing the flow fetches
  the callback origin's allow-list, and that fetch is `connect-src`-bound, which
  production CSP limits to `https:`. So a dApp on `http://localhost` can only do
  this against an II deployed with the dev CSP — which is why the honest advice
  for a demo is to serve the dApp over https.
- **Chrome will ask the user.** A public II origin fetching an allow-list from a
  loopback address is a local-network request, which Chrome gates behind a
  permission prompt ("access other apps and services on this device"). Expected
  on a local setup, absent once both sides are public https.

Finally, the route is a page on the dApp's origin and **cannot be supplied by
II or by the auth client**. The client is headless, and the redirect is a
top-level navigation onto the relying party's own origin — which is also what
generates the session key. So the realistic goal is making that page invisible
(the app's own background and no decision on it), not removing it.

### Apps with no URL routing (e.g. Caffeine)

**This is a hard prerequisite that some app platforms do not currently meet, and
it needs checking before we promise notifications to them.**

Everything above — deep-linking _and_ signed-in tap-through — assumes the app
has **addressable URLs**. Caffeine-built apps were, last time anyone checked,
fully stateful with no URL-based routing: there is no address to point a
notification at. If that is still true, two things break, and the first is worse
than the second:

- **Deep-linking breaks outright.** With nothing to put in `alert.url`, a
  notification can only ever open the app's root, so the context the
  notification was _about_ ("which message", "which order") is lost on arrival.
  A notification that cannot say where it goes is a much weaker product than one
  that can.
- **Signed-in landing breaks**, because the guarded-route pattern needs two real
  routes: the restricted page and the sign-in/callback page.

What such a platform has to add, in dependency order:

1. **Addressable destinations.** Either a real route per notifiable view, or —
   cheaper and probably the right fit for a stateful app — a **single entry route
   that takes an opaque state token** (`/open?s=<token>`) and restores the
   in-app state from it. Notifications then carry that token. This is the
   unavoidable one: without it, notifications are reduced to "something
   happened, open the app".
2. **A sign-in route in the project template.** A hardcoded page that constructs
   the redirect `AuthClient` at page load, memoizes the destination, calls
   `signIn()`, and forwards to the memoized destination on return. It is
   boilerplate, which is exactly why it belongs in the template rather than in
   each app.
3. **A guard on restricted views** calling `isAuthenticated()` and bouncing to
   that route.
4. **`/.well-known/ii-auth-callbacks`** served from the app's origin, listing the
   sign-in route exactly, with CORS.

One constraint that shapes the template: the callback URL must be **protocol +
host + path only** — no query string — so the destination cannot ride on the
callback and must live in memoized flow state. A template that tries
`callback=/sign-in?next=…` will be rejected by the transport.

Open questions to confirm rather than assume:

- Is routing still absent, or has it changed?
- Does the **builder sandbox** differ from a **published** app? URL routing
  inside a live-preview environment (possibly iframed) may be constrained in
  ways the published app is not, so both need checking — a design that works
  only when published is still workable, but it changes what can be
  demonstrated.

**Fallback if routing cannot be added:** notifications still function, but only
as "open the app" — no deep link, no authenticated landing. Worth stating plainly
to set expectations, and it pairs naturally with the `Hidden` content variant,
which also shows a generic message and reveals context only after the tap.

### Updating or dismissing a notification already shown

Yes, via a dApp-chosen `notification_id`, which the service worker maps to the
Web Notification `tag`:

- **Update / replace** — send again with the same `notification_id`; the device
  replaces the shown notification in place rather than stacking a second (e.g.
  "Order shipped" → "Order delivered", or a live score ticking up).
- **Dismiss** — send `content = Dismiss` with that `notification_id`; the SW
  finds notifications with the matching tag and calls `.close()`, showing
  nothing new.

This is complementary to the `topic` delivery header, and the two act at
different stages: `topic` collapses _undelivered_ messages at the relay
(before they reach the device), while `notification_id` updates or dismisses an
_already-delivered_ one on the device.

Caveats:

- **`userVisibleOnly` fights a silent dismiss.** Browsers require every push to
  show _something_; a pure "close and show nothing" push can trigger the
  browser's generic "site updated in background" notification and, if abused, a
  permission penalty. So **update-with-a-new-state is the robust pattern**;
  pure dismiss is best-effort.
- It only works if the device is online to receive the follow-up and the
  notification is still present (the user may have dismissed it already).
- This is **explicit, opt-in** grouping the dApp controls — the opposite of the
  automatic hostname-collapse we rejected. Notifications without a
  `notification_id` never replace each other.

## Alternatives considered


### II hosts the pipeline, rather than each dApp running its own

The alternative is the status quo: every dApp prompts for its own notification
permission, runs its own service worker, holds its own VAPID keypair, and stores
its own subscriptions. It has real advantages — no shared-fate coupling, no new
responsibility for II, no trusted gateway, and each dApp's notification content
never leaves it.

Rejected because the permission is the scarce resource, not the plumbing. A user
declines the *n*th notification prompt far more often than the first, browsers
increasingly bury the prompt, and iOS Safari requires a PWA install per site — so
the per-dApp model works in principle and almost never happens in practice. One
permission at the identity provider is the only version of this that a user
actually says yes to, and it is the one thing a dApp genuinely cannot build for
itself.

The cost of that choice is paid throughout this document and should be read as its
price, not as incidental complexity: II becomes a shared-fate dependency (see
[Shared fate: one permission for every dApp](#shared-fate-one-permission-for-every-dapp)),
II sees notification content unless the app opts into
[end-to-end encryption](#end-to-end-encrypted-apps), and a canister whose real job
is authentication now carries a delivery pipeline — hence
[Push must never degrade authentication](#push-must-never-degrade-authentication).

### II hosts the service worker, rather than each dApp hosting one

Web Push binds a subscription to the origin whose service worker created it. Had
each dApp hosted its own, each would need its own permission grant, and the single
prompt above would be impossible — the two decisions are the same decision. It
also means notifications arrive attributed to `id.ai` rather than to the dApp,
which the UX compensates for by naming the sending app in the body and title, and
which the security model has to defend by forcing attribution to the sender origin
at send time.

### Decisions argued where they arise

Three further alternatives are weighed in place rather than here, because each
needs its surrounding detail to make sense:

- **Gateway versus direct per-device outcalls** —
  [the delivery path](#the-delivery-path-a-trusted-web2-gateway) and
  [the alternative](#the-alternative-direct-per-device-outcalls).
- **Where the sealing runs.** Moving it to the gateway is not merely disallowed
  but useless, and the alternative that does help is a separate canister — see
  [the way to lift that ceiling](#the-way-to-lift-that-ceiling-is-a-separate-canister).
- **Two E2E designs**, vetKeys-sealed versus dApp-held keys, with a
  [recommendation](#recommendation).

## Security model



- **Origin pinning** — a sender can only target anchors that consented to
  _its_ origin; cross-dApp targeting is impossible even with leaked principals.
  Note what "its origin" means when alternative origins are in play — see
  [Which origin is authoritative](#which-origin-is-authoritative).
- **Attribution** — the sender origin shown on the notification is II-derived,
  not dApp-supplied: II forces it to the consented origin and stamps it into the
  payload, so a dApp **cannot choose a different origin's name**. Two limits on
  how far that goes:
  - It does not stop an attacker from _registering a confusable origin_.
    `https://аpp.com` (Cyrillic а) or `https://app.com.evil.co` will pass
    `.well-known` verification on a domain the attacker owns and then be
    stamped as attribution verbatim. Origins must be **canonicalized** —
    punycode, lowercased, default port stripped — at both registration and
    consent, and confusable/mixed-script labels rejected. Without that,
    `https://app.com`, `https://App.com` and `https://app.com:443` are three
    distinct buckets with identical-looking lock-screen labels.
  - It does not stop the _content_ from impersonating someone. `body` is
    free-form; Unicode bidi controls (`U+202E`) or leading newlines can push the
    real origin out of the visible line and render a convincing "Internet
    Identity: verify your recovery phrase" banner. Strip bidi controls and
    collapse whitespace in `title`/`body`, and render attribution as its own
    non-injectable element rather than concatenated into the body string. This
    matters more here than for a generic relay: the notification carries II's
    permission, II's service worker, and an `id.ai` interstitial on tap — the
    highest-credibility surface in the system for a recovery-phrase phish.
- **Content is dApp-controlled (Display mode)** — `title`/`body` are free-form,
  delivered under II's service worker, with lengths capped and attribution shown.
  A compromised dApp can still send misleading text within its own attribution;
  that is inherent to any notification relay and is a documented posture.
  `Hidden` mode avoids it entirely — no app text reaches the SW.
- **Transport confidentiality** — every payload is RFC 8291 encrypted to each
  device, so **no relay or gateway can read it**. Two scope limits: this is
  _transport_ encryption, so for `Display` content II itself sees plaintext (it
  does the encryption); and because the drain draws one `raw_rand` per tick and
  derives per-message ephemeral seeds from it, an observer of the subnet's random
  tape can recompute that tick's ephemeral scalars. So the honest claim is "no
  relay, gateway or network observer can read it" — **not** "only that device
  can". Deriving each seed from `raw_rand ‖ msg_id` would stop one tape
  observation from yielding a whole tick. Keeping content from II-the-service
  altogether is the `Hidden` variant today (and, later, the vetKeys-sealed
  path); see the next section.
- **Coarse rejections** — the dApp learns only about its own relationship with
  a target (`NoConsent`), never device counts or II state. One leak to close:
  `ready` / `retry_after_ms` reflect _aggregate_ load from other senders, so a
  sender that probes them learns something about II's global state. Randomize
  `retry_after_ms` per caller.
- **Error strings are a channel too** — reject reasons returned to senders must
  be a fixed enum, never free-form text echoing caller input, and endpoint URLs
  must never reach canister logs (an endpoint plus a valid VAPID token is a
  push capability, and a stable per-device identifier besides).
- **Redirect-screen icon** — the `/notify` screen shows the app logo only from
  II's curated dApp registry, with a neutral globe fallback. Arbitrary or
  remote icons are deliberately not fetched: doing so would render
  attacker-controlled imagery inside II's trusted chrome and leak the fetch to
  the dApp. If arbitrary-app icons are ever wanted, the least-bad path is a
  vetted icon supplied at sender-registration, not a per-notification URL.
- **VAPID key** — in stable memory today, the same custody posture as the anchor
  salt: node operators could extract it. But **the blast radius is larger than
  "spam"**, and the anchor-salt analogy undersells it. The _same_ stable memory
  holds every device's `p256dh` and `auth` secrets, so an attacker who can read
  it gets both halves: the device secrets let them produce a valid RFC 8291
  ciphertext, and the VAPID key lets them sign a valid request. They can then
  deliver **arbitrary, fully-readable notifications to any subscribed device,
  attributed to any origin the user actually consented to** — which means the
  forged notification's tap-through passes `/notify`'s consent gate and
  deep-links into the real dApp. Salt exposure lets someone _compute
  identifiers_ (passive); this lets someone _write into the user's trusted
  notification surface wearing another app's name_ (active). That is
  phishing capability, not spam.

  Mitigations worth doing regardless of custody: derive per-device transport
  secrets so extracting one does not yield the other, and keep a signed
  monotonic per-endpoint counter of pushes II issued so a service worker can
  flag pushes II did not send. Note there is currently **no detection story** —
  II cannot observe pushes it did not originate — and **no rotation story**:
  rotating the VAPID key invalidates every subscription at once, so the
  "re-enable on your devices" UX is a prerequisite for having a recovery path
  at all, not a later nicety.

  P-256 threshold ECDSA would remove the key from storage entirely, but the IC
  supports only secp256k1 today (see
  [IC capabilities to re-evaluate](#ic-capabilities-to-re-evaluate)).

- **VAPID key initialization must not race.** Generating the key lazily on first
  use puts an `await` on `raw_rand` between the "is it set?" check and the
  write, so two concurrent first callers can both generate and the later write
  wins. A browser that already subscribed with the losing public key is
  **permanently undeliverable**, silently, with no error surfaced anywhere.
  Initialize eagerly in `init`/`post_upgrade` behind a guard, or re-check after
  the `await` and adopt whatever was stored.

### Shared fate: one permission for every dApp

The value proposition — one notification permission covering every dApp — has a
structural consequence that is a security property, not a UX detail: **the
browser's permission belongs to `id.ai`, not to any dApp.** So the browser's
abuse heuristics and the user's own Block button both target `id.ai`.

A single hostile or careless consented sender can therefore end notifications
for **every other dApp on that device**: spam at volume, `Dismiss`-only pushes,
or malformed bodies each trip the browser's generic "site updated in background"
notification, and Chrome's abusive-notification heuristics respond by revoking or
quieting the origin. The user has no way to attribute which app caused it, and no
II-side re-enable UX can undo a browser-level block.

This is created by the single-permission design, so it has to be defended
II-side:

- A **per-origin budget on generic/contentless notifications**, not just on
  volume, with automatic suspension of an origin that exceeds it.
- **Attributable throttling** — surface "app X was rate-limited" in Settings so
  blame lands on the sender rather than on II.
- An **operator kill switch** per origin (see
  [Operating it](#operating-it-controls-alerts-and-rollout)).

### Which origin is authoritative

II lets one origin borrow another's identity derivation: a dApp served from
`https://beta.app.com` may pass `derivationOrigin: https://app.com`, and if
`https://app.com/.well-known/ii-alternative-origins` lists it, II derives the
user's principal from `app.com`. That is what lets a beta site, a custom domain
and a raw canister URL share one identity. It leaves two origins in play:

- **`effectiveOrigin`** — what the principal is derived from (`app.com`).
- **`displayOrigin`** — who the browser is actually talking to
  (`beta.app.com`).

**Decision: `effectiveOrigin` is authoritative for push** — consent rows,
sender registration, and attribution all key on it. This is forced by the
targeting model: the dApp addresses users by an in-app principal derived from
`effectiveOrigin`, and `PRINCIPAL_INDEX` resolves that principal back to
`(anchor, origin_hash)`, so consent must be keyed on the same origin or the
lookup simply does not resolve. It is also the right trust boundary: if
`app.com`'s `.well-known` is trusted enough to let those origins **share the
user's identity**, letting them share push rights is the same trust, not a wider
one.

Three obligations follow, and skipping them is where this becomes a security
problem:

- **The opt-in screen must disclose the origin being granted.** Showing
  "beta.app.com" while recording consent for `app.com` means the user consents to
  something they were never shown, and Settings → Allowed apps then lists an
  origin they don't recognize. When the two differ, show both.
- **`.well-known/ii-push-senders` must live on the `effectiveOrigin`.** A
  borrowing origin cannot register itself; the canonical origin must. Good
  property — state it.
- **Transitivity is intentional and must be documented.** One registration lets
  _every_ origin listed in that alternative-origins file send as `app.com`, with
  no separate opt-in.

### Consent lifecycle: it must not outlive the sender

Consent rows keyed only by `(anchor, origin_hash)` carry no reference to _which
sender_ was registered for that origin. That makes consent survive events it
should not:

- A dApp shuts down, its domain expires, someone else buys it, serves their own
  `.well-known/ii-push-senders`, and registers. **Every user who ever consented
  to that origin is immediately reachable** — attributed as the old brand, with
  tap-through deep-linking into the new owner's site.
- Sender deregistration and re-verification TTLs do not bound this, because they
  govern _sender verification_, not _consent validity_.

Fixes: stamp each consent row with the **sender registration epoch**; invalidate
consent when the registered principal set for an origin changes or the sender is
deregistered, requiring a fresh opt-in; expire consent after a long period with
no delivery; and surface "this app re-registered — re-confirm?" in Settings.

Relatedly, `.well-known` re-verification must not become a **deregistration
primitive**: a competitor who can cause a single 404 or timeout on a sender's
`.well-known` during one lazy re-verify would silently strip its notification
capability. Require K consecutive failures across a multi-day window, and never
deregister on a 5xx or timeout — only on a well-formed file that no longer lists
the sender.

## Privacy



The doc's confidentiality story is about _content_. Metadata is a separate
exposure and, for a notification hub, arguably the more sensitive one — `Hidden`
protects the message text and does nothing for any of this.

**What is genuinely new here.** II already stores which origins an anchor uses
(`StorableApplication`), so the anchor↔origin association is not new. What this
feature adds is the **temporal** dimension — _when_ each app notified each user,
continuously — and the **off-chain export** of a device-linkable identifier.

- **The gateway sees a join key.** Every bundle is
  `{endpoint, headers, authorization, body}`. Grouping by `endpoint` yields, per
  physical device: the set of sender origins notifying it, exact timestamps,
  message sizes, and `Urgency`/`Topic` in cleartext headers. Because one browser
  has one endpoint shared across a user's identities, that set links **a user's
  anchors to each other and their dApps to each other** — precisely the linkage
  per-origin principal derivation exists to prevent. No decryption required.
- **The relays get it too, and more easily than before.** All II-mediated push
  carries **one shared VAPID public key**, so FCM/APNs/Mozilla can isolate II
  traffic and count per-user dApp diversity trivially. Per-dApp VAPID keys — which
  this design frames as a burden it removes — were an unlinkability property.
- **Timing is content.** "Which dApp notified this user at 03:00" is sensitive
  even when the body is opaque.

Mitigations to evaluate: give the gateway per-batch pseudonymous endpoint handles
rather than raw endpoints; shuffle and pad batches so per-origin timing is not
directly readable; and consider per-origin VAPID subkeys to restore relay-side
unlinkability. At minimum, state plainly what the gateway, the relays and node
operators each learn.

## Delivery semantics: what is actually guaranteed



"Best-effort" is not a guarantee, and several features silently depend on which
one holds. Stated explicitly:

- **At-least-once, not at-most-once.** Retries, `drain_epoch` recovery, and
  replicated outcalls all duplicate. `chunk_id` makes _chunk_ resends idempotent;
  `msg_id` + device dedup is what makes duplicates invisible to the user.
- **Unordered.** Nothing in the pipeline preserves order: replication, per-relay
  queueing, and urgency's contribution to drain order all reorder freely.
- **No delivery receipts.** The only per-target signal is `NoConsent`, returned
  at admission. `admitted` means "in II's buffer", never "on the device".

The ordering gap has teeth, because `notification_id` update/replace is defined
in terms of it: "Order delivered" can land before "Order shipped" and leave the
**stale** text on the lock screen permanently, and a `Dismiss` can arrive before
the notification it dismisses and then be a no-op forever. Fix: carry a
**monotonic sequence number per `(origin, anchor, notification_id)`** so the
service worker drops out-of-order updates rather than applying them.

## Duplicate and replay suppression (`msg_id`)



**Built.** Four independent mechanisms make duplicates the norm rather than the
exception, and the fourth is the one that showed up first in practice:

1. **Replicated outcalls** — 34 POSTs per push, no idempotency key in RFC 8030.
2. **Timer duplicate execution** — `ic-cdk-timers` documents that under load
   "timeouts may result in duplicate execution", and the drain is not idempotent.
3. **Client retries and `drain_epoch` recovery** — by design, per the client
   library.
4. **Accumulated subscription rows.** The fan-out sends one push per stored row,
   browsers rotate endpoints, and nothing removed a row — so one browser ended up
   behind two rows that both still delivered. This is what actually produced
   duplicate banners during testing, before any of the above did.

II assigns a `msg_id` **once per admitted message**, stable across delivery
retries, included in the JSON _before_ RFC 8291 encryption — so it is not a
Candid field, no dApp supplies it, and no relay or gateway can read or forge it.
The service worker keeps a bounded set of ids it has shown and drops repeats.

Two implementation notes that are easy to get wrong:

- **The seen-set cannot live in a variable.** A service worker is killed between
  pushes, so an in-memory set forgets exactly what it needs to remember. It is
  held in the Cache API, bounded and pruned oldest-first.
- **A payload with no id is always shown.** Failing open matters: dropping a
  real notification is worse than showing a duplicate, and it keeps an older
  canister working against a newer worker.

Cause as well as symptom: the drain now **removes a subscription when the relay
answers `404`/`410`**. That is the only signal a row is dead, and without it rows
only accumulate while every future send pays for a target that can never receive.
`pushsubscriptionchange` is still missing, so a rotated endpoint lingers until a
relay reports it gone — self-healing now rather than permanent.

**Still to harden: a monotonic window rather than a bounded recency set.** What
is built collapses accidental duplicates, which is what the symptom was. It does
not resist a deliberate replay: a bounded set means eviction, and the attacker
controls the pressure, so flooding a device with fresh notifications flushes the
set and a replayed capture then looks new (clearing site data does the same).
The hardened form keeps a per-origin high-water mark, rejects anything at or
below it, and binds `msg_id` to a short validity window so an old capture fails
on age alone.

Note `msg_id` is a **different thing** from the dApp-facing `notification_id`:
`msg_id` is II-generated and suppresses exact duplicates (show at most once);
`notification_id` is dApp-chosen and _replaces_ a shown notification. Similar
shape, opposite behavior — keep them separate fields.

## End-to-end-encrypted apps



Some apps (e.g. a chat using vetKeys) encrypt content so that only the
recipient can read it — the app backend cannot. Our `Display` path is **not**
E2E: II receives plaintext `title`/`body` and briefly holds it. The `Hidden`
variant lets these apps use II-hosted push without ever handing content to II.

**Correcting an earlier overstatement.** It is tempting to say II can _never_
show decrypted content. That is too strong. The honest constraint is about
_who_ decrypts and _where_, and it comes with a trust dial the user is already
turning:

- **II-the-service decrypts** — the canister and its node operators see
  plaintext. That is just `Display` relabeled: fine for non-sensitive content,
  not E2E.
- **II's service worker decrypts on the user's own device**, using a key it
  legitimately obtains and that II-the-service never sees in the clear. This
  **is** viable. It asks the user to trust II's _client code_ running as them
  on their device — which is only a small step past the trust they already
  place in II by signing in with it (and exactly the trust the `Display` path
  already assumes). This is the basis for showing real content in an
  E2E-friendly way.

So there is a spectrum, from no trust to full trust in II for content:

| Approach                     | Who decrypts           | In-notification         | Trust in II for content               |
| ---------------------------- | ---------------------- | ----------------------- | ------------------------------------- |
| `Hidden` (ships first)       | the app, after tap     | generic ("New message") | none — II never touches content       |
| Design A — vetKeys-sealed    | II's **SW**, on device | full, real text         | II's client code + IC vetKD threshold |
| Design B — dApp-fetch        | II's **SW**, on device | full, real text         | II's client code (+ the dApp)         |
| `Display`                    | II-the-service         | full                    | full — II reads it                    |
| App's own SW (not II-hosted) | the app's own SW       | full                    | none — II isn't in the loop           |

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
  _ciphertext_ to II via `push_send`, as opaque bytes II cannot read.
- II delivers as usual (RFC 8291 wraps the opaque bytes). On the device, II's
  SW authenticates **as the user** to II's canister and requests the vetKD
  decryption key. vetKD returns it **encrypted to the SW's transport key**, so
  no node operator sees the key in the clear; the SW decrypts the content and
  shows the **real text**.
- **What II-the-service sees:** in the honest flow, nothing — not the content,
  not the key in clear. Plaintext exists only inside the user's SW, and II
  stores none of it.
- **The trust that remains:** II's canister _controls the vetKD
  authorization_, so a malicious/compromised II could authorize itself to
  derive a user's key and decrypt. That is the same class of trust the user
  already extends to II for their delegations — not a new kind. No single node
  can derive the key (threshold); II's client code is assumed honest (if it
  weren't, the user's identity is already lost). A small, coherent increment
  over `Hidden`.
- **Availability:** vetKeys is **live on mainnet** (since mid-2025), not a
  future capability — `vetkd_public_key` / `vetkd_derive_key` on curve
  `bls12_381_g2`, with IBE supported. So this design is buildable today.
- **Cost — and this is the binding constraint.** `vetkd_derive_key` is
  ~26.2 B cycles (**~$0.036 per call**) and chain-key fees are charged on
  **every** deployment, system subnets included. So a derive _per notification
  render_ would be **~$357 per 10k-notification blast** — roughly 400× the
  entire outcall cost of the same blast, and the most expensive thing in the
  whole design. Worse, the service worker calls in over **ingress, which carries
  no cycles**, so **II pays** — making an uncached derive a user-triggerable
  cycle drain.

  Therefore: derive **once per `(user, origin)`**, cache the key in the service
  worker, and rate-limit the derive endpoint per anchor. The per-`(user, origin)`
  identity scheme above already permits exactly this — the key is stable, so a
  single derive covers every future notification from that origin. Treat
  per-render derivation as a design error, not a tuning knob.

- **Latency:** the production vetKD key lives on a different subnet, so each
  derive is a cross-subnet round trip — seconds — inside a `push` event
  handler's limited lifetime. Another reason the cached-key path is the only
  viable one.
- No change to the send/admission model — II still just carries bytes it can't
  read.

### Design B — dApp keeps the keys, II's SW fetches + decrypts (alternative)

The push is a bare wake; on receipt II's SW calls an endpoint **on the dApp's
side** to obtain the content, then renders it. Genuinely E2E only if that
endpoint hands back _ciphertext_ the SW can decrypt — which forces one of two
uncomfortable choices:

- **Endpoint returns plaintext.** Then the dApp _backend_ can decrypt, so it is
  "trust the dApp backend," not strict E2E. Simple, no vetKeys; fine for apps
  that don't need backend-blind encryption.
- **Endpoint returns ciphertext + the SW holds the key.** The recipient's key
  lives in the dApp's client (cross-origin from II's SW), so the dApp must
  _provision a key into II's SW_ — now II's SW holds dApp secrets, a real added
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

- **Endpoint host allowlist.** The push endpoint is attacker-supplied. Validate
  it against the known push services at subscribe time (and **re-validate at
  drain**, so pre-existing rows can't bypass a tightened list), reject explicit
  ports, userinfo and non-`https` schemes, and reject private/loopback/link-local
  hosts. Without this, II is an SSRF and DDoS reflector with an *n*× multiplier —
  a free ingress call turns into 34 POSTs at a victim of the caller's choosing.
- **Per-anchor caps.** Cap subscription rows (~10) and consent rows (~100) per
  anchor, with LRU eviction. Nothing bounds them today, ingress is free to the
  caller, and II pays the stable-memory cost — a direct attack on the resource
  this design calls a hard constraint.
- **Reserved outcall budget for authentication.** See
  [Push must never degrade authentication](#push-must-never-degrade-authentication).
- **Drain isolation and non-reentrancy.** Per-entry failure boundaries, an
  attempt counter, a stuck-buffer watchdog, and a claim step before the first
  `await`. Without these, one malformed row wedges the feature globally and
  overlapping ticks double-send.
- ~~**`msg_id` + device-side dedup.**~~ **Built** — see
  [Duplicate and replay suppression](#duplicate-and-replay-suppression-msg_id).
  What remains is hardening it against deliberate replay (monotonic window
  instead of a bounded set), not the duplicate-suppression itself.
- **An acknowledgment signal (`drain_epoch`).** Promoted from implicit — the
  client-side durability story does not work without it. See
  [II's state model](#iis-state-model-stateless-for-campaigns).
- **Stale-subscription cleanup.** <a id="stale-subscription-cleanup"></a>
  `404`/`410` row removal is **built** on the direct path. On the gateway path it
  became possible only once the batch handoff went **non-replicated**: with no
  consensus on the response, the gateway is no longer confined to a deterministic
  ack and can return per-device status inline. Under replicated outcalls this was
  structurally impossible, not merely unbuilt. It still has to be built, and it
  still matters — browsers rotate endpoints continuously, so left alone the
  subscription table grows as O(devices ever registered) rather than O(users) and
  II pays forever to send to dead endpoints. Treat the returned status as
  evidence, not proof: it is one unverified reply from a trusted party, so retire
  a subscription opportunistically on a reported `410` rather than deleting user
  state on a single word. A separate authenticated **pull**, a
  `push_gateway_report` update, or TTL-based GC keyed on last successful delivery
  all remain viable as corroboration. Pairs with `pushsubscriptionchange` below.
- **`pushsubscriptionchange`** — browsers rotate/invalidate subscriptions; the
  service worker must re-subscribe and re-register with II, or delivery
  silently erodes over weeks. Invisible in short-lived testing.
- **Sender deregistration & re-verification TTL** — bound the window after a
  sender principal is compromised or a domain changes hands, and make
  re-verification resistant to being used as a deregistration primitive (see
  [Consent lifecycle](#consent-lifecycle-it-must-not-outlive-the-sender)).
- **Origin canonicalization** — punycode, lowercase, strip default port, reject
  confusable/mixed-script labels, at registration _and_ consent.
- **Input validation and caps** — `title` ≤ 64, `body` ≤ 256, `topic` ≤ 32,
  `notification_id` ≤ 64, `url` bounded, `chunk_id` fixed at 16 bytes, origins
  ≤ 255 bytes **validated rather than trapped on**. Reject with a fixed error
  enum; never return free-form text echoing caller input.
- **Cycle budget + freezing-threshold guard** (deployments that pay fees) — a
  per-origin and global daily burn cap with a circuit breaker, independent of
  the rate bucket, plus a floor that disables push before spend can approach the
  freezing threshold. Past that threshold a canister stops serving updates while
  still serving queries, which for II means **logins fail while the frontend
  still loads**.
- **Buffer sizing & drain fairness** — tune the admission bucket, global cap and
  per-origin reservations, and drain **round-robin across origins** with urgency
  and age only _within_ an origin's slice.
- **Observability** — in-flight buffer depth (canary for stuck timers /
  backpressure), drain lag, admission reject rate per origin, per-origin
  outcall success / 410 / drop counts, and **authentication-path p99 latency**
  (the signal that push is stealing from login).

Must-decide (design, not code):

- **App platforms without URL routing** — confirm whether Caffeine-built apps
  can address a destination at all, in the builder sandbox _and_ published. If
  not, deep-linking and signed-in tap-through are both unavailable there and
  notifications degrade to "open the app". See
  [What about apps with no URL routing](#apps-with-no-url-routing-eg-caffeine).
- **iOS reality** — iOS Safari is the one platform where a PWA install is
  _mandatory_ for Web Push (Android/desktop work in a plain tab). It is also
  flakier and more throttled; "best-effort" is weakest there. Set expectations
  in dApp docs, and surface the "Add to Home Screen" hint only on iOS.

Known-deferred:

- **Replay layering.** The two replay surfaces are handled separately.
  **dApp→II replay** (a resent or duplicated chunk) is solved by the `chunk_id`
  idempotency key. **relay/gateway→device replay** (a captured
  `(jwt, ciphertext, endpoint)` re-POSTed) has no protection in Web Push itself
  — VAPID's JWT is a time-bounded bearer token and AES-GCM gives
  tamper-detection, not anti-replay — so it is handled by `msg_id`, now a v1
  requirement rather than a deferred item. Note the earlier framing that "TLS
  gates capture, so the practical risk is low" does **not** hold on the chosen
  path: the gateway legitimately receives the sealed bundle, so replay needs no
  network interception at all.
- **VAPID rotation** — rotating invalidates every subscription at once; needs
  a "re-enable on your devices" UX. Note this UX is also the only recovery path
  from a VAPID key compromise, so deferring it means deferring recovery.
- **Cleanup hooks** on device/anchor removal.
- **Metadata-privacy mitigations** — endpoint blinding, batch shuffling/padding,
  per-origin VAPID subkeys. See [Privacy](#privacy).

Explicitly rejected:

- **On-device `tag`-based collapsing by hostname** — a dApp sends distinct
  notifications that must not replace each other on the device. Collapsing is
  opt-in per send via `topic`, never automatic per origin.
- **Reusing one RFC 8291 ephemeral key across recipients in a batch.** It would
  roughly halve sealing cost, and the RFC does not forbid it — but it links
  recipients cryptographically and makes one leaked transient key expose the
  whole batch. Not worth it.

## IC capabilities to re-evaluate



Platform features that would change decisions in this doc. Worth re-checking
before implementation, because two of them postdate the design:

- ~~**Non-replicated outcalls (`is_replicated = false`)**~~ — **adopted**: the
  batch handoff to the gateway uses it, which is what makes `410` cleanup
  possible there and drops the payload cost by the full factor of *n*. Still
  marked experimental, so what remains to watch is the API changing under us —
  keep it behind one call site that can revert to replicated plus a transform.
  Field-tested outside II: multidex's price oracle has run on it since
  2025-08-04, which is where the ~*n*× figure was measured.
- **Per-node outcall results.** A management-canister API returning per-node
  responses would give per-device status back on a *replicated* path, which is now
  only interesting as a way to stop depending on an experimental flag. Unverified
  whether it is enabled on mainnet — settle by calling it or checking release
  notes.
- **P-256 (secp256r1) threshold ECDSA** — would move the VAPID key out of
  storage entirely via `sign_with_ecdsa`. Requested since 2024 with **no public
  roadmap item or timeline**, so treat it as "if it ever ships", not "when".
  Migration invalidates existing subscriptions (the public key changes), so it
  pairs with the re-enable UX.
- **vetKeys** — already live; see
  [Design A](#design-a--vetkeys-sealed-content-decrypted-in-iis-sw-preferred-richer-path).
  Client libraries are still pre-1.0 and the JS package was renamed, so pin
  deliberately.

## Push must never degrade authentication



The invariant, stated separately because it is the one that makes this feature
unshippable if violated: **no volume of push traffic may reduce the availability
of sign-in.**

II is not a dedicated push canister. It authenticates every user of the network,
and push shares three resources with that job:

- **The in-flight outcall budget** (3000, subnet-wide). II's own auth paths spend
  from it: DoH for email recovery, JWKS and discovery for OIDC. Push must hold a
  quota well below the cap and **fail closed** — return `ready = false` — rather
  than compete. Note the queue to the management canister is also 500-deep per
  canister pair, so a burst can exhaust the queue and make II's _own_ auth
  outcalls fail synchronously.
- **Instructions per round.** A drain slice must be a small fraction of a round,
  never a whole chunk.
- **Cycles** (on paying deployments), because the freezing threshold is shared:
  push spend must never be able to push II toward the state where updates stop
  and only queries serve.

Concretely: a semaphore with a push-only quota, a drain slice sized from
measurement, a cycle floor, and **authentication p99 latency as a monitored
signal** with push throttling as the automatic response.

## Operating it: controls, alerts and rollout



Metrics alone are not operability. Missing today:

- **A per-origin kill switch.** `push_deregister_sender` is authenticated by the
  _dApp_, so there is no operator-side way to silence an abusive sender. Needs an
  operator-gated `push_set_origin_enabled(origin, bool)` plus a **global feature
  flag** in persistent state to disable push entirely without an upgrade.
- **Alert thresholds and ownership** — on buffer depth (stuck-timer canary),
  drain lag, per-origin reject rate, and auth-path p99. Define who is paged.
- **Gateway operations** — who runs it, how many independent instances (≥2 with
  distinct operators), health probes, automatic switchover, and what happens to
  batches already handed over when one dies. Since the gateway is the shipping
  path and the direct path is a degraded fallback, **gateway down = push down**;
  that needs to be an explicit, accepted statement rather than an implication.
- **Rollout and rollback.** Rollback is an upgrade, which discards the in-flight
  buffer — so a rollback silently drops in-flight campaigns unless `drain_epoch`
  is in place first.
- **A load test.** Every throughput and cost number in this doc is derived, not
  measured. Before deployment: run a 10k-recipient campaign against a local
  replica while measuring authentication latency, and measure the real
  instruction cost of one seal with `performance_counter` (II already runs
  P-256 in `dnssec/`, so this is a short exercise).

## dApp developer integration

What a dApp must do to send its first notification — register as a sender for its
origin, serve the callback allow-list, shape its deep links — is in
[push-api.md](push-api.md#dapp-developer-integration).

## Feasibility and scale



| Metric                               | Gateway (chosen)                                                   | Direct (alternative)    |
| ------------------------------------ | ------------------------------------------------------------------ | ----------------------- |
| App → II for 10k users               | ~10 paced chunks (client-driven)                                   | ~10 paced chunks        |
| Outcalls per 10k blast               | ~25                                                                | ~13k                    |
| Requests actually reaching relays    | ~13k (gateway fans out off-chain)                                  | ~13k × 34 (replication) |
| Per-device status (`410`) observable | yes — non-replicated handoff, so the ack need not be deterministic | no under replication    |
| II **stable storage**                | O(users × origins), flat with volume                               | same                    |
| II in-flight buffer                  | bounded, transient heap                                            | same                    |
| Full-delivery latency, 10k blast     | tens of seconds                                                    | minutes                 |
| Cycles, 34-node **paying** subnet    | ~0.02 T (~$0.03) non-replicated; ~0.7 T (~$0.95) if replicated     | ~2.8 T (~$3.80)         |
| Cycles, canonical II (system subnet) | fee-waived                                                         | fee-waived              |

**Throughput has a global ceiling, and it is shared.** The buffer drains only as
fast as its outcalls resolve, and outcalls take seconds. At a bounded slice per
tick this puts II's sustained rate in the low hundreds of device-messages per
second **across all dApps combined** — so a single 10k-user blast occupies the
global budget for tens of seconds, during which every other origin sees
`ready = false`. For a system positioned as the notification hub for every dApp
on the network, this number is the most important capacity fact in the design and
must be published — and derived from a real measurement, not from the estimates
in this doc.

App → II is feasible either way: the client library streams bounded chunks, II
admits what it has capacity for, and its storage stays flat with volume. The
**gateway is the chosen delivery path** because it turns ~13k in-flight outcalls
into ~25, and that budget is shared with sign-in — a faster drain also recycles
the small buffer sooner, raising admission throughput. Direct delivery remains
fully on-chain and viable at low volume, but under replicated outcalls it also
multiplies every POST by the subnet size and cannot observe `410`, so it is a
degraded fallback rather than a transparent one.

## What this actually costs per user



Storage is O(users × origins), not O(users) — worth being concrete, since the
"flat with volume" claim is about _notification volume_ only:

| Row             | Key                       | Size                                                        | Grows with       |
| --------------- | ------------------------- | ----------------------------------------------------------- | ---------------- |
| Subscription    | `(anchor, endpoint_hash)` | ~300 B typical (endpoint URL dominates), ~1.1 KB worst case | users × devices  |
| Consent         | `(anchor, origin_hash)`   | ~60 B                                                       | users × dApps    |
| Principal index | `principal`               | ~80 B                                                       | users × dApps    |
| Sender registry | `origin_hash`             | ~100 B                                                      | registered dApps |

At 10M users × 2 devices × 10 consented dApps that is roughly **6 GB + 1 GB +
1.2 GB ≈ 8 GB**, against a 500 GiB per-canister limit but sharing a 2 TiB subnet
with anchor data — and **before** any dead rows, which is why
[stale-subscription cleanup](#stale-subscription-cleanup) is a must-fix rather
than housekeeping. Note also the 2 GiB stable-memory-per-upgrade ceiling, which
bounds how large these maps can get before upgrades become a problem.

## Stable memory regions



New regions must claim an unused `MemoryId`. Because nothing in the code forces
this check, record allocations here and verify against `storage.rs` before
adding one — a duplicate index silently interleaves two `StableBTreeMap`s into
the same virtual memory and corrupts both.

| Index | Region                                |
| ----- | ------------------------------------- |
| …     | (existing regions — see `storage.rs`) |
| 31    | MCP registration                      |
| 32    | push consent                          |
| 33    | push principal index                  |
| 34    | push subscriptions                    |
| 35    | push sender registry (proposed)       |

A test asserting all indices are distinct is cheap and worth having.

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
## Status



### Built today (PoC on `feat/push-notifications-poc`)

An inventory, not an explanation — each line links to the section that covers
it. The last four exist because building the PoC proved they had to.

- **Per-device subscribe/unsubscribe, `/authorize` opt-in, per-dApp consent** —
  [the user flow](#turning-notifications-on-first-time-signing-into-a-dapp),
  [consent lifecycle](#consent-lifecycle-it-must-not-outlive-the-sender).
- **`notify_user(principal, alert)`** — one recipient per call. It does **not**
  answer scale, and everything in this doc about throughput, cost and delivery
  assumes the chunked `push_send` that supersedes it:
  [chunked send and flow control](#sending-to-thousands-of-users-chunked-send--two-layer-flow-control).
- **RFC 8291 payload encryption and RFC 8292 VAPID signing, both in-canister** —
  [delivering to devices](#delivering-to-devices).
- **VAPID key generated with `raw_rand`, held in stable memory** — not the
  custody model we would choose. Web Push mandates P-256, the IC's threshold
  ECDSA is secp256k1 only, so the subnet cannot hold the key for us:
  [what that risks](#security-model),
  [what changes if P-256 lands](#ic-capabilities-to-re-evaluate).
- **Tap opens the dApp's deep link directly**, falling back to the consent-gated
  `/notify` redirect when the sender supplied no target —
  [where a notification opens](#where-a-notification-opens),
  [landing signed in](#landing-the-user-already-signed-in).
- **Sender authorization by origin** — a registry keyed by origin hash. The
  original `caller() == in_app_principal` rule could never match an
  inter-canister call, so it admitted only self-sends:
  [how II verifies a sender](#how-ii-verifies-the-sender-is-really-that-dapp).
- **Send-time `alert.url` validation** — what makes opening the deep link
  directly safe:
  [where a notification opens](#where-a-notification-opens).
- **`msg_id` dedup in the service worker, and `410`/`404` row cleanup** —
  [duplicate and replay suppression](#duplicate-and-replay-suppression-msg_id).
- **VAPID key rotation in the browser** — a subscription bound to a superseded
  key is resubscribed rather than failing forever.

### Not built, so the PoC isn't mistaken for a shippable subset

- No `push_send`, so no batching, no chunking, and no client library — a send is
  one `notify_user` per recipient.
- No rate limiting or admission control of any kind — `notify_user` is unmetered.
- No `.well-known/ii-push-senders` verification: senders are registered by an
  operator, so nothing yet proves a canister owns the origin it sends as.
- No endpoint host allowlist, and no per-anchor caps on subscription or consent
  rows.
- `Display` content only — no `Hidden`, which is the variant the design
  recommends shipping first for E2E apps.
- No `pushsubscriptionchange` handling, so a rotated endpoint still leaves a
  stale row until a relay reports it gone.
- Integration and E2E test coverage is thin (unit tests only).

### Proposed (designed above, not yet built)

- Chunked `push_send` with two-layer flow control (II admission + client
  pacing), a sender registry, a stateless-for-campaigns II (transient heap
  buffer, storage O(users × origins)), a durable client library, and delivery
  through a trusted web2 gateway (with direct per-device outcalls as the
  documented alternative/fallback).
- Promoted to v1 requirements by review, and still outstanding: a `drain_epoch`
  acknowledgment signal, an endpoint host allowlist, per-anchor caps, drain
  isolation and non-reentrancy, and a reserved outcall budget that protects
  sign-in. (`msg_id` + device dedup was also promoted, and is now built.)
- No cycles charging to senders in v1 — but a deployment that pays fees needs a
  cycle budget with a circuit breaker regardless. Sender charging is parked as a
  future exploration.

