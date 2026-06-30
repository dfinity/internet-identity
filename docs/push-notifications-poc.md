# II Push Notifications — PoC Design

**Status:** draft · PoC scope
**Authors:** Mario, Claude
**Last updated:** 2026-06-30 (rev: RFC 8291 ciphertext payload — drops the spool entirely)

## 1. Goal

Let a dApp deliver a notification to a user's device, addressed only by the
**in-app principal** the dApp already holds, even when the user is offline.
Internet Identity is the privacy boundary: the mapping
`in-app principal → device endpoint` lives inside II and is exposed to no
one else. The dApp passes a principal; everything else stays opaque.

## 2. Non-goals (for this PoC)

- Consent / opt-in UI. Assume the user has already enabled notifications
  for the relevant app.
- Opt-out, mute, quiet hours, categories.
- Cycle billing of the calling dApp. II eats outcall cycles for the PoC.
- Replacing FCM / APNs. We use standard Web Push and the relays the
  user's browser already chose.
- iOS < 16.4 (no Web Push at all).

## 3. The two architectural choices

### 3.1 II is the push origin

Web Push requires three things on the device, all bound to one origin:

1. A registered **Service Worker**.
2. A **Push Subscription** the worker created via the browser's Push API.
3. **OS-level permission** granted on a past visit.

We have two options for whose origin owns the SW:

- **dApp origin** — the dApp could push the user directly without II.
  Defeats the privacy boundary. **Rejected.**
- **II origin** — the user installs II's PWA once, II's SW owns the push
  subscription, II is the only entity holding the endpoint URLs. The
  dApp's identity becomes payload content rendered by II's SW. **Chosen.**

Consequence: the user installs **II's** page on the device, not the dApp's.
One install per device covers notifications from every dApp the user signs
into.

### 3.2 Payload travels encrypted through the push relay (RFC 8291)

Web Push (RFC 8030) accepts either an empty body or a body encrypted
under the device's keys per RFC 8291. We use the **encrypted body**:

- II encrypts `{title, body}` with AES-128-GCM under a key derived via
  ECDH (II's VAPID key × device's `p256dh`) and HKDF (mixing in the
  device's `auth` secret).
- The push relay forwards the ciphertext blob — never sees plaintext,
  never can.
- The browser decrypts on the device and hands the SW the cleartext
  `{title, body}` directly in the `push` event.

Consequences:

- **No spool on II.** The payload doesn't need to be held anywhere
  outside the message itself.
- **No SW round-trip back to II** to fetch content. The SW renders
  directly from the push event.
- **End-to-end encryption.** The push relay sees only ciphertext;
  even a compromised relay can't read user notifications.
- **Cost.** ~300 lines of crypto in the canister (ECDH + HKDF + AES-128-GCM
  + the RFC 8291 byte layout). Real but bounded.

The empty-body variant (where the SW pulls the payload from II via a
5-minute spool) was discussed and rejected — see §20.

## 4. High-level flow

```mermaid
sequenceDiagram
    autonumber
    participant Dapp as dApp Canister
    participant II as II Canister
    participant Spawn as Spawned Future<br/>(ic_cdk::spawn)
    participant Push as Push Relay<br/>(Apple/Google/Mozilla)
    participant SW as II Service Worker<br/>(on device)
    participant Phone as Phone Screen

    Dapp->>II: notify_user(in_app_principal,<br/>PushAlert{title, body})
    activate II
    Note over II: PRINCIPAL_INDEX[p]<br/>→ (anchor, hostname)
    Note over II: APP_CONSENT[(anchor, host)]<br/>+ caller == allowed_caller
    Note over II: DEVICE_SUBSCRIPTIONS[anchor]<br/>+ rate-limit decrement

    loop for each device
        Note over II: RFC 8291 encrypt<br/>(ECDH × p256dh, HKDF + auth,<br/>AES-128-GCM)
        II->>Spawn: ic_cdk::spawn(send_push)
    end
    II-->>Dapp: Ok(())
    deactivate II
    Note right of Dapp: dApp's await resolves in ~ms

    rect rgba(180, 180, 180, 0.1)
        Note over Spawn,Phone: ── asynchronous, one future per device ──
        activate Spawn
        Spawn->>Push: POST {endpoint}<br/>VAPID-signed, encrypted body
        Push-->>Spawn: 201 / 410 / 5xx
        alt 410 Gone
            Spawn->>II: delete_subscription
        else 5xx / 429
            Note over Spawn: log; no retry in PoC
        end
        deactivate Spawn
        Push-->>SW: push event<br/>(ciphertext)
        activate SW
        Note over SW: browser decrypts<br/>using device's keys
        SW->>Phone: showNotification(hostname,<br/>{ body })
        deactivate SW
        Note over Phone: 📱 banner appears<br/>"FooApp: Bob sent you a message"
    end
```

Things to call out from the diagram:

- **`notify_user` returns `Ok` *before* any HTTPS traffic leaves the
  canister.** The outcall is detached via `ic_cdk::spawn`. dApp
  latency is in ms.
- **The payload is encrypted in II's address space**, before it goes
  anywhere. The push relay only ever sees ciphertext.
- **The SW does no canister round-trip.** It receives the push event
  with the payload already decrypted by the browser, and just calls
  `showNotification`. No `get_pending_for_anchor`, no session
  delegation lookup at push time.
- **`410 Gone` deletes the subscription** — push-relay-driven cleanup
  (§9, §14).
- **`5xx / 429` is logged and dropped.** No retry in PoC.

Notification is rendered by II's SW with the dApp's name in the body
(e.g. `"FooApp: you have a new message"`). At the OS level the
notification is attributed to II (or to whatever brand II ships its PWA
under).

## 5. Data model — why each index exists

Three stable maps, each justified by a question the other two can't answer.

### `DEVICE_SUBSCRIPTIONS : anchor → Vec<DeviceSubscription>`

The actual push targets, plus the per-device crypto material that
RFC 8291 needs to encrypt payloads for this device. Keyed by anchor
because **push subscriptions are per-device, not per-app** — the user
installed II once on this device; that one subscription serves every
app the user notifies through II.

```rust
struct DeviceSubscription {
    endpoint: String,    // RFC 8030 push endpoint
    p256dh:   [u8; 65],  // device's P-256 public key (RFC 8291 ECDH peer)
    auth:     [u8; 16],  // device's auth secret (RFC 8291 HKDF input)
    added_at: u64,
}
```

All three of `endpoint`/`p256dh`/`auth` are load-bearing now — they
travel together at subscription time and are used together on every
push (§11).

Liveness ("is this device still real?") is **not** tracked here — we
rely on the push relay's response codes (`410 Gone` → delete
subscription). This avoids turning the canister into a passive presence
tracker.

### `APP_CONSENT : (anchor, frontend_hostname) → AppConsent`

Per-(user, app) consent + caller authorization. Keyed this way because:

- **We need to know if this anchor opted into notifications from this
  specific app.** Per-anchor isn't enough (consent is per app) and
  per-principal isn't enough (we want anchor-scoped management).
- **We need to know which canister principal is allowed to call
  `notify_user` for this app.** The dApp registers this at consent time.

```rust
struct AppConsent {
    allowed_caller: Principal,
    consented_at:   u64,
}
```

### `PRINCIPAL_INDEX : in_app_principal → (anchor, frontend_hostname)`

A reverse index. `notify_user` arrives with only an in-app principal,
but consent lives at `(anchor, hostname)` and devices live at `anchor`.
Without this index we'd have to derive principals by iterating
`(anchor × hostname)` pairs — not viable.

This is the "we need X because we can't do Y" map: we can derive
principals from `(anchor, hostname)`, but we can't reverse the
derivation. So we store the reverse explicitly, and keep it in sync
with `APP_CONSENT` (insert/delete in the same call).

### No spool — the payload travels through the relay

In the empty-body variant (see §20) the canister had to hold the
notification content in a short-lived `PENDING_NOTIFICATIONS` map
until the Service Worker came back to fetch it. With RFC 8291
encrypted payloads, the content rides through the push relay itself.
**Nothing notification-bearing is stored on II beyond the moment
`notify_user` returns.**

## 6. Types

```candid
type PushAlert = record {
  title : text;
  body  : text;
};

type NotifyError = variant {
  NoConsent;          // (anchor, app) hasn't enabled push
  Unauthorized;       // caller isn't the registered allowed_caller
  NoDevices;          // anchor has no registered devices
  RateLimited;
  PayloadTooLarge;
};

service : {
  // The PoC surface.
  notify_user : (principal, PushAlert) -> (variant { Ok; Err : NotifyError });

  // Out of PoC scope (controllers-only backdoor for tests):
  // - upsert_subscription(endpoint, p256dh, auth)   // caller() = anchor's session principal
  // - set_app_consent(hostname, allowed_caller)
};
```

Limits:

- `title` ≤ 64 bytes, `body` ≤ 256 bytes (UTF-8).
- `endpoint` ≤ 2 KB, `https://`, host IPv6-reachable.

## 7. Authorization model

Three options were on the table:

| Option | Who can call `notify_user` | PoC fit |
|---|---|---|
| **A. Open** | anyone with the principal | simplest, but principal leakage → spam |
| **B. Caller-bound** | only the canister owning the app's frontend origin | most correct, needs origin → canister registry |
| **C. Config-recorded caller** | `APP_CONSENT` carries `allowed_caller` | middle ground; one extra field |

**Choice: C.** Set at consent time; checked on every `notify_user`.

```rust
if caller() != consent.allowed_caller { return Err(Unauthorized); }
```

## 8. The `inspect_message` decision

`inspect_message` is an **admission gate**, not an execution context:

| Capability | `inspect_message` | `update` |
|---|---|---|
| Read state | ✅ | ✅ |
| Write state | ❌ | ✅ |
| Make HTTPS outcalls | ❌ | ✅ |
| Runs on inter-canister calls | ❌ (ingress only) | ✅ |
| Replicated execution | ❌ (single replica) | ✅ (consensus) |
| Cycles charged for "accept" | ❌ | ✅ |

Two consequences:

1. **The outcall must live in an update.** HTTPS outcalls require
   consensus on the response; only replicated mode provides it.

2. **`inspect_message` is silent for the primary caller.** dApp backend
   canisters call `notify_user` inter-canister; `inspect_message` does
   not fire. It's a DoS gate against direct ingress spam, nothing more.

We wire it up, but the authz check in the update handler is the real
gate.

### What `inspect_message` does

Cheap, read-only spam defense for the ingress edge:

```rust
#[ic_cdk::inspect_message]
fn inspect_message() {
    if method_name() != "notify_user" { return accept_message(); }

    let (p, alert): (Principal, PushAlert) =
        match decode_args() { Ok(v) => v, Err(_) => return };

    if alert.title.len() > MAX_TITLE || alert.body.len() > MAX_BODY { return; }
    if !storage::principal_known(p)                                { return; }
    if rate_limiter::peek(caller(), p) == 0                        { return; }

    accept_message();
}
```

Read-only peek — no decrement. The decrement happens in the update.

## 9. Cost model: one outcall per device per notification

The dominant cost is the HTTPS outcall — ~50 M cycles, ~2-5 s. With
RFC 8291 each notification carries its own ciphertext, so there's no
multi-notification-into-one-wakeup batching like the empty-body
variant offered: every notification means one outcall per active
device.

```
per-notification cost  ≈ active_devices × (
                            ~5 M  RFC 8291 encrypt
                          + ~10 M VAPID sign (inline; cacheable in production)
                          + ~50 M HTTPS outcall
                        )
```

The "missing" coalescing is the one real tax we pay for end-to-end
encryption — and we accept it, because the alternative was holding
plaintext on II for 5 minutes per notification waiting for a SW pull
(see §20).

### Dead-device cleanup via push-relay response codes

| Relay response | Action |
|---|---|
| `201 Created` / `202 Accepted` | success — no canister state touched |
| `410 Gone` / `404 Not Found` | **delete this `DeviceSubscription`** |
| `429 Too Many Requests` | log; drop; no retry in PoC |
| `5xx` | log; drop; no retry in PoC |

The first push to a dead device costs one wasted outcall. After that
the subscription is gone. We pay one wasted call per dead device per
lifetime in exchange for storing zero presence state (§14).

### Spawn the outcall

`notify_user` does cheap state work (lookup, authz, encrypt) and
**`ic_cdk::spawn`s the outcall as a detached future** — same detach
pattern `single_flight_cache` uses. The dApp's `await` resolves in
~ms regardless of how many devices are about to be pushed to. The
spawned future runs the POST + handles the response code.

### What we *don't* do in the PoC

- **Multi-notification batching.** A future could combine N alerts into
  one ciphertext POST and have the SW iterate them; this needs a
  debounce window + timer. Deferred (§18).
- **Retry-with-backoff.** A 5xx is logged and dropped. Production
  wants a bounded retry (§18).
- **VAPID JWT cache.** Each spawned future signs inline (~10 M
  cycles). Production caches per push-relay origin via
  [`single_flight_cache`](../src/internet_identity/src/single_flight_cache.rs) (§18).

## 10. The update handler

```rust
#[ic_cdk::update]
fn notify_user(p: Principal, alert: PushAlert) -> Result<(), NotifyError> {
    if alert.title.len() > MAX_TITLE || alert.body.len() > MAX_BODY {
        return Err(NotifyError::PayloadTooLarge);
    }

    let (anchor, hostname) = storage::lookup_principal(p)
        .ok_or(NotifyError::NoConsent)?;
    let consent = storage::get_consent(anchor, &hostname)
        .ok_or(NotifyError::NoConsent)?;

    if caller() != consent.allowed_caller {
        return Err(NotifyError::Unauthorized);
    }

    let devices = storage::devices_for(anchor);
    if devices.is_empty() { return Err(NotifyError::NoDevices); }

    if !rate_limiter::consume(caller(), p) {
        return Err(NotifyError::RateLimited);
    }

    let plaintext = serde_json::to_vec(&NotificationPayload {
        hostname: hostname.clone(),
        title: alert.title.clone(),
        body:  alert.body.clone(),
    }).expect("payload encodes");

    for d in devices {
        // Encrypt under THIS device's keys. RFC 8291 derives a content
        // encryption key from ECDH(II_VAPID_priv, d.p256dh) + HKDF(d.auth).
        let ciphertext = rfc8291::encrypt(&plaintext, &d.p256dh, &d.auth);
        let endpoint = d.endpoint.clone();
        ic_cdk::spawn(async move {
            match send_push(&endpoint, &ciphertext).await {
                Ok(_)                 => {}
                Err(PushError::Gone)  => storage::delete_subscription(&endpoint),
                Err(_)                => { /* log; no retry in PoC */ }
            }
        });
    }
    Ok(())
}
```

`notify_user` is **not `async`** — there is no point where we suspend.
Encryption is synchronous (pure CPU work in the canister). The outcall
is detached. The dApp's call resolves in milliseconds.

## 11. HTTPS outcall + RFC 8291 details

The spawned future POSTs to the push relay with the RFC 8030
envelope and the RFC 8291 ciphertext:

```rust
async fn send_push(endpoint: &str, ciphertext: &[u8]) -> Result<(), PushError> {
    let vapid_jwt = vapid::sign(endpoint).await?;  // ~10M cycles inline
    let req = CanisterHttpRequestArgument {
        url: endpoint.into(),
        method: HttpMethod::POST,
        headers: vec![
            HttpHeader { name: "ttl".into(),               value: "60".into() },
            HttpHeader { name: "urgency".into(),           value: "normal".into() },
            HttpHeader { name: "content-encoding".into(),  value: "aes128gcm".into() },
            HttpHeader { name: "content-type".into(),      value: "application/octet-stream".into() },
            HttpHeader { name: "authorization".into(),
                value: format!("vapid t={vapid_jwt}, k={II_VAPID_PUBKEY_B64URL}") },
        ],
        body: Some(ciphertext.to_vec()),
        max_response_bytes: Some(2 * 1024),
        transform: Some(strip_response_transform),
    };
    let cycles = http_request_required_cycles(&req);
    let response = http_request(req, cycles).await?;
    match response.status {
        201 | 202 => Ok(()),
        404 | 410 => Err(PushError::Gone),
        429       => Err(PushError::RateLimited),
        500..=599 => Err(PushError::Transient),
        other     => Err(PushError::Unexpected(other)),
    }
}
```

### RFC 8291 in three steps

```
1. Generate a fresh ephemeral P-256 keypair (epk_priv, epk_pub).

2. Compute:
     shared_secret = ECDH(epk_priv, device.p256dh)
     ikm           = HKDF(salt = device.auth,
                          ikm  = shared_secret,
                          info = "WebPush: info\x00" ‖ device.p256dh ‖ epk_pub,
                          len  = 32 bytes)
     CEK           = HKDF(salt = random_salt_16,
                          ikm  = ikm,
                          info = "Content-Encoding: aes128gcm\x00",
                          len  = 16 bytes)
     NONCE         = HKDF(salt = random_salt_16,
                          ikm  = ikm,
                          info = "Content-Encoding: nonce\x00",
                          len  = 12 bytes)

3. Body = aes128gcm_header
        ‖ salt(16)
        ‖ rs(4)         (record size, plaintext + 17 padding + tag overhead)
        ‖ idlen(1)      (= 65, length of epk_pub_uncompressed)
        ‖ epk_pub(65)
        ‖ AES-128-GCM(key=CEK, nonce=NONCE,
                       plaintext ‖ 0x02 ‖ padding)
```

Encryption is pure CPU — no canister `await`. Implementation uses
`p256` + `hkdf` + `aes-gcm` crates (already on `Cargo.toml` neighbours).

### VAPID signing — inline for PoC

`vapid::sign` is an async ECDSA signature over the management
canister's `sign_with_ecdsa` (~10 M cycles, ~2 s). PoC signs inline
per outcall. Production hardening (§18) memoises per push-relay origin
via [`single_flight_cache`](../src/internet_identity/src/single_flight_cache.rs)
with `fresh_for ≈ 11 h`.

## 12. The Service Worker side — minimal

With encryption, the SW does almost nothing on push:

```javascript
self.addEventListener('push', (event) => {
  const payload = event.data?.json();       // browser already decrypted
  if (!payload) return;                      // nothing to render
  event.waitUntil(
    self.registration.showNotification(payload.hostname, {
      body: payload.body,
      data: { openUrl: payload.openUrl },
    })
  );
});

self.addEventListener('notificationclick', (e) => {
  const url = e.notification.data?.openUrl ?? 'https://id.ai';
  e.waitUntil(self.clients.openWindow(url));
});
```

No IndexedDB lookup at push time, no canister round-trip, no fallback
branch — the payload either decrypts and renders, or it doesn't (and
the browser shows its generic "background activity" warning, which is
fine).

### Subscription registration (still requires session delegation)

The SW only needs the session-delegation chain at **subscription
time** — to call `upsert_subscription(endpoint, p256dh, auth)` as
the anchor. That's a one-time-per-device thing, not a per-push thing.

```javascript
// On activation, or in the consent UI:
const sub = await registration.pushManager.subscribe({
  userVisibleOnly: true,
  applicationServerKey: II_VAPID_PUBLIC_KEY,
});
const { endpoint, keys: { p256dh, auth } } = sub.toJSON();
const actor = await actorFromSessionDelegation(/* current anchor */);
await actor.upsert_subscription(endpoint, p256dh, auth);
```

### PWA install on iOS still required

Same constraint as before: iOS 16.4+ requires the user to add II's PWA
to the home screen for Web Push to work at all. Independent of RFC 8291.

## 13. Storage / upgrade safety

Three stable-memory regions:

```rust
const MEMORY_ID_DEVICE_SUBSCRIPTIONS: MemoryId = MemoryId::new(N);
const MEMORY_ID_APP_CONSENT:          MemoryId = MemoryId::new(N + 1);
const MEMORY_ID_PRINCIPAL_INDEX:      MemoryId = MemoryId::new(N + 2);
```

**No `PENDING_NOTIFICATIONS` region.** The payload travels through
the push relay; nothing notification-bearing is held on II.

Per the project rule, **stable-memory versioning is sacred**: bump the
storage version, add migrations only via additive new memory IDs, never
repurpose. Each value type carries a `version: u8` byte.

Invariant maintenance:

- `APP_CONSENT[(anchor, host)]` exists ⇔ `PRINCIPAL_INDEX[derive(anchor, host)]` exists.
- Anchor deletion sweeps `APP_CONSENT[(anchor, *)]`,
  `PRINCIPAL_INDEX[derive(anchor, *)]`, and `DEVICE_SUBSCRIPTIONS[anchor]`.
- `post_upgrade` does **nothing notification-specific** — there is no
  spool to clean, no inflight flag to sweep. Ordinary II upgrade
  semantics apply.

A spawned outcall future that's mid-`await` when the canister
upgrades simply dies; its target notification is lost. With no retry
in PoC, the user misses that one notification. Acceptable for a
spike.

## 14. Privacy of the data

> "this should not be available to anyone except II"

End-to-end encrypted payloads make this section much shorter:

- **No public query exposes `DeviceSubscription` or `AppConsent`.** The
  user's own listing uses an authenticated method that resolves via
  `anchor → …`.
- **The push relay never sees plaintext.** Every push body is RFC 8291
  ciphertext encrypted under the device's `p256dh`/`auth`. Even a
  fully compromised relay cannot read user notifications.
- **No spool means no notification content is stored on II.** Once
  `notify_user` returns, the only trace is whatever the dApp keeps in
  its own canister.
- **Endpoint URLs never echo back in errors.** Error variants stay
  opaque.
- **`inspect_message` rejecting on "principal unknown"** leaks the
  existence of a subscription. PoC accepts this; production should
  reject in constant work.

### Passive presence — what the design deliberately doesn't track

II already knows what its users do *actively*: auth ceremonies,
session-delegation mint times, every dApp's authenticated calls. The
notification system adds **zero new passive observation channels**:

- No `last_seen_at` timestamp per device (an earlier draft had this;
  dropped — see §20).
- No per-notification delivery log.
- No "device pulled" event log (because the SW doesn't pull anything).

Liveness is inferred from push-relay `410 Gone` responses, which are
vendor-authoritative and don't require II to passively observe user
behaviour.

### Side channels via error variants

A dApp can still probe a user's state by observing which `NotifyError`
comes back: `NoConsent`, `NoDevices`, `RateLimited`. PoC accepts the
probing surface for debuggability. Production hardening collapses this
to a single opaque `NotDelivered` variant (§18).

## 15. Rate limiting (PoC sketch)

Two layers:

- **Per-(caller_canister, target_principal) token bucket** — bounds
  individual app/user pairs. Heap-resident, resets on upgrade.
  Budget e.g. 10 / hour, 50 / day.
- **Global outcall budget** capping total push outcalls per minute,
  bounding II's cycle exposure under a multi-app spike.

The empty-body variant had per-device coalescing softening the outcall
budget; with one-outcall-per-notification the per-(caller, target)
limit is doing more of the work.

## 16. Platform reality

| Platform | What user must do |
|---|---|
| Android Chrome / Firefox / desktop browsers | Visit II's page once, grant permission. PWA install not required. SW persists. |
| **iOS 16.4+** | **Must add II's PWA to home screen.** Web Push for non-installed PWAs is not supported. Apple platform constraint. |
| iOS < 16.4 | No Web Push, period. |

## 17. End-to-end walkthrough

```rust
// dApp canister code
let _: (Result<(), NotifyError>,) = ic_cdk::call(
    INTERNET_IDENTITY_CANISTER_ID,
    "notify_user",
    (user_in_app_principal, PushAlert {
        title: "FooApp".into(),
        body:  "Bob sent you a message".into(),
    }),
).await.expect("call to II failed");
```

What happens, step by step:

1. **Inter-canister call lands at II.** `inspect_message` doesn't
   fire (it's not ingress).
2. **Shape check** (sizes).
3. **`PRINCIPAL_INDEX[p]`** → `(anchor, hostname)` or `Err(NoConsent)`.
4. **`APP_CONSENT[(anchor, hostname)]`** → consent record or
   `Err(NoConsent)`.
5. **`caller() == consent.allowed_caller`** or `Err(Unauthorized)`.
6. **`DEVICE_SUBSCRIPTIONS[anchor]`** → device list (or
   `Err(NoDevices)`).
7. **Rate-limit decrement** or `Err(RateLimited)`.
8. **For each device**: RFC 8291 encrypt the JSON `{hostname, title,
   body}` under that device's keys. Then `ic_cdk::spawn` an outcall
   future for that device.
9. **Return `Ok(())`** to the dApp. Wall time: a few ms.

Asynchronously, each spawned future:

10. **Sign VAPID JWT** for this push-relay origin.
11. **POST the ciphertext** to the device's endpoint with the RFC 8030
    headers + VAPID auth.
12. **Apply response code:**
    - `201/202` → done.
    - `404/410` → delete the `DeviceSubscription`.
    - `5xx`/`429` → log and drop. No retry in PoC.

On the device:

13. **Push relay forwards** the encrypted payload to the OS.
14. **Browser decrypts** using the SW's subscription private key.
15. **SW's `push` handler** reads the cleartext, calls
    `showNotification(hostname, { body })`.
16. **Phone screen lights up.**

## 18. Open questions

1. **Session-delegation TTL** still bounds **when a device can first
   subscribe** (`upsert_subscription` needs the SW to be authenticated
   as the anchor). Once subscribed, the SW does nothing
   anchor-authenticated at push time, so push delivery doesn't degrade
   when the session expires. Re-subscribe only when the user comes
   back to II.
2. **Compromised `allowed_caller`** — a compromised dApp canister can
   spam until revoked. PoC has no kill switch; revocation lands with
   opt-out.
3. **VAPID key management.** II needs a stable VAPID keypair. Generate
   at first init, store the private half in stable memory, expose the
   public half via a query.
4. **Notification re-attribution.** OS shows "Internet Identity" as
   the sender. Privacy *feature* at the OS level — the device doesn't
   know which dApp pinged.
5. **Error variant unification.** Collapse `NoConsent` / `NoDevices` /
   `RateLimited` into a single opaque `NotDelivered` for the public
   surface; keep granular variants behind a debug flag.

### Production hardening (not "open" — known with clear shape)

- **Retry-with-backoff for transient relay failures.** Today a
  `5xx`/`429` is dropped. Production wants bounded retry — the moment
  a queue + timer earns its complexity.
- **Multi-notification batching.** Combine multiple alerts for the
  same device into one ciphertext per outcall. Requires a debounce
  window + timer + SW-side iteration. Recovers the empty-body
  variant's per-device coalescing benefit without giving up
  end-to-end encryption.
- **VAPID JWT caching** via
  [`single_flight_cache`](../src/internet_identity/src/single_flight_cache.rs).
  Drops per-push signing cost from ~10 M cycles to amortised ~0.

## 19. Minimal first slice (1-day spike)

1. Add the three stable regions + memory IDs.
2. Add `upsert_subscription` (anchor-authenticated) +
   controllers-only `set_app_consent` backdoor for tests.
3. **Implement RFC 8291 encryption module.** Pure-CPU, fully
   unit-testable against the spec's worked example (RFC 8291 §5).
4. **Implement VAPID signing module.** ES256 JWT over management
   canister `sign_with_ecdsa`. Returns the `Authorization` header
   value.
5. Implement `notify_user`: validate → lookup → authz → rate limit →
   encrypt per device → `ic_cdk::spawn` outcall future. Return Ok.
6. Implement `inspect_message` gate.
7. Implement the spawned `send_push` outcall + response-code handling.
8. PocketIC test: register a fake device with known
   `(p256dh, auth)` from RFC 8291 §5, set consent, call `notify_user`,
   capture the mock receiver's request, decrypt it offline with the
   test vector keys, assert plaintext matches.

What's deliberately NOT in the spike: queue, timer, retry/backoff,
debounce/batching, VAPID cache, error-variant unification, frontend
SW + PWA manifest + consent UI.

## 20. Variant: empty-body + SW pull (rejected)

An earlier version of this design avoided RFC 8291 entirely by
shipping **empty push bodies** and having the SW round-trip back to
II for the payload. Recorded here so the trail of reasoning is
preserved.

### How it worked

- `notify_user` spooled `{anchor, hostname, alert, expires_at}` to a
  `PENDING_NOTIFICATIONS` stable map (5-min TTL).
- Each device had an `inflight_outcall_at` flag to coalesce: a wakeup
  already in flight to a device → new notifications spooled but no
  new outcall.
- The spawned outcall POSTed an **empty body** to the relay.
- On wake, the SW used the session-delegation IDB record to call
  `get_pending_for_anchor`, fetched every spooled payload for that
  anchor, marked them consumed, and rendered.

### Why we rejected it for the PoC

- **Round-trip latency.** The SW had to call II before rendering,
  adding ~1-3 s to the user-visible delay on every push.
- **Plaintext on II.** Notification bodies sat in stable memory for
  up to 5 minutes — a real storage footprint to enumerate / sweep
  / privacy-review.
- **Two-call delivery path.** "Wake the device, then re-call II" has
  more failure modes than "send the content, browser decrypts."
- **No end-to-end encryption.** The push relay sees nothing in the
  empty variant, but II itself sees plaintext until the SW pulls.

### What it *was* good for

- **Per-device coalescing.** N notifications in a burst collapsed to
  1 outcall. RFC 8291 loses this; you get it back with the debounce
  window (§18 production hardening).
- **No canister-side crypto.** Empty body needed no encryption code.
  The trade-off: ~300 lines of RFC 8291 in the canister vs. a
  stateful spool + reverse-pull API.

If signing volume gets large enough that ~5 M cycles of encryption
per device per notification matters, we'd revisit by adding the
batching hardening (§18) — not by reverting to the empty-body
variant.

