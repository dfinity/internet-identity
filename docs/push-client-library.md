# Push notifications — the dApp-side client library

The helper a dApp runs to feed II a large send at a pace it accepts, retry safely, and
hold the durable campaign state that keeps II stateless. Reference material — rationale
is in [push-notifications.md](push-notifications.md).

Because II stores nothing per send, the durable coordination is a library the dApp
runs: it keeps the list, sends it in paced pieces, retries failures, tracks who was
notified, and personalises text. The dApp calls one "notify these users" method.

Moving this out costs no security: II re-validates sender-origin, consent and
origin-pinning on **every** chunk, so a buggy or malicious library can only mismanage
its own campaign — never fake consent, target another dApp's users, or exceed limits.
**The library is a convenience, never a control.**

### Where it runs, and the one real choice

It needs durable state and a scheduler, since a campaign outlives any single call. Two
hosts:

|                 | dApp **canister** (recommended)                 | dApp **web2 backend**                       |
| --------------- | ----------------------------------------------- | ------------------------------------------- |
| Durability      | stable memory, survives upgrades                | whatever the backend already has            |
| Scheduling      | `ic_cdk_timers`                                 | cron / job runner                           |
| Calls II via    | inter-canister call (**can attach cycles**)     | ingress (**cannot attach cycles**)          |
| Sender identity | its own canister principal — registers directly | needs a canister to call on its behalf      |
| Fits            | on-chain apps, the default                      | apps whose audience already lives off-chain |

Prefer the canister: it's the only host that can attach cycles (for a future
sender-pays) and whose principal II can register directly. A web2 backend needs a
companion canister as the registered sender anyway, so it runs both.

### State it must keep

Per campaign, durable:

```
Campaign {
  campaign_id       : text            // dApp-chosen, unique
  default_alert     : PushAlert       // shared text, if any
  delivery          : PushDelivery    // urgency / ttl / topic for the campaign
  created_at        : nanos
  state             : Building | Sending | Paused | Done | Failed
  cursor            : nat64           // index of the next unsent target
  last_drain_epoch  : nat64           // II's epoch as of the last confirmed chunk
}

Target {                              // one row per recipient
  principal   : Principal             // the user's in-app principal for THIS origin
  alert       : opt PushAlert         // personalization override, else default
  status      : Pending | InFlight | Admitted | NoConsent | Invalid | Dropped
  chunk_id    : opt blob              // which chunk carried it
  attempts    : nat8
}
```

Two things implementations get wrong:

- **`Admitted` means "in II's buffer", not "delivered"** — there are no receipts, so
  present it to the dApp as "sent", never "the user got it".
- **`cursor` + `last_drain_epoch` are the recovery state** — on restart or when II's
  `drain_epoch` moves, everything `InFlight`/`Admitted` since that epoch must be re-sent.

### The send loop

```
for each batch of ≤1000 targets from cursor:
    chunk_id = hash(campaign_id, cursor)        # deterministic → idempotent retry
    r = push_send(chunk_id, delivery, default_alert, recipients)

    if r.drain_epoch != last_drain_epoch:       # II was upgraded
        rewind cursor to the oldest unconfirmed chunk
        last_drain_epoch = r.drain_epoch
        continue

    mark r.rejected targets by reason           # NoConsent → terminal, not retry
    mark the rest Admitted; advance cursor

    if !r.ready:
        sleep(r.retry_after_ms + jitter)        # jitter is not optional
```

Details that matter:

- **`chunk_id` must be deterministic** — derive it from
  `(campaign_id, cursor)`, never from a random value or a timestamp. That is
  what makes a retry idempotent instead of a duplicate. Fix it at **16 bytes**;
  II's dedup set is bounded and LRU-evicted, so oversized or unbounded ids are
  rejected.
- **Jittered exponential backoff, always.** A bare `retry_after_ms` makes every
  rejected sender retry at the same instant, and after an II upgrade every
  client with lost chunks retries simultaneously — a thundering herd precisely
  when II is least able to absorb it.
- **Pipeline no more than a few chunks.** The output queue between two canisters
  is 500 deep, and the point of pacing is to absorb inter-canister latency, not
  to race II's admission control.
- **`NoConsent` is terminal.** It means the user revoked or never granted; retrying
  it wastes admission budget forever. Distinguish it from capacity rejections,
  which _are_ retryable.
- **Chunk by bytes as well as by count.** ≤1000 targets _and_ under the
  size ceiling — with heavy per-recipient personalization the byte bound binds
  first. II enforces both server-side; hitting them is a client bug.

### What it owes the dApp

The public surface should be small enough that the common case is one call:

```
notify(campaign_id, targets, default_alert, delivery) -> CampaignHandle
status(campaign_id) -> { total, sent, no_consent, pending, state }
pause(campaign_id) / resume(campaign_id) / cancel(campaign_id)
```

Everything else — chunking, pacing, retry, epoch recovery, backoff, templating — is
internal; a dApp author never needs to know what a chunk is.

- **Templating / personalization** — expand `template + per-user data` into a
  per-recipient `alert` override entirely client-side. **II never sees a
  template**, which is what keeps personalization off II's storage.
- **Prioritization** — the library decides which campaign or segment goes first.
  Note this is _campaign_ ordering, distinct from per-message `urgency`, which
  rides the relay header and contributes to II's drain order.
- **Status/reporting** — aggregate per-chunk results into campaign progress,
  labelled honestly: `sent` (admitted), `no_consent`, `pending`. Not
  "delivered" — nothing in this design can tell the dApp that.

### Failure modes the library is responsible for

| Failure                            | What the library must do                                        |
| ---------------------------------- | --------------------------------------------------------------- |
| `ready = false`                    | back off with jitter; do **not** advance the cursor             |
| `SenderUnverified`                 | surface the hint once, then retry with backoff — self-heals     |
| `drain_epoch` moved (II upgraded)  | rewind to the oldest unconfirmed chunk and re-send              |
| Its own host restarts mid-campaign | resume from `cursor`; re-send anything `InFlight`               |
| `NoConsent` for a target           | mark terminal, stop retrying, surface to the dApp               |
| Call trapped / rejected            | retry the _same_ `chunk_id`; never mint a new one               |
| Campaign outlives its own deadline | `ttl_seconds` already bounds it II-side; mark `Dropped` locally |

Because retries and epoch-recovery both re-send chunks, **duplicate delivery is
expected by design** — which is the other half of why `msg_id` dedup on the
device is a v1 requirement, not a nicety.

### Sender verification is the library's job, not the dApp's

The dApp's only obligation is to publish `/.well-known/ii-push-senders`; registration
logic belongs here. On `SenderUnverified`, log the hint II returns (the file and
principal) once per origin, then retry with backoff — II verifies in the background, so
a correct file self-heals and a missing one gives one actionable log line instead of a
silent failure. Expose `forceReverify()` (over `push_register_sender`) for a developer
who just fixed their file and doesn't want to wait.

This avoids the day-one failure: publish the file, forget the setup call, and every
send fails with an error that looks like an II bug.

