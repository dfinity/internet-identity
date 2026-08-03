# Push notifications — the dApp-side client library

The helper a dApp runs to feed II a large send at a pace II can accept,
retry safely, and keep the durable campaign state that lets II stay stateless.

Design rationale lives in [push-notifications.md](push-notifications.md); this
document is reference material and assumes you have read at least
[how it works, in plain terms](push-notifications.md#how-it-works-in-plain-terms).



> _In short: because II stores nothing per send, a small library on the dApp's
> side keeps the list, sends it to II in paced pieces, retries failures, tracks
> who got notified, and personalizes text. The dApp calls one simple
> "notify these users" method; the library handles the rest._

Since II is stateless for campaigns, the durable coordination is a library the
dApp runs. This is where the heavy list and its bookkeeping live — where the
volume originates.

Security is unaffected by moving this out: II re-validates sender-origin,
consent and origin-pinning on **every** chunk, so a buggy or malicious library
cannot fake consent, target another dApp's users, or exceed admission limits —
it can only mismanage its own campaign. **The library is a convenience, never a
control.**

### Where it runs, and the one real choice

The library must live somewhere with durable state and a scheduler, because it
owns a campaign that outlives any single call. Two viable hosts:

|                 | dApp **canister** (recommended)                 | dApp **web2 backend**                       |
| --------------- | ----------------------------------------------- | ------------------------------------------- |
| Durability      | stable memory, survives upgrades                | whatever the backend already has            |
| Scheduling      | `ic_cdk_timers`                                 | cron / job runner                           |
| Calls II via    | inter-canister call (**can attach cycles**)     | ingress (**cannot attach cycles**)          |
| Sender identity | its own canister principal — registers directly | needs a canister to call on its behalf      |
| Fits            | on-chain apps, the default                      | apps whose audience already lives off-chain |

The canister host is the recommended shape: it is the only one that can attach
cycles (relevant if sender-pays ever lands) and the only one whose sender
identity is a principal II can register directly. A web2 backend still needs a
small companion canister to be the registered sender, so it ends up running both.

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

Two things worth being precise about, because they are where implementations go
wrong:

- **`status = Admitted` means "II accepted it into its buffer", not
  "delivered".** There are no delivery receipts. The library must not present
  `Admitted` to the dApp as "the user got it"; the honest label is "sent".
- **`cursor` + `last_drain_epoch` together are the recovery state.** On restart,
  or when II's `drain_epoch` moves, everything `InFlight`/`Admitted` since that
  epoch is suspect and must be re-sent (see below).

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

Everything else — chunking, pacing, retry, epoch recovery, backoff, templating —
is internal. A dApp author should never have to know what a chunk is.

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
| `drain_epoch` moved (II upgraded)  | rewind to the oldest unconfirmed chunk and re-send              |
| Its own host restarts mid-campaign | resume from `cursor`; re-send anything `InFlight`               |
| `NoConsent` for a target           | mark terminal, stop retrying, surface to the dApp               |
| Call trapped / rejected            | retry the _same_ `chunk_id`; never mint a new one               |
| Campaign outlives its own deadline | `ttl_seconds` already bounds it II-side; mark `Dropped` locally |

Because retries and epoch-recovery both re-send chunks, **duplicate delivery is
expected by design** — which is the other half of why `msg_id` dedup on the
device is a v1 requirement, not a nicety.

