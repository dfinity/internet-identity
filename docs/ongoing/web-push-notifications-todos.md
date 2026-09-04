# Web Push notification PR-stack TODOs

These are implementation follow-ups discovered while documenting the open Web Push notification stack. They are kept separate from the main and dApp integration designs so those documents describe the intended contract rather than temporary gaps in the current branches.

## Client library: own the campaign and pending-content lifecycle

**Repository:** [internet-identity-notifications-client](https://github.com/dfinity/internet-identity-notifications-client)

The current clients only wrap `notification_send`. They need to accept a campaign from the app and manage its durable state inside the dApp canister.

- [ ] Define the campaign, recipient, pending-content, progress, and terminal-state types.
- [ ] Store campaign progress and pending content durably inside the dApp canister.
- [ ] Accept a campaign ID and notification list as the common send API.
- [ ] Store each recipient's content before submitting its ping to II.
- [ ] Split campaigns into bounded `notification_send` batches.
- [ ] Pace retries using `retry_after_ms`, with jitter, without advancing unaccepted recipients.
- [ ] Persist the last `resend_epoch` and recover affected campaign work when it changes.
- [ ] Resume incomplete campaigns after a dApp canister upgrade.
- [ ] Aggregate accepted and rejected recipients into an honest campaign status that does not claim delivery receipts.
- [ ] Add update, remove, pause, resume, cancel, and cleanup operations.
- [ ] Refactor the Motoko package to follow the
      [`identity-attributes`](https://mops.one/identity-attributes) layout: make
      the root `src/lib.mo` a bare mixin and move public types, low-level II
      calls, and implementation details into separate modules.
- [ ] Have the Motoko mixin own the durable campaign and pending-content state,
      inject `ii_pending_notifications`, and expose campaign operations to the
      containing actor.
- [ ] Add the equivalent Rust pending-content abstraction and minimal endpoint wiring.
- [ ] Keep `sendersDocument` and `SendersDocument` as the only well-known setup
      required from the app while the initial sender-binding mechanism remains.
- [ ] Add campaign restart, partial acceptance, epoch change, expiry, update, and removal tests.

## Service worker: pull content from every sender canister

**PR:** [#4258](https://github.com/dfinity/internet-identity/pull/4258)

The current service worker reads only the first principal from `/.well-known/ii-notification-senders` and treats it as the content canister. The sender document authorizes several canisters, and any of them may own notification content. List order should not assign one canister a separate, undocumented role.

- [ ] Parse every valid principal in `senders`, within the same maximum accepted by the backend.
- [ ] Query `ii_pending_notifications` on every listed canister as the user.
- [ ] Run independent canister pulls concurrently, with the existing timeout and failure handling applied to each one.
- [ ] Reconcile notifications per origin and canister, not only per origin.
- [ ] Namespace the browser notification tag by canister principal and notification ID so two canisters may use the same ID without replacing each other.
- [ ] Store the canister principal with the notification data needed for reconciliation and click handling.
- [ ] Treat a failed or timed-out pull as unknown state for that canister. Keep its currently displayed notifications rather than closing them.
- [ ] Treat a successful empty response as an instruction to close every notification previously shown for that canister.
- [ ] Refresh and retry the sender document when one cached canister is removed or replaced.
- [ ] Add tests for multiple senders, duplicate IDs across canisters, one failed sender, one empty sender, and removal of a sender from the well-known document.
- [ ] Update the dApp integration design once the final multi-canister behavior is implemented.

## Sender authorization: replace the well-known binding with app-session registration

**Depends on:** revocable app sessions and the shared account-by-principal
index.

The well-known document and its consent-time HTTP outcall are interim plumbing.
Once an app frontend can call II through a session that resolves to an identity,
origin, and account, it can register its sender canisters directly for that
account's notification authorization.

- [ ] Define how an app supplies its sender canister principals to `AuthClient`.
- [ ] Add a session-authenticated II method that records the sender set without
      accepting an origin from the caller.
- [ ] Store the sender set per account or session-backed notification
      authorization, with an explicit cap and refresh policy.
- [ ] Specify how ending or expiring the app session removes or stops refreshing
      the sender authority and notification access.
- [ ] Resolve the notification recipient through the shared account index and
      authorize the calling canister against that recipient's registered set.
- [ ] Do not deduplicate a sender binding across users; one user's frontend
      cannot authorize a sender for another user's notification access.
- [ ] Remove the consent-time HTTP outcall, cached well-known binding, and the
      caller-supplied `origin` from `notification_send` after migration.
- [ ] Update the client libraries and integration design once the session-backed
      interface is fixed.
