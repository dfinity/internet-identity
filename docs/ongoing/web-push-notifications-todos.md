# Web Push notification PR-stack TODOs

These are implementation follow-ups discovered while documenting the open Web Push notification stack. They are kept separate from the design and integration guide so those documents describe the intended contract rather than temporary gaps in the current branches.

## Client library: own the campaign and pending-content lifecycle

**Repository:** [internet-identity-notifications-client](https://github.com/dfinity/internet-identity-notifications-client)

The current clients are thin wrappers around `notification_send`. The intended integrator surface is higher-level: an app supplies a campaign and the library owns the durable coordination inside the dApp canister.

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
- [ ] Add a Motoko mixin that owns the state and injects `ii_pending_notifications`.
- [ ] Add the equivalent Rust pending-content abstraction and minimal endpoint wiring.
- [ ] Keep `sendersDocument` and `SendersDocument` as the only well-known setup required from the app.
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
- [ ] Update the integration guide once the final multi-canister behavior is implemented.
