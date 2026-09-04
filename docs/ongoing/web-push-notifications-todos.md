# Web Push notification PR-stack TODOs

These are implementation follow-ups discovered while documenting the open Web Push notification stack. They are kept separate from the design and integration guide so those documents describe the intended contract rather than temporary gaps in the current branches.

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
