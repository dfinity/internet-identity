# Notification chat

One chat room, kept by this canister, which also serves the web page. Anyone
signed in with Internet Identity may join. Every message notifies every other
member through Internet Identity's notifications.

## Calling conventions

- `join`, `leave`, `send` and `claim_name` are updates and need a signed
  caller: the anonymous principal is refused with `sign in first`. The queries
  `room`, `notification_metrics`, `notification_config` and `get_api_doc`
  take any caller.
- Every timestamp (`joined_at`, `at`, `start`, `since`) is a `nat64` of
  nanoseconds since the Unix epoch.
- Text is UTF-8. A message is cut to its first 500 characters, a name to its
  first 32; neither is refused for length.
- Refusals are traps whose message is one of the quoted phrases below.

## Methods

- `join : () -> ()` puts the caller in the room, which is what makes them a
  recipient of what others send. Joining again changes nothing. Refused with
  `the room is full` once 100 principals are in.
- `leave : () -> ()` takes the caller out, which is what stops the
  notifications. Nothing happens for a caller who is not in.
- `send : (text) -> ()` posts a message. Refused with `join the room first`
  for a caller who is not in. It notifies every other member: the title is the
  writer's name, or the ends of their principal without one; the body is the
  text; the link is this app's origin. Each message is a notification of its
  own, with no key, so a newer message never replaces an earlier one.
- `claim_name : (text) -> (text)` names the caller, replacing any name they
  had, and returns the name given: as written when nobody else holds it,
  otherwise the first free one of `name_1`, `name_2`, … Names are compared
  without regard to case. Refused with `a name needs at least one character`
  for blank input. A name is kept until the caller claims another.
- `room : () -> (Room) query` returns the members with when they joined, the
  newest 100 messages oldest first (`from`, `at`, `text`), and `names`: the
  claimed names of the members and of whoever wrote a message still shown.
  Nothing announces a new message to a caller; read the room again (the page
  reads it every three seconds).
- `notification_metrics : () -> (Metrics) query` returns the notification
  library's delivery counts per hour (`queued`, `accepted`, `received`,
  `opened`, `dropped`, `deferred`), the backlog waiting to be accepted, and
  the last misconfiguration it noted, which stays noted after a fix.
- `notification_config : () -> (NotificationConfig) query` returns the origin
  notifications are sent for and the Internet Identity canister they go
  through, each null where unset. While either is unset, every send is
  counted and dropped.
- `http_request` serves the page and the `/.well-known/` files.
- `_internet_identity_notification_content`, `_received` and `_opened` exist
  for Internet Identity alone, which signs its calls; any other caller gets
  null or nothing.

## What persists and what can be undone

- A message cannot be edited or deleted. It leaves the room once 100 newer
  ones have arrived.
- A canister upgrade empties the room (members and messages). Names and the
  notification library's state survive it.
- There are no fees, tokens or transfers: no call costs the caller anything
  beyond the call itself.

## Notifications

A member is notified only where they allowed this app to notify them in
Internet Identity, on a browser registered for push, and only while the
canister is configured as `notification_config` reports. What became of each
notification is in `notification_metrics`.
