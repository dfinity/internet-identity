# Integrating with II Web Push notifications

This guide describes what a dApp must provide to send notifications through Internet Identity. For the design and trust model, see [Web Push notifications](web-push-notifications.md).

> **Status:** The feature is not enabled in production and `internet-identity-notifications-client` has not been published yet. The interfaces below describe the current implementation and may change before release.

The Rust and Motoko send clients live in [internet-identity-notifications-client](https://github.com/dfinity/internet-identity-notifications-client). The examples below use the Motoko package name it will expose when published.

## What an app provides

An integration has four parts:

1. Ask II to offer notification consent during sign-in.
2. Publish the canisters allowed to send for the app origin.
3. Expose the notifications currently pending for the signed-in principal.
4. Store the content, then send II a content-free ping to wake the user's devices.

The sender and content provider can be the same canister, but they do not have to be. Every canister that calls `notification_send` must appear in the sender document. The first canister in that document is also the one from which the service worker pulls content.

## 1. Request consent during sign-in

Set `iiNotifications` to `true` in the II authorization request:

```ts
const request = {
  kind: "authorize-client",
  sessionPublicKey,
  maxTimeToLive,
  derivationOrigin,
  iiNotifications: true,
};
```

II may then offer the user notification consent for the origin they are signing in to. The application still receives an ordinary delegated identity. Its principal is the recipient used when sending a notification.

The option is currently implemented by the II window transport. Support in the published `AuthClient` API is required before general release. Applications should not maintain a second notification-consent flow of their own.

## 2. Publish the authorized senders

Before requesting consent, serve this file from the exact app origin used during sign-in:

```text
/.well-known/ii-notification-senders
```

Its body lists every canister allowed to send notifications for that origin:

```json
{
  "senders": [
    "bkyz2-fmaaa-aaaaa-qaaaq-cai",
    "bd3sg-teaaa-aaaaa-qaaba-cai"
  ]
}
```

II fetches the document when the user grants consent. The request must return `200 OK` with valid JSON and at least one valid canister principal. II reads at most 20 senders.

Order matters to the service worker. The first principal is the content canister and must implement `ii_pending_notifications`. Any remaining principals may send pings for the origin but are not queried for content.

For a canister-hosted frontend, serve the document as a certified asset. The response, including its headers, must fit within 64 KiB. The origin must be publicly reachable over HTTPS, so this part of the flow does not work against localhost.

The Motoko client can produce the JSON body:

```motoko
import Notifications "mo:ii-notification-client";
import Principal "mo:base/Principal";

let body = Notifications.sendersDocument([
  Principal.fromText("ryjl3-tyaaa-aaaaa-aaaba-cai"),
]);
```

The Rust client provides the equivalent `well_known::SendersDocument` helper.

## 3. Expose the pending notifications

The II service worker calls this query as the user's per-app principal:

```candid
type PendingNotification = record {
  id : text;
  title : text;
  body : opt text;
};

service : {
  ii_pending_notifications : () -> (vec PendingNotification) query;
};
```

Return the complete set currently pending for `caller`, not only the notification that caused the latest ping.

The service worker reconciles this set with the notifications already shown for the origin:

- A new `id` is displayed.
- An existing `id` is updated in place.
- A previously displayed `id` that is no longer returned is closed.

The `id` therefore needs to be stable for the lifetime of the logical notification. Notification content remains in the dApp canister and is never sent through II.

### Motoko mixin example

A small mixin can expose the pull endpoint while the application keeps ownership of its storage and lookup rules.

`NotificationTypes.mo`:

```motoko
module {
  public type PendingNotification = {
    id : Text;
    title : Text;
    body : ?Text;
  };
};
```

`NotificationPull.mo`:

```motoko
import Principal "mo:base/Principal";
import Types "NotificationTypes";

mixin (pendingFor : Principal -> [Types.PendingNotification]) {
  public shared query ({ caller }) func ii_pending_notifications()
    : async [Types.PendingNotification] {
    pendingFor(caller)
  };
};
```

Include it in the application actor and pass the function that reads the application's notification store:

```motoko
import NotificationPull "NotificationPull";
import NotificationTypes "NotificationTypes";
import Principal "mo:base/Principal";

persistent actor {
  func pendingFor(caller : Principal)
    : [NotificationTypes.PendingNotification] {
    // Return every notification currently pending for this principal.
    []
  };

  include NotificationPull(pendingFor);
};
```

The callback must derive the result from `caller`. Do not accept the recipient as an argument to the public query, since the service worker already authenticates as that recipient.

## 4. Store content, then send the ping

Store or update the pending notification before calling II. A device may fetch immediately after II accepts the ping, so sending first creates a race in which the service worker wakes before the content exists.

With the Motoko client:

```motoko
import Notifications "mo:ii-notification-client";
import Principal "mo:base/Principal";
import Text "mo:base/Text";

transient let iiCanisterId = Principal.fromText("rdmx6-jaaaa-aaaaa-aaadq-cai");
transient let client = Notifications.NotificationClient(
  iiCanisterId,
  "https://app.example",
);

func ping(recipient : Principal, id : Text)
  : async Notifications.NotificationSendResponse {
  await client.notify([
    {
      id = Text.encodeUtf8(id);
      recipient;
      urgency = ?#normal;
      expires_at = null;
    },
  ])
};
```

`recipient` is the principal obtained from the user's II delegation for this app and account. `origin` is fixed when the client is constructed and must match the origin used during sign-in and in the sender document.

The `id` in the send request correlates accepted and rejected entries in the response. It is not delivered to the service worker. The pending notification's text `id` is what controls display, replacement, and removal. Applications may use the same logical identifier for both, but they are carried through different paths.

Store the content separately for each recipient. II rejects a principal that did not grant consent for the declared origin.

## Handle the response

`notification_send` returns a batch result:

- `accepted` is the number of pings placed in II's transient buffer. It does not mean they were delivered.
- `rejected` reports `no_consent`, `not_subscribed`, or `invalid` for individual request IDs.
- `retry_after_ms` asks the sender to retry work that was not accepted because the buffer was full.
- `resend_epoch` changes when an II upgrade drops the transient buffer.

Persist the last observed `resend_epoch`. If it changes, resend relevant pings that may have been buffered under the previous epoch. A repeated ping is safe because the service worker reconciles the current pending set by ID.

The Motoko client provides `resendEpochChanged` for comparing and updating the stored epoch. The Rust client provides `ResendEpochTracker`.

## Updating and removing notifications

To update a notification, replace its stored title or body while keeping the same pending ID, then send another ping. Each device updates the notification with that ID in place.

To remove a notification, remove its ID from the pending set and send another ping. On the next pull, each reached device closes the notification because the app no longer reports it as pending.

Without another ping, a closed app has no reason to pull the changed set, so updates and removals are not propagated until the next notification wake-up.

## Integration checklist

- Use one canonical HTTPS origin consistently for sign-in, consent, the sender document, and `notification_send`.
- Set `iiNotifications: true` in the II authorization request.
- Publish `/.well-known/ii-notification-senders` before asking for consent.
- List every canister that may call `notification_send`.
- Store pending notifications by the per-app principal from the II delegation.
- Expose `ii_pending_notifications` as an authenticated query over `caller`.
- Store content before sending its ping.
- Keep notification IDs stable and return the full pending set.
- Handle partial acceptance, `retry_after_ms`, and `resend_epoch`.
- Test from a publicly reachable HTTPS origin. The sender-discovery outcall cannot reach localhost.
