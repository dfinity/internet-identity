# Integrating with II Web Push notifications

This guide describes the intended dApp integration for sending notifications through Internet Identity. For the design and trust model, see [Web Push notifications](web-push-notifications.md).

> **Status:** The feature is not enabled in production and `internet-identity-notifications-client` has not been published yet. The API shown here is the target integrator surface. Exact names may change before release.

The Rust and Motoko clients live in [internet-identity-notifications-client](https://github.com/dfinity/internet-identity-notifications-client).

## What an app has to do

The common integration has three parts:

1. Ask II to offer notification consent during sign-in.
2. Publish the canisters allowed to send for the app origin.
3. Include the notification client in the backend and give it campaigns to send.

The app should not implement its own push queue, pending-notification endpoint, batching, retry loop, or II upgrade recovery. Those belong to the client library.

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

II may then offer notification consent for the origin the user is signing in to. The application still receives an ordinary delegated identity. Its principal is the recipient used when creating a notification.

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

For a canister-hosted frontend, serve the document as a certified asset. The response, including its headers, must fit within 64 KiB. The origin must be publicly reachable over HTTPS, so this part of the flow does not work against localhost.

The Motoko client generates the JSON body with `sendersDocument`. The Rust client provides the equivalent `well_known::SendersDocument` helper.

Every canister that sends a campaign for the origin must be listed. The service worker will query the listed canisters for pending content, so each canister using the notification client exposes the standard pull endpoint.

## 3. Include the client in the backend

The notification client runs inside the dApp canister. Campaign and pending-notification state therefore lives in the dApp's stable memory, but its schema and lifecycle are managed by the client library rather than by application code.

For Motoko, the intended integration is a mixin:

```motoko
import Notifications "mo:ii-notification-client/Notifications";
import Principal "mo:base/Principal";

persistent actor {
  include Notifications({
    iiCanisterId = Principal.fromText("rdmx6-jaaaa-aaaaa-aaadq-cai");
    origin = "https://app.example";
  });

  // Application methods and state.
};
```

The mixin owns the notification state and injects the service-worker query into the actor's Candid interface:

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

The service worker calls this query as the user's per-app principal. The client uses `caller` to return that recipient's complete pending set. The integrator does not implement or call this method directly.

The Rust client should provide the same campaign and pending-state abstraction. Rust does not have Motoko's `include` mechanism, so the exact endpoint wiring differs, but the application should not have to reimplement the storage and reconciliation contract.

## 4. Submit a campaign

The app gives the client a campaign ID and a list of notifications. Each notification contains its recipient and content:

```motoko
await sendNotificationCampaign({
  campaignId = "weekly-summary";
  notifications = [
    {
      id = "summary-42";
      recipient = alice;
      title = "Your weekly summary";
      body = ?"Three new updates";
      urgency = ?#normal;
      expiresAt = null;
    },
    {
      id = "summary-43";
      recipient = bob;
      title = "Your weekly summary";
      body = ?"One new update";
      urgency = ?#normal;
      expiresAt = null;
    },
  ];
});
```

This is the target shape, not a finalized method signature. The important boundary is that the app supplies the campaign and the client performs the delivery work.

For each recipient, the client stores the content before asking II to send a ping. This prevents a device from waking before the content is available. The client then batches and paces `notification_send` calls until the campaign has been admitted or reaches a terminal state.

`recipient` is the principal obtained from the user's II delegation for this app and account. The configured `origin` must match the origin used during sign-in and the one serving the sender document.

## What the client handles

The client owns:

- Durable campaign state and progress.
- Pending content indexed by recipient and notification ID.
- The authenticated `ii_pending_notifications` query.
- Storing content before sending its ping.
- Splitting large campaigns into bounded batches.
- Pacing and retrying calls when II returns `retry_after_ms`.
- Partial acceptance and terminal recipient rejections.
- Persisting and reacting to `resend_epoch` changes.
- Resuming campaigns after the dApp canister upgrades.
- Updating and removing pending notifications.
- Producing the well-known sender document.

II still validates the sender, origin, consent, recipient, and subscription on every accepted notification. The client is responsible for campaign coordination, not authorization.

## Campaign status

II reports admission into its transient buffer, not delivery to a device. The client should expose campaign progress using terms such as:

- `pending`: not yet accepted by II.
- `sent`: accepted into II's buffer.
- `no_consent`: rejected because the recipient cannot be notified for this origin.
- `not_subscribed`: consent exists but no device can currently be reached.
- `invalid`: the request or expiry is invalid.

It must not report `delivered`, since neither II nor the client receives a device receipt.

If II's `resend_epoch` changes, the client resends the affected pings. Duplicate pings are safe because the service worker reconciles the dApp's current pending content by notification ID.

## Updating and removing notifications

The application updates or removes notifications through the client library.

Updating content while keeping the same notification ID updates the displayed notification in place after the next ping. Removing the ID from the pending set and sending another ping closes it on each reached device after the next pull.

The application decides when a notification is no longer pending. The client handles the corresponding state update and wake-up.

## Integration checklist

- Use one canonical HTTPS origin for sign-in, consent, the sender document, and campaign sending.
- Set `iiNotifications: true` in the II authorization request.
- Publish `/.well-known/ii-notification-senders` before asking for consent.
- List every canister that includes the notification client and sends for the origin.
- Include and configure the client in the dApp backend.
- Pass the client stable notification IDs, recipient principals, content, and delivery options.
- Display campaign progress as admission status, not confirmed delivery.
- Test from a publicly reachable HTTPS origin. Sender discovery cannot reach localhost.
