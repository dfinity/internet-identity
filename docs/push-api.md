# Push notifications — II interface and dApp integration

The Candid surface a dApp sends through, and how to integrate one. Reference material
— rationale is in [push-notifications.md](push-notifications.md).

## Integrating a dApp

Setup is one step: serve `.well-known/ii-push-senders` listing your backend canister
principal. II verifies the file itself (on first consent, or on your first send), so
there's no registration call to remember — `push_send` just returns `SenderUnverified`
until it does, and the client library retries until a correctly published file
resolves. (`push_register_sender` only forces an immediate re-check after you edit the
file.)

Then hand the client library a campaign; it chunks, paces, retries and reports:

```rust
// Non-sensitive app: show the content.
push.broadcast(
    PushContent::Display { title, body },
    PushDelivery { urgency: High, ttl_seconds: 3600, topic: Some("balance") },
    &my_user_principals,        // library chunks + paces these
).await?;

// E2E app: send nothing sensitive; content is revealed on tap.
push.broadcast(
    PushContent::Hidden { category: Some(Message) },
    PushDelivery { urgency: High, ttl_seconds: 3600, topic: None },
    &my_user_principals,
).await?;
```

The library splits the audience into ≤1000-target chunks, calls `push_send` per chunk,
paces on `ready`/`retry_after_ms`, retries with a stable `chunk_id`, and aggregates
results into campaign status. The dApp handles no keys, no crypto, no Web Push, and no
per-user state. Delivery is best-effort with no receipts — the only per-user signal is
`NoConsent`.


## The Candid interface

One send entry point, `push_send`: a shared `default_alert` plus recipients that may
each override it, so the same call does broadcast and per-user text. The inline
comments cover the rest.

```candid
type PushCategory = variant { Message; Transfer; Update; Generic };

// The content variant is what makes end-to-end encryption possible later
// (see the end-to-end-encryption section below). Display content is read by II
// and shown verbatim — fine for non-sensitive notifications, but II sees it.
// Hidden content is never sent to II at all: the payload carries no message
// text, so an E2E sender structurally cannot leak content. The service
// worker renders an II-controlled generic string keyed by category, and the
// real message is revealed on tap-through when the app decrypts it.
type PushContent = variant {
  Display : record {             // II-visible; transport-encrypted only, NOT E2E
    title : text;                // ≤ 64 bytes
    body : text;                 // ≤ 256 bytes
  };
  Hidden : record {              // content-free; E2E-safe by construction
    category : opt PushCategory; // maps to II-controlled copy ("New message", …)
  };
  Dismiss;                       // close the shown notification named by notification_id; renders nothing
};

type PushAlert = record {
  content : PushContent;
  url : opt text;                // tap-through target; must be same-origin as sender
  notification_id : opt text;    // dApp's id for this notification (maps to the Web Notification `tag`):
                                 // reuse it to UPDATE the shown one, or pair with Dismiss to close it
};

type PushUrgency = variant { VeryLow; Low; Normal; High };

type PushDelivery = record {     // RFC 8030 relay headers; plaintext, relay-visible
  urgency : opt PushUrgency;     // default Normal; also orders II's drain
  ttl_seconds : opt nat32;       // default ~4h; 0 = only if online now; clamped to a max
  topic : opt text;              // ≤ 32 chars base64url; collapse key
};

type PushRejection = variant {
  NoConsent;                     // unknown target OR not consented to *your* origin (merged on purpose)
  AlertInvalid : text;
};

type PushResult = record {
  admitted : nat32;              // accepted into the in-flight buffer for delivery (NOT delivered)
  rejected : vec record { index : nat32; reason : PushRejection };
  ready : bool;                  // false → II is at capacity; stop and retry after retry_after_ms
  retry_after_ms : opt nat32;    // Layer-1 backpressure hint
};

// One recipient. `alert` is an optional per-recipient override; when null the
// recipient uses the chunk's shared `default_alert`. This is how one endpoint
// covers both cases: broadcast = shared default + all overrides null;
// personalized = per-recipient overrides. `null` costs ~1 byte, so broadcast
// stays compact (the content isn't repeated per recipient).
type PushRecipient = record {
  target : principal;            // in-app principal
  alert : opt PushAlert;         // override; falls back to default_alert
};

service : {
  push_register_sender : (origin : text) -> (variant { Ok; Err : text });
  push_deregister_sender : (origin : text) -> (variant { Ok; Err : text });

  // Submit ONE chunk (≤ ~1000 recipients, ≤ 2 MB). II admits what it has
  // capacity for (Layer 1) and returns per-recipient rejections plus a
  // backpressure signal. Each recipient's effective alert is its own override
  // or, if null, `default_alert`; if both are null it is rejected
  // (AlertInvalid). The client library owns the campaign, chunking, pacing,
  // retry, status, templating and prioritization — II holds no campaign state.
  push_send : (
    chunk_id : blob,             // per-chunk idempotency (short-lived heap dedup)
    delivery : PushDelivery,     // shared across the chunk (urgency / ttl / topic)
    default_alert : opt PushAlert, // shared alert for recipients that don't override
    recipients : vec PushRecipient
  ) -> (PushResult);
}
```

The `content`/`PushDelivery` split mirrors the trust boundaries: `Display` is
transport-encrypted but II-readable (fine for non-sensitive text); `Hidden` never
reaches II (preserving E2E — an E2E sender simply has no field for message text);
`PushDelivery` headers are plaintext the relay sees. `push_send` returns on admission,
not delivery — "admitted" means "in II's buffer", nothing more.

