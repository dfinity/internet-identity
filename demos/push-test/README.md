# II Push Notifications — Smoke Test

Minimal browser app that exercises the six push methods on a test II
canister running the code from PR #4109
(`feat/push-notifications-poc`). Registers a Service Worker, subscribes
via FCM/Mozilla with the canister's VAPID key, grants consent, then
fires `notify_user` and watches the OS notification tray.

## Running

```bash
cd demos/push-test
npm install
npm run dev  # serves on http://localhost:5175
```

Open http://localhost:5175 in **Chrome** (or Firefox — but the flow was
tuned against Chrome + FCM).

## Configuration

Fill in the three fields at the top of the page:

- **II URL** — the test canister's FE URL (e.g.
  `http://<canister-id>.localhost:4943` for local dfx, or the test
  canister's `https://` URL on mainnet).
- **II Canister ID** — the target canister's principal.
- **Anchor Number** — your identity number on that canister.

## Flow

Click the four buttons top to bottom:

1. **Sign in** — opens the II popup, get a delegation for
   `http://localhost:5175`.
2. **Register SW + subscribe + push_subscribe_device** — one click,
   handles all of: SW registration, notification permission,
   `pushManager.subscribe()`, and shipping the subscription up to II.
3. **push_grant_consent** — records that this origin can push you.
4. **notify_user** — fires the encrypted outcall. The notification
   appears in your desktop tray within a few seconds.

## Troubleshooting

- **"Notification permission: denied"** — reset the site permission in
  Chrome settings and retry.
- **`push_subscribe_device` errors on `p256dh` length** — subscription
  came back malformed; unsubscribe and retry (`pushManager.getSubscription()
  .then(s => s.unsubscribe())`).
- **`notify_user` returns Ok but no notification appears** — the outcall
  reached FCM but FCM's response was non-2xx. Check
  `dfx canister logs <ii-canister>` (or the test canister equivalent)
  for the `push outcall non-2xx status:` line.
- **VAPID key mismatch after re-deploying the canister** — the browser
  bakes the VAPID pub key into its subscription; if the canister's key
  changes, the relay rejects. Unsubscribe on the browser side and
  re-subscribe.
