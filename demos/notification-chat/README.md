# Notification chat

The smallest app that notifies through Internet Identity: one room, and every
message in it notifies everyone else who has joined. It exists to try the
notifications feature end to end, from the consent screen to the push and to
the click that brings the user back.

It is a project of its own inside this repository (its own Cargo workspace,
`package.json` and `icp.yaml`), so it can be moved out as it stands.

## What it does

- **Sign in** signs in with Internet Identity over ICRC-25 and, as part of the
  same ceremony, asks whether this app may notify the user
  (`requestNotificationConsent`). Allowing registers the browser for Web Push.
  Once signed in, the identity joins the room. **Sign out** leaves it.
- **Send** stores the message and notifies every other member through the
  [`identity-notifications`](vendor/identity-notifications) library. The
  notification's title names who wrote it, its body is the message, and its
  link is this app's origin, so acting on it opens the app. Every notification
  uses the same key, so a recipient who has not acted on the previous one sees
  it replaced by the latest message rather than stacked beside it.
- The **messages** box shows who sent what and when; the **list on the right**
  shows who is in the room. The page reads the room every few seconds.
- **Show notification stats** opens the library's delivery funnel: how many
  notifications were queued, accepted by Internet Identity, shown by a browser,
  and acted on, plus what was dropped or deferred, the backlog waiting to be
  sent, and whether the canister is misconfigured.

Anyone signed in can join. The room holds 100 members and the 100 newest
messages, and lives on the heap, so an upgrade empties it.

## Layout

| Path                            | What it is                                                                         |
| ------------------------------- | ---------------------------------------------------------------------------------- |
| `lib.rs`                        | The canister: the room's methods, the notification endpoints, and the frontend.    |
| `room.rs`                       | The room itself, as plain state with unit tests.                                   |
| `chat.did`                      | The Candid interface, exported from the code (`BLESS=1 cargo test` rewrites it).   |
| `src/`                          | The frontend: one page, one script, one stylesheet, built with Vite.               |
| `asset_util/`                   | Asset certification, a copy of `src/asset_util` on the ic-cdk the library needs.   |
| `vendor/identity-notifications` | The notification library, vendored while its repository is internal.               |
| `vendor/icp-sdk-auth-*.tgz`     | A build of `@icp-sdk/auth` with `requestNotificationConsent`, not yet published.   |
| `build.sh`                      | Builds the frontend into `dist/`, then the canister into `notification_chat.wasm`. |

## Building

Requirements: the Rust toolchain pinned in `rust-toolchain.toml` (rustup picks
it up), [`ic-wasm`](https://github.com/dfinity/ic-wasm) 0.8.5, and the Node
version in `.node-version`.

```bash
./build.sh
```

The canister embeds `dist/` at compile time, so the frontend has to be built
before any `cargo` command, `cargo test` included.

Checks:

```bash
npm run check          # tsc
npm run format-check   # prettier
cargo fmt --all --check
cargo clippy --all-targets -- -D clippy::all -D warnings
cargo test
```

## Deploying

The canister is configured through two environment variables in `icp.yaml`,
read by the notification library:

- `notification_sender`: the Internet Identity canister it sends through, and
  whose signed caller info it trusts when it serves a notification's content.
  Mainnet's by default.
- `notification_origin`: this app's own origin, exactly as the browser reaches
  it. It names the canister, so it is only known once the canister exists.

So a first deployment to mainnet (`-e ic`) is two deploys:

```bash
icp deploy -e ic        # creates the canister; nothing is sent yet
icp canister status notification_chat -e ic --id-only
# put https://<id>.icp0.io into notification_origin in icp.yaml
icp deploy -e ic
```

Until the origin is set, the stats panel says `misconfigured: Origin` and every
notification is dropped. The canister serves
`/.well-known/ii-notification-senders` naming itself, which is how Internet
Identity learns it may send for that origin.

The frontend signs in with mainnet's Internet Identity (`https://id.ai/authorize`,
`rdmx6-jaaaa-aaaaa-aaadq-cai`). To use another one, build with `VITE_II_URL` and
`VITE_II_CANISTER_ID` set, and point `notification_sender` at that canister.

## Local development

`icp network start` and `icp deploy` work as for any canister, and the page is
served at `http://<canister-id>.localhost:8000`. Nothing can be sent without an
Internet Identity to send through: one from this repository deployed on the
same network, with `notifications_enabled` and the two
`notifications_allow_insecure_*` flags set so that an `http://` origin and a
local push relay are accepted. The Playwright fixtures under
`src/frontend/tests/e2e-playwright/fixtures/notifications` show that setup.

`npm run dev` serves the frontend on <http://localhost:8082> and forwards `/api`
to the replica (`REPLICA_URL`, default `http://127.0.0.1:8000`); tell it which
canister with `?canisterId=<id>`. That origin is not the canister's, so
Internet Identity treats it as a different app: use it for working on the page,
and the canister's own origin for notifications.
