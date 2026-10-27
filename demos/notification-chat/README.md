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
  (`requestNotificationConsent`) and what they are called (`requestAttributes`
  for `name`). Allowing registers the browser for Web Push. Once signed in, the
  identity joins the room. **Sign out** leaves it.
- **Names.** A name Internet Identity hands over at sign-in is claimed for the
  identity; an identity without one (a passkey alone carries none) can claim
  one on the line next to its name, and either can change it later. Names are
  unique among the names claimed, compared without regard to case: a second
  Alice becomes `Alice_1`, a third `Alice_2`. Everywhere a name is shown,
  hovering over it shows the principal, and somebody without a name is shown
  as their principal's ends. Names outlive an upgrade; the room does not.
- **Send** stores the message and notifies every other member through the
  [`identity-notifications`](https://crates.io/crates/identity-notifications)
  library. The notification's title is the writer's name, its body is the
  message, and its link is this app's origin, so acting on it opens the app.
  Notifications carry no key, so each message is a notification of its own:
  none replaces another, and a burst of messages is a burst of notifications.
- The **messages** box shows who sent what and when; the **list on the right**
  shows who is in the room. The page reads the room every few seconds.
- **Show notification stats** opens a panel that says what the canister is
  configured to send for and through, warns when this page or the Internet
  Identity it signs in with do not match, and shows the library's delivery
  funnel: how many notifications were queued, accepted by Internet Identity,
  shown by a browser, and acted on, plus what was dropped or deferred and the
  backlog waiting to be sent.

Anyone signed in can join. The room holds 100 members and the 100 newest
messages, and lives on the heap, so an upgrade empties it.

## Service discoverability

The app follows the [service discoverability
skill](https://skills.internetcomputer.org/skills/service-discoverability/),
so an agent can find and use it:

- `/.well-known/ic-architecture` lists the canisters that make up the app:
  this one. The canister writes the file when it starts, with its own id, so
  no id is committed. Serving this file opts the app into ICP MCP and counts
  as the operator's acceptance of the [ICP MCP App Operator
  Terms](https://internetcomputer.org/icp-mcp/app-operator-terms/): whoever
  deploys it is agreeing on the operator's behalf. Removing the file (and
  deploying) stops new reads and calls.
- The Candid interface is in the canister's public `candid:service` metadata,
  which `build.sh` attaches; do not strip it.
- `get_api_doc` returns [`api_doc.md`](api_doc.md): what the types do not say,
  such as units, which calls need a signed caller, what cannot be undone, and
  how the notifications behave. Keep it true to the code; the terms require
  it to describe every method's effects honestly.
- `/.well-known/ii-derivation-origin` is not served: the app derives
  identities for the origin it is visited on, which is what its absence
  means. Its absence is a certified 404, as for any path the canister does
  not have, so a gateway passes it on as one rather than refusing an
  uncertified answer as a 503.
- The optional query layer (`schema` and `execute`) is not implemented: the
  room is small enough to read whole.

To check a deployment:

```bash
curl -s https://<origin>/.well-known/ic-architecture | jq '.canisters[].id'
icp canister metadata notification_chat candid:service -e ic
icp canister call notification_chat get_api_doc '()' -e ic
```

## Layout

| Path         | What it is                                                                         |
| ------------ | ---------------------------------------------------------------------------------- |
| `lib.rs`     | The canister: the room's methods, the notification endpoints, and the frontend.    |
| `room.rs`    | The room itself, as plain state with unit tests.                                   |
| `names.rs`   | The rule that keeps claimed names unique, with unit tests.                         |
| `api_doc.md` | What `get_api_doc` returns: the interface's semantics, for an agent.               |
| `chat.did`   | The Candid interface, exported from the code (`BLESS=1 cargo test` rewrites it).   |
| `src/`       | The frontend: one page, one script, one stylesheet, built with Vite.               |
| `build.sh`   | Builds the frontend into `dist/`, then the canister into `notification_chat.wasm`. |

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

Until the origin is set, the stats panel says so and every notification is
dropped. The canister serves
`/.well-known/ii-notification-senders` naming itself, which is how Internet
Identity learns it may send for that origin.

### Pointing at another Internet Identity

The frontend signs in with mainnet's Internet Identity (`https://id.ai/authorize`,
`rdmx6-jaaaa-aaaaa-aaadq-cai`), and the canister sends through the same one.
Both halves have to name the same deployment, or consent given at one is
unknown to the other; the stats panel says so when they differ. To use another
one, for instance the staging deployment at `https://beta.id.ai`, whose
backend canister is `fgte5-ciaaa-aaaad-aaatq-cai`:

1. Put its backend canister into `notification_sender` in `icp.yaml`.
2. Build the frontend with the same canister and that instance's authorize URL,
   which is its frontend's URL plus `/authorize`. The build runs inside
   `icp deploy`, so the variables go on that command:

   ```bash
   VITE_II_URL=https://beta.id.ai/authorize \
   VITE_II_CANISTER_ID=fgte5-ciaaa-aaaad-aaatq-cai \
   icp deploy -e ic
   ```

   Or keep them in a `.env` file next to `package.json`, which Vite reads and
   git ignores.

The instance must have notifications enabled (`notifications_enabled`, and the
frontend's push flag), or sign-in works but every consent request is refused.

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
