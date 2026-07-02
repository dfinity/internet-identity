# SSO-gated test app

A minimal relying-party app, written in **Motoko**, that **gates a
protected resource on a certified Internet Identity `sso:<domain>`
attribute**. It exists to exercise II's SSO role-based access
enforcement end to end.

- **Enforces SSO attributes** — the protected resource is handed out
  only to a caller who signed in through the org's SSO and presented a
  certified `sso:<domain>` org-membership attribute. A passkey / CLI /
  MCP session, or a user whose org IdP granted no role for this app,
  has no such attribute and is turned away.
- **Trusted domains** — the app's frontend origin
  (`https://screenshot-parents-explosion-display.trycloudflare.com`) is
  configured as the only allowed `implicit:origin`. A bundle minted for
  any other origin is rejected.

It's built on the [`identity-attributes`](https://mops.one/identity-attributes)
Mops package, which pairs with `@icp-sdk/auth` v7.

## How it works

```
frontend                     II                    sso_gated_app (this canister)
   │                          │                             │
   │ _internet_identity_sign_in_start() ───────────────────▶│  mint single-use nonce
   │◀──────────────────────────────────────── nonce ────────│
   │ signIn() + requestAttributes({ sso:<domain>:*, nonce }) │
   │─────────────────────────▶│  (SSO ceremony; II certifies │
   │◀──── {data, signature} ───│   sso:<domain> only if the   │
   │                          │   IdP granted a role)         │
   │ _internet_identity_sign_in_finish()  (wrapped in AttributesIdentity)
   │──────────────────────────────────────────────────────▶│  IC verifies signature;
   │                          │      canister checks origin,  │  onVerified records
   │◀───────────────── #ok / #err ──────────────────────────│  { name; email; sso }
   │ getProtectedResource() ───────────────────────────────▶│  #ok iff session.sso ≠ null
   │◀──────────── #ok "…member of dfinity.org" / #err ───────│
```

The IC verifies the attribute signature during ingress; the canister
reads the verified signer + payload via `mo:core/CallerAttributes`. The
`identity-attributes` mixin then checks the signer is trusted, the
origin is allowed, the nonce is one this canister issued (single-use),
and the bundle is fresh — before running our `onVerified` callback.

**Enforcement is ours to impose:** the mixin _accepts_ a non-SSO bundle
(reporting `sso = null`). `getProtectedResource` grants access only when
the recorded session carries a non-null trusted SSO domain — see
[`src/main.mo`](./src/main.mo).

## Configuration

Set in [`icp.yaml`](./icp.yaml) under
`canisters[].settings.environment_variables`:

| Variable                    | Meaning                                                                                                                                         |
| --------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------- |
| `trusted_attribute_signers` | II canister principal whose signature the IC must have verified. **Change the default** (mainnet II) to the II instance you're testing against. |
| `frontend_origins`          | Exact origin(s) allowed in `implicit:origin` — the "trusted domains". Pre-set to the Cloudflare tunnel origin.                                  |
| `trusted_sso_domains`       | Org SSO domain(s) accepted for `sso:<domain>:*` keys. **Required** — if unset, every SSO bundle is rejected as `#UntrustedSsoSource`.           |

## Build & deploy

Requires [`mops`](https://mops.one) (`npm i -g ic-mops`) and
[`icp`](https://github.com/dfinity/icp-cli). The Motoko compiler is
resolved from the `[toolchain]` pin in `mops.toml`.

```sh
cd demos/sso-gated-app
./build.sh          # produces sso_gated_app.wasm.gz + sso_gated_app.did
icp deploy          # deploys using ./icp.yaml
```

After deploying, put the app's canister id into the frontend's
`This app's canister id` field, and set `trusted_attribute_signers` to
the II you're testing against (redeploy or
`icp canister settings update sso_gated_app --add-environment-variable trusted_attribute_signers=<principal>`).

## Frontend

```sh
cd frontend
npm install
npm run dev         # vite dev server; accepts tunnel Host headers
```

Expose it at the trusted origin, e.g.:

```sh
cloudflared tunnel --url http://localhost:5173
# → https://screenshot-parents-explosion-display.trycloudflare.com
```

Open the tunnel URL, fill in the II URL, the II canister id (signer),
and this app's canister id, then **Sign in with SSO & verify** followed
by **Call protected resource**.

## Test scenario

With the org IdP configured so that **Alice** has a granted role for
this app and **Bob** does not:

| Who                         | `sign_in_finish`                                               | `getProtectedResource`    |
| --------------------------- | -------------------------------------------------------------- | ------------------------- |
| **Alice** (role granted)    | `#ok` — II certified `sso:<domain>`                            | `#ok` — access granted    |
| **Bob** (no role)           | `#err` (no SSO attribute in bundle) or `#ok` with `sso = null` | `#err(#SsoRequired)`      |
| Passkey / CLI / MCP session | no bundle presented                                            | `#err(#NotAuthenticated)` |

## Files

| Path                     | What                                                 |
| ------------------------ | ---------------------------------------------------- |
| `src/main.mo`            | The gated actor (mixin + SSO gate).                  |
| `mops.toml`, `mops.lock` | Motoko dependencies (`identity-attributes`, `core`). |
| `build.sh`               | Compiles to `sso_gated_app.wasm.gz`.                 |
| `icp.yaml`               | Standalone deployment + env-var config.              |
| `sso_gated_app.did`      | Generated Candid interface.                          |
| `frontend/`              | Vite driver that runs the flow.                      |
