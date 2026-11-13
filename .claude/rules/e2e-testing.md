---
paths:
  - "src/frontend/tests/**"
  - "scripts/dev-e2e*"
  - "playwright.config.ts"
---

# E2E testing (Playwright)

## Dev scripts

Three scripts in `scripts/` wrap the full e2e stack. Use them instead of running icp-cli and canister installs by hand:

- `scripts/dev-e2e-setup`: idempotent setup. Builds canisters, starts the icp network, OpenID providers and the dev server, and exits 0 when the stack is ready. Re-running rebuilds only what changed (about 1s as a no-op, about 25s after a backend change).
- `scripts/dev-e2e-setup --teardown`: tears the stack down.
- `scripts/dev-e2e`: interactive. Runs setup plus a file watcher, opens Playwright in `--ui` mode, and tears down on Ctrl+C. Use it when the user asks to run the e2e tests themselves. When it fits, offer it to them too: it helps for visually reviewing a flow as a user, or when a run gets stuck or unclear and needs a human's eyes and judgement.

The `e2e-playwright` job in `.github/workflows/canister-tests.yml` is the CI reference if the scripts drift from it.

## Running tests

- Start the stack with `./scripts/dev-e2e-setup`, then run `npx playwright test --project=desktop --workers=1`, narrowed to a spec file (`npx playwright test <path to .spec.ts> ...`) or to tests by name (`-g "<name pattern>"`).
- Run with `--workers=1`, as CI does: specs share canister state, and parallel workers produce false failures.
- After changing code, re-run `./scripts/dev-e2e-setup` before the next test run. The tests run against the deployed canisters, so a change takes effect only once setup rebuilds them (see Stack gotchas).
- For broad frontend changes (auth flows, consent, sessions), run the full suite, not just the new spec.
- Add `--trace on` to debug a failure.
- Tear the stack down with `./scripts/dev-e2e-setup --teardown` when done.

## Environment the scripts set

- `TLS_DEV_SERVER=1`: the dev server serves HTTPS; tests use HTTPS URLs.
- `NO_HOT_RELOAD=1`: tests run against deployed canister code, not the vite dev build.
- `SEPARATE_FRONTEND_CANISTER=1`: `https://backend.id.ai` maps to the backend canister and `https://id.ai` to the frontend canister. The backend's `backend_origin` must be `https://backend.id.ai`.

## Local OpenID providers

The ports are declared at the top of `scripts/dev-e2e-setup`. Read them from there rather than hardcoding a list:

- `OPENID_PORTS`: every provider the stack starts.
- `DIRECT_OPENID_PORTS`: the subset configured as direct providers.
- `PAIRWISE_OPENID_PORT`: a separate Entra-style provider issuing a pairwise `sub` plus a stable `oid`, for the non-`sub` gating tests (`SSO_ENTRA_DISCOVERY_DOMAIN` in `fixtures/sso.ts`).

`fixtures/sso.ts` also defines `SSO_GATING_DISCOVERY_DOMAIN`: the public-`sub` SSO provider under an alternate host (`127.0.0.1` instead of `localhost`), so the `sub` gating tests get a discovery-cache entry the other tests never touch. Use the Entra domain, not this alias, for non-`sub` gating.

## Stack gotchas

- With `NO_HOT_RELOAD=1` and `SEPARATE_FRONTEND_CANISTER=1` the browser loads assets from the canister. `.svelte` changes take effect only after `dev-e2e-setup` reinstalls it.
- Re-running `dev-e2e-setup` rebuilds a canister only when files in its own directory (or `src/frontend`) are newer than its wasm. Changes to shared crates (`src/internet_identity_interface`, `src/asset_util`) and changed init args are not picked up: run `--teardown` first.
- A test that signs up through OpenID/SSO and signs in again in the same context gets an auto-redirecting IdP popup. Call `await page.context().clearCookies()` between the flows.
- Don't edit spec files while a run is in flight: Playwright recompiles the spec against a cached `utils.ts` and fails with a one-off `SyntaxError: does not provide an export named X`.
- pocket-ic clock drift: in long sessions or after a VM resume the replica clock falls behind the host. Past 5 minutes the agent rejects certificates with `TrustError: Certificate is signed more than 5 minutes in the past`, and every flow that round-trips a delegation hangs to the test timeout, often several at once late in a run. Confirm by comparing `date -u` with the replica's cert timestamps, and restart the stack (`--teardown`, then setup). Don't "fix" tests for it.

## Writing tests

- Select through the accessibility tree: `getByRole` with a name (partial names are fine and intentional). If an element has no accessible name, add one to the product; don't select by id or class.
- Use page-object fixtures with stateless helpers. Each operation asserts it reached or left its view, and the test ends with an explicit postcondition (`managePage.assertVisible()`). Put reusable interactions in `tests/e2e-playwright/utils.ts`.
- Rely on auto-waiting, web-first assertions (`toHaveText`, `toHaveCount`, `toBeEnabled`). Don't read `textContent()` or `.count()` straight after an action, and never wait on a fixed timeout to make a test pass.
- Start `waitForEvent` before the action that triggers it. Install `page.clock` before navigating.
- Give canister-bound expectations an explicit, longer timeout, and keep the sum of waits under the test timeout.
- After clicking an optimistic toggle, wait for `toBeEnabled` before asserting its state.
- Model another device with a new browser context, not a new page. Close every context you create in `finally`, and attach the virtual authenticator to the page where WebAuthn runs.
- Use the shared `II_URL` constant; never hardcode origins, ports or URLs without a named constant.
- Fixtures and the test app must not swallow failures, and fakes must not hide missing behaviour. Never change product behaviour to make a test pass.

## Bisecting a regression

1. Confirm the test passes on `main`'s frontend source.
2. Re-apply the branch's changes one directory at a time, re-running `dev-e2e-setup`.
3. Once a directory breaks the test, bisect within it.
4. Watch for keyed `{#each}` blocks whose key isn't unique in the test setup (fixtures sharing `client_id = "internet_identity"`).

## Stopping everything

`./scripts/dev-e2e-setup --teardown` handles the normal case. If something is stuck after an interrupted run:

1. Replica: `icp network stop`, or kill `icp-cli-network-launcher`.
2. Dev server: `lsof -iTCP:5173 -sTCP:LISTEN -nP` and kill it.
3. OpenID providers: resolve `OPENID_PORTS` from `scripts/dev-e2e-setup`, then `lsof -iTCP:<port> -sTCP:LISTEN -nP` for each and kill.
4. Playwright workers: `ps aux | grep -iE 'playwright|chromedriver'`.
