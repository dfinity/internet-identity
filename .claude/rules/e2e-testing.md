---
paths:
  - "src/frontend/tests/**"
  - "scripts/dev-e2e*"
  - "playwright.config.ts"
---

# E2E testing (Playwright)

## Dev scripts

`e2e/use-dev-scripts` Three scripts in `scripts/` wrap the full e2e stack. Use them instead of running icp-cli and canister installs by hand:

- `scripts/dev-e2e-setup`: idempotent setup. Builds canisters, starts the icp network, OpenID providers and the dev server, and exits 0 when the stack is ready. Re-running rebuilds only what changed (about 1s as a no-op, about 25s after a backend change).
- `scripts/dev-e2e-setup --teardown`: tears the stack down.
- `e2e/offer-interactive-runs` `scripts/dev-e2e`: interactive. Runs setup plus a file watcher, opens Playwright in `--ui` mode, and tears down on Ctrl+C. Use it when the user asks to run the e2e tests themselves. When it fits, offer it to them too: it helps for visually reviewing a flow as a user, or when a run gets stuck or unclear and needs a human's eyes and judgement.

The `e2e-playwright` job in `.github/workflows/canister-tests.yml` is the CI reference if the scripts drift from it.

## Running tests

- `e2e/run-narrowed` Start the stack with `./scripts/dev-e2e-setup`, then run `npx playwright test --project=desktop --workers=1`, narrowed to a spec file (`npx playwright test <path to .spec.ts> ...`) or to tests by name (`-g "<name pattern>"`). The projects in `playwright.config.ts` are `desktop`, `mobile` and `chrome-extension`; specs under `e2e-playwright-chrome-extension` only run in `chrome-extension`, which has its own `testDir`.
- `e2e/one-worker` Run with `--workers=1`, as CI does: specs share canister state, and parallel workers produce false failures.
- `e2e/rerun-setup` After changing code, re-run `./scripts/dev-e2e-setup` before the next test run. The tests run against the deployed canisters, so a change takes effect only once setup rebuilds them (see Stack gotchas).
- `e2e/full-suite-for-broad-changes` For broad frontend changes (auth flows, consent, sessions), run the full suite in all three projects, as CI does, not just the new spec.
- `e2e/trace-failures` Add `--trace on` to debug a failure.
- `e2e/teardown-when-done` Tear the stack down with `./scripts/dev-e2e-setup --teardown` when done.

## Environment the scripts set

- `TLS_DEV_SERVER=1`: the dev server serves HTTPS; tests use HTTPS URLs.
- `NO_HOT_RELOAD=1`: tests run against deployed canister code, not the vite dev build.
- `SEPARATE_FRONTEND_CANISTER=1`: `https://backend.id.ai` maps to the backend canister and `https://id.ai` to the frontend canister. The backend's `backend_origin` must be `https://backend.id.ai`.

## Local OpenID providers

`e2e/read-ports-from-script` The ports are declared at the top of `scripts/dev-e2e-setup`. Read them from there rather than hardcoding a list:

- `OPENID_PORTS`: every provider the stack starts.
- `DIRECT_OPENID_PORTS`: the subset configured as direct providers.
- `PAIRWISE_OPENID_PORT`: a separate Entra-style provider issuing a pairwise `sub` plus a stable `oid`, for the non-`sub` gating tests (`SSO_ENTRA_DISCOVERY_DOMAIN` in `fixtures/sso.ts`).

`fixtures/sso.ts` also defines `SSO_GATING_DISCOVERY_DOMAIN`: the public-`sub` SSO provider under an alternate host (`127.0.0.1` instead of `localhost`), so the `sub` gating tests get a discovery-cache entry the other tests never touch. Use the Entra domain, not this alias, for non-`sub` gating.

## Stack gotchas

- `e2e/canister-assets` With `NO_HOT_RELOAD=1` and `SEPARATE_FRONTEND_CANISTER=1` the browser loads assets from the canister. `.svelte` changes take effect only after `dev-e2e-setup` reinstalls it.
- `e2e/force-rebuild` Re-running `dev-e2e-setup` rebuilds a canister only when files in its own directory (or `src/frontend`) are newer than its wasm. Changes to shared crates (`src/internet_identity_interface`, `src/asset_util`) and changed init args are not picked up, and `--teardown` doesn't help: it keeps the wasm files and leaves a network it didn't start running. Delete the affected `internet_identity.wasm.gz` or `internet_identity_frontend.wasm.gz` before setup to force a rebuild and reinstall. Changed `test_app` args need an explicit upgrade of that canister.
- `e2e/clear-cookies-between-flows` A test that signs up through OpenID/SSO and signs in again in the same context gets an auto-redirecting IdP popup. Call `await page.context().clearCookies()` between the flows.
- `e2e/no-edits-during-runs` Don't edit spec files while a run is in flight: Playwright recompiles the spec against a cached `utils.ts` and fails with a one-off `SyntaxError: does not provide an export named X`.
- `e2e/clock-drift` pocket-ic clock drift: in long sessions or after a VM resume the replica clock falls behind the host. Past 5 minutes the agent rejects certificates with `TrustError: Certificate is signed more than 5 minutes in the past`, and every flow that round-trips a delegation hangs to the test timeout, often several at once late in a run. Confirm by comparing `date -u` with the replica's cert timestamps, and restart the stack (`--teardown`, then setup). Don't "fix" tests for it.

## Writing tests

- `e2e/select-by-role` Select through the accessibility tree: `getByRole` with a name (partial names are fine and intentional). If an element has no accessible name, add one to the product; don't select by id or class.
- `e2e/page-objects` Use page-object fixtures with stateless helpers. Each operation asserts it reached or left its view, and the test ends with an explicit postcondition (`managePage.assertVisible()`). Put reusable interactions in `tests/e2e-playwright/utils.ts`.
- `e2e/use-fixtures` Before writing any setup or interaction, read `tests/e2e-playwright/fixtures/` and `utils.ts`, and use what is there: `identities`, `signInWithIdentity` and `addAuthenticatorForIdentity` for passkey identities; `openIdUsers`, `signInWithOpenId` and `signInWithOpenIdPopup` for OpenID; `openSsoPopup` for SSO; `authorizePage` and `authorizedPrincipal` for one sign-in at an app, `testApp` for several; `managePage` for the manage pages. Never re-implement in a spec what a fixture or helper does: a sign-up, a pop-up sign-in, clearing storage, a patched browser API.
- `e2e/extend-fixtures` When the setup a test needs is not in the fixtures yet, add it there (a fixture in `fixtures/`, or a helper in `utils.ts`) and call it from the spec. Don't write it inline, and don't add a local helper to a spec file.
- `e2e/shared-setup-in-hooks` Steps every test in a `describe` repeats go in its `test.beforeEach`, not copied into each test.
- `e2e/web-first-assertions` Rely on auto-waiting, web-first assertions (`toHaveText`, `toHaveCount`, `toBeEnabled`). Don't read `textContent()` or `.count()` straight after an action, and never wait on a fixed timeout to make a test pass.
- `e2e/listen-before-acting` Start `waitForEvent` before the action that triggers it. Install `page.clock` before navigating.
- `e2e/canister-timeouts` Give canister-bound expectations an explicit, longer timeout, and keep the sum of waits under the test timeout.
- `e2e/optimistic-toggle-wait` After clicking an optimistic toggle, wait for `toBeEnabled` before asserting its state.
- `e2e/contexts-for-devices` Model another device with a new browser context, not a new page. Close every context you create in `finally`, and attach the virtual authenticator to the page where WebAuthn runs.
- `e2e/shared-urls` Use the shared `II_URL` constant; never hardcode origins, ports or URLs without a named constant.
- `e2e/no-swallowing-fixtures` Fixtures and the test app must not swallow failures, and fakes must not hide missing behaviour. Never change product behaviour to make a test pass.

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
