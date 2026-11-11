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
- `scripts/dev-e2e`: interactive. Runs setup plus a file watcher and opens Playwright in `--ui` mode; tears down on Ctrl+C.

The `e2e-playwright` job in `.github/workflows/canister-tests.yml` is the CI reference if the scripts drift from it.

## Running tests

```bash
./scripts/dev-e2e-setup
npx playwright test --project=desktop --workers=1 --grep "Authorize ready message"
npx playwright test --project=desktop --workers=1 src/frontend/tests/e2e-playwright/routes/recovery.spec.ts

# After more code changes, re-run setup:
./scripts/dev-e2e-setup

# When done:
./scripts/dev-e2e-setup --teardown
```

For the user (interactive): `./scripts/dev-e2e`.

Run with `--workers=1`, as CI does: specs share canister state, and parallel workers produce false failures. For broad frontend changes (auth flows, consent, sessions), run the full suite, not just the new spec.

Other invocations: `npx playwright test <file> -g "<name pattern>"`, `--ui`, `--trace on`.

## Environment the scripts set

- `TLS_DEV_SERVER=1`: the dev server serves HTTPS; tests use HTTPS URLs.
- `NO_HOT_RELOAD=1`: tests run against deployed canister code, not the vite dev build.
- `SEPARATE_FRONTEND_CANISTER=1`: `https://backend.id.ai` maps to the backend canister and `https://id.ai` to the frontend canister. The backend's `backend_origin` must be `https://backend.id.ai`.

## Local OpenID providers

The ports are declared at the top of `scripts/dev-e2e-setup`. Read them from there rather than hardcoding a list:

- `OPENID_PORTS`: every provider the stack starts.
- `DIRECT_OPENID_PORTS`: the subset configured as direct providers.
- `PAIRWISE_OPENID_PORT`: an Entra-style provider issuing a pairwise `sub` plus a stable `oid`, used by the non-`sub` gating tests. It is one of the SSO discovery providers under a distinct host, so the gating tests get a discovery-cache entry the other tests never touch. See `SSO_ENTRA_DISCOVERY_DOMAIN` in `fixtures/sso.ts`.

## Stack gotchas

- With `NO_HOT_RELOAD=1` and `SEPARATE_FRONTEND_CANISTER=1` the browser loads assets from the canister. `.svelte` changes take effect only after `dev-e2e-setup` reinstalls it.
- Re-running `dev-e2e-setup` does not re-apply changed init args to an installed canister. Run `--teardown` first.
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
- Fixtures that bind a fixed port offset it by `TEST_PARALLEL_INDEX`. Assertions on canister-wide counters use `>=`.
- Fixtures and the test app must not swallow failures, and fakes must not hide missing behaviour. Never change product behaviour to make a test pass.
- Cover each new flow with an e2e test, including the authorize path from an app. Gate any randomness in II behind an init-arg flag rather than patching `Math.random`.

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
