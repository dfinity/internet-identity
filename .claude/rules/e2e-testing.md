---
paths:
  - "src/frontend/tests/**"
  - "scripts/dev-e2e*"
  - "playwright.config.ts"
---

# E2E testing (Playwright)

## Dev scripts

Three scripts in `scripts/` wrap the full e2e stack — use them instead of
running icp-cli + canister installs by hand:

- **`scripts/dev-e2e-setup`** — idempotent setup. Builds canisters, starts the
  icp network, OpenID providers, and dev server. Exits 0 when the stack is
  ready. Re-running rebuilds only what changed (~1s no-op, ~25s after a backend
  change).
- **`scripts/dev-e2e-setup --teardown`** — tears the stack down.
- **`scripts/dev-e2e`** — interactive: calls setup + file watcher + opens
  Playwright in `--ui` mode. Tears down on Ctrl+C.

The `e2e-playwright` job in `.github/workflows/canister-tests.yml` is the CI
reference if the scripts ever drift from it.

## Running tests (agent workflow)

```bash
./scripts/dev-e2e-setup
npx playwright test --project=desktop --grep "Authorize ready message"
# or a whole spec file:
npx playwright test --project=desktop src/frontend/tests/e2e-playwright/routes/recovery.spec.ts

# After more code changes, just re-run setup:
./scripts/dev-e2e-setup

# When done:
./scripts/dev-e2e-setup --teardown
```

## Running for the user (interactive)

```bash
./scripts/dev-e2e
```

Builds canisters, starts the stack + file watcher, opens Playwright `--ui`.

## Environment the scripts set

- `TLS_DEV_SERVER=1` — dev server serves HTTPS; tests use HTTPS URLs.
- `NO_HOT_RELOAD=1` — tests run against deployed canister code, not vite JIT.
- `SEPARATE_FRONTEND_CANISTER=1` — `https://backend.id.ai` maps to backend
  canister, `https://id.ai` to frontend canister. Backend's `backend_origin`
  must be `https://backend.id.ai`.

## Local OpenID providers

The ports are declared at the top of `scripts/dev-e2e-setup` — read them from
there rather than hardcoding a list, since they change as providers are added:

- `OPENID_PORTS` — every provider the stack starts.
- `DIRECT_OPENID_PORTS` — the subset configured as direct providers.
- `PAIRWISE_OPENID_PORT` — an Entra-style provider issuing a pairwise `sub`
  (per client) plus a stable `oid`, used by the non-`sub` gating tests. It is
  the same provider as one of the SSO discovery hosts but under a distinct
  host, so the gating tests get a discovery-cache entry the un-gated tests
  never touch. See `SSO_ENTRA_DISCOVERY_DOMAIN` in `fixtures/sso.ts`.

## Key gotchas

- `--workers=1` is non-negotiable. Tests share canister state; parallelism
  causes false negatives.
- With `NO_HOT_RELOAD=1` + `SEPARATE_FRONTEND_CANISTER=1` the browser loads
  assets from the canister, not vite. `.svelte` changes don't take effect
  until the canister is reinstalled — re-run `dev-e2e-setup`.
- Test fixture cookie gotcha: when a test signs up via OpenID/SSO then signs
  in again in the same context, the IdP popup auto-redirects. Fix with
  `await page.context().clearCookies()` between flows.

## Bisecting a regression

1. Confirm the test passes on `main`'s frontend src.
2. Re-apply branch changes one directory at a time, re-run `dev-e2e-setup`.
3. Once a directory breaks the test, bisect within it.
4. Watch for keyed `{#each}` where the key prop isn't unique in the test
   setup (e.g. fixtures sharing `client_id = "internet_identity"`).

## Useful invocations

```bash
npm run test:e2e-playwright                                              # full suite
npx playwright test routes/authorize/delegationTtl.spec.ts --workers=1   # single file
npx playwright test routes/index.spec.ts -g "Sign in with last used"     # by name pattern
npx playwright test --ui                                                 # UI mode
npx playwright test routes/... --workers=1 --trace on                    # with trace
```

## Stopping everything

`./scripts/dev-e2e-setup --teardown` handles the normal case. If something is
stuck (Ctrl+C'd partway), kill what's left manually:

1. Replica: `icp network stop` or kill `icp-cli-network-launcher`.
2. Dev server: `lsof -iTCP:5173 -sTCP:LISTEN -nP` and kill.
3. OpenID providers: resolve `OPENID_PORTS` from `scripts/dev-e2e-setup`, then
   `lsof -iTCP:<port> -sTCP:LISTEN -nP` for each and kill.
4. Playwright workers: `ps aux | grep -iE 'playwright|chromedriver'`.

## Flaky-looking failures that are really stale-environment artifacts

Before treating an intermittent e2e failure as a test or product bug, rule out
two environment causes — both look exactly like flakes:

- **pocket-ic clock drift.** In long sessions (many full-suite runs) or after a
  VM resume, the `icp network` (pocket-ic) replica keeps its own clock while the
  host wall clock can jump forward (e.g. a resume bumping the date by hours).
  Once they diverge by >5 min, the browser's IC agent rejects the replica's
  delegation certificates with `TrustError: Certificate is signed more than 5
  minutes in the past`, and every flow that round-trips a delegation
  (OpenID/SSO sign-in, cross-device, CLI authorize) fails — usually as a 60s
  test-timeout hang, sometimes a whole cluster of failures in one run. Suspect
  this when time-sensitive IC flows start failing late in a long run, especially
  several at once or with that `TrustError` in the browser console. Confirm by
  comparing `date -u` to the replica's cert timestamps; fix by restarting the
  stack (`scripts/dev-e2e-setup --teardown && scripts/dev-e2e-setup`) to
  re-sync the replica clock. Do not "fix" tests for this.

- **Editing a spec mid-run.** Don't edit e2e test files while a run is in
  flight. Playwright recompiles the changed spec against an already-cached
  `utils.ts`, producing a one-off `SyntaxError: does not provide an export
  named X`. It's not a real failure — it disappears on the next clean run.
