---
paths:
  - "src/internet_identity/tests/**"
  - "src/canister_tests/**"
  - "src/**/*.test.ts"
  - "src/**/*.rs"
  - "src/frontend/tests/**"
  - "src/frontend/src/**"
---

# Testing

## Where coverage goes

- `test/integration-over-mocks` Behaviour is proven by PocketIC integration tests (canister) and Playwright e2e tests (frontend and full flows), not by mocked unit tests, and never mock the module under test. Don't write component tests: never mount a Svelte component in a unit test. Logic too involved to cover in e2e moves out of the component into plain functions (`fe/logic-in-flows`), and those get unit tests.
- `test/unit-test-pure-functions` Unit- or property-test non-trivial pure functions: parsers, validators, codecs, bit manipulation, limits, including the negative cases.
- `test/endpoint-authorization-tests` Every new endpoint gets candid-level integration tests covering who may call it: the owner, another identity, the anonymous principal, and the feature disabled.
- `test/init-arg-config-tests` Every new init-arg field gets a config test (`tests/integration/config/`): set at install, changed and unset on upgrade.
- `test/prove-behavior-change` A deliberate behaviour change ships with a test proving the old behaviour is now refused.
- `test/regression-tests` A bug fix ships with its regression test, including security look-alikes (`evil-example.com` for `example.com`).
- `test/multi-user-isolation` Test multi-user isolation explicitly: one user's code, session or state is refused for another.
- `test/test-real-code` Don't test copied logic: a test calls the real code, next to where it lives.
- `test/no-stub-tests` Don't write tests for temporary stubs, and never commit tests that read local files outside the repo.

## Tests must be able to fail

- `test/assert-outcomes` Assert the outcome, not that a mechanism ran: verify a signature against the root key rather than checking it is non-empty; assert the selected value rather than clicking the default.
- `test/pre-and-postconditions` Assert preconditions, then a required postcondition, including that parties the code shouldn't touch are unchanged.
- `test/prove-tests-fail` Show that each new test fails when the code it guards is broken (revert or sabotage the fix, run it, restore).
- `test/boundary-literals` Assert exactly what the test name claims, at the boundary (`max` and `max + 1`). Write expected limits as literals rather than importing the implementation's constant or deriving them from the output.
- `test/full-shape-matches` No vacuous `if let`/permissive `match` in assertions: match the full shape (`matches!(r, Ok(Err(..)))`). `#[should_panic]` carries `expected = "..."`.
- `test/stable-error-assertions` Assert errors by a stable variant or substring, not volatile values.

## Hygiene

- `test/constructors-on-type` Put constructors on the type (`Notification::new`) instead of local test factories, and name helper arguments for what they mean.
- `test/shared-init-args` Build canister init args on the shared base helpers (`arg_with_captcha_disabled()`), and keep each test focused on one thing.
- `test/tick-until` PocketIC is deterministic, but loop (with a cap) on `tick()` until the awaited state is reached rather than ticking a fixed number of times.
- `test/restore-globals` Restore patched globals by descriptor; give blocking clients timeouts.
- `test/sentence-names` Name tests as sentences describing the behaviour.
