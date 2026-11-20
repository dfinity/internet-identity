---
paths:
  - "src/internet_identity/**"
  - "src/internet_identity_frontend/**"
  - "src/internet_identity_interface/**"
  - "src/asset_util/**"
  - "src/canister_tests/**"
---

# Backend (Rust canister code)

See also `candid-api.md`, `stable-memory.md` and `security.md`.

## No trapping

`be/no-trap` Canisters must not trap. Do not introduce `panic!`, `.unwrap()`, `.expect(...)`, `unreachable!()`, `todo!()`, `unimplemented!()`, `assert!`, `[index]` or panicking arithmetic outside test code (`be/test-code-may-unwrap`).

- `be/result-not-trap` Return `Result<T, E>` and propagate with `?` instead of trapping.
- `be/no-index` Index collections with `.get()`, `.find()`, `iter_mut().find()` and `let ... else`, never `[i]`, and don't carry an index across borrows.
- `be/checked-arithmetic` Use `checked_*` arithmetic and `try_from` conversions; no `as` casts between integer types. Prefer `checked_*` plus an error over `saturating_*` when the clamped value would be acted on or an id handed out twice.
- `be/truncate-at-char-boundary` Truncate strings only at a char boundary.
- `be/no-default-for-missing` Never `unwrap_or_default()` a value that is then persisted or authorized on. A missing record is an error, not an empty default.
- `be/no-expect-to-document` Push back on reviewer suggestions (Copilot and others) to add `.expect(...)` or `debug_assert!` "to document the invariant".
- `be/storable-decode` `Storable::from_bytes` cannot return an error, so it may `expect` with an actionable message. Make decoding unable to fail instead: see `stable-memory.md`, and use the bounded helpers (`slice_to_bounded_32`) rather than `copy_from_slice`.
- `be/storable-encode` `Storable::to_bytes` cannot return an error either, so encoding may `expect` with an actionable message. Encoding a well-formed value must not be able to fail.
- `be/outcall-transform-trap` HTTP outcall transform functions cannot return or log errors, so they may trap on a response that is unusable as a whole. Skip a single malformed element (a key without a `kid`) rather than rejecting the response.
- `be/test-code-may-unwrap` `#[cfg(test)]` code and test-only crates (`src/canister_tests`) may `unwrap`/`expect`/`panic!`; prefer `expect("context")`.

## State changes, errors and `await`

- `be/err-does-not-roll-back` On the IC, returning `Err` doesn't undo state changes. Every mutation made before it is committed: stable-memory writes (`Storage::write`, map inserts and removes, counter bumps) and changes to heap state (`state` module, `thread_local!`). Reads, locals and building the response don't count. Only a trap rolls back. Run every check that can fail before the first mutation, and never use `?` after one. Each `await` commits the same way (next rule). The activity bookkeeping done by `check_authz_and_record_activity` is the exception: it records that the caller acted and is meant to commit even when the call then fails validation.
- `be/recheck-after-await` Every `await` is a commit point and lets other messages run. Re-read and re-check (authorization, existence, generation) anything captured before it; never write back a value loaded before it. Re-derive the value from its source and compare it with the one captured before (`if <the same lookup> != Some(anchor_number) { return Err(...) }`), in one check.
- `be/lock-needs-recovery` A lock or "in progress" flag written before an `await` needs a recovery path.
- `be/no-check-await-write` Don't check, await, then write (get-or-create with `raw_rand`). An install hook can't await, so initialize from a zero-delay timer scheduled at install that re-checks after its await (`set_salt_if_unset`), and refuse dependent calls until it has run.
- `be/never-drop-result` Never drop a result: no `let _ =` on a `Result`, check what `.remove()`/`.insert()` return, and use `#[must_use]` where a value must be consumed.
- `be/insert-detects-collision` A map insert must not silently overwrite an existing binding: detect the collision and return an error.
- `be/time-once` Call `time()` once at the endpoint and pass `now: Timestamp` (nanoseconds) down.
- `be/log-invariant-violations` Log internal-invariant violations with `ic_cdk::println!("ERROR: ...")`, at the call site that handles the error, never inside an error conversion (`map_err` target, `From` impl): a conversion only maps.

## Layering

- `be/thin-endpoints` `main.rs` is a thin entry point, and every endpoint follows the same order: authorize (through `authz_utils`: `check_authz_and_record_activity`, `anchor_operation_with_authz_check`, or the gate of the feature that owns the caller, like `mcp::authorize_mcp_session` for registered MCP session keys), validate shape, destructure, narrow, make one call to the module that owns the feature, map its result. Feature logic lives in its module, not in `main.rs`, including multi-phase flows and the re-checks between their `await`s.
- `be/anchor-ops-through-authz` Anchor-mutating updates go through `anchor_operation_with_authz_check` / `post_operation_bookkeeping` so they are archived and counted.
- `be/validate-shape-at-endpoint` Endpoints validate shape. Convert raw candid input once, through `TryFrom`, into a `Validated*` request whose fields are shared newtypes (`ApplicationOrigin`), collecting all errors.
- `be/destructure-requests-fully` Destructure it completely at the top of the endpoint, without `..`, so the API carries no field the endpoint doesn't use and a new field is a compile error until it is handled. A field that is deliberately left unused needs a code comment saying why.
- `be/no-trusting-raw-input` Business logic never assumes a raw `String`, number or byte vector was validated upstream. Choose per case how it gets valid input: a type owned by the business module that only the module's own checks can construct, or validation at the function's entry. A type fits when the value crosses several functions or modules, is persisted or authorized on, or carries an invariant beyond shape (the email belongs to a known anchor, the timestamp is in the future). Validating at the entry fits when the input is used in one place, or when a type would only wrap a single check. Either way the module's types are independent of the API's, so the two evolve separately.
- `be/narrow-arguments` Narrow before calling: pass each function only the values it uses, not the whole request, anchor or state.
- `be/map-errors-once` Map errors in one place. Business logic returns `Result<T, ModuleError>` and propagates with `?`; the endpoint converts that error to the method's candid error type once, not by building an error response at each failure.
- `be/abstract-storage` Storage is abstract: it takes and returns domain types, keeps `Storable*` types, internal numbers (`ApplicationNumber`) and index sync to itself (inside `Storage::write`), and contains no feature logic.
- `be/invariant-in-lowest-layer` Enforce an invariant in the lowest layer that must hold it.
- `be/one-owner-for-related-maps` Keep maps that must agree behind one struct that owns every mutation, or model multi-step state as one enum, so inconsistent combinations can't be represented.
- `be/state-in-state-module` Keep mutable state in the `state` module and don't nest `STATE.with` borrows.
- `be/no-hidden-global-reads` A helper doesn't read global state behind the caller's back: pass what it needs.

## Rust style

- `be/import-types-unqualified` Import types at the top of the module and use them unqualified (`ApplicationOrigin`, not `crate::types::ApplicationOrigin`).
- `be/call-functions-through-module` Import modules rather than functions, and call a function through its module (`storage::write(...)`, `mem::take(...)`), so every call shows where the function comes from.
- `be/one-module-per-use` One `use` statement imports from one module (no nested `{a::{b}}`).
- `be/no-mod-rs` New modules use `foo.rs` + `foo/bar.rs`, not `foo/mod.rs`. Test fixtures go in `foo/fixtures.rs` or a `#[cfg(test)]` module.
- `be/unwrap-at-source` Unwrap an `Option` or `Result` where it is produced (`let Some(anchor_number) = ... else { return Err(...) };` or `?`). Don't carry `*_maybe` bindings to a later check.
- `be/shadow-with-validated` Shadow a raw value with its validated form (`let request = ValidatedRequest::try_from(request)?;`), so the raw value can't be used afterwards.
- `be/exhaustive-match` Prefer a full `match` over `if let` on enums, and keep distinct cases in distinct arms (no catch-all `_` merging them).
- `be/one-question-mark-per-line` At most one `?` per line.
- `be/visible-early-exit` A `?` ends its own `let` or statement, so every early exit is at the end of a line: never inside a tuple, a struct literal, a call argument or a match arm. Bind the value first (`let sender = principal(&bytes)?;`). Test code may keep a `?` inside an assertion's arguments.
- `be/no-mutation-in-conditions` No state-changing calls inside `if` conditions.
- `be/deterministic-collections` Use `BTreeSet`/`BTreeMap` (deterministic order).
- `be/option-not-sentinel` Use `Option` for absence, never sentinels like `""` or `0`.
- `be/derive-enumerations` Derive enumerations from the type (`AttributeName::all()`) rather than a hand-kept list.
- `be/dependency-injection` Use dependency injection rather than `#[cfg(test)]` / `#[cfg(not(test))]` branches or test-only thread-locals in production code, so the production branch is the one tested.
- `be/no-widened-visibility` Don't widen visibility (`pub`, `pub(crate)`) or add test-only accessors and `#![allow(dead_code)]` to keep code alive. Delete unused code. Don't add thin `pub` re-export wrappers.
- `be/no-clippy-allow` Don't add `#[allow(clippy::...)]`; restructure instead (a type alias, a smaller function).
- `be/short-functions` Keep a function body under about 50 lines. Re-check the whole function after every addition, not just the lines you added: functions grow long a few lines at a time. When one passes the limit, split it into named steps, e.g. one function per `match` arm.
- `be/canister-dependencies` New canister dependencies: check maintenance, advisories, wasm/`no_std` fit and the wasm size budget, and use `default-features = false`. Dev-dependencies use `.workspace = true`. List every added, removed or changed dependency in the PR description with its reason (see `scripts-and-ci.md`).
- `be/newtype-when-meaningful` Make a value a newtype rather than a type alias when it has rules of its own, needs its own methods, or is easy to mix up with another value of the same underlying type.
- `be/named-fields` Prefer named fields over tuples and tuple structs, except for a single-field newtype.
- `be/field-init-shorthand` Build a struct field from a binding of the same name, so the literal uses the shorthand: shadow the binding with its converted form first (`let subscription = WebPushSubscription::Ready(subscription);`, then `PwaSubscription { subscription, .. }`), and destructure the source in a conversion rather than projecting `value.field` into each field.
- `be/avoid-result-option` Avoid `Result<Option<T>, E>`: split the lookup that may find nothing from the check that may refuse, and let the caller combine them. Where it is the right shape, the doc comment says why.
- `be/std-naming` Follow the standard library's naming: `new` for a constructor, `try_new` when it can fail, `from_*` next to a `From` impl; `as_*`/`to_*`/`into_*` for conversions; `foo()` for a getter, with `set_foo`, `take_foo` and `replace_foo`.
- `be/std-traits` Implement the standard traits (`TryFrom`, `FromStr`, `Display`, `Default`) instead of custom methods that do the same, and `#[derive]` them when possible.
- `be/flexible-borrows` Take `&str`, `&[T]` and `Option<&T>`, not `&String`, `&Vec<T>` or `&Option<T>`.
- `be/borrow-over-clone` Borrow instead of calling `.clone()` to satisfy the borrow checker, and prefer unique ownership over `Rc`/`Arc`.
- `be/explicit-inferred-types` Annotate the type where inference hides it: the result of `.into()`, `.parse()` or `.collect()`, and long iterator chains. Prefer a typed binding over a turbofish.
- `be/loops-over-long-chains` Use a `for` loop when an iterator chain gets long or nested, and don't use `for_each`.
- `be/functions-over-macros` Prefer functions, types and traits over macros. Use `macro_rules!` only when they can't do the job, and a proc macro last.
- `be/unsafe-safety-comment` Don't write `unsafe` when a safe way exists. Every `unsafe` block gets a `// SAFETY:` comment saying why it is sound.

## Clippy

`be/run-clippy` Run `cargo fmt` and `cargo clippy` after Rust changes, before committing. CI (`.github/workflows/rust.yml`) runs two passes, and both must be clean. In a fresh checkout, create `dist/` first: the frontend canister embeds it with `include_dir!`, and CI creates an empty one the same way.

```bash
mkdir -p dist
cargo clippy -- -D clippy::all -D warnings -A clippy::manual_range_contains
cargo clippy --tests --benches -- -D clippy::all -D warnings -A clippy::manual_range_contains
```

The first pass catches dead code that only test code uses; the second lints the tests. Add `-p <crate>` to narrow a local run. CI runs clippy on every PR of a stack, so a symbol must be used in the same PR that defines it.

## Integration tests (PocketIC)

`be/test-canisters-script` `scripts/test-canisters.sh` prepares everything the integration tests (`cargo test -p internet_identity --test integration ...`) need, at the repository root: the PocketIC server binary `pocket-ic`, the built `internet_identity.wasm.gz` and `archive.wasm.gz`, and the previous release wasms (`internet_identity_previous.wasm.gz`, `archive_previous.wasm.gz`) for the upgrade and rollback tests, plus the pinned `internet_identity_doh_allowlist.wasm.gz` (through `scripts/fetch-doh-allowlist-wasm`) for the DoH allowlist migration tests. Read it before preparing them by hand.

`be/pin-pocket-ic` Download the exact pinned PocketIC server version, never `latest`. The pin lives in `scripts/test-canisters.sh` (`POCKET_IC_SERVER_VERSION`) and `.github/workflows/canister-tests.yml`, and must match the `pocket-ic` client crate in `Cargo.toml`. Bump all of them together. Look the pin up rather than trusting a version quoted anywhere:

```bash
grep POCKET_IC_SERVER_VERSION scripts/test-canisters.sh
gh release view <VERSION> --repo dfinity/pocketic --json assets --jq '.assets[].name'
```

`be/pocket-ic-platform-asset` The release ships `pocket-ic-{arm64,x86_64}-{darwin,linux}.gz`. Pick the one matching your machine, not the one the script names: `scripts/test-canisters.sh` hardcodes `x86_64`, which is wrong on Apple Silicon. Gunzip it to `pocket-ic` at the repository root and `chmod +x` it.

`be/pocket-ic-errors` A loader error such as `rosetta error: failed to open elf at /lib64/ld-linux-x86-64.so.2` or `Exec format error` means the binary doesn't match the platform: fetch the right asset, never emulate. A server that starts but fails every test at setup usually means the version doesn't match the `pocket-ic` crate: check the pin.
