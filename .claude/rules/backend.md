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

Canisters must not trap. Do not introduce `panic!`, `.unwrap()`, `.expect(...)`, `unreachable!()`, `todo!()`, `unimplemented!()`, `assert!`, `[index]` or panicking arithmetic outside `#[cfg(test)]`.

- Return `Result<T, E>` and propagate with `?` instead of trapping.
- Index collections with `.get()`, `.find()`, `iter_mut().find()` and `let ... else`, never `[i]`, and don't carry an index across borrows.
- Use `checked_*` arithmetic and `try_from` conversions; no `as` casts between integer types. Prefer `checked_*` plus an error over `saturating_*` when the clamped value would be acted on or an id handed out twice. Truncate strings only at a char boundary.
- Never `unwrap_or_default()` a value that is then persisted or authorized on. A missing record is an error, not an empty default.
- Push back on reviewer suggestions (Copilot and others) to add `.expect(...)` or `debug_assert!` "to document the invariant".

Exceptions, each to be kept as narrow as it is:
- `Storable::from_bytes` cannot return an error, so it may `expect` with an actionable message. Make decoding unable to fail instead: see `stable-memory.md`, and use the bounded helpers (`slice_to_bounded_32`) rather than `copy_from_slice`.
- HTTP outcall transform functions cannot return or log errors, so they may trap on a response that is unusable as a whole. Skip a single malformed element (a key without a `kid`) rather than rejecting the response.
- `#[cfg(test)]` code may `unwrap`/`expect`; prefer `expect("context")`.

## State changes, errors and `await`

- On the IC, returning `Err` doesn't undo state changes. Every mutation made before it is committed: stable-memory writes (`Storage::write`, map inserts and removes, counter bumps) and changes to heap state (`state` module, `thread_local!`). Reads, locals and building the response don't count. Only a trap rolls back. Run every check that can fail before the first mutation, and never use `?` after one. Each `await` commits the same way (next rule).
- Every `await` is a commit point and lets other messages run. Re-read and re-check (authorization, existence, generation) anything captured before it; never write back a value loaded before it. A lock or "in progress" flag written before an `await` needs a recovery path.
- Don't check, await, then write (lazy salts, get-or-create with `raw_rand`). Initialize at install, or refuse.
- Never drop a result: no `let _ =` on a `Result`, check what `.remove()`/`.insert()` return, and use `#[must_use]` where a value must be consumed. A map insert must not silently overwrite an existing binding: detect the collision and return an error.
- Call `time()` once at the endpoint and pass `now: Timestamp` (nanoseconds) down.
- Log internal-invariant violations with `ic_cdk::println!("ERROR: ...")`.

## Layering

- `main.rs` is a thin entry point: authorize first (through `authz_utils`: `check_authz_and_record_activity`, `anchor_operation_with_authz_check`), validate, then call the module that owns the feature. Feature logic lives in its module, not in `main.rs`.
- Anchor-mutating updates go through `anchor_operation_with_authz_check` / `post_operation_bookkeeping` so they are archived and counted.
- Endpoints validate shape. Convert raw candid input once, through `TryFrom`, into a `Validated*` request whose fields are shared newtypes (`ApplicationOrigin`), collecting all errors. Destructure it completely at the top of the endpoint, without `..`, so the API carries no field the endpoint doesn't use and a new field is a compile error until it is handled. A field that is deliberately left unused needs a code comment saying why.
- Business logic never takes a raw `String`, number or byte vector and hopes it was validated. It takes types that can only hold valid values. A business module owns those types, independently of the API. They can enforce more than shape (the email belongs to a known anchor, the timestamp is in the future), and only the module's own checks can construct them. A module may instead accept a less specific input and validate it itself, but then the check sits at its entry. The API and the module evolve independently.
- Storage is abstract: it takes and returns domain types, keeps `Storable*` types, internal numbers (`ApplicationNumber`) and index sync to itself (inside `Storage::write`), and contains no feature logic. Enforce an invariant in the lowest layer that must hold it.
- Keep maps that must agree behind one struct that owns every mutation, or model multi-step state as one enum, so inconsistent combinations can't be represented.
- Keep mutable state in the `state` module and don't nest `STATE.with` borrows. A helper doesn't read global state behind the caller's back: pass what it needs.

## Rust style

- Import types and functions at the top of the module, merged per path; no inline fully-qualified paths.
- New modules use `foo.rs` + `foo/bar.rs`, not `foo/mod.rs`. Test fixtures go in `foo/fixtures.rs` or a `#[cfg(test)]` module.
- No bare `bool` parameters or returns from functions that aren't predicates: use an enum, a `Result`, or two functions (`enable_x` / `disable_x`).
- Prefer a full `match` over `if let` on enums, keep distinct cases in distinct arms (no catch-all `_` merging them), at most one `?` per line, and no state-changing calls inside `if` conditions.
- Use `BTreeSet`/`BTreeMap` (deterministic order) and `Option` for absence, never sentinels like `""` or `0`. Derive enumerations from the type (`AttributeName::all()`) rather than a hand-kept list.
- Use dependency injection rather than `#[cfg(test)]` / `#[cfg(not(test))]` branches or test-only thread-locals in production code, so the production branch is the one tested.
- Don't widen visibility (`pub`, `pub(crate)`) or add test-only accessors and `#![allow(dead_code)]` to keep code alive. Delete unused code. Don't add thin `pub` re-export wrappers.
- Don't add `#[allow(clippy::...)]`; restructure instead (a type alias, a smaller function).
- Keep function bodies short enough to read on one screen.
- New canister dependencies: check maintenance, advisories, wasm/`no_std` fit and the wasm size budget, and use `default-features = false`. Dev-dependencies use `.workspace = true`. List every added, removed or changed dependency in the PR description with its reason (see `scripts-and-ci.md`).

## Clippy

Run `cargo fmt` and `cargo clippy` after Rust changes, before committing. CI (`.github/workflows/rust.yml`) runs two passes, and both must be clean:

```bash
cargo clippy -- -D clippy::all -D warnings -A clippy::manual_range_contains
cargo clippy --tests --benches -- -D clippy::all -D warnings -A clippy::manual_range_contains
```

The first pass catches dead code that only test code uses; the second lints the tests. Add `-p <crate>` to narrow a local run. CI runs clippy on every PR of a stack, so a symbol must be used in the same PR that defines it.

## Integration tests (PocketIC)

`scripts/test-canisters.sh` prepares everything the integration tests (`cargo test -p internet_identity --test integration ...`) need, at the repository root: the PocketIC server binary `pocket-ic`, the built `internet_identity.wasm.gz` and `archive.wasm.gz`, and the previous release wasms (`internet_identity_previous.wasm.gz`, `archive_previous.wasm.gz`) for the upgrade and rollback tests. Read it before preparing them by hand.

Download the exact pinned PocketIC server version, never `latest`. The pin lives in `scripts/test-canisters.sh` (`POCKET_IC_SERVER_VERSION`) and `.github/workflows/canister-tests.yml`, and must match the `pocket-ic` client crate in `Cargo.toml`. Bump all of them together. Look the pin up rather than trusting a version quoted anywhere:

```bash
grep POCKET_IC_SERVER_VERSION scripts/test-canisters.sh
gh release view <VERSION> --repo dfinity/pocketic --json assets --jq '.assets[].name'
```

The release ships `pocket-ic-{arm64,x86_64}-{darwin,linux}.gz`. Pick the one matching your machine, not the one the script names: `scripts/test-canisters.sh` hardcodes `x86_64`, which is wrong on Apple Silicon. Gunzip it to `pocket-ic` at the repository root and `chmod +x` it.

A loader error such as `rosetta error: failed to open elf at /lib64/ld-linux-x86-64.so.2` or `Exec format error` means the binary doesn't match the platform: fetch the right asset, never emulate. A server that starts but fails every test at setup usually means the version doesn't match the `pocket-ic` crate: check the pin.
