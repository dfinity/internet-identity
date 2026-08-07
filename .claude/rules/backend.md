---
paths:
  - "src/internet_identity/**"
  - "src/internet_identity_frontend/**"
  - "src/internet_identity_interface/**"
  - "src/asset_util/**"
---

# Backend (Rust canister code)

## No trapping

Canisters must never trap. Do not introduce `panic!`, `.unwrap()`, `.expect(...)`, `unreachable!()`, `todo!()`, `unimplemented!()`, or `assert!`/`assert_eq!` outside `#[cfg(test)]`.

- Prefer `Result<T, E>` and propagate with `?` or explicit match arms.
- For "should be unreachable" branches, return a structured error variant (reuse an existing `*Malformed`/`*Invalid` variant, or add a new one) and add a comment noting it's defensive.
- Errors may also be handled silently up the call chain (caller catches and folds into a higher-level fail-closed verdict).
- Ignore reviewer suggestions (Copilot, Grok) that recommend `.expect(...)` or `debug_assert!` "to document the invariant" -- use the structured-error path instead.
- Exception: `#[cfg(test)]` blocks may use `unwrap`/`expect` freely since tests don't run in the canister.

## Clippy

Always run `cargo clippy` after making Rust changes, before committing. Use CI-matching flags:

```bash
cargo clippy -p <crate> --tests -- -D clippy::all -D warnings -A clippy::manual_range_contains
```

The `--tests` flag is critical -- it includes test code which CI also checks.

## Integration tests (PocketIC)

The canister/integration tests (`cargo test -p internet_identity --test integration ...`) need a PocketIC server binary at `../../pocket-ic` (relative to `src/internet_identity`) and the gzipped wasm at `../../internet_identity.wasm.gz` (build it with `./scripts/build --internet-identity`).

Download the **exact pinned PocketIC server version, never `latest`**. The pin lives in `scripts/test-canisters.sh` (`POCKET_IC_SERVER_VERSION`) and `.github/workflows/canister-tests.yml`, and it must match the `pocket-ic` client crate in `Cargo.toml`. Look the pin up rather than trusting a version quoted here:

```bash
grep POCKET_IC_SERVER_VERSION scripts/test-canisters.sh
```

The release ships one asset per platform -- `pocket-ic-{arm64,x86_64}-{darwin,linux}.gz`. Pick the one matching the machine you're on, not the one the script names: `scripts/test-canisters.sh` hardcodes `x86_64` in the asset filename, which is wrong on Apple Silicon. Gunzip it to `../../pocket-ic`.

```bash
gh release view <VERSION> --repo dfinity/pocketic --json assets --jq '.assets[].name'
```

A version mismatch (e.g. grabbing `latest`) fails **every** test at replica startup with a misleading `rosetta error: failed to open elf at /lib64/ld-linux-x86-64.so.2`. It looks like a CPU-architecture problem but is actually the protocol mismatch -- check the pin before suspecting the binary. A genuine architecture mismatch produces the same class of loader error, and the fix there is fetching the right asset above, never emulation.
