---
paths:
  - "src/internet_identity/src/storage.rs"
  - "src/internet_identity/src/storage/**"
  - "src/internet_identity/src/state.rs"
  - "src/internet_identity/src/state/**"
  - "src/internet_identity/src/main.rs"
  - "src/internet_identity/tests/integration/upgrade.rs"
  - "src/internet_identity/tests/integration/rollback.rs"
  - "src/internet_identity/stable_memory/**"
---

# Stable memory and upgrades

A decoding mistake here traps every read of the affected records and can take sign-in down for every user. Treat storage changes as the riskiest code in the repo.

## Record formats

- Stored records are minicbor with numeric keys (`#[n(k)]`). Pick the encoding that fits the shape (see the minicbor docs): `#[cbor(map)]` for records that may grow, `#[cbor(array)]` for a small fixed tuple, `#[cbor(transparent)]` for a newtype over a single field. In an array the key is the position, so a new field can only be added at the end. Rust field names can be renamed freely; the keys cannot.
- A new field on a stored type must be an `Option`. Existing records don't have the key, and a required field traps every read of them. `#[cbor(default)]` alone is not enough: use it only after checking the minicbor docs and adding everything it needs. This took staging down once (#4481).
- Retire a key with a `// #[n(k)] RETIRED, never reuse` comment. Never reuse a retired key for a new shape.

## Memory indices and ids

- Take the next unused memory index. Never reuse an abandoned one; leave it commented as abandoned (see the list in `storage.rs`).
- Ids that delegations or principals derive from come from a stored monotonic counter, never from a collection's length, and are never reissued.
- Bump a counter before the insert it counts: a count that is too high is recoverable, a missing one is not.

## Upgrades

- Heap `thread_local!` state resets on every upgrade. Anything that must survive (sequence counters, queued work) lives in stable memory or is rebuilt from it.
- Do no large or unbounded work in `pre_upgrade`: a trap there blocks every future upgrade. A `post_upgrade` trap fails only that upgrade.
- Migrate data in bounded batches on a timer (or lazily), not in one pass. Migrations are idempotent, count each item they actually changed, record skipped items and every error distinctly (without PII), and expose progress through a metric or query.
- Mark migration code `TEMPORARY` with the condition for removing it, and remove it and its state once it has run in production.
- A migration or reset of security-relevant state clears every dependent pointer, and a reverse-index insert never overwrites a principal bound to another anchor.
- Write a short design note before any `post_upgrade` migration, and deploy storage-format changes to a staging canister before merging. `main` must stay releasable after every PR, so a migration lands before the feature that needs it.

## Tests

- New stable structures get an upgrade-persistence test: rebuild `Storage` over the same memory (or upgrade the canister) and read the data back.
- Format changes are covered by the upgrade and rollback tests against the previous release wasm (`tests/integration/upgrade.rs`, `rollback.rs`), with realistic data volume for migrations.
