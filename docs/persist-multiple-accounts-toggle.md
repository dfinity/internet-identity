# Persist the "multiple accounts" toggle in the backend

**Date:** 2026-06-10
**Updated:** 2026-06-11 — split scoped session delegations into [their own doc](./scoped-session-delegations.md); this design now depends on them.

## TL;DR

The "Enable multiple accounts" toggle on the authorize screen is in-memory only and resets on every page load and every identity switch. I propose persisting it per `(anchor, application)` by extending the existing `AnchorApplicationConfig` stable structure on the backend (please see [why not on browser storage](#dont-persist-use-browser-localstorage) below). Marginal storage is a few bytes per row where the toggle is set, with zero overhead for users who never enable it. No new storage map, no migration.

**Depends on [scoped session delegations](./scoped-session-delegations.md).** Persisting the toggle backend-side makes its reads and writes canister calls, and today every canister call rides on a device-rooted delegation capped at ~30 minutes — so a returning user would pay a full WebAuthn ceremony just to _render_ their persisted preference. The session-delegations design adds longer-lived, scope-limited delegations that authenticate these reads without a fresh passkey ceremony. The toggle is the first consumer of the `account_management` scope; this design should ship together with — or after — that one.

## Context

### What "multiple accounts" is today

A toggle on the new-styling authorize flow (`src/frontend/src/routes/(new-styling)/authorize/views/ContinueView.svelte`). When the user enables it:

- The frontend calls `get_accounts` + `get_default_account` for the current origin.
- The user can create / rename / delete up to 5 named accounts per origin and pick a default.
- The toggle gates the UI — when off, the user sees the single-default-account view; when on, the per-app account list.

### What's already persisted

The accounts themselves and the default selection are fully persisted backend-side:

- `StorableAccount` — name + seed material (`src/internet_identity/src/storage/storable/account.rs`)
- `StorableAccountReferenceList` — which accounts belong to which `(anchor, application_number)`
- `AnchorApplicationConfig` — per-`(anchor, application)` config, today just `default_account_number`
- Candid surface: `get_accounts`, `create_account`, `update_account`, `set_default_account`, `get_default_account`

`AnchorApplicationConfig` is keyed by `(AnchorNumber, ApplicationNumber)`, the `application_number` being looked up from `origin` via `lookup_or_insert_application_number_with_origin`.

### What's not persisted

The toggle itself. `isMultipleAccountsEnabled` lives as Svelte `$state` in `ContinueView.svelte` (line 48). It is reset to `false` on identity switch (line 90, PR #3721) and lost on every reload, browser change, and device change. It is not in localStorage either.

## Problem

A user enables multiple accounts on dapp X, creates three named accounts, returns the next day:

- The accounts are still there on the canister.
- The toggle is off, so the UI shows the single-default-account view and hides them.
- The user has to re-enable the toggle to even see their own accounts.

The same happens on every device, every identity switch, every reload. This silently regresses the user's chosen account-management mode to the default each time.

## Goals

- Persist the toggle per `(anchor, origin)` so it survives reloads, identity switches, and device changes.
- Minimal storage and call-cost overhead.
- Forward-compatible: leave room for further per-app preferences without re-versioning Candid each time.

## Non-goals

- Persisting other UI chrome (theme, layout, etc.) — see "Where this data should live" below.
- Generalizing `AnchorApplicationConfig` into an open key-value map.
- Changing the existing account / default-account storage or APIs.

## Dependencies

This design **depends on the [scoped session delegations](./scoped-session-delegations.md) design landing first** (or alongside). Without scoped session delegations, persisting the toggle is a UX regression — a returning user pays a full WebAuthn ceremony to render the screen that shows their preference, undoing the benefit of persistence. Scoped session delegations make the relevant reads (`get_accounts`, `get_default_account`, the new `get_anchor_application_config`) ceremony-free for low-stakes use, which is what makes persistence net-positive.

Concretely:

- The two new endpoints below (`get_anchor_application_config`, `set_anchor_application_config`) opt into the `account_management` scope, so they accept both device-rooted callers and session-scoped callers.
- The `ContinueView.svelte` wiring (below) reads the persisted toggle via the resolved session-scoped actor, falling back to the device-rooted delegation when one is live and to a full-auth ceremony when neither is.

Recommended order: ship the session-delegations backend first (its endpoints are inert until consumed) → this toggle persistence → frontend wiring of both.

## Proposal

### Storage change

Extend `AnchorApplicationConfig` (`src/internet_identity/src/storage/storable/anchor_application_config.rs`) with one additive cbor field:

```rust
#[derive(Encode, Decode, Default, Clone, Ord, Eq, PartialEq, PartialOrd)]
#[cbor(map)]
pub struct AnchorApplicationConfig {
    #[n(0)]
    pub default_account_number: Option<StorableAccountNumber>,

    #[n(1)]
    pub multiple_accounts_enabled: Option<bool>,
}
```

Because the struct uses `#[cbor(map)]`, the new field is wire-compatible with already-written entries — old data decodes with `multiple_accounts_enabled = None`. No migration code.

### Storage convention

Match the existing "skip the empty state" convention:

| Row state                                                                  | Interpretation                  |
| -------------------------------------------------------------------------- | ------------------------------- |
| No row in the BTreeMap                                                     | Toggle off, default = synthetic |
| Row with `default_account_number: None`, `multiple_accounts_enabled: None` | Toggle off, default = synthetic |
| Row with `multiple_accounts_enabled: Some(true)`                           | Toggle on                       |
| Row with `multiple_accounts_enabled: Some(false)`                          | Toggle off, explicitly set      |

We **only write** a row when the user enables the toggle (`Some(true)`) — turning it off writes `Some(false)` if the row already exists for other reasons (a default is set), otherwise the row is left absent. We do not delete rows when the user turns the toggle off, matching today's behavior for `set_default_account(account_number = None)`.

The mapping from row state to user-visible state is wrapped in a single helper in `account_management.rs` so callers never have to reason about the distinction.

### Candid surface

Add two methods:

```candid
get_anchor_application_config :
    (IdentityNumber, FrontendHostname) ->
        (variant { Ok : AnchorApplicationConfigInfo;
                   Err : GetAnchorApplicationConfigError }) query;

set_anchor_application_config :
    (IdentityNumber, FrontendHostname, AnchorApplicationConfigInfo) ->
        (variant { Ok;
                   Err : SetAnchorApplicationConfigError });

type AnchorApplicationConfigInfo = record {
    default_account_number : opt AccountNumber;
    multiple_accounts_enabled : opt bool;
};
```

Rationale for a dedicated endpoint rather than bundling into `get_default_account`:

- `AccountInfo` is per-_account_; this is per-_application_. Conflating them confuses the model.
- A dedicated endpoint gives a natural home for future per-application preferences without churning the Candid surface again.

`set_default_account` and `get_default_account` remain unchanged. `set_anchor_application_config` is allowed to set or clear _either_ field; the two never need to be set atomically.

### Frontend wiring

In `ContinueView.svelte`:

1. On authentication for the current origin, call `get_anchor_application_config` (alongside the existing `get_default_account`), and bind the returned `multiple_accounts_enabled` to `isMultipleAccountsEnabled`.
2. When the user flips the toggle, call `set_anchor_application_config` with the new value. Fire-and-forget; failure is non-blocking (the local state still reflects the user gesture).
3. Drop the identity-switch reset on line 90. The new value will be loaded for the next identity instead.

## Storage cost

Per-row cost of the new field:

| Case                                                           | Extra bytes vs. today                                             |
| -------------------------------------------------------------- | ----------------------------------------------------------------- |
| Row exists, toggle never set (`None`)                          | 0 (field omitted in cbor map)                                     |
| Row exists, toggle set to `Some(true)` or `Some(false)`        | ~3 bytes                                                          |
| Row created solely because of the toggle (no default ever set) | ~50–70 bytes (16-byte key + small value + BTreeMap node overhead) |

Order-of-magnitude projection:

- Realistic (~5% of active anchors enable on ~1–2 dapps): **~5 MB total** across the canister.
- Pessimistic (every anchor enables on every dapp): **<1 GB**.

Against the canister's stable memory footprint, both numbers are negligible. The cbor-additive encoding means we pay nothing for users who never touch the toggle.

## Call costs

- **Reads** — one extra query call on the authorize page (`get_anchor_application_config`) — cycle-free, runs in parallel with `get_default_account` / `get_accounts`. Negligible.
- **Writes** — one update call per toggle flip. Toggle flips are rare user gestures; cost is comparable to `set_default_account`.
- **Storage growth** — additive cbor field, no migration write amplification at upgrade.

## Privacy and auth

- **Caller authorization**: `set_anchor_application_config` requires the caller to be authenticated as a device of the anchor, same model as `set_default_account`. Both new endpoints additionally accept a session-scoped caller carrying the `account_management` scope — see [endpoint opt-in](./scoped-session-delegations.md#endpoint-opt-in-and-the-default-deny-rule) in the session-delegations doc.
- **New information leaked**: none. The `(anchor, application_number)` association already exists via `StorableAccountReferenceList` and `AnchorApplicationConfig.default_account_number`. The toggle adds one bit to a relation that's already on the canister.
- **Cross-origin**: the frontend always writes for the current origin, derived from the authorize request, not from caller input. Same surface as the existing account-management endpoints.

## Where this data should live

II is identity infrastructure, not a general-purpose preference store. The boundary we propose:

> **`AnchorApplicationConfig` holds preferences that change _identity behavior_ per application — not UI chrome.**

The multiple-accounts toggle qualifies because it determines:

- Which accounts a dapp sees in the response.
- Which auth path runs (single-default vs. account-picker).

`default_account_number` is already there for the same reason. The toggle is the next field in the same category.

UI chrome (theme, layout, animation prefs) does **not** qualify and should not be added to this struct. We explicitly document this in a doc-comment on `AnchorApplicationConfig` so future additions are deliberate. If the team later wants a generic per-anchor preference store, it should be a separate construct with its own design discussion — not a quiet drift in scope here.

The value proposition of persisting backend-side (rather than localStorage) is **cross-device sync**: a user who enables multiple accounts on their phone sees the same view on their laptop. That's the unique capability II offers as a central identity layer, and it's the reason this data belongs here rather than in the browser.

There is also a maintenance argument independent of cross-device sync: **related state for one feature should live in one place**. The accounts, their names, their seeds, and the default selection are all stored backend-side. Putting the toggle that gates the whole feature in browser storage would split the persistence layer for a single feature across two systems with different lifetimes, different failure modes, different test surfaces, and different debugging stories. Every future change to multi-accounts ("can a dapp clear all accounts?", "what happens when the user resets their browser?", "how do we expose this in stats?") would have to reason about both halves and keep them in sync. Concentrating the state in `AnchorApplicationConfig` keeps one code path, one storage contract, one place to look when something goes wrong.

## Alternatives considered

### Identity-level metadata (`MetadataMapV2`, `identity_metadata_replace`)

Stash the toggle under a key like `"account_settings:<origin>"` in the existing identity metadata bag.

Rejected because:

- Wrong scope — identity metadata is anchor-global, not per-application. We would re-implement origin keying inside an unstructured map.
- Loses type safety; everything becomes a string blob.
- Pollutes a shared map used for unrelated purposes.

### Side-table `StableBTreeMap<(AnchorNumber, ApplicationNumber), ()>`

A dedicated map of "(anchor, app) pairs where the toggle is on."

Rejected because:

- Same per-row overhead as adding a new row to `AnchorApplicationConfig`.
- Duplicates the BTreeMap that already exists for this exact key.
- Splits per-application config across two data structures, harming locality.

### Pack the toggle into `default_account_number`

Redefine the field as an enum `Default { Synthetic { multi_enabled: bool }, Numbered(AccountNumber) }`.

Rejected because:

- Saves ~3 bytes per row at the cost of conflating two unrelated concepts.
- Requires non-trivial migration code (the cbor representation of the field changes).
- Couples future toggles to this enum.

### Bundle the toggle into `get_default_account` / `AccountInfo`

Add `multiple_accounts_enabled` to whatever `get_default_account` already returns.

Rejected because:

- `AccountInfo` is per-account; the toggle is per-application. Mixing them is a category error and will look stranger as more per-application fields are added.
- A dedicated endpoint gives a clean extension point.

### Don't persist; use browser localStorage

Rejected because:

- Doesn't survive cache clear, profile change, or device switch.
- Loses the cross-device-sync value-prop that makes II a useful identity layer.
- **Splits the persistence layer for a single feature across two systems.** The accounts, names, seeds, and default selection already live in stable memory; the toggle would live in the browser. That means two storage contracts, two failure modes (canister upgrade vs. browser cache clear), two debugging stories, and two test surfaces — all for one feature. Every future change to multi-accounts would have to reason about both halves and keep them coherent (e.g., what does it mean for the toggle to be "on" in localStorage when the backend has no accounts for this origin?). The cost compounds with every subsequent change.
- **Diverges from the surrounding pattern.** `default_account_number` — the closest sibling to this toggle — is already persisted in `AnchorApplicationConfig`. Storing the toggle elsewhere creates an arbitrary split inside the same logical config object: half its fields backend-side, half browser-side. Any future per-application preference would face the same choice, with no principled answer.
- **Hurts observability.** Backend-side state can be counted and reported in `stats/event_stats/`. Browser-local state is invisible to the canister, so we lose the ability to answer basic product questions like "how many users have multi-accounts enabled" without bolt-on client telemetry.

## Migration and rollout

- **Migration**: none. The cbor `#[cbor(map)]` encoding makes `#[n(1)]` additive. Existing data decodes with `multiple_accounts_enabled = None`. New data with the field set encodes the extra ~3 bytes.
- **Rollout**: ship backend first (new candid + storage + tests), then frontend wiring. There is no flag to gate this — the frontend simply starts calling the new endpoint, which returns `None` for users who never enabled it (preserving today's behavior).
- **Rollback safety**: a previous II version can decode rows written by the new version because the extra cbor field is skipped on decode. Worst case is "the toggle field is ignored" for the duration of the rollback. No data loss.
- **Stable structures version**: unchanged. No new memory region, no new versioned schema.

## Testing

### Rust unit tests

- `account_management::get_anchor_application_config` returns defaults when no row exists.
- `set_anchor_application_config` + `get_*` round-trips both fields independently.
- Existing rows (with `default_account_number` only) decode correctly with the new struct definition — round-trip test: old-cbor bytes → decode → check `multiple_accounts_enabled = None` → re-encode → decode → unchanged.

### Playwright E2E

- Enable toggle, reload page, confirm toggle is still on and account list is shown.
- Sign out, sign back in with the same anchor, confirm toggle state.
- Sign in with a different anchor on the same dapp, confirm independent toggle state.

## Open questions

- **Do we want to evolve `get_default_account` later?** The new endpoint partly subsumes it. We can leave the old one in place forever, or deprecate it in a future cleanup. Out of scope for this change.
- **Should toggle-off delete the row when it would otherwise be empty?** Today's code doesn't compact empty rows; we mirror that here.
