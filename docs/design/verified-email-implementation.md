# Verified emails — implementation checklist

**Companion to:** [verified-email-attributes.md](verified-email-attributes.md) — the design doc, which explains the _why_. This file is the _what to do_, one phase at a time, with concrete file paths and changes.

**How to use this:**

- Items are discrete units of work with file:line citations and named function targets.
- Phases ship in order (1a → 1b → 1c → 2 → 3); within a phase, sub-sections can land in parallel.
- Open decisions (cap, pending intent, etc.) and locked decisions (cap=5, wizard rename, etc.) are documented in the design doc's "Open decisions checklist" — refer there before starting any sub-section that touches them.
- LLM agents implementing this should treat the checklist as canonical for "what to do" and consult the design doc for "why" when a decision is ambiguous.

---

## Locked decisions baked into this checklist

- Cap = 5 verified emails per anchor.
- Wizard directory: `setupEmailRecovery/` → `emailVerification/`. Shared views directory renames similarly to drop "recovery".
- Candid evolution: additive new methods + legacy facades. Both live through Phases 1a-1b; legacy removed in 1c.
- Storage: batched migration (1a lazy + 1b sweep + 1c cleanup). No big-bang `post_upgrade` rewrite.
- Pending intent: FE-call path (commit with `is_recovery: false`; recovery-flow caller invokes `verified_email_set_recovery` after polling sees success). Flagged "still open" in the doc — implementation can proceed on this path and revisit only if it turns out to be wrong.
- Subject prefix: `NONCE_PREFIX` flips to `II-Verify-`; `find_nonce_in` accepts both prefixes permanently.
- No per-dapp pin; no global "don't share" toggle; no persistent retraction mechanism.
- `last_shared_email_scope` lives on the anchor (canister-side), travels across devices.

---

## Phase 1a — New schema + lazy migration on touch

### 1a.A. Backend: new types

- [ ] Create [src/internet_identity/src/storage/storable/verified_email.rs](../../src/internet_identity/src/storage/storable/verified_email.rs):
  ```rust
  #[derive(Encode, Decode, Clone, Debug, Eq, PartialEq)]
  #[cbor(map)]
  pub struct StorableVerifiedEmail {
      #[n(0)] pub address: String,
      #[n(1)] pub verified_at: Timestamp,
      #[n(2)] pub last_used_for_recovery_at: Option<Timestamp>,
      #[n(3)] pub is_recovery: bool,
  }
  ```
- [ ] Add `impl From<StorableEmailRecoveryCredential> for StorableVerifiedEmail` — maps `created_at → verified_at`, `last_used → last_used_for_recovery_at`, sets `is_recovery: true`. One-way only (this is the lazy-migration synthesizer).
- [ ] Add candid mirror `VerifiedEmail` in [src/internet_identity_interface/src/internet_identity/types/email_recovery.rs](../../src/internet_identity_interface/src/internet_identity/types/email_recovery.rs) with the same shape.
- [ ] Add `impl From<StorableVerifiedEmail> for VerifiedEmail` and the reverse.

### 1a.B. Backend: dual storage on anchor

- [ ] [src/internet_identity/src/storage/storable/anchor.rs:30](../../src/internet_identity/src/storage/storable/anchor.rs:30) — add the new field alongside the legacy:
  ```rust
  #[n(N1)] pub email_recovery: Option<Vec<StorableEmailRecoveryCredential>>,  // legacy; never written by new code
  #[n(N2)] pub verified_emails: Option<Vec<StorableVerifiedEmail>>,            // new
  ```
  Pick a new `#[n(...)]` field number; do not reuse the legacy one.

### 1a.C. Backend: read/write abstraction

- [ ] Add to [src/internet_identity/src/storage/anchor.rs](../../src/internet_identity/src/storage/anchor.rs):

  ```rust
  pub fn read_verified_emails(anchor: &StorableAnchor) -> Vec<StorableVerifiedEmail> {
      if let Some(v) = &anchor.verified_emails { return v.clone(); }
      if let Some(legacy) = &anchor.email_recovery {
          return legacy.iter().cloned().map(StorableVerifiedEmail::from).collect();
      }
      Vec::new()
  }

  pub fn write_verified_emails(anchor: &mut StorableAnchor, new: Vec<StorableVerifiedEmail>) {
      anchor.verified_emails = Some(new);
      anchor.email_recovery = None;  // clear legacy
  }
  ```

- [ ] Audit every direct access to `anchor.email_recovery`. Route through `read_verified_emails` / `write_verified_emails`. Expected call sites: [smtp.rs:727](../../src/internet_identity/src/email_recovery/smtp.rs:727), [remove.rs:51](../../src/internet_identity/src/email_recovery/remove.rs:51), [prepare.rs:582](../../src/internet_identity/src/email_recovery/prepare.rs:582), candid response builders in main.rs.
- [ ] Unit test: legacy-only anchor (`verified_emails: None`, `email_recovery: Some(...)`) reads back correctly via `read_verified_emails`. First write through `write_verified_emails` clears the legacy field.

### 1a.D. Backend: subject-prefix flip + cap + new helpers

- [ ] [mod.rs:111](../../src/internet_identity/src/email_recovery/mod.rs:111) — `NONCE_PREFIX = "II-Verify-"`.
- [ ] [smtp.rs:436](../../src/internet_identity/src/email_recovery/smtp.rs:436) (`find_nonce_in`) — accept both `II-Verify-` and `II-Recovery-` permanently. Unit test asserting both parse.
- [ ] [mod.rs](../../src/internet_identity/src/email_recovery/mod.rs) — `pub const MAX_VERIFIED_EMAILS_PER_ANCHOR: usize = 5;`.
- [ ] [prepare.rs:45](../../src/internet_identity/src/email_recovery/prepare.rs:45) (`prepare_add`) — enforce cap-5 using `read_verified_emails(anchor).len()`.
- [ ] [smtp.rs:727](../../src/internet_identity/src/email_recovery/smtp.rs:727) — replace "overwrite the Vec with a single entry" with append-via-abstraction. New entries commit with `is_recovery: false`.
- [ ] New function `set_recovery(anchor, address)` (in a new `src/internet_identity/src/email_recovery/set_recovery.rs`): atomically set `is_recovery: true` on the matching entry, clear it on every other entry of the same anchor, persist via `write_verified_emails`. Reject if the address isn't found.
- [ ] [remove.rs:51](../../src/internet_identity/src/email_recovery/remove.rs:51) — route through `read_verified_emails` / `write_verified_emails`; behaviour for multi-entry follows naturally.

### 1a.E. Backend: candid surface

- [ ] [internet_identity.did:1570](../../src/internet_identity/internet_identity.did:1570) — add:

  ```
  type VerifiedEmail = record {
      address: text;
      verified_at: nat64;
      last_used_for_recovery_at: opt nat64;
      is_recovery: bool;
  };

  verified_email_prepare_add  : (IdentityNumber, EmailRecoveryDnsInput) -> (variant { Ok : EmailRecoveryChallenge; Err : EmailRecoveryError });
  verified_email_set_recovery : (IdentityNumber, text) -> (variant { Ok; Err : EmailRecoveryError });
  verified_email_remove       : (IdentityNumber, text) -> (variant { Ok; Err : EmailRecoveryError });
  list_verified_emails        : (IdentityNumber) -> (vec VerifiedEmail) query;
  ```

- [ ] [src/internet_identity/src/main.rs](../../src/internet_identity/src/main.rs) — add handlers that delegate to the email_recovery module. Keep the existing `email_recovery_credential_prepare_add` and `email_recovery_credential_remove` handlers as facades:
  - `email_recovery_credential_prepare_add` → calls `prepare_add`, then `set_recovery` on commit so the new entry comes out with `is_recovery: true` (preserves legacy semantics).
  - `email_recovery_credential_remove` → calls the underlying remove against the entry currently flagged as recovery.
- [ ] `npm run generate` after the `.did` change to refresh frontend types.

### 1a.F. Backend: integration tests

Two test files, split by concern:

- [ ] **Keep** [src/internet_identity/tests/integration/email_recovery.rs](../../src/internet_identity/tests/integration/email_recovery.rs) — this is the regression net for the legacy candid surface (`email_recovery_credential_prepare_add`, `email_recovery_credential_remove`, the recovery-as-login flow). All of it still exists through Phases 1a-1b as facades over the new storage. Update only what changes:
  - Existing assertions referencing `II-Recovery-` prefix → flip to assert `II-Verify-` for newly-issued nonces.
  - Keep at least one test exercising the dual-accept parser by feeding an inbound message with `II-Recovery-<hex>` Subject and asserting it still resolves.
  - Add a back-compat test: `email_recovery_credential_prepare_add` results in the new entry having `is_recovery: true` (via the facade calling `set_recovery` on commit).
- [ ] **New file** `src/internet_identity/tests/integration/verified_emails.rs` — covers the new feature surface, no legacy concerns:
  - Add via `verified_email_prepare_add` + SMTP flow → entry committed with `is_recovery: false`.
  - Cap-5: a 6th `verified_email_prepare_add` is rejected.
  - `verified_email_set_recovery` sets `is_recovery: true` and atomically clears it on every other entry of the same anchor.
  - `verified_email_remove` deletes a non-recovery entry without affecting any other.
  - `list_verified_emails` returns entries in their stored order.
  - **Lazy migration**: a legacy-only anchor (`verified_emails: None`, `email_recovery: Some(...)`) returns the expected synthesized entry via `list_verified_emails`. The next `verified_email_*` write clears the legacy field.
- [ ] Update DKIM test fixtures: [dkim/canonicalize.rs:254-255](../../src/internet_identity/src/dkim/canonicalize.rs:254), [types/smtp.rs:662](../../src/internet_identity_interface/src/internet_identity/types/smtp.rs:662) — flip to `II-Verify-deadbeef`. Keep one `II-Recovery-` fixture in the dual-accept tests.

### 1a.G. Frontend: rename + parameterize the wizard

- [ ] Rename [src/frontend/src/lib/components/wizards/setupEmailRecovery/](../../src/frontend/src/lib/components/wizards/setupEmailRecovery/) to `src/frontend/src/lib/components/wizards/emailVerification/`. Update import paths everywhere.
- [ ] Rename the shared views directory [src/frontend/src/lib/components/wizards/emailRecovery/shared/](../../src/frontend/src/lib/components/wizards/emailRecovery/shared/) similarly to drop "recovery" from the path (e.g. `src/frontend/src/lib/components/wizards/shared/email/`).
- [ ] Add a `markAsRecovery: boolean` prop to the wizard's top-level component.
- [ ] On polling success: if `markAsRecovery === true`, call `verified_email_set_recovery`; else the entry stays with `is_recovery: false`.
- [ ] Existing recovery-flow caller passes `markAsRecovery={true}`; new settings-panel caller passes `markAsRecovery={false}`.

### 1a.H. Frontend: new "Verified emails" settings panel

- [ ] Create `src/frontend/src/lib/components/settings/VerifiedEmailsPanel.svelte`:
  - Lists `VerifiedEmail` rows from `list_verified_emails`.
  - Per row: address, `verified_at`, "Used for recovery" radio (mutex across rows), Remove button.
  - "Add an email" button → mounts the wizard with `markAsRecovery: false`.
  - Recovery radio click → `verified_email_set_recovery`.
  - Remove button → `verified_email_remove`.
- [ ] Mount under the `/manage` route.
- [ ] **No "Don't share my email" toggle.** Sharing happens (or doesn't) per-request in the consent dialog. Settings has no global sharing control.
- [ ] Translation strings via `$t` calls; don't edit `.po` files directly.

### 1a.I. Frontend: /authorize empty-state inline flow

- [ ] [attributes.ts:674-678](../../src/frontend/src/lib/stores/channelHandlers/attributes.ts:674) — replace the silent empty-set short-circuit with an inline "Add a verified email" affordance that opens the wizard inside the authorize popup. On wizard success, the consent dialog re-evaluates sources and proceeds.

### 1a.J. Frontend: e2e tests (file split + new specs)

**Fixture rename:**

- [ ] Rename [src/frontend/tests/e2e-playwright/fixtures/emailRecovery.ts](../../src/frontend/tests/e2e-playwright/fixtures/emailRecovery.ts) → `emailVerification.ts`. The contents (waiters, nonce regex, mailbox helpers) are about the verification flow, not specifically about recovery.
- [ ] Update the nonce regex inside the renamed file from `/II-Recovery-[0-9a-f]{16}/` to `/II-(Verify|Recovery)-[0-9a-f]{16}/`. Both prefixes must still match because tests will be exercising both new flows (which emit `II-Verify-`) and dual-accept paths (which feed `II-Recovery-` inputs to assert backward compatibility).
- [ ] Sweep all imports that referenced `emailRecovery.ts` → point at `emailVerification.ts`.

**Spec split:**

- [ ] **Rename** [src/frontend/tests/e2e-playwright/routes/emailRecovery.spec.ts](../../src/frontend/tests/e2e-playwright/routes/emailRecovery.spec.ts) → `recoveryEmailLogin.spec.ts`. Scope it to **only** the recovery-as-login flow (anonymous user proves email ownership and gets a delegation — uses `recover@<domain>` mailbox). This concern is orthogonal to verified-emails-as-attribute-sources and stays roughly as-is.
- [ ] Audit the renamed spec and remove any test cases that exercise the "setup a recovery email" flow — those move to the new file below.
- [ ] **New spec** `src/frontend/tests/e2e-playwright/routes/verifiedEmails.spec.ts` — exercises the new feature surface:
  - Add a verified email via the new settings panel → entry lands with no recovery flag.
  - Promote a non-recovery entry to recovery via the row's radio → previously-recovery entry is demoted atomically.
  - Remove a verified email → it disappears from `list_verified_emails`.
  - Cap-5: attempting to add a 6th surfaces the right error.
  - Legacy-flow back-compat: launching the wizard from the existing "Add a recovery email" entry point still produces an entry with `is_recovery: true`.

### 1a.K. Boundary-node coordination

- [ ] **Out of repo** — confirm with the relay owners whether the deployed SMTP relay does any Subject-level filtering, purpose-based bucketing, or operator-doc references to "recovery email".
- [ ] If yes → coordinate the relay-side regex to accept both `II-Recovery-` and `II-Verify-` (matches the canister's parser). No grace period; both accepted permanently.
- [ ] If no → record the confirmation. Nothing to do.

### 1a.L. Documentation cleanup

- [ ] [types/email_recovery.rs:78](../../src/internet_identity_interface/src/internet_identity/types/email_recovery.rs:78) — update doc comment about nonce format.
- [ ] Sweep comments referring to "recovery email" where the broader meaning is now correct — switch to neutral wording. Leave comments that genuinely refer to the `is_recovery` role untouched.

---

## Phase 1b — Controller-driven sweep of the long tail

Ships in release N+1, after Phase 1a is stable in production.

### 1b.A. Backend: sweep mechanism

- [ ] Add `verified_email_migration_cursor: Option<AnchorNumber>` to persistent state (`None` = sweep complete).
- [ ] New internal function `migrate_anchors_batch(batch_size: usize)`:
  - From the current cursor, iterate up to `batch_size` anchors.
  - For each, call `read_verified_emails(anchor)` then `write_verified_emails(anchor, ...)` — no-op for already-migrated anchors, copies legacy → new for the rest.
  - Advance the cursor.
- [ ] **Controller-callable** (not timer-driven). Expose as a controller-only update method.
- [ ] Add query `verified_email_migration_status() -> { cursor, complete: bool, total_remaining: u64 }` so the operator knows when it's done.

### 1b.B. Backend: tests

- [ ] Integration test: anchors with legacy data, run `migrate_anchors_batch` until cursor reaches the end, confirm all anchors have `verified_emails: Some(...)` and `email_recovery: None`.
- [ ] Idempotency test: running the sweep over already-migrated anchors is a no-op.

### 1b.C. Operator runbook

- [ ] Write a short runbook (either in repo at [docs/](../) or external if that's where ops lives): pre-migration status check, invocation pattern (batch size, cadence), post-migration confirmation. Include rollback notes — the legacy field is still present until 1c, so any individual anchor can be inspected for both fields.

---

## Phase 1c — Drop legacy field

Ships in release N+2, after the sweep reports complete on prod and at least one full release cycle has passed.

### 1c.A. Backend

- [ ] Confirm `verified_email_migration_status()` reports `complete: true` on prod and has been stable for the buffer window.
- [ ] Remove `email_recovery: Option<Vec<StorableEmailRecoveryCredential>>` from `StorableAnchor`.
- [ ] Delete `StorableEmailRecoveryCredential`, its `From`/`Into` impls, and its source file [src/internet_identity/src/storage/storable/email_recovery_credential.rs](../../src/internet_identity/src/storage/storable/email_recovery_credential.rs).
- [ ] Delete the synthesis path from `read_verified_emails`; it now reads `verified_emails` directly. `write_verified_emails` no longer needs to clear the legacy field.
- [ ] Remove the legacy candid type `EmailRecoveryCredential` from [src/internet_identity_interface/src/internet_identity/types/email_recovery.rs](../../src/internet_identity_interface/src/internet_identity/types/email_recovery.rs).
- [ ] Remove the legacy candid methods `email_recovery_credential_prepare_add` and `email_recovery_credential_remove` from [internet_identity.did](../../src/internet_identity/internet_identity.did) and their handlers in main.rs.
- [ ] Remove `verified_email_migration_cursor` and `verified_email_migration_status` from state — they served their purpose.
- [ ] (Optional, cosmetic) Rename `src/internet_identity/src/email_recovery/` modules to `verified_emails/` to match the model. Defer if it adds noise to the cleanup PR.

### 1c.B. Tests

- [ ] Verify the dual-accept `II-Recovery-` parser still works (intentionally kept indefinitely). Don't drop unless you've actively decided to.

---

## Phase 2 — Verified emails as attribute sources

### 2.A. Backend

- [ ] [types/attributes.rs](../../src/internet_identity_interface/src/internet_identity/types/attributes.rs) — add `AttributeScope::Verified { address_hash: String }` variant. Implement `Display` / parse to handle `verified:<16-hex>:<name>` consistently with the existing `openid:` / `sso:` shapes.
- [ ] [attributes.rs:513](../../src/internet_identity/src/attributes.rs:513) (`list_available_attributes`) — extend the loop. After the existing `openid_credentials` walk, walk `read_verified_emails(anchor)`. For each entry, surface `verified:<H(addr)>:email` and `verified:<H(addr)>:verified_email`. Hash: SHA-256 of the lowercased address, truncated to the first 16 hex chars. **Read through the abstraction** so this works on both migrated and not-yet-migrated anchors.
- [ ] Same module, `prepare_attributes` / `prepare_icrc3_attributes` — resolve `verified:<H>:email` by finding the entry whose hash matches (also via `read_verified_emails`).
- [ ] Skip `verified:<H>:name` (no name claim from the verification flow). Mirror the existing exclusion pattern that skips `sso:<domain>:verified_email`.

### 2.B. Frontend

- [ ] [AttributeConsentView.svelte:165-173](<../../src/frontend/src/routes/(new-styling)/authorize/views/AttributeConsentView.svelte:165>) (`scopedProviderLabel`) — extend to recognise `verified:` and return a localised "Verified email" string.
- [ ] Add the "Verified email" translation string.

### 2.C. Tests

- [ ] Backend unit test: anchor with one verified email surfaces it via `list_available_attributes`; `prepare_icrc3_attributes` returns its value.
- [ ] Backend unit test: hash stability — same address always hashes to the same scope key.
- [ ] **New e2e spec** `src/frontend/tests/e2e-playwright/routes/emailAsAttribute.spec.ts`:
  - Dapp requests `email` against an anchor with one verified email → consent dialog shows the "Verified email (<address>)" row → user accepts → dapp receives the value.
  - Same with multiple verified emails → consent picker chevron expands to show each → user picks one → only that value is shared.
  - Anchor with zero sources → empty-state inline "Add a verified email" affordance appears (Phase 1a's empty-state flow), wizard completes, control returns to the consent dialog with a now-available source.

---

## Phase 3 — Smart-routing + last-used default

### 3.A. Backend

- [ ] Add `last_shared_email_scope: Option<String>` to `StorableAnchor` (new `#[n(N)]` tag).
- [ ] [attributes.rs](../../src/internet_identity/src/attributes.rs) — `prepare_icrc3_attributes` (or the equivalent commit path): when the user shares an unscoped `email` / `verified_email`, write the resolved scope into `last_shared_email_scope` on the anchor. **Deny does not update this field** — we track last _shared_, not last action.
- [ ] Expose `last_shared_email_scope` to the FE — add it to `IdentityInfo` (or whatever the FE reads at authorize time) as `opt text`.

### 3.B. Frontend

- [ ] [AttributeConsentView.svelte](<../../src/frontend/src/routes/(new-styling)/authorize/views/AttributeConsentView.svelte>) — `selections` map init (around line 130). Change `selectedIndex: 0` to resolve via:
  1. If `anchor.last_shared_email_scope` matches one of this group's option scopes → that index.
  2. Else smart-routing: current session signed in via OIDC/SSO → that source; passkey + any verified email → first such entry; else first available.
  3. Else `0`.
- [ ] No other UI changes. No "Primary" badge, no "remember this choice" affordance, no first-time priming flow, no per-dapp pin, no global "don't share" toggle.

### 3.C. Tests

- [ ] Backend unit test: sharing an unscoped `email` updates `last_shared_email_scope`. Deny-all doesn't.
- [ ] Backend unit test: stale-source — `last_shared_email_scope` points at a now-removed entry; resolution falls back to smart-routing silently.
- [ ] Frontend test: pre-selection matches `last_shared_email_scope` after a previous share.

---

## Cross-cutting before merge (every PR)

- [ ] `cargo fmt`, `cargo clippy`, `cargo test` per [backend-format](../../.identity-team-skills/skills/backend-format/SKILL.md).
- [ ] `npm run check`, `npm run lint`, `npm run test`, `npm run format`.
- [ ] `npm run generate` after every `.did` change.
- [ ] Don't commit `.po` files — the translation bot owns those.
- [ ] User-visible strings say "verify"/"verified", not "recovery" (except where they specifically refer to the recovery role).

## Suggested PR breakdown

- `feat(be,fe): verified emails as a first-class anchor primitive (lazy migration from recovery-email storage)` — Phase 1a end to end.
- `feat(be): controller-driven sweep of legacy email_recovery entries` — Phase 1b.
- `chore(be): drop legacy email_recovery field after migration sweep completes` — Phase 1c.
- `feat(be,fe): verified emails as ICRC-3 attribute sources` — Phase 2.
- `feat(be,fe): last-used default in attribute consent dialog` — Phase 3.
