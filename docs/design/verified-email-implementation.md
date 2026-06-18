# Verified emails — implementation checklist

**Companion to:** [verified-email-attributes.md](verified-email-attributes.md) — the design doc, which explains the _why_. This file is the _what to do_, one phase at a time, with concrete file paths and changes.

**How to use this:**

- Items are discrete units of work with file:line citations and named function targets.
- Phases ship in order (1 → 2 → 3); each phase is a single release.
- Locked decisions are stated below — refer there before starting any sub-section that touches them.
- LLM agents implementing this should treat this checklist as canonical for "what to do" and consult the design doc for "why" when a decision is ambiguous.

---

## Locked decisions

- **Verified emails are independent from the recovery email.** Existing recovery storage, candid, wizard, and `II-Recovery-` subject prefix are untouched. Verified emails get their own storage, candid, wizard, and `II-Verify-` subject prefix.
- Cap = 5 verified emails per anchor.
- `StorableVerifiedEmail` carries only `{ address, verified_at }`. No `is_recovery` flag — recovery is a separate concept that lives in `email_recovery`.
- No migration. The new `verified_emails` field is additive; `minicbor-derive` forward-compatibility handles legacy anchors.
- Subject prefix: `find_nonce_in` accepts both `II-Recovery-` (existing) and `II-Verify-` (new) so the verified-email flow can issue nonces with the new prefix.
- `PendingKind::VerifyEmail { anchor }` is the new variant that disambiguates verified-email verification from the existing recovery flows after the SMTP gateway hands the inbound message off.
- No per-dapp pin, no global "don't share" toggle, no persistent retraction mechanism (consent dialog fires per request).
- `last_shared_email_scope` lives on the anchor (canister-side), travels across devices.
- Cross-promotion (same address as both recovery and verified) is out of scope for v1; user verifies twice.

---

## Phase 1 — Verified emails primitive

### 1.A. Backend: new types

- [ ] Create [src/internet_identity/src/storage/storable/verified_email.rs](../../src/internet_identity/src/storage/storable/verified_email.rs):
  ```rust
  #[derive(Encode, Decode, Clone, Debug, Eq, PartialEq)]
  #[cbor(map)]
  pub struct StorableVerifiedEmail {
      #[n(0)] pub address: String,
      #[n(1)] pub verified_at: Timestamp,
  }
  ```
- [ ] Add candid mirror `VerifiedEmail` in [src/internet_identity_interface/src/internet_identity/types/email_recovery.rs](../../src/internet_identity_interface/src/internet_identity/types/email_recovery.rs) (or a new sibling module if you prefer; the types module is fine):
  ```rust
  pub struct VerifiedEmail {
      pub address: String,
      pub verified_at: Timestamp,
  }
  ```
- [ ] Add `impl From<StorableVerifiedEmail> for VerifiedEmail` and the reverse.

### 1.B. Backend: anchor field (additive, no migration)

- [ ] [src/internet_identity/src/storage/storable/anchor.rs:30](../../src/internet_identity/src/storage/storable/anchor.rs:30) — add the new field alongside the existing one. The legacy `email_recovery` field stays exactly as it is.
  ```rust
  pub struct StorableAnchor {
      // ...
      #[n(N1)] pub email_recovery: Option<Vec<StorableEmailRecoveryCredential>>,   // existing — DO NOT TOUCH
      #[n(N2)] pub verified_emails: Option<Vec<StorableVerifiedEmail>>,            // new
      // ...
  }
  ```
  Pick a new `#[n(...)]` field number for `verified_emails`; do not reuse the legacy one. Existing anchors decode with `verified_emails: None` automatically — no migration logic needed.

### 1.C. Backend: PendingKind variant + SMTP dispatch

- [ ] [src/internet_identity/src/email_recovery/pending.rs:172](../../src/internet_identity/src/email_recovery/pending.rs:172) — add a new variant to `PendingKind`:
  ```rust
  pub enum PendingKind {
      Register { anchor: AnchorNumber },           // existing — recovery setup
      Recover { session_pk: SessionKey },          // existing — recovery login
      VerifyEmail { anchor: AnchorNumber },        // new — verified-email add
  }
  ```
- [ ] [src/internet_identity/src/email_recovery/smtp.rs:278](../../src/internet_identity/src/email_recovery/smtp.rs:278) — extend the dispatch match in `handle_smtp_request` to include a new arm:
  ```rust
  let kind = match (&c.kind, recipient_flow) {
      (PendingKind::Register { anchor }, RecipientFlow::Setup) => SnapshotKind::Setup { anchor: *anchor },
      (PendingKind::Recover { session_pk }, RecipientFlow::Recovery) => SnapshotKind::Recovery { session_pk: session_pk.clone() },
      (PendingKind::VerifyEmail { anchor }, RecipientFlow::Setup) => SnapshotKind::VerifyEmail { anchor: *anchor },
      _ => return None,  // recipient ↔ kind mismatch
  };
  ```
- [ ] Add `SnapshotKind::VerifyEmail { anchor }` to the snapshot enum in [smtp.rs](../../src/internet_identity/src/email_recovery/smtp.rs) and wire the rest of the verification pipeline (DKIM check, DMARC, DNSSEC/DoH) to it. The pipeline itself is identical to the Setup arm; the only difference is **where the committed entry lands**: instead of writing to `Anchor.email_recovery`, write a new `StorableVerifiedEmail` to `Anchor.verified_emails`.
- [ ] The shared inbound code (DKIM body-hash, partial-verification record, pending-status transitions, polling cadence) is reused unchanged.

### 1.D. Backend: subject prefix

- [ ] [src/internet_identity/src/email_recovery/mod.rs:111](../../src/internet_identity/src/email_recovery/mod.rs:111) — leave `NONCE_PREFIX = "II-Recovery-"` as-is (it's used by the recovery flow). Add a new constant:
  ```rust
  pub const VERIFIED_EMAIL_NONCE_PREFIX: &str = "II-Verify-";
  ```
- [ ] Update `format_nonce` ([rng.rs:73](../../src/internet_identity/src/email_recovery/rng.rs:73)) — it currently reads `NONCE_PREFIX`. Either parameterize it on which prefix to use (preferred), or fork into `format_recovery_nonce` and `format_verified_email_nonce`. The verified-email-prepare-add path uses the new prefix; the recovery-prepare-add path keeps using the old one.
- [ ] [src/internet_identity/src/email_recovery/smtp.rs:436](../../src/internet_identity/src/email_recovery/smtp.rs:436) (`find_nonce_in`) — extend to try both prefixes. Once it finds a match, the pending-entry lookup is the same for either; `PendingKind` disambiguates after that. Unit test asserting both prefixes parse.
- [ ] Add a unit test that the recovery flow still issues `II-Recovery-` nonces and the verified-email flow issues `II-Verify-`.

### 1.E. Backend: cap + helpers

- [ ] [src/internet_identity/src/email_recovery/mod.rs](../../src/internet_identity/src/email_recovery/mod.rs) — `pub const MAX_VERIFIED_EMAILS_PER_ANCHOR: usize = 5;`.
- [ ] New module `src/internet_identity/src/verified_emails/` (or extend `email_recovery/` if you prefer — the verification primitive is genuinely shared). New file `verified_emails/prepare.rs` mirroring [prepare.rs](../../src/internet_identity/src/email_recovery/prepare.rs):
  - `pub async fn prepare_add(anchor: AnchorNumber, dns_input: EmailRecoveryDnsInput, now_secs: u64) -> Result<EmailRecoveryChallenge, EmailRecoveryError>` — same structure as the recovery `prepare_add`, but stores `PendingKind::VerifyEmail { anchor }` and issues the nonce with the `II-Verify-` prefix.
  - Enforce the cap: reject if `anchor.verified_emails.as_ref().map_or(0, |v| v.len()) >= MAX_VERIFIED_EMAILS_PER_ANCHOR`.
- [ ] New `verified_emails/remove.rs`: `pub fn remove(anchor: &mut Anchor, address: &str) -> Result<Operation, RemoveError>`. Find the matching entry in `verified_emails`, remove it. Does not touch `email_recovery`.
- [ ] Commit logic on verification success (in `submit_leaf.rs` or wherever the `SnapshotKind::VerifyEmail` arm lands): append a new `StorableVerifiedEmail { address, verified_at: now }` to `Anchor.verified_emails`. Initialize the Vec if `None`.

### 1.F. Backend: candid surface

- [ ] [internet_identity.did:1570](../../src/internet_identity/internet_identity.did:1570) — add:

  ```
  type VerifiedEmail = record {
      address: text;
      verified_at: nat64;
  };

  verified_email_prepare_add : (IdentityNumber, EmailRecoveryDnsInput) -> (variant { Ok : EmailRecoveryChallenge; Err : EmailRecoveryError });
  verified_email_remove      : (IdentityNumber, text) -> (variant { Ok; Err : EmailRecoveryError });
  list_verified_emails       : (IdentityNumber) -> (vec VerifiedEmail) query;
  ```

- [ ] [src/internet_identity/src/main.rs](../../src/internet_identity/src/main.rs) — add handlers that delegate to the new module. **Do not touch the existing `email_recovery_*` handlers.**
- [ ] `npm run generate` after the `.did` change to refresh frontend types.

### 1.G. Backend: tests

- [ ] Existing [src/internet_identity/tests/integration/email_recovery.rs](../../src/internet_identity/tests/integration/email_recovery.rs) — should pass unchanged. The recovery flow is unmodified.
- [ ] **New file** `src/internet_identity/tests/integration/verified_emails.rs`:
  - Add via `verified_email_prepare_add` + completing the SMTP flow (subject `II-Verify-<hex>`, recipient `register@<domain>`) → entry committed in `Anchor.verified_emails`, `Anchor.email_recovery` unchanged.
  - Cap-5: a 6th `verified_email_prepare_add` is rejected.
  - `verified_email_remove` deletes a matching entry without touching `email_recovery`.
  - `list_verified_emails` returns entries in their stored order.
  - Same-anchor parallelism: a user can have a recovery email AND verified emails simultaneously, including the same address in both buckets (verified twice).
  - Subject-prefix dispatch: an inbound message with `II-Recovery-<hex>` Subject still hits the recovery flow (PendingKind::Register); an inbound with `II-Verify-<hex>` hits the verified-email flow (PendingKind::VerifyEmail).
- [ ] DKIM test fixtures: update one fixture in [dkim/canonicalize.rs:254-255](../../src/internet_identity/src/dkim/canonicalize.rs:254) to exercise `II-Verify-deadbeef` alongside the existing `II-Recovery-deadbeef`. Same for [types/smtp.rs:662](../../src/internet_identity_interface/src/internet_identity/types/smtp.rs:662).

### 1.H. Frontend: new email-verification wizard

- [ ] Create `src/frontend/src/lib/components/wizards/emailVerification/EmailVerificationWizard.svelte` — **parallel to**, not a rename of, [setupEmailRecovery/SetupEmailRecoveryWizard.svelte](../../src/frontend/src/lib/components/wizards/setupEmailRecovery/SetupEmailRecoveryWizard.svelte). Reuses:
  - The shared `SendConfirmationEmail` dialog from [emailRecovery/shared/views/](../../src/frontend/src/lib/components/wizards/emailRecovery/shared/views/) (renders whatever nonce the canister returns; no changes needed).
  - The polling helper `runEmailRecoveryPoll` from [emailRecovery/shared/](../../src/frontend/src/lib/components/wizards/emailRecovery/shared/) (purpose-neutral once the pending entry exists).
- [ ] Wizard flow: address input → call `verified_email_prepare_add` → show the dialog with the canister-issued nonce (`II-Verify-…`) → poll for status → success/failure view.
- [ ] **No `markAsRecovery` prop, no recovery branching.** The wizard is exclusively for adding entries to `verified_emails`. If a user wants their address as a recovery email too, they use the existing recovery flow separately.

### 1.I. Frontend: "Verified emails" settings panel

- [ ] New `src/frontend/src/lib/components/settings/VerifiedEmailsPanel.svelte`:
  - Lists `VerifiedEmail` rows from `list_verified_emails`.
  - Per row: address, `verified_at`, Remove button.
  - "Add an email" button → mounts the new wizard.
  - **No "Used for recovery" radio** (recovery is a separate concept managed in the existing recovery section of the dashboard).
  - **No global "Don't share my email" toggle.**
- [ ] Mount under the `/manage` route, alongside (not replacing) the existing recovery email section.
- [ ] Copy follows the "Copy and tone" guidance in the design doc — empty state leads with user benefit, not "add an email" as a bare CTA. Strings via `$t` calls; don't edit `.po` files directly.

### 1.J. Frontend: /authorize empty-state inline flow

- [ ] [attributes.ts:674-678](../../src/frontend/src/lib/stores/channelHandlers/attributes.ts:674) — replace the silent empty-set short-circuit with an inline "Verify an email" affordance that opens the new wizard inside the authorize popup. On wizard success, the consent dialog re-evaluates sources and proceeds.
- [ ] Copy on this prompt and on the wizard's other surfaces (settings empty state, wizard success state) follows the "Copy and tone" guidance in the design doc. Lead with user benefit; make "Skip for now" visibly available; do not imply the user must verify. The strings ship via `$t` calls and should land alongside a UX review of the proposed wording.

### 1.K. Frontend: e2e tests

- [ ] [src/frontend/tests/e2e-playwright/fixtures/emailRecovery.ts](../../src/frontend/tests/e2e-playwright/fixtures/emailRecovery.ts) — keep this file scoped to the recovery flow. Existing tests should pass unchanged.
- [ ] **New fixture** `src/frontend/tests/e2e-playwright/fixtures/emailVerification.ts`: helpers for the verified-email flow (waiters, nonce regex `/II-Verify-[0-9a-f]{16}/`, mailbox helpers — likely shares ~80% with the recovery fixture; extract genuinely shared bits into a smaller shared module if it's clean).
- [ ] **New spec** `src/frontend/tests/e2e-playwright/routes/verifiedEmails.spec.ts`:
  - Add a verified email via the new settings panel → entry appears in `list_verified_emails`.
  - Remove a verified email → it disappears.
  - Cap-5: attempting to add a 6th surfaces the right error.
  - The recovery email flow remains untouched: adding a recovery email through the existing CTA still produces an `email_recovery` entry and does NOT populate `verified_emails`.

### 1.L. Documentation cleanup

- [ ] No changes to existing recovery-related doc comments. They still describe the recovery flow accurately.
- [ ] Add module-level doc comments to the new `verified_emails/` module explaining the relationship to `email_recovery/` (sibling, shares verification primitive, disambiguated by `PendingKind`).

---

## Phase 2 — Verified emails as attribute sources

### 2.A. Backend

- [ ] [types/attributes.rs](../../src/internet_identity_interface/src/internet_identity/types/attributes.rs) — add `AttributeScope::Verified { address_hash: String }` variant. Implement `Display` / parse to handle `verified:<16-hex>:<name>` consistently with the existing `openid:` / `sso:` shapes.
- [ ] [attributes.rs:513](../../src/internet_identity/src/attributes.rs:513) (`list_available_attributes`) — extend the loop. After the existing `openid_credentials` walk, walk `anchor.verified_emails` (not `email_recovery` — recovery emails are intentionally not exposed). For each entry, surface `verified:<H(addr)>:email` and `verified:<H(addr)>:verified_email`. Hash: SHA-256 of the lowercased address, truncated to the first 16 hex chars.
- [ ] Same module, `prepare_attributes` / `prepare_icrc3_attributes` — resolve `verified:<H>:email` by finding the entry in `verified_emails` whose hash matches.
- [ ] Skip `verified:<H>:name` (no name claim from the verification flow). Mirror the existing exclusion pattern that skips `sso:<domain>:verified_email`.

### 2.B. Frontend

- [ ] [AttributeConsentView.svelte:165-173](<../../src/frontend/src/routes/(new-styling)/authorize/views/AttributeConsentView.svelte:165>) (`scopedProviderLabel`) — extend to recognise `verified:` and return a localised "Verified email" string.
- [ ] Add the "Verified email" translation string.

### 2.C. Tests

- [ ] Backend unit test: anchor with one verified email surfaces it via `list_available_attributes`; `prepare_icrc3_attributes` returns its value.
- [ ] Backend unit test: hash stability — same address always hashes to the same scope key.
- [ ] Backend unit test: the recovery email is **not** exposed via the attribute system (asserting an anchor with only a recovery email and no verified emails returns no email attributes).
- [ ] **New e2e spec** `src/frontend/tests/e2e-playwright/routes/emailAsAttribute.spec.ts`:
  - Dapp requests `email` against an anchor with one verified email → consent dialog shows the "Verified email (<address>)" row → user accepts → dapp receives the value.
  - Same with multiple verified emails → consent picker chevron expands to show each → user picks one → only that value is shared.
  - Anchor with zero sources (no OIDC, no SSO, no verified email) → empty-state inline "Verify an email" affordance appears (Phase 1's empty-state flow), wizard completes, control returns to the consent dialog with a now-available source.

---

## Phase 3 — Smart-routing + last-used default

### 3.A. Backend

- [ ] Add `last_shared_email_scope: Option<String>` to `StorableAnchor` (new `#[n(N)]` tag, additive).
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
- [ ] User-visible strings for the verified-emails feature say "verify"/"verified". Recovery-flow strings stay as they are.

## Suggested PR breakdown

- `feat(be,fe): verified emails as a new first-class anchor primitive` — Phase 1 end to end.
- `feat(be,fe): verified emails as ICRC-3 attribute sources` — Phase 2.
- `feat(be,fe): last-used default in attribute consent dialog` — Phase 3.
