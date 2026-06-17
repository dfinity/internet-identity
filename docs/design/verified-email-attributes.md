# Verified emails on Internet Identity

**Status:** Draft
**Owner:** Mario Ruci
**Last edited:** 2026-06-16
**Companion:** [verified-email-implementation.md](verified-email-implementation.md) — the operational checklist (file paths, concrete changes, PR breakdown). This doc explains the _why_; the checklist is _what to do_.

## TL;DR

Today, Internet Identity has a single "recovery email" slot per anchor, verified through an inbound DKIM/DMARC/DNSSEC ownership-proof flow. Dapps can request user attributes (`email`, `name`, `verified_email`) but only from linked OpenID or SSO sources; the recovery email is invisible to that surface.

This doc proposes promoting "verified emails" to a **first-class anchor primitive** that supersedes the current single-slot recovery model and unlocks two downstream uses:

1. **Phase 1 — Verified emails as a first-class anchor primitive.** The user can register multiple proven-ownership email addresses on their anchor. One entry at a time can be designated as the recovery email (today's single use case). The verification mechanism is the existing inbound DKIM flow, generalized. Phase 1 ships in three releases: 1a introduces the new schema and lazy-migrates on every anchor touch, 1b runs a controller-driven sweep for dormant anchors, 1c drops the legacy storage field. See "Storage model and batched migration" for the rationale.
2. **Phase 2 — Verified emails as attribute sources.** All verified email entries become candidate sources in the existing ICRC-3 attribute system under a new `verified:<H(address)>:email` scope. Dapps requesting `email` against a passkey-only anchor with at least one verified email now receive a value via the per-request consent dialog.
3. **Phase 3 — Smart-routing + last-used default.** The consent dialog pre-selects the user's last-shared choice (or a smart-routed default for first-time users), and the canister tracks it automatically. There is no explicit "primary email" or "don't share" setting — every share is an explicit click in the consent dialog, and the user's revealed preference (including denials) is what shapes future defaults.

Phases ship in this order. Phase 1a stands on its own (users get value from multiple proven emails regardless of attribute exposure). Phase 2 builds on Phase 1a's storage abstraction and can ship before 1b/1c complete. Phase 3 builds on Phase 2.

---

## Background

### The existing ICRC-3 attribute system

Source files:
Internet Identity ships an ICRC-3 attribute system that lets dapps request `email`, `name`, and `verified_email` from a signed-in user, scoped per source (`openid:<issuer>:<name>`, `sso:<domain>:<name>`) or unscoped (the canister picks any matching source). All values today come from `Anchor.openid_credentials` — OIDC- and SSO-linked credentials. The consent UI is per-request and lives in [`AttributeConsentView.svelte`](<../../src/frontend/src/routes/(new-styling)/authorize/views/AttributeConsentView.svelte>) (multi-source attributes get a chevron-expandable picker).

### The existing email-recovery flow

Internet Identity also ships an inbound-DKIM ownership-proof flow used to verify a user's recovery email: the user sends a message from the address they claim to `register@<domain>`, an off-canister SMTP relay forwards it to the canister, and the canister verifies DKIM / DMARC / DNSSEC + DoH before binding the address to the anchor. The verified address is stored in `Anchor.email_recovery` (today a `Vec` used cap-1 by convention). The verification dialog and the wizard live under [src/frontend/src/lib/components/wizards/](../../src/frontend/src/lib/components/wizards/); the verification primitives live under [src/internet_identity/src/email_recovery/](../../src/internet_identity/src/email_recovery/) and [src/internet_identity/src/dkim/](../../src/internet_identity/src/dkim/) / [dmarc/](../../src/internet_identity/src/dmarc/) / [dnssec/](../../src/internet_identity/src/dnssec/) / [doh/](../../src/internet_identity/src/doh/). The recovery email is not exposed to the attribute system.

Phase 1's implementation analysis section cites the specific constants, candid methods, and file:line references where each piece of work lands.

---

## Phase 1 — Verified emails as a first-class anchor primitive

### Concept

Each anchor can carry up to 5 verified email addresses. One entry at a time can be designated as the recovery email — that's the only per-entry flag. Verification is the existing inbound DKIM challenge flow. Today's single recovery-email slot becomes "the entry whose `is_recovery` flag is true".

A verified email's mere existence makes it eligible to be shared with dapps — there is **no per-entry "shareable" toggle and no global "don't share" toggle**. Sharing is always an explicit click in the per-request consent dialog. A user who never wants to share simply denies every time; "Deny all" is already a one-click action in the consent UI.

### Storage model and batched migration

The existing storage layout could technically be extended in-place by adding an optional `is_recovery: Option<bool>` field to `StorableEmailRecoveryCredential`. That would avoid a migration entirely. Rejected: the resulting code reads `cred.is_recovery.unwrap_or(true)` against a type literally named `EmailRecoveryCredential` even when the entry has nothing to do with recovery. Readers a year from now will be confused, and the misnaming compounds every place the type appears.

Better to do the rename properly and pay the migration cost once. The migration ships in three releases.

#### Target schema

New storable type, named to match the generalized meaning, with field renames where the legacy names had become misleading:

```rust
#[derive(Encode, Decode, Clone, Debug, Eq, PartialEq)]
#[cbor(map)]
pub struct StorableVerifiedEmail {
    #[n(0)] pub address: String,
    #[n(1)] pub verified_at: Timestamp,                   // ← was `created_at` on the legacy type
    #[n(2)] pub last_used_for_recovery_at: Option<Timestamp>,  // ← was `last_used`
    #[n(3)] pub is_recovery: bool,                        // ← non-optional; always set on write
}
```

Anchor schema carries both fields during the migration window:

```rust
pub struct StorableAnchor {
    // ...
    #[n(N1)] pub email_recovery: Option<Vec<StorableEmailRecoveryCredential>>,  // legacy; never written by new code
    #[n(N2)] pub verified_emails: Option<Vec<StorableVerifiedEmail>>,            // new; authoritative once set
    // ...
}
```

Pick a new `#[n(...)]` field number for `verified_emails`. Don't reuse the legacy field's number (`minicbor-derive`'s `#[cbor(map)]` would break stable-memory compatibility otherwise).

#### Read/write abstraction

All access goes through two helpers. New code never touches `anchor.email_recovery` or `anchor.verified_emails` directly.

```rust
pub fn read_verified_emails(anchor: &StorableAnchor) -> Vec<StorableVerifiedEmail> {
    if let Some(v) = &anchor.verified_emails {
        return v.clone();
    }
    // Lazy migration: synthesize from legacy when the new field is absent.
    if let Some(legacy) = &anchor.email_recovery {
        return legacy.iter().cloned().map(StorableVerifiedEmail::from).collect();
    }
    Vec::new()
}

pub fn write_verified_emails(anchor: &mut StorableAnchor, new: Vec<StorableVerifiedEmail>) {
    anchor.verified_emails = Some(new);
    anchor.email_recovery = None;  // clear legacy: new field is now authoritative for this anchor
}
```

The `From<StorableEmailRecoveryCredential> for StorableVerifiedEmail` impl maps `created_at → verified_at`, `last_used → last_used_for_recovery_at`, and sets `is_recovery: true` (legacy entries are recovery by definition — that's the only kind that ever got written under the old schema).

#### Why batched, not big-bang

II has many anchors. A full migration in `post_upgrade` is unsafe: the hook has a finite cycle budget, a write storm at upgrade time blocks rollback, and the operational risk is concentrated in one moment with no observability between "start" and "finish".

Batched migration is the standard pattern. The work ships across three releases:

**Phase 1a — schema lands, lazy migration on touch.** Both fields coexist on the anchor. `read_verified_emails` prefers the new field, synthesizes from legacy when absent. Every write goes through `write_verified_emails`, which sets the new field and clears the legacy one. Active anchors migrate organically as users authenticate, add devices, log in. No post-upgrade hook.

**Phase 1b — controller-driven sweep of the long tail.** A stable cursor walks anchors and migrates the ones that haven't been touched. Ships ~one release after 1a so we can verify the lazy path is working in production before sweeping. Runs in batches under operator control; not a timer (no benefit from automated cycle pressure, and the operator wants to pick the low-traffic window).

**Phase 1c — drop the legacy field.** After the sweep reports complete on prod, remove `email_recovery: Option<Vec<StorableEmailRecoveryCredential>>` and `StorableEmailRecoveryCredential` from the schema entirely. Cleanup release. Safe because every anchor has been touched.

The reverse `address-hash → AnchorNumber` index ([src/internet_identity/src/storage/storable/email_recovery_address_hash.rs](../../src/internet_identity/src/storage/storable/email_recovery_address_hash.rs)) doesn't migrate — it was always per-address, not per-purpose. Multiple addresses pointing at the same anchor is already N entries pointing at the same value.

#### Per-entry cap

`MAX_VERIFIED_EMAILS_PER_ANCHOR = 5`. Decided. Enforced at write time in both `verified_email_prepare_add` and the legacy `email_recovery_credential_prepare_add` facade. Uses `read_verified_emails(anchor).len()` so the cap is consistent across migrated and not-yet-migrated anchors.

#### `is_recovery` mutex

Exactly zero or one entry on a given anchor has `is_recovery: true`. The new `verified_email_set_recovery` call atomically sets the flag on the target entry and clears it on every other entry of the same anchor in a single mutation, then calls `write_verified_emails`.

### Candid types

New candid type `VerifiedEmail` matching the storable shape, added to [src/internet_identity_interface/src/internet_identity/types/email_recovery.rs](../../src/internet_identity_interface/src/internet_identity/types/email_recovery.rs). The legacy `EmailRecoveryCredential` candid type stays defined as long as the legacy facade methods exist (Phase 1a through 1c — at which point both go away together).

Legacy candid methods return `EmailRecoveryCredential` (unchanged wire shape, preserving back-compat). New candid methods return `VerifiedEmail`.

### Verification flow

Reuses the existing primitive verbatim: user composes an email from the address they're claiming, addressed to `register@<mailbox-domain>`, with a `Subject:` containing the canister-issued nonce. The off-canister relay forwards via `smtp_request`. The canister verifies DKIM, DMARC, and DNSSEC/DoH. On success, the entry is committed to `verified_emails` with `is_recovery: false`.

**Decoupling intent from verification.** The pending challenge proves _ownership_ of the address. Whether the new entry should be marked for recovery is set in a separate authenticated call after the entry exists. This keeps the SMTP/DKIM path as a pure ownership-proof primitive — no purpose data leaks through the verification pipeline. See "Open decisions: pending intent" below.

### Implementation analysis: reuse, generalize, fork

#### What we reuse verbatim

| Layer                                                                                | Reuse                                                                                |
| ------------------------------------------------------------------------------------ | ------------------------------------------------------------------------------------ |
| Canister-side SMTP candid methods (`smtp_request`, `smtp_request_validate`)          | unchanged — no new methods, no signature changes                                     |
| DKIM signature verification                                                          | unchanged                                                                            |
| DMARC alignment (deferred today)                                                     | unchanged                                                                            |
| DNSSEC + DoH key resolution paths                                                    | unchanged                                                                            |
| Pending-challenge map mechanics + TTL + eviction                                     | unchanged                                                                            |
| `register@<domain>` mailbox                                                          | unchanged — same user-part for adding any verified email, regardless of intended use |
| Shared `SendConfirmationEmail.svelte` dialog                                         | unchanged                                                                            |
| FE poll cadence (`runEmailRecoveryPoll`)                                             | unchanged                                                                            |
| `recover@<domain>` mailbox + login flow                                              | unchanged (login-via-email is a different feature)                                   |
| Canister-side mailbox-domain derivation (`mailbox_domains()` from `related_origins`) | unchanged — no DNS/MX/cert changes                                                   |

#### Off-canister SMTP relay (boundary node) — coordinate, may need updates

The off-canister SMTP relay that fronts the canister is a separate service (not in this repo). It receives inbound mail to `register@<domain>` / `recover@<domain>`, calls `smtp_request_validate` at `RCPT TO` time, and calls `smtp_request` after DATA. Based on the canister-side design at [smtp.rs:1-58](../../src/internet_identity/src/email_recovery/smtp.rs:1) the canister is the sole authority on whether a message belongs to a pending challenge (it parses the Subject and silently drops unknown nonces). The relay's role is envelope forwarding, not Subject filtering.

That said, we should **explicitly confirm** with the relay owners that the deployed relay does not have any of the following recovery-specific assumptions baked in:

- **Subject regex pre-filter** — e.g. only forwarding messages whose Subject matches `II-Recovery-[0-9a-f]{16}`. If such a filter exists, it must be widened to `II-(Recovery|Verify)-[0-9a-f]{16}` during the grace period, then narrowed to `II-Verify-` once the canister stops accepting the legacy prefix.
- **Operator-facing documentation / runbooks** referring to "recovery emails" as the only accepted message kind — needs neutral wording so on-call doesn't reject legitimate verified-email traffic as out-of-scope.
- **Per-message tagging** (correlation IDs, metrics labels) that classifies traffic as "recovery". Metrics should switch to neutral labels ("verified-email setup", say) or grow a second label to distinguish recovery-flagged from generic adds.
- **Rate-limit buckets keyed by intended purpose.** If the relay rate-limits "recovery-email" traffic differently from other traffic, the new generic verified-email flow should be on a separate bucket so legitimate add-an-email traffic doesn't burn the recovery rate limit (and vice versa).

If the relay is purely an envelope forwarder with no Subject-level inspection — which is what the canister-side comments imply — then **no relay-side changes are needed**. We need an explicit confirmation, not an assumption.

**Action item:** before Phase 1 lands, get a written confirmation from whoever owns the deployed SMTP relay that one of the following holds:

1. The relay does no Subject-level filtering and no purpose-based bucketing → no changes needed.
2. There are Subject-level / purpose-level hooks → list them and coordinate the corresponding update window with the canister grace period.

#### What we generalize

**1. Subject-line nonce prefix: `II-Recovery-` → `II-Verify-`**

- One-line constant change at [mod.rs:111](../../src/internet_identity/src/email_recovery/mod.rs:111).
- `format_nonce` ([rng.rs:73](../../src/internet_identity/src/email_recovery/rng.rs:73)) reads the constant, so newly-issued nonces auto-update to the new prefix.
- **`find_nonce_in` ([smtp.rs:436](../../src/internet_identity/src/email_recovery/smtp.rs:436)) accepts both prefixes indefinitely.** The prefix isn't a security primitive — it's a regex anchor for finding the 16-hex suffix, and a matching pending entry only exists if the canister itself issued it. Accepting `II-(Recovery|Verify)-<16-hex>` is exactly as secure as accepting only one. This avoids both (a) a second NNS proposal to remove the legacy prefix later and (b) the up-to-30-minute UX drop for in-flight users at upgrade time. Cost: ~5 lines of permanent code that say "we also accept the old prefix".
- Doc comments referencing the old prefix: [types/email_recovery.rs:78](../../src/internet_identity_interface/src/internet_identity/types/email_recovery.rs:78). Update.
- Test fixtures and assertions to update (cosmetic; can land alongside or later): [dkim/canonicalize.rs:254-255](../../src/internet_identity/src/dkim/canonicalize.rs:254), [types/smtp.rs:662](../../src/internet_identity_interface/src/internet_identity/types/smtp.rs:662), several assertions in [tests/integration/email_recovery.rs](../../src/internet_identity/tests/integration/email_recovery.rs). Add a unit test that asserts both prefixes still parse.
- Playwright fixtures and specs: regex `/II-Recovery-[0-9a-f]{16}/` in [tests/e2e-playwright/fixtures/emailRecovery.ts:122](../../src/frontend/tests/e2e-playwright/fixtures/emailRecovery.ts:122), [tests/e2e-playwright/routes/emailRecovery.spec.ts](../../src/frontend/tests/e2e-playwright/routes/emailRecovery.spec.ts). Update to `/II-Verify-[0-9a-f]{16}/` to match what the canister will emit going forward.

**2. `PendingKind::Register` semantics narrowed to "address-ownership proof"**

Variant signature stays the same — `Register { anchor }`. The semantic meaning changes: it no longer carries the intent to write a recovery credential. On verification success, the canister commits a `VerifiedEmail` entry with `is_recovery: false`. The FE then makes a separate authenticated call to set `is_recovery: true` if (and only if) the user launched from the recovery-flow entry point.

**3. Storage**

New `StorableVerifiedEmail` type with renamed fields (`verified_at`, `last_used_for_recovery_at`, `is_recovery: bool`). New `verified_emails` field on the anchor lives alongside the legacy `email_recovery` field during the migration window. All access through `read_verified_emails` / `write_verified_emails` helpers, which lazy-migrate on every anchor touch. See "Storage model and batched migration" above for the three-release rollout (1a lazy + 1b sweep + 1c drop legacy).

**4. Candid surface: additive**

Add new methods alongside the existing ones, preserving the legacy facade:

```
# new — verified-emails feature
type VerifiedEmail = record {
    address: text;
    verified_at: nat64;
    last_used_for_recovery_at: opt nat64;
    is_recovery: bool;
};

verified_email_prepare_add  : (IdentityNumber, EmailRecoveryDnsInput) -> (variant { Ok : EmailRecoveryChallenge; Err : EmailRecoveryError });
verified_email_set_recovery : (IdentityNumber, text) -> (variant { Ok; Err : EmailRecoveryError });  # promotes one entry; clears others
verified_email_remove       : (IdentityNumber, text) -> (variant { Ok; Err : EmailRecoveryError });
list_verified_emails        : (IdentityNumber) -> (vec VerifiedEmail) query;

# legacy — preserved for compatibility through Phase 1a–1c; thin facades that read/write
# through the same storage abstraction. Removed in Phase 1c alongside the legacy storage field.
email_recovery_credential_prepare_add  → calls prepare_add internally, then verified_email_set_recovery on commit
email_recovery_credential_remove       → calls verified_email_remove for the entry currently is_recovery
```

Login-flow methods (`email_recovery_prepare_delegation`, `_status`, `_submit_dkim_leaf`, `_resolve_via_doh`, `_get_delegation`) and gateway methods (`smtp_request*`) **are not changed**.

**5. Frontend wizards**

The `setupEmailRecovery` wizard ([SetupEmailRecoveryWizard.svelte](../../src/frontend/src/lib/components/wizards/setupEmailRecovery/SetupEmailRecoveryWizard.svelte)) is purpose-tied to recovery via its copy and its post-success flagging. Parameterize it:

```
<EmailVerificationWizard
  markAsRecovery={true}
  copy="recovery"
/>
```

The wizard's internal flow (open dialog → user sends email → poll → success/failure views) is purpose-agnostic. After success it conditionally calls `verified_email_set_recovery` only when `markAsRecovery` is true; otherwise the entry stays as a generic verified email.

Rename the directory from `setupEmailRecovery/` to `emailVerification/`. The recovery-flow entry point becomes a thin caller that mounts the generic wizard with `markAsRecovery={true}`. The shared views directory ([emailRecovery/shared/](../../src/frontend/src/lib/components/wizards/emailRecovery/shared/)) renames similarly to drop "recovery" from its path — both the add flow and the login flow share purpose-neutral verification views now.

The dialog itself ([SendConfirmationEmail.svelte](../../src/frontend/src/lib/components/wizards/emailRecovery/shared/views/SendConfirmationEmail.svelte)) renders the canister-issued nonce verbatim — once the prefix is `II-Verify-`, the dialog shows `II-Verify-…` with no FE changes.

#### What stays recovery-only

| Element                                                        | Reason                                      |
| -------------------------------------------------------------- | ------------------------------------------- |
| `RECOVERY_RECIPIENT_USER = "recover"` mailbox                  | Only used by the anonymous login flow       |
| `PendingKind::Recover { session_pk }`                          | Carries session-key state specific to login |
| `recoverWithEmail/RecoverWithEmailWizard.svelte`               | The login UX                                |
| `email_recovery_prepare_delegation` / `_get_delegation` candid | Login API                                   |

These don't change. Recovery-as-a-login-method is a separate feature from verified-emails-as-a-shareable-attribute; they share the verification primitive but diverge after it. Keeping them separate keeps the doc honest about what is and isn't being touched.

### Migration

Detailed in "Storage model and batched migration" above. Summary:

- **Phase 1a (this release).** Both `email_recovery` and `verified_emails` fields live on the anchor. All access through `read_verified_emails` / `write_verified_emails` helpers. Active anchors migrate organically on next write. No post-upgrade hook.
- **Phase 1b (release N+1).** Controller-driven sweep walks anchors that haven't been touched and migrates them in batches.
- **Phase 1c (release N+2, after sweep confirms complete).** Drop the legacy field and `StorableEmailRecoveryCredential` from the schema. Legacy candid methods (`email_recovery_credential_*`) and the legacy candid type are removed at this point too.

The surrounding pieces are also versioned:

- **Subject prefix.** Phase 1a flips `NONCE_PREFIX` to `"II-Verify-"`; `find_nonce_in` accepts both prefixes indefinitely. No grace-period removal needed (see "Subject-line nonce prefix" under "What we generalize" above).
- **Candid surface.** Legacy `email_recovery_credential_*` methods stay live through Phase 1c; they're removed alongside the legacy storage field, since they were facades around it. External integrators get one deprecation window covering both phases.

Reverse index: no migration. The `address-hash → AnchorNumber` map already supports N-addresses-per-anchor and is unchanged across all three phases.

### Verification UX (settings entry point)

Reuses the shipped dialog ([SendConfirmationEmail.svelte](../../src/frontend/src/lib/components/wizards/emailRecovery/shared/views/SendConfirmationEmail.svelte)), with one prefix update visible to the user.

Today the dialog renders:

```
TO       register@id.ai
FROM     marioruci15@gmail.com  ✓
SUBJECT  II-Recovery-173036316cf99279
BODY     (anything, leave it blank)

[Open in mail app]
[I've sent the email]
```

After Phase 1, the same dialog renders:

```
TO       register@id.ai
FROM     marioruci15@gmail.com  ✓
SUBJECT  II-Verify-173036316cf99279
BODY     (anything, leave it blank)

[Open in mail app]
[I've sent the email]
```

The dialog's title ("Verify your email") and body ("Send the email below to confirm") are already generic and need no change.

### Settings UX — "Verified emails" panel

New section on the identity management page (`/manage`).

**Content:**

- One row per verified email entry. Each row shows:
  - The email address.
  - Verification timestamp (tertiary styling).
  - A "Used for recovery" radio button (radio across all rows — only one can be selected; selecting another atomically demotes the previous one).
  - A "Remove" button.
- An "Add an email" button — opens the verification wizard. New entries land in the list with no recovery flag; the user can promote any entry to recovery later via the row's radio.
  No global "don't share" toggle. Sharing happens (or doesn't) per-request in the consent dialog, which already provides a one-click "Deny all". A user who never wants to share simply denies every time — that's the system learning their revealed preference.

**States:**

- **Empty state** (no verified emails): the panel shows the "Add an email" CTA prominently with explanatory copy ("Verified emails can be used to recover your account, and apps that ask for your email can request to receive one").
- **Single entry, marked recovery**: the row shows the recovery radio selected. No "promote to recovery" UI is needed since it's already the recovery email.
- **Multiple entries**: standard list; recovery radio is mutex across rows.

The shipped recovery-flow entry point (today's "Add a recovery email" button somewhere in `/manage`) stays as a shortcut: clicking it launches the same wizard with `is_recovery: true` queued to apply on success. Users who think of the flow as "set up account recovery" get exactly the same UX they have today; users who think of it as "add a verified email I can also share" find the new panel.

### Entry points

Two surfaces launch the verification wizard, both pointing at the same component:

**1. Dashboard.** "Add an email" in the Verified emails panel, or the legacy "Add a recovery email" shortcut.

**2. `/authorize`, conditionally.** When a dapp requests `email` and the anchor has zero verified emails (and no OIDC/SSO source either), the consent handler today short-circuits to an empty response ([attributes.ts:674-678](../../src/frontend/src/lib/stores/channelHandlers/attributes.ts:674)). Replace that short-circuit with an inline affordance: "_this dapp wants your email; verify one now?_" with an "Add a verified email" button that opens the wizard inside the authorize popup. On success, control returns to the consent dialog which now has a source to offer.

Both entry points hand the wizard the same component, the same candid call, the same SMTP flow. The only difference is the surrounding navigation.

### Phase 1 open decisions

- [ ] **Pending intent.** When a user launches the wizard from the recovery-flow entry point, the new entry needs to end up with `is_recovery: true`. Two ways to get there: (a) the FE always commits with `is_recovery: false` and then makes a follow-up `verified_email_set_recovery` call once polling sees success, or (b) the pending challenge carries the intent so the entry comes out of verification already flagged. (a) is one extra round-trip but keeps the ownership-proof pipeline purpose-agnostic; (b) is atomic but couples verification to intent.
- [ ] **Boundary-node SMTP relay coordination.** II uses an off-canister SMTP relay (run by the boundary-node team) to forward inbound verification emails to the canister. The canister-side parser looks for `II-Recovery-` in the Subject today and will look for `II-Verify-` after Phase 1a. If the relay also filters on the Subject before forwarding — a regex pre-filter, purpose-based metric labels, rate-limit buckets — it needs to be updated to accept both prefixes. We need a written confirmation from the relay owners on whether any such filtering exists, and a coordinated change if it does.
- [ ] **Phase 1b sweep mechanism.** Phase 1b is the migration step that walks dormant anchors and rewrites their storage from the legacy field to the new one. The question is how it's invoked: a controller-callable canister method (operator picks the window and batch size by hand) or an automated timer (runs on every replica without operator intervention). Controller-callable is the safer default — explicit go/no-go, lower-traffic windows, easier to pause if something goes wrong. Timer-driven would add replica-side cycle pressure for marginal benefit.
- [ ] **Phase 1b batch size.** How many anchors per sweep invocation. Drives the per-call cycle cost and the total number of operator invocations needed to clear the long tail. Tune in staging before running on prod.
- [ ] **Phase 1c timing.** Phase 1c is the cleanup release that removes the legacy `email_recovery` field, `StorableEmailRecoveryCredential`, and the legacy candid facade methods. It can only ship after Phase 1b reports the sweep complete. How long to wait between "1b complete on prod" and "1c shipped" — a buffer that gives any leftover lazy-migration corner case a chance to surface in production traffic. At least one full release cycle is the conservative answer.
- [ ] **Translation strings.** Final user-facing wording for the new strings introduced by Phase 1 and Phase 2: the Verified emails panel header, the "Used for recovery" row label, the "Add an email" CTA, the row "Remove" / confirmation copy, and the "Verified email" source label that also appears in the Phase 2 consent dialog.

---

## Phase 2 — Verified emails as attribute sources

### Scope syntax

New scope variant: `verified:<H(address)>:email` and `verified:<H(address)>:verified_email`.

- The scope id is the hash of the address (e.g. SHA-256 truncated to 16 hex). Rationale:
  - **Privacy** — the plaintext address doesn't appear in the candid key, so dapps querying `list_available_attributes` see hashes only.
  - **Stability** — remove + re-add of the same address yields the same scope id, so any user-side bookmarks / dapp-side caches keyed on scope survive.
  - **Reorderability** — the storage list can be rearranged without changing scope keys.
- `name` is **not** supported for verified emails (no name claim collected during the inbound DKIM flow). `list_available_attributes` excludes `verified:<H>:name` like it does today for `sso:<domain>:verified_email`.

### Plumbing

**Canister:**

- Add `AttributeScope::Verified { address_hash: String }` to the scope enum in [types/attributes.rs](../../src/internet_identity_interface/src/internet_identity/types/attributes.rs).
- Extend `Anchor.list_available_attributes` ([attributes.rs:513](../../src/internet_identity/src/attributes.rs:513)) to also walk the result of `read_verified_emails(anchor)` (the storage abstraction introduced in Phase 1a). Every entry surfaces `verified:<H(address)>:email` and `verified:<H(address)>:verified_email` — verified existence is the sole eligibility test, no per-entry flag. Reading through the abstraction means Phase 2 works correctly on both migrated and not-yet-migrated anchors.
- Extend `Anchor.prepare_attributes` / `prepare_icrc3_attributes` to resolve `verified:<H>:email` keys by looking up the entry whose `H(address)` matches (also via `read_verified_emails`).

**OIDC/SSO unchanged.** They continue to surface under their existing `openid:` / `sso:` scopes alongside `verified:` entries. The user's verified-email entries are an _additional_ attribute bucket, not a replacement for IdP-sourced credentials.

### Frontend

**No consent-dialog changes.** The shipped [`AttributeConsentView.svelte`](<../../src/frontend/src/routes/(new-styling)/authorize/views/AttributeConsentView.svelte>) + [`AttributePicker.svelte`](<../../src/frontend/src/routes/(new-styling)/authorize/views/AttributePicker.svelte>) already merge multi-source unscoped requests into a chevron-expandable group. New `verified:` options appear as additional entries in that group automatically.

**One label addition.** A new translation string for the "Verified email" source label so the consent picker can render "Verified email (alice@gmail.com)" rather than a raw scope key. The source-label resolution lives in `scopedProviderLabel` ([AttributeConsentView.svelte:165-173](<../../src/frontend/src/routes/(new-styling)/authorize/views/AttributeConsentView.svelte:165>)) — extend to recognise `verified:` and return a localised "Verified email" string; the address is already surfaced as the value column.

### Attribute-name coverage

| Source               | `email` | `name`             | `verified_email`           |
| -------------------- | ------- | ------------------ | -------------------------- |
| OIDC (e.g. Google)   | ✅      | ✅                 | ✅                         |
| SSO (e.g. Okta)      | ✅      | ✅                 | ❌ (kept out, intentional) |
| Verified email entry | ✅      | ❌ (no name claim) | ✅                         |

A verified email is considered to satisfy `verified_email` because the inbound DKIM proof is the same kind of evidence OIDC's `email_verified: true` represents — both demonstrate the user controls the mailbox.

### Phase 2 open decisions

- [ ] **Address-hash function and truncation length.** The new `verified:<H(address)>:email` scope identifies an entry by hash so the plaintext address doesn't appear in the candid key. Which hash, and how many bytes do we keep? Full SHA-256 is 64 hex chars and overkill; the first 16 hex (~64 bits) is enough collision resistance for per-anchor uniqueness without bloating every `list_available_attributes` response. The hash input is the address as already stored — canonical-lowercased — so no further normalization needed.

---

## Phase 3 — Smart-routing + last-used default

The consent dialog pre-selects what the user picked last time (or smart-routes if it's their first time). The user never has to declare a default — the system learns from their first authorize flow. Sharing is always an explicit click in the dialog; denying is always one click in "Deny all".

### State model

One new field on the anchor:

- `last_shared_email_scope: Option<String>` — updated by the canister whenever the user shares an email through the consent dialog. Holds the scope key (e.g. `openid:https://accounts.google.com`, `verified:<H>`). Internal — not surfaced as a settings control.

### Resolution

For unscoped `email` / `verified_email` requests:

1. The FE selects a default for the consent dialog:
   - If `last_shared_email_scope` is set and the matching source still exists → that scope's index in the picker.
   - Else apply smart-routing (current session signed in via OIDC/SSO → that source; passkey + any verified email → first such entry; else first available).
2. User reviews / changes / accepts in the picker (or denies).
3. On Continue with a non-empty selection, the canister updates `last_shared_email_scope` to whatever was shared.

"Deny all" doesn't update `last_shared_email_scope` — we track the last _shared_ choice, not the last action. So denying once doesn't change the default the next time.

Scoped requests (`openid:...:email`, `verified:<H>:email`, etc.) bypass step 1 — the dapp asked for a specific source. They go straight to the consent dialog with that source pre-selected; user-side accept/deny semantics are unchanged.

### Frontend changes

Two surgical edits to the existing consent UI:

**1.** [`AttributeConsentView.svelte`](<../../src/frontend/src/routes/(new-styling)/authorize/views/AttributeConsentView.svelte>) — `selections` map init (around line 130). Default `selectedIndex` resolves via last-used → smart-routing → 0.

**2.** On Continue (the `handleContinue` callback), send the chosen attribute keys back as today, and the canister stores the resolved scope into `last_shared_email_scope` as a side effect of `prepare_icrc3_attributes`.

Nothing else changes. No new dialog modes, no "Primary" badge, no "remember this choice" affordance, no first-time priming flow. The user never has to declare a default — the system learns from their first authorize flow.

### Phase 3 open decisions

- [ ] **Stale-source fallback.** The canister remembers the user's last-shared scope (e.g. `openid:https://accounts.google.com`) so the consent dialog can pre-select it the next time the user authorizes. If that source has since been removed from the anchor — the user unlinked Google, deleted the verified email, etc. — what does the dialog default to? Options: silently fall back to smart-routing (current session's IdP first, then any available source), or surface a one-time "your previous default isn't available anymore" notice. Silent fallback is lower-friction but less transparent.
- [ ] **Cross-device persistence of `last_shared_email_scope`.** The field is stored on the anchor (canister state), so the same value reaches every device the user signs in from. Alternative would be per-device `localStorage`, which would mean a new device sees a different default than the user's primary device. Anchor-side is more consistent with how II treats other anchor-level state.
- [ ] **Deterministic ordering for "first available" fallback.** When smart-routing has nothing better to recommend (no last-shared source, no current-session IdP signal), it picks "the first available email source". What does "first" mean — most-recently-verified, insertion order in the storage list, alphabetical by address? Most-recently-verified is likeliest to be the email the user actually wants to share but introduces an ordering dependency on the upgrade-timing of recent writes.

---

## Sequencing

| Step                                | Touches                                                                                                                                                                                                                                                                                                                                                                                                           | Ships independently?                                                                     |
| ----------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------- |
| Phase 1a backend                    | new `StorableVerifiedEmail` type, new `verified_emails` field on `StorableAnchor` alongside legacy `email_recovery`, `read_verified_emails` / `write_verified_emails` helpers, cap-5 enforcement, `verified_email_*` candid + `VerifiedEmail` type, legacy facades for `email_recovery_credential_*`, subject-prefix flip (with dual-accept parser). Lazy migration on every anchor touch — no post-upgrade hook. | Yes                                                                                      |
| Phase 1a FE                         | generic email-verification wizard, new "Verified emails" settings panel, dashboard wiring, /authorize empty-state inline flow                                                                                                                                                                                                                                                                                     | After Phase 1a BE                                                                        |
| Phase 1a boundary-node coordination | external — written confirmation or coordinated change to the SMTP relay                                                                                                                                                                                                                                                                                                                                           | Must complete before Phase 1a BE ships                                                   |
| Phase 1b backend                    | controller-callable batched sweep of anchors still on legacy storage, stable cursor, `verified_email_migration_status()` query                                                                                                                                                                                                                                                                                    | Ships ~one release after 1a; runs in production until status reports complete            |
| Phase 1c backend                    | drop legacy `email_recovery` field, `StorableEmailRecoveryCredential`, and the `email_recovery_credential_*` candid facades; remove legacy candid type. Cleanup release.                                                                                                                                                                                                                                          | After 1b sweep reports complete on prod                                                  |
| Phase 2                             | `AttributeScope::Verified` variant, `list_available_attributes` + `prepare_*` extensions reading through `read_verified_emails`, FE source label                                                                                                                                                                                                                                                                  | After Phase 1a (works on both migrated and not-yet-migrated anchors via the abstraction) |
| Phase 3                             | one anchor field, resolution logic, `AttributeConsentView` default-selection edit                                                                                                                                                                                                                                                                                                                                 | After Phase 2                                                                            |

The boundary-node coordination is the only step on the critical path that depends on a team outside this codebase. Phase 2 doesn't have to wait for 1b/1c — it reads through the storage abstraction, so unmigrated anchors are still serveable.

---

## Out of scope

- **Push notifications.** Separate workstream — WebPush + Service Worker on the II origin. The two channels are orthogonal: email-attribute settings and push allowlists may share a settings UI but share no implementation.
- **Per-dapp pseudonymous email aliases (`<hash>@id.ai`).** Discussed as a possible future phase once verified emails exist as a primitive. Requires an MX server commitment that's a separate organisational decision. See prior art in [#3760](https://github.com/dfinity/internet-identity/pull/3760) for the inbox/postbox path the team has explored and parked.
- **Cross-attribute pinning (primary name, primary DoB, etc.).** Email-specific concerns drove this design; other attributes don't have the "multiple sources, hard to pick" problem.
- **Per-dapp persistent overrides.** Considered and rejected — the Apple "Hide my email" analogy doesn't transfer because we share the user's real address, not per-app aliases. Without per-app aliases, persistent per-dapp overrides add complexity without solving a real problem.

---

## Open decisions checklist

**Phase 1:**

- [ ] **Pending intent.** When the wizard runs from the recovery entry point, the new entry needs `is_recovery: true` on commit. Two ways to land it: (a) the FE always commits with `is_recovery: false` and then makes a follow-up `verified_email_set_recovery` call once polling succeeds, or (b) the pending challenge carries the intent through the verification pipeline. (a) keeps the ownership-proof pipeline purpose-agnostic at the cost of one extra round-trip.
- [ ] **Boundary-node SMTP relay coordination.** II uses an off-canister SMTP relay (boundary-node team) to forward inbound verification emails to the canister. The canister-side Subject parser looks for `II-Recovery-` today and will look for `II-Verify-` after Phase 1a. If the relay does any Subject-level filtering, purpose-based metric labels, or rate-limit buckets keyed on the prefix, those need updating to accept both. Need a written confirmation on what (if anything) is in place.
- [ ] **Phase 1b sweep mechanism.** Phase 1b walks anchors that haven't been touched since 1a shipped and migrates them to the new storage layout in batches. Choose: a controller-callable canister method (operator picks the window and batch size by hand) or an automated timer (runs on every replica without operator intervention). Controller-callable is the safer default.
- [ ] **Phase 1b batch size.** How many anchors per sweep call. Per-call cycle cost vs. total operator invocations to clear the long tail. Tune in staging before prod.
- [ ] **Phase 1c timing.** Phase 1c is the cleanup release that removes the legacy `email_recovery` field, `StorableEmailRecoveryCredential`, and the legacy candid facade methods. It ships after Phase 1b reports the sweep complete. How long to wait between "1b complete on prod" and "1c shipped" — a buffer for any leftover lazy-migration corner case to surface. At least one full release cycle is the conservative answer.
- [ ] **Translation strings.** Final user-facing wording for the new strings introduced by Phase 1 and 2: the Verified emails panel header, the "Used for recovery" row label, the "Add an email" CTA, the row remove confirmation, and the "Verified email" source label that also appears in the Phase 2 consent dialog picker.

**Decided (Phase 1):**

- Per-anchor cap: 5 verified emails.
- Wizard directory: `setupEmailRecovery/` renames to `emailVerification/`; the shared views directory renames to drop "recovery". The recovery-flow entry point becomes a thin call site mounting the generic wizard with `markAsRecovery={true}`.
- Candid evolution: additive — new `verified_email_*` methods alongside the legacy `email_recovery_credential_*` facades, with both available through Phases 1a-1b and the legacy ones removed in 1c alongside the legacy storage field.

**Phase 3:**

- [ ] **Stale-source fallback.** When `last_shared_email_scope` points to a source that's no longer on the anchor (unlinked Google, removed verified email, etc.), what does the consent dialog default to? Silently fall back to smart-routing, or surface a one-time "your previous default isn't available" notice. Silent fallback is lower-friction but less transparent.
- [ ] **Cross-device persistence of `last_shared_email_scope`.** The field lives on the anchor (canister state) so it reaches every device. Alternative: per-device `localStorage` — but then a new device sees a different default than the primary device. Anchor-side is more consistent with how II treats other per-anchor state.
- [ ] **Deterministic ordering for "first available" fallback.** When smart-routing has nothing better — no last-shared source, no current-session IdP signal — it falls back to "first available source". Which one is "first": most-recently-verified, insertion order in the storage list, or alphabetical by address? Most-recently-verified is likeliest to be the email the user wants but introduces an ordering dependency on the relative timestamps of recent writes.
