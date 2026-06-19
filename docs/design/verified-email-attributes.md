# Verified emails on Internet Identity

**Status:** Draft
**Owner:** Mario Ruci
**Last edited:** 2026-06-17
**Companion:** [verified-email-implementation.md](verified-email-implementation.md) — the operational checklist (file paths, concrete changes, PR breakdown). This doc explains the _why_; the checklist is _what to do_.

## TL;DR

Internet Identity has a single "recovery email" slot per anchor, verified through an inbound DKIM/DMARC/DNSSEC ownership-proof flow. Dapps can request `email` / `name` / `verified_email` attributes but only from linked OpenID/SSO sources — the recovery email is invisible to that surface.

This doc proposes adding **verified emails as a new, independent first-class anchor primitive** that lives alongside the existing recovery-email slot. The recovery flow stays untouched; verified emails are a separate bucket the user can populate independently.

Four phases, each shipping as a single release:

1. **Phase 1 — Verified emails as a first-class anchor primitive.** The user can register multiple verified email addresses on their anchor through a new flow that reuses the existing inbound-DKIM verification primitive. New storage field, new candid surface, new wizard, and a narrow dashboard panel listing only `verified_emails` entries. Existing recovery flow is unchanged.
2. **Phase 1.5 — Reach page (unified emails dashboard).** Pure frontend work. The narrow panel from Phase 1 widens into a "Reach" page that surfaces emails from all sources — OIDC, SSO, and `verified_emails` — in one verified list, plus a separate "Unverified emails" section listing OIDC/SSO emails where the IdP didn't vouch for verification. Each unverified row has a "Verify" CTA that opens the Phase 1 wizard pre-filled with that address; on success the entry joins the verified list. No backend changes.
3. **Phase 2 — Verified emails as attribute sources.** Verified email entries surface in the existing ICRC-3 attribute system under a new `verified:<H(address)>:email` scope. Dapps requesting `email` against a passkey-only anchor with at least one verified email now receive a value via the consent dialog.
4. **Phase 3 — Smart-routing + last-used default.** The consent dialog pre-selects the user's last-shared choice (or a smart-routed default for first-time users), and the canister tracks it automatically.

Phases ship in order. Phase 1 stands on its own (users get value from verifying emails). Phase 1.5 is pure FE on top of Phase 1's backend and can ship independently of Phase 2/3.

## Background

### The existing ICRC-3 attribute system

Internet Identity ships an ICRC-3 attribute system that lets dapps request `email`, `name`, and `verified_email` from a signed-in user, scoped per source (`openid:<issuer>:<name>`, `sso:<domain>:<name>`) or unscoped (the canister picks any matching source). All values today come from `Anchor.openid_credentials` — OIDC- and SSO-linked credentials. The consent UI is per-request and lives in [`AttributeConsentView.svelte`](<../../src/frontend/src/routes/(new-styling)/authorize/views/AttributeConsentView.svelte>) (multi-source attributes get a chevron-expandable picker).

### The existing email-recovery flow

Internet Identity also ships an inbound-DKIM ownership-proof flow used to verify a user's recovery email: the user sends a message from the address they claim to `register@<domain>`, an off-canister SMTP relay forwards it to the canister, and the canister verifies DKIM / DMARC / DNSSEC + DoH before binding the address to the anchor. The verified address is stored in `Anchor.email_recovery`. The flow's wizards live under [src/frontend/src/lib/components/wizards/](../../src/frontend/src/lib/components/wizards/); the verification primitives live under [src/internet_identity/src/email_recovery/](../../src/internet_identity/src/email_recovery/) and [src/internet_identity/src/dkim/](../../src/internet_identity/src/dkim/) / [dmarc/](../../src/internet_identity/src/dmarc/) / [dnssec/](../../src/internet_identity/src/dnssec/) / [doh/](../../src/internet_identity/src/doh/).

**This doc does not change the recovery flow.** Verified emails are a parallel new feature that shares the verification primitive (inbound DKIM challenge) but uses separate storage, separate candid methods, a separate wizard component, a separate subject prefix, and a separate `PendingKind` variant. The two concepts coexist; a user can have any combination of recovery email and verified emails.

---

## Phase 1 — Verified emails as a first-class anchor primitive

### Concept

Each anchor can carry up to 5 verified email addresses. Each entry is just `{ address, verified_at }` — no role flags, no shareability toggle, no recovery designation. Verification reuses the existing inbound DKIM challenge flow, but with a new subject prefix and a new `PendingKind` variant so the canister can route the inbound message to the right destination after verification.

A verified email's mere existence makes it eligible to be shared with dapps. The user decides on every request, in the consent dialog, which (if any) email to share. There's no per-entry "shareable" toggle and no global "don't share" toggle.

**Verified emails are independent from the recovery email.** They can overlap (a user can verify the same address both as a recovery email and as a verified email — they go through both flows) or differ entirely. Cross-promotion ("also set as recovery" / "also add as verified") is out of scope for v1 but a plausible future enhancement.

### Storage — additive, no migration

New anchor field, sitting alongside the existing recovery-email field:

```rust
pub struct StorableAnchor {
    // ...
    pub email_recovery: Option<Vec<StorableEmailRecoveryCredential>>,  // existing — untouched
    pub verified_emails: Option<Vec<StorableVerifiedEmail>>,           // new
    // ...
}

#[derive(Encode, Decode, Clone, Debug, Eq, PartialEq)]
#[cbor(map)]
pub struct StorableVerifiedEmail {
    #[n(0)] pub address: String,
    #[n(1)] pub verified_at: Timestamp,
}
```

Pick a new `#[n(...)]` field number for `verified_emails`; do not touch the legacy `email_recovery` field. The anchor schema uses `minicbor-derive` with `#[cbor(map)]`, which is forward-compatible across optional-field additions: existing anchors decode with `verified_emails: None` and behave exactly as they do today until they add their first verified email.

No migration logic, no post-upgrade hook, no read/write abstraction. The two storage locations are entirely independent.

Cap at 5 entries per anchor (bounds stable-memory growth, suffices for any real user). Enforced at `verified_email_prepare_add` time.

### Verification flow

User initiates "Add a verified email" from the dashboard's new "Verified emails" panel or from the authorize-flow empty-state prompt. The wizard:

1. Asks for the email address.
2. Issues a challenge via `verified_email_prepare_add` — the canister registers a pending challenge with `PendingKind::VerifyEmail { anchor }` and returns a nonce with the new `II-Verify-` prefix.
3. Shows the existing send-this-email confirmation dialog. The dialog renders whatever nonce the canister returns; with the new prefix, the user sees `Subject: II-Verify-<nonce>`.
4. Polls for status.
5. On success, the entry lands in `Anchor.verified_emails`.

The inbound verification mechanism is identical to the recovery flow: same `register@<domain>` mailbox, same DKIM/DMARC/DNSSEC/DoH stack, same SMTP gateway methods (`smtp_request`, `smtp_request_validate`), same pending-challenge map mechanics. The canister disambiguates between flows via the `PendingKind` on the pending challenge after the inbound message is matched:

- `PendingKind::Register { anchor }` — existing, drives the recovery-email setup flow. Writes to `email_recovery`.
- `PendingKind::Recover { session_pk }` — existing, drives the recovery-as-login flow.
- `PendingKind::VerifyEmail { anchor }` — **new**, drives the verified-email add flow. Writes to `verified_emails`.

The subject-prefix parser (`find_nonce_in`) accepts both `II-Recovery-` and `II-Verify-` prefixes — it's just an anchor for the 16-hex nonce suffix, not a security primitive. Once the nonce matches a pending entry, dispatch is by `PendingKind`.

### Candid surface

New methods, additive:

```
type VerifiedEmail = record {
    address: text;
    verified_at: nat64;
};

verified_email_prepare_add : (IdentityNumber, EmailRecoveryDnsInput) -> (variant { Ok : EmailRecoveryChallenge; Err : EmailRecoveryError });
verified_email_remove      : (IdentityNumber, text) -> (variant { Ok; Err : EmailRecoveryError });
list_verified_emails       : (IdentityNumber) -> (vec VerifiedEmail) query;
```

Existing `email_recovery_*` candid methods stay live and unchanged. There are no facade methods, no deprecation layer, no migration window for integrators. The two surfaces are independent.

Shared inbound primitives (`smtp_request`, `smtp_request_validate`, `EmailRecoveryDnsInput`, `EmailRecoveryChallenge`, `EmailRecoveryError`) are reused as-is — they're already generic ownership-proof types; we're just adding a new consumer.

### Frontend

**New "Verified emails" panel** on the `/manage` dashboard — narrow version, ships with Phase 1. Lists only `verified_emails` entries (OIDC/SSO sources are out of scope here; Phase 1.5 widens this panel into the unified "Reach" page). Lets the user add and remove. No "set as recovery" radio (that's a recovery-flow concern, separate). No global "don't share" toggle.

**New email-verification wizard** at `src/frontend/src/lib/components/wizards/verifiedEmail/`. Parallel to the existing `setupEmailRecovery/` wizard, not a rename of it. The new wizard:

- Reuses the shared `SendConfirmationEmail` dialog (renders whatever nonce the canister returns, including the new `II-Verify-` prefix).
- Reuses the polling helper (`runEmailRecoveryPoll` — the polling pipeline is purpose-neutral once the pending entry exists).
- Calls `verified_email_prepare_add` instead of `email_recovery_credential_prepare_add`.

**/authorize empty-state inline flow.** When a dapp requests `email` and the anchor has no source at all (no OIDC, no SSO, no verified email), the consent handler today short-circuits to an empty response ([attributes.ts:674-678](../../src/frontend/src/lib/stores/channelHandlers/attributes.ts:674)). Replace with an inline "this dapp wants your email; verify one now?" affordance that opens the new wizard inside the authorize popup. On success, control returns to the consent dialog with the new entry available.

The existing recovery-flow entry point on `/manage` ("Add a recovery email" CTA, wherever it lives) is **unchanged**. Users who think of the flow as "set up account recovery" still find it in the same place and get the same UX they have today. Users who think of it as "I want to share my email with a dapp" find the new Verified emails panel.

### Copy and tone

Sharing a verified email is always **optional** — every consent surface gives the user a clear way out, and the consent dialog still fires on every authorize. But the copy on the new surfaces (the empty-state inline prompt in `/authorize`, the settings panel CTAs, and the success states of the wizard) should frame the share as a net benefit rather than as a friction the user is forced through.

**Principles:**

- Lead with what the user gets ("stay in touch with the dapp", "easier account recovery"), not what the dapp is asking for.
- Make the skip path visible without making it feel like the wrong choice.
- Never imply the user "must" verify or share — the dapp can ask again on the next authorize flow if the user skips.
- Match the existing consent dialog's neutral tone for the per-request picker (the user has already chosen to engage at that point — the dialog should be matter-of-fact, not promotional). The existing consent dialog itself stays unchanged; the principles above apply only to the new surfaces (empty-state prompt, settings panel CTAs, wizard success state).

Final wording lands with UX review.

### Verification UX

The new wizard mounts the shipped send-this-email dialog ([SendConfirmationEmail.svelte](../../src/frontend/src/lib/components/wizards/emailRecovery/shared/views/SendConfirmationEmail.svelte)). The dialog renders the canister-issued nonce verbatim, so once the prefix is `II-Verify-`, it shows up automatically:

```
TO       register@id.ai
FROM     marioruci15@gmail.com  ✓
SUBJECT  II-Verify-173036316cf99279
BODY     (anything, leave it blank)

[Open in mail app]
[I've sent the email]
```

The only user-visible difference from the recovery flow's dialog is the subject prefix.

### Phase 1 open decisions

- [ ] **Cross-promotion (v2 framing).** A user who wants the same address as both their recovery email and a shareable verified email currently has to verify it twice. Worth designing in v2: a "also set as my recovery email" affordance on the new verified-email row, and a "also add as a verified email" affordance on the recovery email management view. Both would skip re-verification since the address is already DKIM-proven in one bucket. Out of scope for v1.

---

## Phase 1.5 — Reach page (unified emails dashboard)

### Concept

The narrow Verified emails panel from Phase 1 widens into a page titled **"Reach"** with the subtitle "How apps can reach you when you sign in." The page presents the user's emails across all sources in two sections:

- **Verified emails** — emails from OIDC credentials where `email_verified: true`, SSO credentials where the IdP vouches for the address, and `verified_emails` entries from Phase 1. Rendered as one unified list, deduped by address.
- **Unverified emails** — emails from OIDC/SSO credentials where the IdP did not vouch for verification (`email_verified: false`). Each row has a "Verify" CTA that opens the Phase 1 wizard pre-filled with the address; on success, the address joins the Verified list.

This phase is purely frontend work. No new candid, no new storage, no new backend logic. It depends on Phase 1's backend but doesn't gate Phase 2 or Phase 3.

### Verified emails section

- Lists the union of `openid_credentials` (where verified), SSO credentials (where verified), and `verified_emails` entries from Phase 1.
- **Dedup by address.** Same address in multiple sources (e.g. an OIDC cred + a `verified_emails` entry the user added through DKIM) renders as one row. Source label prefers the IdP name when present; verification date uses whichever source produced it.
- **Source icon** per row: per-IdP icon (Google, Microsoft, Apple, etc.) for entries backed by an OIDC/SSO credential; a generic envelope icon for entries backed only by `verified_emails`.
- **Remove button** is only shown on rows backed exclusively by `verified_emails`. Rows backed by an IdP-issued credential don't show Remove — removing such an email means unlinking the IdP, which is a different concern handled elsewhere on `/manage`. Silently hidden, no tooltip.
- **Cap counter** "N of 5 verified emails" rendered below the list, always visible.
- **"Add an email"** CTA → mounts the wizard from Phase 1.

### Unverified emails section

- Lists `openid_credentials` and SSO credentials whose `email_verified` claim is false.
- Per row: address, source label ("Microsoft · Not verified"), source icon, "Verify" CTA.
- **Section hidden entirely** when there are no unverified entries.

### Verify-from-unverified flow

When the user clicks Verify on an unverified row:

1. The Phase 1 wizard mounts with the address pre-filled and read-only (the user can't accidentally verify a different address than the one they clicked).
2. Standard `verified_email_prepare_add` → DKIM challenge → poll.
3. On success, a new `StorableVerifiedEmail` entry lands in `Anchor.verified_emails`.
4. The dashboard re-fetches. The address now appears in both `openid_credentials` (the unverified IdP cred, untouched) and `verified_emails` (the new II-DKIM entry). Dedup shows one row in the Verified section, sourced from the IdP, with II's verification date.

The original OIDC/SSO credential is not modified — the IdP still thinks the email is unverified. But II has its own proof, so the attribute-resolution downstream of Phase 2 correctly surfaces it:

- `verified:<H(addr)>:verified_email` resolves to the address (Phase 2 reads `verified_emails`).
- `openid:<issuer>:verified_email` still doesn't resolve (the IdP claim is false).
- A dapp asking unscoped `verified_email` gets the value via the `verified:` path.

### Phase 1.5 locked decisions

- **Page name:** "Reach" (with subtitle "How apps can reach you when you sign in.").
- **Dedup rule:** one row per address. When an address is in both `openid_credentials` (or SSO) and `verified_emails`, prefer the IdP source label; the verification date is whichever source produced it most recently.
- **Source icons:** per-IdP (Google, Microsoft, Apple) for IdP-backed entries; generic envelope for `verified_emails`-only entries.
- **Remove on IdP-backed rows:** hidden, no tooltip.
- **Verify CTA on SSO rows:** behaves identically to OIDC — same DKIM wizard, same `verified_email_prepare_add` flow.
- **Cap counter:** always visible.
- **Unverified section visibility:** hidden when empty.

### Phase 1.5 open decisions

- [ ] **Counter format.** "2 of 5 verified emails" (mockup wording), "2/5", or something else. Bikeshed-easy.
- [ ] **Section subtitles.** Mockup uses "Apps can request one of these to reach you. Never shared without your consent." and "Verify one of these so apps can use it to reach you." Final wording with UX review.

---

## Phase 2 — Verified emails as attribute sources

### Scope syntax

New scope: `verified:<H(address)>:email` and `verified:<H(address)>:verified_email`.

- Scope id is SHA-256 of the lowercased address, truncated to the first 16 hex chars — short, plenty of collision resistance, doesn't leak the plaintext into the candid key.
- `name` is not supported (no name claim from the verification flow). `list_available_attributes` excludes `verified:<H>:name` — mirrors the existing exclusion of `sso:<domain>:verified_email`.

### Plumbing

**Canister:**

- Add `AttributeScope::Verified { address_hash: String }` to the scope enum in [types/attributes.rs](../../src/internet_identity_interface/src/internet_identity/types/attributes.rs).
- Extend `Anchor.list_available_attributes` ([attributes.rs:513](../../src/internet_identity/src/attributes.rs:513)) to also walk `Anchor.verified_emails`. Each entry surfaces `verified:<H(addr)>:email` and `verified:<H(addr)>:verified_email`.
- Extend `Anchor.prepare_attributes` / `prepare_icrc3_attributes` to resolve `verified:<H>:email` keys by hash lookup against `verified_emails`.
- **Do not** expose `Anchor.email_recovery` as an attribute. The recovery email stays private to the recovery flow; users who want to share their recovery address with dapps verify it separately as a verified email.
- OIDC/SSO sources unchanged.

### Frontend

No consent-dialog changes beyond a new "Verified email" source label. The shipped [`AttributeConsentView.svelte`](<../../src/frontend/src/routes/(new-styling)/authorize/views/AttributeConsentView.svelte>) + [`AttributePicker.svelte`](<../../src/frontend/src/routes/(new-styling)/authorize/views/AttributePicker.svelte>) already group multi-source unscoped requests into a chevron-expandable picker; new `verified:` options appear as additional entries automatically.

The source-label resolution lives in `scopedProviderLabel` ([AttributeConsentView.svelte:165-173](<../../src/frontend/src/routes/(new-styling)/authorize/views/AttributeConsentView.svelte:165>)) — extend to recognise `verified:` and return a localised "Verified email" string; the address is already surfaced as the value column.

### Email-attribute coverage

| Source             | `email`     | `verified_email`           |
| ------------------ | ----------- | -------------------------- |
| OIDC (e.g. Google) | ✅          | ✅                         |
| SSO (e.g. Okta)    | ✅          | ❌ (kept out, intentional) |
| Verified email     | ✅          | ✅                         |
| Recovery email     | not exposed | not exposed                |

A verified email satisfies `verified_email` because the inbound DKIM proof is the same kind of evidence OIDC's `email_verified: true` represents — both demonstrate the user controls the mailbox.

(`name` is unaffected by this design; it continues to come from OIDC/SSO sources only, exactly as today.)

### Phase 2 open decisions

- [ ] **Address-hash function and truncation length.** The new `verified:<H(address)>:email` scope identifies an entry by hash so the plaintext address doesn't appear in the candid key. Recommend SHA-256 truncated to the first 16 hex chars — short, plenty of collision resistance, doesn't bloat `list_available_attributes` responses. The hash input is the address as already stored (canonical-lowercased), so no further normalization needed.

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

Scoped requests (`openid:...:email`, `verified:<H>:email`, etc.) bypass step 1 — the dapp asked for a specific source. They go straight to the consent dialog with that source pre-selected.

### Frontend changes

Two surgical edits to the existing consent UI:

**1.** [`AttributeConsentView.svelte`](<../../src/frontend/src/routes/(new-styling)/authorize/views/AttributeConsentView.svelte>) — `selections` map init (around line 130). Default `selectedIndex` resolves via last-used → smart-routing → 0.

**2.** On Continue (the `handleContinue` callback), send the chosen attribute keys back as today, and the canister stores the resolved scope into `last_shared_email_scope` as a side effect of `prepare_icrc3_attributes`.

Nothing else changes. No new dialog modes, no "Primary" badge, no "remember this choice" affordance, no first-time priming flow.

### Phase 3 open decisions

- [ ] **Stale-source fallback.** When `last_shared_email_scope` points to a source that's no longer on the anchor (unlinked Google, removed verified email, etc.), what does the consent dialog default to? Silently fall back to smart-routing, or surface a one-time "your previous default isn't available" notice. Silent fallback is lower-friction but less transparent.
- [ ] **Cross-device persistence of `last_shared_email_scope`.** The field lives on the anchor (canister state) so it reaches every device. Alternative: per-device `localStorage` — but then a new device sees a different default than the primary device. Anchor-side is more consistent with how II treats other per-anchor state.
- [ ] **Deterministic ordering for "first available" fallback.** When smart-routing has nothing better — no last-shared source, no current-session IdP signal — it falls back to "first available source". Which one is "first": most-recently-verified, insertion order in the storage list, or alphabetical by address? Most-recently-verified is likeliest to be the email the user wants but introduces an ordering dependency on the relative timestamps of recent writes.

---

## Sequencing

| Step            | Touches                                                                                                                                                                                                                                                                           |
| --------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Phase 1 backend | new `StorableVerifiedEmail` type, new `verified_emails` field on `StorableAnchor`, new `PendingKind::VerifyEmail` variant, SMTP-dispatch arm for the new variant, `find_nonce_in` accepts `II-Verify-` alongside `II-Recovery-`, `verified_email_*` candid + `VerifiedEmail` type |
| Phase 1 FE      | new email-verification wizard (parallel to `setupEmailRecovery/`), narrow "Verified emails" settings panel, /authorize empty-state inline flow                                                                                                                                    |
| Phase 1.5 FE    | widen the panel into the "Reach" page — unified Verified/Unverified sections sourced from `openid_credentials` + `verified_emails`, dedup, source icons, cap counter, Verify-from-unverified flow                                                                                 |
| Phase 2         | `AttributeScope::Verified` variant, `list_available_attributes` + `prepare_*` extensions reading from `verified_emails`, FE source label                                                                                                                                          |
| Phase 3         | one anchor field, resolution logic, `AttributeConsentView` default-selection edit                                                                                                                                                                                                 |

---

## Out of scope

- **Push notifications.** Separate workstream — WebPush + Service Worker on the II origin. The two channels are orthogonal: email-attribute settings and push allowlists may share a settings UI but share no implementation.
- **Per-dapp pseudonymous email aliases (`<hash>@id.ai`).** Discussed as a possible future phase once verified emails exist as a primitive. Requires an MX server commitment that's a separate organisational decision. See prior art in [#3760](https://github.com/dfinity/internet-identity/pull/3760) for the inbox/postbox path the team has explored and parked.
- **Cross-attribute pinning (primary name, primary DoB, etc.).** Email-specific concerns drove this design; other attributes don't have the "multiple sources, hard to pick" problem.
- **Cross-promotion between recovery and verified emails.** A user who wants the same address in both buckets verifies twice today. Cross-promotion ("also set as recovery" on a verified-email row, "also add as verified" on the recovery email view) is a plausible v2 enhancement; both would skip re-verification since the address is already DKIM-proven.
- **Modifying the existing recovery flow.** Recovery storage, candid, wizard, and `II-Recovery-` subject prefix are all unchanged.

---

## Open decisions checklist

**Phase 1:**

- [ ] Cross-promotion v2 design (deferred).

**Phase 1.5:**

- [ ] Counter format ("2 of 5 verified emails" vs alternatives).
- [ ] Section subtitle wording (lands with UX review).

**Phase 2:**

- [ ] Address-hash function and truncation length (recommend SHA-256 first 16 hex).

**Phase 3:**

- [ ] Stale-source fallback behaviour (silent vs notice).
- [ ] Cross-device persistence of `last_shared_email_scope` (recommend anchor-side).
- [ ] Deterministic ordering for "first available" fallback.
