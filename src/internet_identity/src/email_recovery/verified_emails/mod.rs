//! Verified emails — a parallel first-class anchor primitive.
//!
//! Sits next to `email_recovery` (the recovery-credential flow) and
//! reuses the same verification pipeline: SMTP gateway intake, DKIM
//! signature validation, DMARC alignment, the DNSSEC skeleton/DoH
//! path picker, the pending-challenge map. The differences are:
//!
//! - **Storage**: writes land on `Anchor::verified_emails` rather
//!   than `Anchor::email_recovery`. No reverse-address index — a
//!   verified email is only addressable through its owning anchor.
//! - **Nonce prefix**: tokens issued here begin with
//!   [`super::VERIFIED_EMAIL_NONCE_PREFIX`] (`II-Verify-`) so an
//!   inbound challenge email can never be cross-applied between the
//!   two flows. `smtp::find_nonce_in` walks both prefixes and the
//!   pending entry's `PendingKind` ensures the match resolves to the
//!   right flow.
//! - **Cap**: an anchor can hold at most
//!   [`super::MAX_VERIFIED_EMAILS_PER_ANCHOR`] verified addresses.
//!   The cap is checked at prepare time so the FE wizard can show
//!   a "limit reached" notice without round-tripping the magic
//!   email.
//!
//! Why a sibling submodule rather than a top-level crate-root
//! module: the verification primitive is genuinely shared — the
//! same SMTP handler, the same DKIM/DMARC verifier, the same DoH
//! resolver and DNSSEC skeleton path picker — and sitting under
//! `email_recovery::` keeps the shared helpers (`prepare_common`,
//! `bind_verified_email`, `format_nonce_with_prefix`) reachable
//! without re-exporting them through the crate root.

pub mod prepare;
pub mod remove;

pub use prepare::prepare_add;
pub use remove::remove;
