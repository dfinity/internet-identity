//! Verified emails — a parallel first-class anchor primitive.
//!
//! Sits at the crate root next to `email_recovery` (the recovery-
//! credential flow). The two concepts are parallel — neither owns
//! the other — and the directory layout reflects that. The verified-
//! email flow reuses the verification pipeline that lives under
//! [`crate::email_recovery`] (SMTP gateway intake, DKIM signature
//! validation, DMARC alignment, DNSSEC skeleton/DoH path picker,
//! the pending-challenge map) by calling into a few `pub(crate)`
//! helpers there. The differences:
//!
//! - **Storage**: writes land on `Anchor::verified_emails` rather
//!   than `Anchor::email_recovery`. No reverse-address index — a
//!   verified email is only addressable through its owning anchor.
//! - **Nonce prefix**: tokens issued here begin with
//!   [`crate::email_inbound::VERIFIED_EMAIL_NONCE_PREFIX`]
//!   (`II-Verify-`) so an inbound challenge email can never be
//!   cross-applied between the two flows. `smtp::find_nonce_in`
//!   walks both prefixes and the pending entry's `PendingKind`
//!   ensures the match resolves to the right flow.
//! - **Cap**: an anchor can hold at most
//!   [`crate::email_inbound::MAX_VERIFIED_EMAILS_PER_ANCHOR`]
//!   verified addresses. The cap is checked at prepare time so the
//!   FE wizard can show a "limit reached" notice without round-
//!   tripping the magic email.

pub mod prepare;
pub mod remove;

pub use prepare::prepare_add;
pub use remove::{remove, RemoveError};
