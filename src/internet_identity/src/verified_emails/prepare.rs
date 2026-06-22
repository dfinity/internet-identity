//! `verified_email_prepare_add` — start a new verified-email flow.
//!
//! Authenticated entry point. Mirrors `email_recovery::prepare::prepare_add`
//! but parks a `PendingKind::VerifyEmail { anchor }` and issues the
//! nonce with the [`crate::email_inbound::VERIFIED_EMAIL_NONCE_PREFIX`]
//! (`II-Verify-`) prefix so an inbound challenge email cannot be
//! cross-applied between the recovery and verified-email flows.
//!
//! All input validation, address normalisation, path picking
//! (DNSSEC skeleton vs DoH allowlist), and nonce generation is
//! delegated to `email_recovery::prepare::prepare_common`. The only
//! verified-email-specific work here is the per-anchor cap check:
//! reject up front if the anchor already holds the maximum number
//! of verified addresses.

use crate::email_inbound::{
    prepare::prepare_common, PendingKind, MAX_VERIFIED_EMAILS_PER_ANCHOR,
    VERIFIED_EMAIL_NONCE_PREFIX,
};
use crate::state;
use internet_identity_interface::internet_identity::types::email_challenge::{
    EmailChallenge, EmailChallengeDnsInput, EmailChallengeError,
};
use internet_identity_interface::internet_identity::types::AnchorNumber;

/// Body of the canister method `verified_email_prepare_add(anchor, dns_input)`.
///
/// Authentication and authz checks have already been performed by
/// the canister-method layer in `main.rs` — only the anchor's owner
/// can add a verified email to it. This function sees only the
/// happy-path arguments.
///
/// `now_secs` is `ic_cdk::api::time() / 1_000_000_000`, hoisted out
/// for testability.
pub async fn prepare_add(
    anchor: AnchorNumber,
    dns_input: EmailChallengeDnsInput,
    now_secs: u64,
) -> Result<EmailChallenge, EmailChallengeError> {
    // Per-anchor cap. Checked before issuing the nonce so the FE
    // wizard can show a "limit reached" notice without the user
    // having to send a magic email first. Compared inclusively
    // because we're about to add one more.
    let current = state::anchor(anchor).verified_emails.len();
    if current >= MAX_VERIFIED_EMAILS_PER_ANCHOR {
        return Err(EmailChallengeError::InternalCanisterError(format!(
            "anchor already holds {current} verified emails — limit is {MAX_VERIFIED_EMAILS_PER_ANCHOR}",
        )));
    }

    prepare_common(
        dns_input,
        now_secs,
        PendingKind::VerifyEmail { anchor },
        VERIFIED_EMAIL_NONCE_PREFIX,
    )
    .await
}
