use crate::email_inbound::{
    prepare::prepare_common, PendingKind, MAX_VERIFIED_EMAILS_PER_ANCHOR,
    VERIFIED_EMAIL_NONCE_PREFIX,
};
use crate::state;
use internet_identity_interface::internet_identity::types::email_challenge::{
    EmailChallenge, EmailChallengeDnsInput, EmailChallengeError,
};
use internet_identity_interface::internet_identity::types::{AnchorNumber, AuthorizationKey};

/// `now_secs` is `ic_cdk::api::time() / 1_000_000_000`, hoisted for tests.
///
/// `authorization_key` is the method that passed `check_authorization`
/// — pinned so finalize re-auths the device/OpenID/recovery credential
/// itself, not a short-lived temp-key principal.
pub async fn prepare_add_verified_email(
    anchor: AnchorNumber,
    dns_input: EmailChallengeDnsInput,
    now_secs: u64,
    authorization_key: AuthorizationKey,
) -> Result<EmailChallenge, EmailChallengeError> {
    // Cap-check at prepare so the FE can show a "limit reached"
    // notice without the user sending the magic email first.
    let current = state::anchor(anchor).verified_emails.len();
    if current >= usize::from(MAX_VERIFIED_EMAILS_PER_ANCHOR) {
        return Err(EmailChallengeError::LimitReached {
            limit: MAX_VERIFIED_EMAILS_PER_ANCHOR,
        });
    }

    prepare_common(
        dns_input,
        now_secs,
        PendingKind::VerifyEmail {
            anchor,
            authorization_key,
        },
        VERIFIED_EMAIL_NONCE_PREFIX,
    )
    .await
}
