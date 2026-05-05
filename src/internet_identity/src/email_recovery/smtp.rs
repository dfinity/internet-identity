//! `smtp_request` dispatcher — verifies an inbound email and
//! completes the bound flow.
//!
//! The off-chain SMTP gateway forwards every inbound message via
//! `smtp_request(SmtpRequest)`. We read the recipient (`To:`) of the
//! envelope to decide what kind of action this email is meant to
//! perform:
//!
//! - `register@id.ai` — setup completion. Bind the verified
//!   `From:` address to the anchor named in the pending challenge.
//! - `recover@id.ai` — recovery completion. *(Reserved; lands in
//!   the recovery follow-up PR.)*
//!
//! Any other recipient is silently dropped (a 200-equivalent
//! response — we don't want to leak information about which mailbox
//! names the canister recognises).
//!
//! The verification pipeline:
//!
//! 1. Pull the `Subject:` header and find the canister-issued nonce
//!    (`II-Recovery-…`) inside it.
//! 2. Look up the pending challenge by nonce. Reject if missing or
//!    expired (the FE will see `Expired` on its next poll).
//! 3. For the DoH path stored on the pending challenge, async-fetch
//!    `<selector>._domainkey.<domain>` via `crate::doh::fetch_txt`,
//!    along with `_dmarc.<domain>`.
//! 4. Run `crate::dmarc::verify_email` against the fetched TXT
//!    records. This does the full DKIM signature check + DMARC
//!    alignment + `Subject` in `h=` enforcement (the `h=` check was
//!    added during the PR 5 review).
//! 5. Verify the verified `From:` matches the address claimed at
//!    prepare time. The two could diverge if the user typed the
//!    wrong address into the wizard but sent from a different one.
//! 6. Bind the credential to the anchor and flip the pending
//!    challenge to `Succeeded`.
//!
//! Failures at any step flip the pending challenge to
//! `Failed(reason)` so the FE's poll surfaces a useful error.
//! `smtp_request` itself returns `SmtpResponse::Ok` either way —
//! the gateway doesn't get a useful signal from per-message
//! "verification failed" answers, and feeding those back would let
//! it probe the canister for which nonces exist.

use super::pending::{PendingKind, PendingStatus};
use crate::email_recovery::pending;
use crate::state;
use internet_identity_interface::internet_identity::types::email_recovery::{
    EmailRecoveryCredential, EmailRecoveryError,
};
use internet_identity_interface::internet_identity::types::smtp::{
    smtp_err, validate_smtp_request, SmtpRequest, SmtpResponse, SMTP_ERR_SYNTAX_ERROR,
};
use internet_identity_interface::internet_identity::types::AnchorNumber;

/// Recipient mailbox name for the setup flow.
pub const SETUP_RECIPIENT_USER: &str = "register";
/// Recipient mailbox name for the recovery flow (reserved — handled
/// by the recovery follow-up PR).
pub const RECOVERY_RECIPIENT_USER: &str = "recover";
/// The domain on which both flows expect to receive mail. The
/// gateway operator chooses this — `id.ai` is the design-doc
/// example. Future enhancement: lift this into a deploy-arg.
pub const SETUP_RECIPIENT_DOMAIN: &str = "id.ai";

/// Whether `to` matches `<user>@<domain>` exactly, case-insensitive
/// on both halves. Defence-in-depth against a direct caller
/// constructing an SmtpRequest with `to.user="register"` but a
/// different domain to bypass recipient dispatch.
fn recipient_matches(
    to: &internet_identity_interface::internet_identity::types::smtp::SmtpAddress,
    expected_user: &str,
    expected_domain: &str,
) -> bool {
    to.user.eq_ignore_ascii_case(expected_user)
        && to.domain.eq_ignore_ascii_case(expected_domain)
}

/// Outcome the canister method in `main.rs` returns to the gateway.
/// We only ever return `Ok` for verification results — see the
/// module-level note on why we don't surface verification failures.
pub async fn handle_smtp_request(request: SmtpRequest) -> SmtpResponse {
    // Bound-check up front so a malformed gateway-side payload
    // returns a clean syntax error instead of trapping somewhere
    // inside the verifier.
    if let Err(e) = validate_smtp_request(&request) {
        return e;
    }

    // Recipient dispatch. Setup is the only flow wired up in this
    // PR; recovery is reserved (and silently no-ops, see module
    // note). We match on the *full* recipient address (user +
    // domain), case-insensitively, so a direct caller can't bypass
    // dispatch by spoofing just the user-part with a different
    // domain — `smtp_request` is an open update.
    let envelope = match request.envelope.as_ref() {
        Some(e) => e,
        None => return smtp_err(SMTP_ERR_SYNTAX_ERROR, "Missing envelope"),
    };
    if !recipient_matches(&envelope.to, SETUP_RECIPIENT_USER, SETUP_RECIPIENT_DOMAIN) {
        // Drop with Ok — we don't emit a per-recipient signal back
        // to the gateway.
        return SmtpResponse::Ok {};
    }

    let message = match request.message.as_ref() {
        Some(m) => m,
        None => return SmtpResponse::Ok {},
    };

    // Extract the canister-issued nonce from the Subject header. If
    // there's no Subject, no II-Recovery- prefix, or no pending
    // challenge for the discovered nonce, treat the message as not
    // for us and silently drop.
    let Some(nonce) = extract_nonce_from_subject(message) else {
        return SmtpResponse::Ok {};
    };

    let now_secs = now_secs();

    // Snapshot the pending challenge enough to drive the rest of
    // the pipeline. We borrow only briefly so the async outcalls
    // below don't hold a `RefCell` across an await (which would
    // trap inside the canister).
    let snapshot = match pending::with_mut(&nonce, now_secs, |c| match &c.kind {
        PendingKind::Register { anchor } => Some(PendingSnapshot {
            anchor: *anchor,
            claimed_address: c.claimed_address.clone(),
            selector: c.selector.clone(),
        }),
    }) {
        Some(Some(s)) => s,
        // Either nonce is unknown / expired (None), or the pending
        // entry is for a flow this PR doesn't handle yet
        // (Some(None)). Drop silently — see module note.
        _ => return SmtpResponse::Ok {},
    };

    // Run the full verification pipeline. Any failure flips the
    // challenge to `Failed(reason)` for the FE poll; we never
    // bubble the error to the gateway.
    let outcome = verify_setup_email(&request, &snapshot, now_secs).await;
    match outcome {
        Ok(()) => {
            // Write the credential to the anchor and mark the
            // challenge Succeeded.
            if let Err(e) = bind_credential_to_anchor(&snapshot, now_secs) {
                pending::with_mut(&nonce, now_secs, |c| {
                    c.status = PendingStatus::Failed(e);
                });
            } else {
                pending::with_mut(&nonce, now_secs, |c| {
                    c.status = PendingStatus::Succeeded;
                });
            }
        }
        Err(reason) => {
            pending::with_mut(&nonce, now_secs, |c| {
                c.status = PendingStatus::Failed(reason);
            });
        }
    }

    SmtpResponse::Ok {}
}

/// Snapshot of the pending challenge taken under the brief `RefCell`
/// borrow so the async verification pipeline doesn't have to hold
/// the cell across awaits.
#[derive(Clone, Debug)]
struct PendingSnapshot {
    anchor: AnchorNumber,
    /// Lowercased canonical form (matches what `prepare_add` stored).
    claimed_address: String,
    selector: String,
}

/// Look up the `Subject:` header and find a `II-Recovery-…` nonce
/// inside it. Case-insensitive on both the header name and the
/// nonce prefix; matches up to the first whitespace, comma, or
/// newline after the prefix.
///
/// Returns the nonce in **canonical** form (the prefix as the
/// canister emits it, plus the suffix lowercased), so the caller
/// can hand it directly to the pending map without re-normalising.
fn extract_nonce_from_subject(
    message: &internet_identity_interface::internet_identity::types::smtp::SmtpMessage,
) -> Option<String> {
    let subject = message
        .headers
        .iter()
        .find(|h| h.name.eq_ignore_ascii_case("Subject"))?;
    find_nonce_in(&subject.value)
}

/// Same logic as `extract_nonce_from_subject` but takes the raw
/// header value — extracted into its own function so the unit
/// tests can drive it without a full `SmtpMessage`.
fn find_nonce_in(haystack: &str) -> Option<String> {
    let prefix = super::NONCE_PREFIX;
    // Case-insensitive search: the user might paste the prefix in
    // a different case from the canister's canonical form (some
    // mail clients title-case Subject content). We match on the
    // lowercased haystack but return the canister's canonical
    // prefix + the suffix bytes from the original, lowercased.
    let lower = haystack.to_ascii_lowercase();
    let prefix_lower = prefix.to_ascii_lowercase();
    let start = lower.find(&prefix_lower)?;
    let after_prefix = start + prefix.len();
    let suffix_chars = haystack[after_prefix..].chars();
    let suffix: String = suffix_chars
        .take_while(|c| c.is_ascii_hexdigit())
        .collect::<String>()
        .to_ascii_lowercase();
    if suffix.len() != super::NONCE_SUFFIX_BYTES * 2 {
        return None;
    }
    Some(format!("{prefix}{suffix}"))
}

/// Run the full verification pipeline against a known pending
/// challenge. Returns `Ok(())` on success, or
/// `Err(EmailRecoveryError)` for a typed reason that is suitable
/// to stash on the pending challenge for the FE's poll.
async fn verify_setup_email(
    request: &SmtpRequest,
    snapshot: &PendingSnapshot,
    now_secs: u64,
) -> Result<(), EmailRecoveryError> {
    // Pull the registered domain off the claimed address so we know
    // what to fetch via DoH.
    let domain = snapshot
        .claimed_address
        .rsplit_once('@')
        .map(|(_, d)| d.to_string())
        .ok_or_else(|| {
            EmailRecoveryError::InternalCanisterError("stored claimed address has no '@'".into())
        })?;

    // Fetch DKIM TXT and DMARC TXT (best-effort) via DoH. The DoH
    // module's heap cache means repeat traffic for the same
    // selector + domain is a microsecond cache hit.
    let dkim_fqdn = format!("{}._domainkey.{}", snapshot.selector, domain);
    let dmarc_fqdn = format!("_dmarc.{}", domain);

    let dkim_bytes = crate::doh::fetch_txt(&dkim_fqdn, &domain)
        .await
        .map_err(|e| map_doh_error(e, &domain))?;
    // DMARC is optional in our model: if the lookup fails for any
    // reason (no record published, transient quorum miss), we tell
    // the verifier `None`, which forces the strict "DKIM d= must
    // equal From domain" alignment rule. Logged, but not surfaced
    // as a hard error.
    let dmarc_bytes_opt = (crate::doh::fetch_txt(&dmarc_fqdn, &domain).await).ok();

    let dkim_txt = std::str::from_utf8(&dkim_bytes)
        .map_err(|_| EmailRecoveryError::DohFetchFailed("DKIM TXT is not valid UTF-8".into()))?;
    let dmarc_txt_opt = match dmarc_bytes_opt.as_deref().map(std::str::from_utf8) {
        Some(Ok(s)) => Some(s),
        Some(Err(_)) | None => None,
    };

    // Run the combined DKIM + DMARC verifier.
    let status = crate::dmarc::verify_email(request, dkim_txt, dmarc_txt_opt, now_secs);
    match status {
        crate::dmarc::EmailVerificationStatus::Verified { .. } => {}
        crate::dmarc::EmailVerificationStatus::Unverified { reason, .. } => {
            return Err(EmailRecoveryError::EmailVerificationFailed(format!(
                "{reason:?}"
            )));
        }
    }

    // Verify the From: matches the claimed address. The verifier
    // already checks that From:'s domain aligns with DKIM's d=,
    // and that the domain matches the DMARC record — so the only
    // gap is that an attacker who controls a different mailbox at
    // the same domain could otherwise complete a victim's setup.
    // Pin the address explicitly here.
    if let Some(message) = request.message.as_ref() {
        let from = extract_from_address(message)?;
        if !from.eq_ignore_ascii_case(&snapshot.claimed_address) {
            return Err(EmailRecoveryError::AddressMismatch);
        }
    }

    Ok(())
}

/// Pull the verified `From:` address out of the message headers and
/// canonicalise it to lowercase `local@domain`. Returns
/// `AddressMismatch` rather than `MalformedFromHeader` because by
/// this point DKIM verify has already accepted the message; if the
/// From header is malformed in some way the verifier didn't catch
/// here, treating it as "doesn't match" gives the same observable
/// behaviour to the user.
fn extract_from_address(
    message: &internet_identity_interface::internet_identity::types::smtp::SmtpMessage,
) -> Result<String, EmailRecoveryError> {
    let from_header = message
        .headers
        .iter()
        .find(|h| h.name.eq_ignore_ascii_case("From"))
        .ok_or(EmailRecoveryError::AddressMismatch)?;
    let value = from_header.value.trim();
    // `From:` is RFC 5322 `address-list` in the general case, but
    // DMARC requires exactly one mailbox. The DMARC verifier already
    // enforced that, so by the time we're here the value is a
    // single mailbox in either `addr-spec` or `name-addr` form.
    let addr_spec = if let Some(start) = value.rfind('<') {
        let end = value
            .rfind('>')
            .ok_or(EmailRecoveryError::AddressMismatch)?;
        if end <= start + 1 {
            return Err(EmailRecoveryError::AddressMismatch);
        }
        &value[start + 1..end]
    } else {
        value
    };
    let (local, domain) = addr_spec
        .split_once('@')
        .ok_or(EmailRecoveryError::AddressMismatch)?;
    Ok(format!(
        "{}@{}",
        local.to_ascii_lowercase(),
        domain.to_ascii_lowercase()
    ))
}

/// Map a `DohError` to the appropriate `EmailRecoveryError` for the
/// FE poll. Configuration-level failures (allowlist miss, missing
/// `DohConfig`) become `DomainNotAllowlisted` so the FE shows
/// "operator hasn't enabled this domain"; transport-level failures
/// (quorum miss, all providers down, malformed responses) become
/// `DohFetchFailed` so the FE shows "transient error, try again";
/// caller-bug variants (`InvalidName`, `NameOutsideRegisteredDomain`)
/// surface as `InternalCanisterError` because they shouldn't reach
/// here in practice (`prepare_add` already validates the inputs).
fn map_doh_error(err: crate::doh::DohError, domain: &str) -> EmailRecoveryError {
    use crate::doh::DohError;
    match err {
        DohError::DomainNotAllowed | DohError::NotConfigured => {
            EmailRecoveryError::DomainNotAllowlisted(domain.to_string())
        }
        DohError::AllProvidersFailed => {
            EmailRecoveryError::DohFetchFailed("all DoH providers failed".into())
        }
        DohError::QuorumFailed { agreeing, total } => EmailRecoveryError::DohFetchFailed(format!(
            "DoH quorum failed: {agreeing} of {total} providers agreed",
        )),
        DohError::ResponseMalformed(msg) => {
            EmailRecoveryError::DohFetchFailed(format!("DoH response malformed: {msg}"))
        }
        DohError::InvalidName(msg) => EmailRecoveryError::InternalCanisterError(format!(
            "DoH rejected query name: {msg}"
        )),
        DohError::NameOutsideRegisteredDomain {
            name,
            registered_domain,
        } => EmailRecoveryError::InternalCanisterError(format!(
            "DoH rejected name {name:?} as outside registered domain {registered_domain:?}"
        )),
    }
}

/// Write the verified credential to the anchor named in the pending
/// challenge. Inline rather than going through
/// `anchor_operation_with_authz_check` because there's no caller to
/// authenticate (this happens in response to a DKIM-verified email,
/// not a user call) and the operation type is bookkeeping-light.
fn bind_credential_to_anchor(
    snapshot: &PendingSnapshot,
    now_secs: u64,
) -> Result<(), EmailRecoveryError> {
    let mut anchor = state::anchor(snapshot.anchor);
    anchor.email_recovery = Some(EmailRecoveryCredential {
        address: snapshot.claimed_address.clone(),
        // `Timestamp` is nanoseconds since epoch (see
        // `internet_identity_interface::types::Timestamp`). We work
        // in seconds internally; convert at the boundary.
        created_at: now_secs.saturating_mul(1_000_000_000),
        last_used: None,
    });
    state::storage_borrow_mut(|storage| storage.write(anchor))
        .map_err(|e| EmailRecoveryError::InternalCanisterError(format!("write anchor: {e:?}")))?;
    Ok(())
}

#[cfg(not(test))]
fn now_secs() -> u64 {
    ic_cdk::api::time() / 1_000_000_000
}

#[cfg(test)]
fn now_secs() -> u64 {
    1_700_000_000
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extract_nonce_finds_canister_format() {
        let nonce = format!("{}{}", super::super::NONCE_PREFIX, "0123456789abcdef");
        let subject = format!("Hello world {nonce} please verify");
        assert_eq!(find_nonce_in(&subject), Some(nonce));
    }

    #[test]
    fn extract_nonce_case_insensitive_prefix() {
        let suffix = "deadbeefcafe1234";
        let subject = format!("II-RECOVERY-{suffix}"); // user-typed all-caps
        let want = format!("{}{}", super::super::NONCE_PREFIX, suffix);
        assert_eq!(find_nonce_in(&subject), Some(want));
    }

    #[test]
    fn extract_nonce_rejects_wrong_suffix_length() {
        let bad = format!("{}{}", super::super::NONCE_PREFIX, "0123"); // too short
        assert_eq!(find_nonce_in(&bad), None);
    }

    #[test]
    fn extract_nonce_returns_none_on_no_prefix() {
        assert_eq!(find_nonce_in("just some random subject"), None);
    }

    #[test]
    fn extract_from_addrspec_form() {
        use internet_identity_interface::internet_identity::types::smtp::{
            SmtpHeader, SmtpMessage,
        };
        use serde_bytes::ByteBuf;
        let msg = SmtpMessage {
            headers: vec![SmtpHeader {
                name: "From".into(),
                value: "Alice@Gmail.COM".into(),
            }],
            body: ByteBuf::new(),
        };
        assert_eq!(extract_from_address(&msg).unwrap(), "alice@gmail.com");
    }

    #[test]
    fn extract_from_name_addr_form() {
        use internet_identity_interface::internet_identity::types::smtp::{
            SmtpHeader, SmtpMessage,
        };
        use serde_bytes::ByteBuf;
        let msg = SmtpMessage {
            headers: vec![SmtpHeader {
                name: "From".into(),
                value: "\"Alice Example\" <Alice@Gmail.COM>".into(),
            }],
            body: ByteBuf::new(),
        };
        assert_eq!(extract_from_address(&msg).unwrap(), "alice@gmail.com");
    }
}
