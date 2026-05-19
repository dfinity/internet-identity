//! `smtp_request` dispatcher — verifies an inbound email and either
//! finishes the bound flow (DoH path) or hands off to
//! `submit_dkim_leaf` for the second half (DNSSEC path).
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
//! Any other recipient is rejected with 550 ("No such user here").
//! The set of mailboxes this canister recognises is part of the
//! public Candid surface (anyone can read `register@<d>` /
//! `recover@<d>` off the .did), so there's no secret to hide on this
//! axis — a sender targeting an unknown mailbox is a caller error
//! and surfaces as one. (Per-pending-challenge state, by contrast,
//! is per-user and stays behind a silent drop further down.)
//!
//! Envelopes that don't carry exactly one recipient (empty `to`, or
//! multi-recipient) are rejected with 551 ("User not local") instead
//! — distinct from 550 so the gateway can tell envelope-shape
//! problems from per-recipient ones. Recovery emails never legitimately
//! address a CC/BCC alongside `register@…` / `recover@…`, so accepting
//! a multi-recipient envelope would let an attacker BCC themselves a
//! copy of the user's canister-signed challenge nonce.
//!
//! The verification pipeline forks by path:
//!
//! - **DoH path** (no DNSSEC chain cached at prepare time): fetch
//!   the DKIM TXT (and DMARC, optionally) via `crate::doh::fetch_txt`,
//!   run the full `dmarc::verify_email` pipeline against the in-flight
//!   message body, and finalize — bind the credential and flip
//!   `status` to a terminal variant. One round-trip, finishes
//!   synchronously inside this call.
//!
//! - **DNSSEC path** (skeleton chain cached at prepare time): we
//!   don't yet have the DKIM public key (the selector only becomes
//!   known once the email arrives, via the `s=` tag in the
//!   DKIM-Signature header). Parse the signature, canonicalise the
//!   body and verify `bh=`, compute the SHA-256 over the canonical
//!   signed-headers input, and stash a small partial-verification
//!   record (~500 B) on the pending challenge. The body is dropped
//!   — once `bh=` validates the body's bytes can't change without
//!   breaking the hash. Flip `status` to `NeedDkimLeaf { selector }`
//!   so the FE walks DNSSEC for that one leaf and finishes the
//!   pipeline via `email_recovery_submit_dkim_leaf`.
//!
//! Failures at any DoH-path step flip the pending challenge to
//! `Failed(reason)` so the FE's poll surfaces a useful error.
//! `smtp_request` itself returns `SmtpResponse::Ok` either way —
//! the gateway doesn't get a useful signal from per-message
//! "verification failed" answers, and feeding those back would let
//! it probe the canister for which nonces exist.

use super::pending::{PartialVerification, PendingKind, PendingStatus};
use crate::email_recovery::pending;
use crate::state;
use internet_identity_interface::internet_identity::types::email_recovery::{
    EmailRecoveryCredential, EmailRecoveryError,
};
use internet_identity_interface::internet_identity::types::smtp::{
    smtp_err, validate_smtp_request, SmtpRequest, SmtpResponse, SMTP_ERR_MAILBOX_UNAVAILABLE,
    SMTP_ERR_SYNTAX_ERROR, SMTP_ERR_USER_NOT_LOCAL,
};
use internet_identity_interface::internet_identity::types::{AnchorNumber, SessionKey};

// Recipient user-parts (`register`, `recover`) live on the parent
// `email_recovery` module so the shared mailbox-domain helpers and
// the user-facing mailbox labels can read them too. Imported here
// for the dispatch logic.
use super::{RECOVERY_RECIPIENT_USER, SETUP_RECIPIENT_USER};

/// Whether `to` matches `<expected_user>@<one of the configured
/// mailbox domains>`, case-insensitive on both halves. The set of
/// accepted domains comes from the `related_origins` deploy arg via
/// [`super::mailbox_domains`] — on prod that's typically `id.ai` +
/// the `*.icp0.io` aliases, on beta it's `beta.id.ai`. The same
/// WASM works for both deployments because the domain isn't pinned
/// in source.
///
/// Defence-in-depth against a direct caller constructing an
/// `SmtpRequest` with `to.user="register"` but a different domain
/// to bypass recipient dispatch — only domains the deploy arg
/// authorised count.
fn recipient_matches(
    to: &internet_identity_interface::internet_identity::types::smtp::SmtpAddress,
    expected_user: &str,
) -> bool {
    if !to.user.eq_ignore_ascii_case(expected_user) {
        return false;
    }
    let to_domain = to.domain.to_ascii_lowercase();
    super::mailbox_domains().iter().any(|d| d == &to_domain)
}

/// Query-mode counterpart to [`handle_smtp_request`]. The off-chain
/// SMTP gateway calls this at `RCPT TO` time — *before* it pulls
/// the message body from the sending MTA — to decide whether to
/// accept the connection at all.
///
/// Accepts an envelope iff it carries *exactly one* recipient and
/// that recipient is `register@<d>` or `recover@<d>` (case-insensitive)
/// for some `d` in [`super::mailbox_domains`] — i.e. for any host
/// listed in the `related_origins` deploy arg. On prod that's
/// typically `id.ai` plus the `*.icp0.io` aliases; on beta it's
/// `beta.id.ai`.
///
/// Two distinct rejection codes, so the gateway can tell envelope-
/// shape problems apart from per-recipient ones:
/// - **551** ("User not local") — envelope doesn't carry exactly one
///   recipient. Multi-recipient envelopes are refused even when one
///   of the recipients is ours: a legitimate recovery email only
///   ever targets `register@…` / `recover@…`, so additional CCs/BCCs
///   alongside ours can only come from a phishy forwarder trying to
///   exfiltrate the user's canister-signed challenge nonce. Empty
///   `to` is in the same bucket.
/// - **550** ("Mailbox unavailable" / "No such user here") — single
///   recipient, but not one of our reserved mailboxes.
///
/// The query is open — anyone can call it — but it has no side
/// effects and leaks nothing beyond the deploy arg, which is already
/// public.
pub fn handle_smtp_request_validate(request: SmtpRequest) -> SmtpResponse {
    if let Err(e) = validate_smtp_request(&request) {
        return e;
    }
    let envelope = match request.envelope.as_ref() {
        Some(e) => e,
        None => return smtp_err(SMTP_ERR_SYNTAX_ERROR, "Missing envelope"),
    };
    let to = match single_recipient(envelope) {
        Some(to) => to,
        None => {
            return smtp_err(
                SMTP_ERR_USER_NOT_LOCAL,
                "Recovery emails must have exactly one recipient",
            );
        }
    };
    if recipient_matches(to, SETUP_RECIPIENT_USER) || recipient_matches(to, RECOVERY_RECIPIENT_USER)
    {
        return SmtpResponse::Ok {};
    }
    smtp_err(
        SMTP_ERR_MAILBOX_UNAVAILABLE,
        "Recipient is not a known mailbox on this canister",
    )
}

/// Return the sole recipient of `envelope` if there is exactly one,
/// or `None` if the envelope is empty or carries more than one
/// recipient. Multi-recipient envelopes don't fit the recovery flows
/// (see [`handle_smtp_request_validate`] for the threat-model note).
fn single_recipient(
    envelope: &internet_identity_interface::internet_identity::types::smtp::SmtpEnvelope,
) -> Option<&internet_identity_interface::internet_identity::types::smtp::SmtpAddress> {
    if envelope.to.len() != 1 {
        return None;
    }
    envelope.to.first()
}

/// Outcome the canister method in `main.rs` returns to the gateway.
/// Returns `Err(551)` for an envelope that doesn't carry exactly one
/// recipient, `Err(550)` for a single-recipient envelope whose
/// recipient isn't one of our reserved mailboxes, `Err(555)` for a
/// malformed request shape, and `Ok` for *verification* outcomes —
/// see the module-level note on why we don't surface per-message
/// verification failures.
pub async fn handle_smtp_request(request: SmtpRequest) -> SmtpResponse {
    // Bound-check up front so a malformed gateway-side payload
    // returns a clean syntax error instead of trapping somewhere
    // inside the verifier.
    if let Err(e) = validate_smtp_request(&request) {
        return e;
    }

    // Recipient dispatch. We accept either of the two reserved
    // recipients (`register@id.ai` for setup, `recover@id.ai` for
    // recovery); after the pending lookup we cross-check that the
    // recipient matches the `PendingKind` of the entry, so a direct
    // caller can't trick us into running a recovery flow against a
    // setup challenge or vice versa. We match on the *full* recipient
    // address (user + domain), case-insensitively, so a direct caller
    // can't bypass dispatch by spoofing just the user-part with a
    // different domain — `smtp_request` is an open update.
    //
    // We require *exactly one* recipient on the envelope. Multi-
    // recipient envelopes addressed (in part) to a recovery mailbox
    // can only come from a phishy forwarder — a legitimate recovery
    // email never targets a CC/BCC alongside `register@…` /
    // `recover@…`, so accepting one would let an attacker exfiltrate
    // the user's canister-signed challenge nonce. Reject with 551
    // ("User not local") to mirror what `smtp_request_validate`
    // says at RCPT TO time; 550 is reserved for the single-recipient
    // "unknown mailbox" case so the gateway can tell the two apart.
    let envelope = match request.envelope.as_ref() {
        Some(e) => e,
        None => return smtp_err(SMTP_ERR_SYNTAX_ERROR, "Missing envelope"),
    };
    let to = match single_recipient(envelope) {
        Some(to) => to,
        None => {
            return smtp_err(
                SMTP_ERR_USER_NOT_LOCAL,
                "Recovery emails must have exactly one recipient",
            );
        }
    };
    let recipient_flow = if recipient_matches(to, SETUP_RECIPIENT_USER) {
        RecipientFlow::Setup
    } else if recipient_matches(to, RECOVERY_RECIPIENT_USER) {
        RecipientFlow::Recovery
    } else {
        // Unknown recipient → 550 ("No such user here"). The set of
        // mailboxes we handle is in the public Candid surface, so
        // there's no secret to hide and a sender targeting any other
        // mailbox is a caller error that deserves an SMTP-level
        // signal. Mirrors what `smtp_request_validate` returns at
        // RCPT TO time so callers that skipped validate (or hit this
        // method directly) get the same answer.
        return smtp_err(
            SMTP_ERR_MAILBOX_UNAVAILABLE,
            "Recipient is not a known mailbox on this canister",
        );
    };

    let message = match request.message.as_ref() {
        Some(m) => m,
        None => {
            return SmtpResponse::Ok {};
        }
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
    //
    // Note: this lookup *lazily evicts* the entry if it has aged
    // past `CHALLENGE_TTL_SECS` — `pending::with_mut` checks the
    // TTL before invoking the closure and drops the entry on
    // expiry. So a stale nonce surfaces as `SmtpResponse::Ok {}` /
    // status `Expired` without consuming further canister cycles,
    // and the global eviction sweep on the next `insert_with_eviction`
    // call cleans up any other expired entries.
    let snapshot = match pending::with_mut(&nonce, now_secs, |c| {
        let kind = match (&c.kind, recipient_flow) {
            (PendingKind::Register { anchor }, RecipientFlow::Setup) => {
                SnapshotKind::Setup { anchor: *anchor }
            }
            (PendingKind::Recover { session_pk }, RecipientFlow::Recovery) => {
                SnapshotKind::Recovery {
                    session_pk: session_pk.clone(),
                }
            }
            // Recipient ↔ kind mismatch. Could be a forged `to:`
            // value from a direct caller, or a benign cross-up
            // (the user copy-pasted the recovery-flow nonce into
            // an email addressed to `register@id.ai`). Either way:
            // drop silently.
            _ => return None,
        };
        Some(PendingSnapshot {
            kind,
            claimed_address: c.claimed_address.clone(),
            registered_domain: c.registered_domain.clone(),
            is_dnssec_path: c.cached_root_dnskey.is_some(),
            cached_dmarc_txt: c.cached_dmarc_txt.clone(),
            partial_set: c.partial_verification.is_some(),
            already_terminal: matches!(
                c.status,
                PendingStatus::Succeeded
                    | PendingStatus::Failed(_)
                    | PendingStatus::Expired
                    | PendingStatus::NeedDkimLeaf { .. }
            ),
        })
    }) {
        Some(Some(s)) => s,
        // Either nonce is unknown / expired (None), or the pending
        // entry's kind didn't match the recipient (Some(None)).
        // Drop silently — see module note.
        _ => return SmtpResponse::Ok {},
    };

    // Idempotency: if the pending entry already moved past
    // `Pending` (a terminal status, or `NeedDkimLeaf` set by an
    // earlier delivery of the same nonce), don't re-process. The
    // gateway sometimes redelivers; we silently treat the second
    // call as a no-op rather than risk overwriting state.
    if snapshot.already_terminal || snapshot.partial_set {
        return SmtpResponse::Ok {};
    }
    if snapshot.is_dnssec_path {
        // DNSSEC path — pre-DKIM-key verification only. Body is
        // dropped after `bh=` validates; status flips to
        // `NeedDkimLeaf { selector }` so the FE submits the leaf.
        match prepare_partial_verification(&request, &snapshot, now_secs) {
            Ok(partial) => {
                let selector = partial.selector.clone();
                pending::with_mut(&nonce, now_secs, |c| {
                    c.partial_verification = Some(partial);
                    c.status = PendingStatus::NeedDkimLeaf { selector };
                });
            }
            Err(e) => {
                pending::with_mut(&nonce, now_secs, |c| {
                    c.status = PendingStatus::Failed(e);
                });
            }
        }
    } else {
        // DoH path — the canister can fetch the DKIM TXT itself, so
        // verification finishes synchronously inside this one call.
        let outcome = verify_setup_email_doh(&request, &snapshot, now_secs).await;
        match outcome {
            Ok(()) => match &snapshot.kind {
                SnapshotKind::Setup { anchor } => {
                    if let Err(e) = bind_credential(*anchor, &snapshot.claimed_address, now_secs) {
                        pending::with_mut(&nonce, now_secs, |c| {
                            c.status = PendingStatus::Failed(e);
                        });
                    } else {
                        pending::with_mut(&nonce, now_secs, |c| {
                            c.status = PendingStatus::Succeeded;
                        });
                    }
                }
                SnapshotKind::Recovery { session_pk } => {
                    match stamp_recovery_delegation(&snapshot, session_pk).await {
                        Ok(outcome) => {
                            pending::with_mut(&nonce, now_secs, |c| {
                                c.recovery_outcome = Some(outcome);
                                c.status = PendingStatus::Succeeded;
                            });
                        }
                        Err(e) => {
                            pending::with_mut(&nonce, now_secs, |c| {
                                c.status = PendingStatus::Failed(e);
                            });
                        }
                    }
                }
            },
            Err(reason) => {
                pending::with_mut(&nonce, now_secs, |c| {
                    c.status = PendingStatus::Failed(reason);
                });
            }
        }
    }

    SmtpResponse::Ok {}
}

/// Snapshot of the pending challenge taken under the brief `RefCell`
/// borrow so the async verification pipeline doesn't have to hold
/// the cell across awaits.
#[derive(Clone, Debug)]
pub(super) struct PendingSnapshot {
    kind: SnapshotKind,
    /// Lowercased canonical form (matches what `prepare_add` stored).
    claimed_address: String,
    /// Pinned at prepare time; `submit_dkim_leaf` rejects DKIM leaves
    /// at any other zone.
    registered_domain: String,
    /// `true` when prepare took the DNSSEC path (root DNSKEY + zones
    /// map cached). Decides whether `smtp_request` finishes here or
    /// hands off to the FE for the DKIM leaf walk.
    is_dnssec_path: bool,
    /// Pre-validated DMARC TXT bytes from the DNSSEC path. `Some` means
    /// the FE included a DMARC leaf in the skeleton bundle; `None`
    /// means it didn't (we'll fall back to strict `d=` alignment at
    /// submit-leaf time on the DNSSEC path), or we're on the DoH path
    /// (DMARC fetched at email time).
    cached_dmarc_txt: Option<Vec<u8>>,
    /// `true` if the entry already has a `partial_verification`. Used
    /// only to short-circuit redelivered emails on the DNSSEC path.
    partial_set: bool,
    /// `true` if `status` is already terminal or `NeedDkimLeaf` —
    /// the gateway redelivered an email we already processed.
    already_terminal: bool,
}

#[derive(Clone, Debug)]
enum SnapshotKind {
    /// Setup-flow snapshot. The anchor was supplied by the
    /// (authenticated) caller at prepare time and is pinned.
    Setup { anchor: AnchorNumber },
    /// Recovery-flow snapshot. The anchor isn't yet known — it's
    /// resolved at submit-leaf time from the verified `From:` via
    /// the reverse address index. The cached `session_pk` is what
    /// the eventual delegation will be stamped for.
    Recovery { session_pk: SessionKey },
}

/// Recipient-half of the dispatch — paired with the entry's
/// `PendingKind` to ensure they match.
#[derive(Clone, Copy, Debug)]
enum RecipientFlow {
    /// Inbound to `register@id.ai` — bind the verified `From:`
    /// address to the anchor named in the pending challenge.
    Setup,
    /// Inbound to `recover@id.ai` — stamp a delegation seed for the
    /// session_pk cached in the pending challenge and mark the
    /// challenge `RecoveryReady`.
    Recovery,
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

// =========================================================================
// DNSSEC path: pre-DKIM-key verification — produces a PartialVerification
// record that submit_dkim_leaf later finishes.
// =========================================================================

/// Run as much of the DKIM pipeline as we can without the public key:
/// parse the signature header, validate `bh=` against the canonicalised
/// body, build the canonical signed-headers input, and SHA-256 it. The
/// resulting `PartialVerification` record is enough for
/// `submit_dkim_leaf` to finish the signature check once the FE
/// supplies the DKIM TXT.
fn prepare_partial_verification(
    request: &SmtpRequest,
    snapshot: &PendingSnapshot,
    now_secs: u64,
) -> Result<PartialVerification, EmailRecoveryError> {
    let message = request.message.as_ref().ok_or_else(|| {
        EmailRecoveryError::EmailVerificationFailed("missing message body".into())
    })?;

    // Use the *first* DKIM-Signature header. RFC 6376 permits
    // multiple — a forwarder may re-sign — but the recovery surface
    // only trusts the originating-domain signer (enforced by the `d=`
    // anchor check below), and that signature is by convention placed
    // first by every signer we've observed. If the first header is
    // unparseable or doesn't match the registered domain, we reject
    // rather than walking the rest of the list: that policy keeps the
    // attack surface small for an authentication-critical path and
    // matches what `crate::dkim::verify` does on its main entry.
    let dkim_header = message
        .headers
        .iter()
        .find(|h| h.name.eq_ignore_ascii_case("DKIM-Signature"))
        .ok_or_else(|| {
            EmailRecoveryError::EmailVerificationFailed("no DKIM-Signature header".into())
        })?;
    let sig = crate::dkim::parse_dkim_signature(&dkim_header.value).map_err(|e| {
        EmailRecoveryError::EmailVerificationFailed(format!("DKIM-Signature parse: {e}"))
    })?;

    // Reject simple/* on the header side. See design §5.2 — the
    // canister-side verifier always uses relaxed header canonicalisation;
    // simple-header signers are too rare to support.
    if sig.c_header != crate::dkim::HeaderCanon::Relaxed {
        return Err(EmailRecoveryError::EmailVerificationFailed(
            "header canonicalisation must be relaxed".into(),
        ));
    }

    // Signature-header-only tag enforcement (design §5.4). These are
    // the same checks the DoH path runs inside `crate::dkim::verify`;
    // running them here closes the DNSSEC-path parity gap surfaced by
    // the audit: without these, a sender on a DNSSEC-signed domain
    // could verify with a stale `x=` (replay window stretches to the
    // RRSIG validity, days–weeks), a future-dated `t=`, or an `h=`
    // that doesn't cover `Subject` (nonce-rewrite by a MITM). The
    // DNS-record-dependent tag checks (`i=` alignment under `t=s`,
    // and `t=y` testing-mode) run later in
    // `submit_dkim_leaf::run_submit` once the DKIM leaf has been
    // DNSSEC-verified.
    crate::dkim::check_signature_not_expired(&sig, now_secs)
        .map_err(|reason| EmailRecoveryError::EmailVerificationFailed(format!("{reason:?}")))?;
    crate::dkim::check_signature_not_from_future(&sig, now_secs)
        .map_err(|reason| EmailRecoveryError::EmailVerificationFailed(format!("{reason:?}")))?;
    // Subject must be in the signed `h=` list (design §5.4). The
    // challenge nonce lives in `Subject:`; a signature that doesn't
    // cover it would let a man-in-the-middle rewrite the nonce on a
    // legitimately-signed email. Runs before the body-hash check
    // (the order the DoH path uses too) so this fast check on
    // already-parsed tags fails first when both apply.
    crate::dkim::check_subject_signed(&sig).map_err(|_| EmailRecoveryError::SubjectNotSigned)?;
    let subject_signed = true;

    // Body hash check (`bh=`). After this passes, the body bytes
    // can't change without breaking the hash, so we drop them.
    let canonical_body = match sig.c_body {
        crate::dkim::BodyCanon::Relaxed => crate::dkim::relaxed_body(&message.body),
        crate::dkim::BodyCanon::Simple => crate::dkim::simple_body(&message.body),
    };
    let computed_bh = crate::dkim::body_hash_sha256(&canonical_body, sig.l);
    if computed_bh.as_slice() != sig.bh.as_slice() {
        return Err(EmailRecoveryError::EmailVerificationFailed(
            "computed body hash does not match bh=".into(),
        ));
    }

    // Compute the canonical signed-headers input and SHA-256 it. The
    // 32-byte digest is the input both supported algorithms verify
    // over (RSA-SHA256 via PKCS#1 v1.5 prehash, Ed25519-SHA256 via
    // RFC 8463).
    let signed_data =
        crate::dkim::build_header_hash_input(&message.headers, &sig, &dkim_header.value);
    use sha2::{Digest, Sha256};
    let mut hasher = Sha256::new();
    hasher.update(&signed_data);
    let headers_digest: [u8; 32] = hasher.finalize().into();

    // Verify the From: matches the claimed address; cache the
    // lowercased canonical form so submit_dkim_leaf doesn't re-parse
    // the message later.
    let from_address_lc = extract_from_address(message)?;
    if !from_address_lc.eq_ignore_ascii_case(&snapshot.claimed_address) {
        return Err(EmailRecoveryError::AddressMismatch);
    }

    // Reject signatures whose `d=` doesn't anchor in the registered
    // domain (or a strict subdomain of it). A divergent `d=` is
    // either a forwarder's re-signing (which we don't trust because
    // the recovery surface needs originating-domain authentication)
    // or an unrelated relay; either way submit_dkim_leaf would walk
    // the wrong zone and fail later with `DkimLeafMismatch`.
    let d = &sig.d;
    let zone = &snapshot.registered_domain;
    let d_in_zone = d.eq_ignore_ascii_case(zone)
        || (d.len() > zone.len()
            && d.to_ascii_lowercase().ends_with(&zone.to_ascii_lowercase())
            && d.as_bytes()[d.len() - zone.len() - 1] == b'.');
    if !d_in_zone {
        return Err(EmailRecoveryError::EmailVerificationFailed(format!(
            "DKIM d={d} is not within the claimed zone {zone}"
        )));
    }

    Ok(PartialVerification {
        headers_digest,
        signature: sig.b.clone(),
        selector: sig.s.clone(),
        signing_domain: sig.d.clone(),
        signing_auid: sig.i.clone(),
        algorithm: sig.algorithm,
        from_address_lc,
        subject_signed,
    })
}

// =========================================================================
// DoH path: full verification synchronously inside smtp_request.
// =========================================================================

/// Run the legacy single-pass verification pipeline against a known
/// pending challenge: fetch the DKIM TXT (and DMARC) via DoH, run
/// `dmarc::verify_email`, and confirm the From: matches the claimed
/// address. Returns `Ok(())` on success, or `Err(EmailRecoveryError)`
/// for a typed reason that's suitable to stash on the pending
/// challenge for the FE's poll.
async fn verify_setup_email_doh(
    request: &SmtpRequest,
    snapshot: &PendingSnapshot,
    now_secs: u64,
) -> Result<(), EmailRecoveryError> {
    // We need a selector to fetch the DKIM TXT, and on the DoH path
    // we don't have one cached at prepare time. Read it directly
    // from the email's DKIM-Signature header.
    let message = request.message.as_ref().ok_or_else(|| {
        EmailRecoveryError::EmailVerificationFailed("missing message body".into())
    })?;
    let dkim_header = message
        .headers
        .iter()
        .find(|h| h.name.eq_ignore_ascii_case("DKIM-Signature"))
        .ok_or_else(|| {
            EmailRecoveryError::EmailVerificationFailed("no DKIM-Signature header".into())
        })?;
    let sig = crate::dkim::parse_dkim_signature(&dkim_header.value).map_err(|e| {
        EmailRecoveryError::EmailVerificationFailed(format!("DKIM-Signature parse: {e}"))
    })?;
    let domain = snapshot
        .claimed_address
        .rsplit_once('@')
        .map(|(_, d)| d.to_string())
        .ok_or_else(|| {
            EmailRecoveryError::InternalCanisterError("stored claimed address has no '@'".into())
        })?;
    let dkim_fqdn = format!("{}._domainkey.{}", sig.s, domain);
    let dmarc_fqdn = format!("_dmarc.{}", domain);
    let dkim_bytes = crate::doh::fetch_txt(&dkim_fqdn, &domain)
        .await
        .map_err(|e| map_doh_error(e, &domain))?;
    // DMARC: a quorum of providers reporting "no record" is a valid
    // DNS state ("no policy published" per RFC 7489) and lets the
    // verifier fall back to strict alignment (design §6.3). Any other
    // failure mode — transient outage, quorum disagreement, transport
    // error — must NOT silently take the same fallback: that would
    // turn a transient DoH outage into a quietly stricter check that
    // could break legitimate setup flows. Propagate everything else.
    let dmarc_bytes_opt = match crate::doh::fetch_txt(&dmarc_fqdn, &domain).await {
        Ok(bytes) => Some(bytes),
        Err(crate::doh::DohError::NoAnswer) => None,
        Err(e) => return Err(map_doh_error(e, &domain)),
    };

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
    let from = extract_from_address(message)?;
    if !from.eq_ignore_ascii_case(&snapshot.claimed_address) {
        return Err(EmailRecoveryError::AddressMismatch);
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
pub(super) fn extract_from_address(
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
    // Apply the same RFC 5321 §4.5.3.1 caps as `prepare_add` did
    // when accepting the claimed address. Defense in depth: a real
    // SMTP path won't deliver an oversized address (RFC 5321 line
    // limits stop it well before us), but we don't want to rely on
    // the gateway to enforce that.
    if addr_spec.len() > super::MAX_ADDRESS {
        return Err(EmailRecoveryError::AddressMismatch);
    }
    let (local, domain) = addr_spec
        .split_once('@')
        .ok_or(EmailRecoveryError::AddressMismatch)?;
    if local.is_empty()
        || domain.is_empty()
        || local.len() > super::MAX_LOCAL_PART
        || domain.len() > super::MAX_DOMAIN
    {
        return Err(EmailRecoveryError::AddressMismatch);
    }
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
        // `NoAnswer` from the DMARC fetch is handled inline at the
        // call site (it switches the verifier to the strict-alignment
        // fallback). If we land here it's from the DKIM fetch — a
        // quorum of providers saying "no DKIM record at this selector",
        // which means the signature's public key is gone (key rotation
        // or a forged `s=` tag). Treat that as a verification rejection
        // rather than a transient DoH outage.
        DohError::NoAnswer => EmailRecoveryError::EmailVerificationFailed(
            "DKIM record not found at signed selector".into(),
        ),
        DohError::ResponseMalformed(msg) => {
            EmailRecoveryError::DohFetchFailed(format!("DoH response malformed: {msg}"))
        }
        DohError::InvalidName(msg) => {
            EmailRecoveryError::InternalCanisterError(format!("DoH rejected query name: {msg}"))
        }
        DohError::NameOutsideRegisteredDomain {
            name,
            registered_domain,
        } => EmailRecoveryError::InternalCanisterError(format!(
            "DoH rejected name {name:?} as outside registered domain {registered_domain:?}"
        )),
    }
}

/// Write the verified credential to the anchor. Inline rather than
/// going through `anchor_operation_with_authz_check` because there's
/// no caller to authenticate (this happens in response to a
/// DKIM-verified email, not a user call) and the operation type is
/// bookkeeping-light.
///
/// Shared between the DoH path here in `smtp.rs` and the DNSSEC
/// submit-leaf path in `submit_leaf.rs`.
pub(super) fn bind_credential(
    anchor: AnchorNumber,
    claimed_address: &str,
    now_secs: u64,
) -> Result<(), EmailRecoveryError> {
    let mut a = state::anchor(anchor);
    // `email_recovery` is a `Vec` in the data model; the API caps it
    // at one entry, so any prior binding is replaced (and we leave it
    // up to the caller to assert "only one" via the prepare path).
    a.email_recovery = vec![EmailRecoveryCredential {
        address: claimed_address.to_string(),
        // `Timestamp` is nanoseconds since epoch (see
        // `internet_identity_interface::types::Timestamp`). We work
        // in seconds internally; convert at the boundary.
        created_at: now_secs.saturating_mul(1_000_000_000),
        last_used: None,
    }];
    state::storage_borrow_mut(|storage| storage.write(a)).map_err(|e| match e {
        // The reverse-address index hit a "this address is already
        // bound to a different anchor" — surface that as the
        // user-facing error rather than the InternalCanisterError
        // catch-all. See `Storage::update_email_recovery_lookup`.
        crate::storage::StorageError::EmailRecoveryAddressAlreadyBound { .. } => {
            EmailRecoveryError::AddressAlreadyRegistered
        }
        other => EmailRecoveryError::InternalCanisterError(format!("write anchor: {other:?}")),
    })?;
    Ok(())
}

/// Build a `PendingSnapshot` shape suitable for
/// `stamp_recovery_delegation` from the data submit_leaf.rs already
/// has at hand. Used so the same delegation-stamping helper serves
/// both the DoH path (called inline in `handle_smtp_request`) and
/// the DNSSEC path (called from `submit_leaf::submit_dkim_leaf`
/// after the leaf admits and the cryptographic check passes).
pub(super) fn recovery_snapshot(
    claimed_address: String,
    registered_domain: String,
    session_pk: SessionKey,
) -> PendingSnapshot {
    PendingSnapshot {
        kind: SnapshotKind::Recovery { session_pk },
        claimed_address,
        registered_domain,
        is_dnssec_path: true,
        cached_dmarc_txt: None,
        partial_set: false,
        already_terminal: false,
    }
}

/// Recovery-flow counterpart to `bind_credential`: resolve the
/// verified `From:` address (already checked to match the claimed
/// address by the verification pipeline) to its bound anchor via the
/// reverse index, derive the delegation seed, and add the canister
/// signature so a subsequent `email_recovery_get_delegation` query
/// can retrieve it.
///
/// Returns the `RecoveryOutcome` to cache on the pending challenge —
/// `email_recovery_status` reads it to answer with
/// `RecoveryReady { user_key, expiration, anchor_number }`, and
/// `email_recovery_get_delegation` reads the cached `seed` to look
/// up the signature without re-deriving from the anchor.
pub(super) async fn stamp_recovery_delegation(
    snapshot: &PendingSnapshot,
    session_pk: &SessionKey,
) -> Result<super::pending::RecoveryOutcome, EmailRecoveryError> {
    use ic_certification::Hash;

    // The verifier already checked that From == claimed_address, so
    // this lookup is for the address the user typed at prepare time
    // *and* signed mail from. If the address isn't bound to any
    // anchor (registration step never happened, or the user removed
    // the credential after starting the wizard), fail closed.
    let anchor_number = state::storage_borrow(|storage| {
        storage.lookup_anchor_with_email_recovery_address(&snapshot.claimed_address)
    })
    .ok_or(EmailRecoveryError::AddressNotRegistered)?;

    // The signature-map operations need the canister salt. In
    // production it's already initialised (every prior delegation
    // call paid that cost); we await defensively in case this is
    // the very first delegation since deploy.
    state::ensure_salt_set().await;

    let expiration =
        ic_cdk::api::time().saturating_add(crate::delegation::DEFAULT_EXPIRATION_PERIOD_NS);
    let seed: Hash = calculate_email_recovery_seed(&snapshot.claimed_address, anchor_number);

    state::signature_map_mut(|sigs| {
        crate::delegation::add_delegation_signature(sigs, session_pk.clone(), &seed, expiration);
    });
    crate::update_root_hash();

    let user_key = crate::delegation::der_encode_canister_sig_key(seed.to_vec());

    Ok(super::pending::RecoveryOutcome {
        user_key,
        expiration,
        anchor_number,
        seed,
    })
}

/// Compute the canister-signature seed binding a recovery email to
/// an anchor. Mirrors `openid::calculate_delegation_seed` in shape:
/// length-prefixed components hashed with SHA-256, prefixed by the
/// canister-wide salt so principals are unique to *this* II
/// instance.
///
/// The components are:
/// - `salt` (32 bytes) — the canister-wide salt set on first call.
/// - the literal byte string `"email-recovery"` — domain separator,
///   so this seed never collides with the OpenID, anchor, or
///   account seeds.
/// - the lowercased canonical address (variable, ≤ 254 bytes per
///   RFC 5321 §4.5.3.1, enforced upstream).
/// - the anchor number as little-endian bytes.
pub(crate) fn calculate_email_recovery_seed(
    address: &str,
    anchor_number: AnchorNumber,
) -> ic_certification::Hash {
    use sha2::{Digest, Sha256};

    const DOMAIN_SEPARATOR: &[u8] = b"email-recovery";

    let salt = state::salt();
    let address_lower = address.to_ascii_lowercase();

    let mut blob: Vec<u8> = Vec::new();
    blob.push(salt.len() as u8);
    blob.extend_from_slice(&salt);

    blob.push(DOMAIN_SEPARATOR.len() as u8);
    blob.extend_from_slice(DOMAIN_SEPARATOR);

    blob.push(address_lower.len() as u8);
    blob.extend_from_slice(address_lower.as_bytes());

    blob.push(anchor_number.to_le_bytes().len() as u8);
    blob.extend_from_slice(&anchor_number.to_le_bytes());

    let mut hasher = Sha256::new();
    hasher.update(&blob);
    hasher.finalize().into()
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

    fn smtp_envelope(user: &str, domain: &str) -> SmtpRequest {
        smtp_envelope_with_recipients(&[(user, domain)])
    }

    fn smtp_envelope_with_recipients(recipients: &[(&str, &str)]) -> SmtpRequest {
        use internet_identity_interface::internet_identity::types::smtp::{
            SmtpAddress, SmtpEnvelope,
        };
        SmtpRequest {
            envelope: Some(SmtpEnvelope {
                from: SmtpAddress {
                    user: "sender".into(),
                    domain: "example.com".into(),
                },
                to: recipients
                    .iter()
                    .map(|(u, d)| SmtpAddress {
                        user: (*u).into(),
                        domain: (*d).into(),
                    })
                    .collect(),
            }),
            message: None,
            gateway_flags: None,
        }
    }

    fn assert_smtp_ok(resp: SmtpResponse) {
        assert!(
            matches!(resp, SmtpResponse::Ok {}),
            "expected Ok, got {resp:?}"
        );
    }

    fn assert_smtp_err_code(resp: SmtpResponse, expected: u64) {
        match resp {
            SmtpResponse::Err(e) => assert_eq!(e.code, expected, "wrong error code: {e:?}"),
            other => panic!("expected Err({expected}), got {other:?}"),
        }
    }

    /// Configure the per-deploy `related_origins` so
    /// `super::mailbox_domains()` returns the listed hosts.
    /// Recipient dispatch and the `smtp_request_validate` query
    /// both pull the accepted domain set from this list.
    fn set_related_origins(origins: &[&str]) {
        crate::state::persistent_state_mut(|p| {
            p.related_origins = Some(origins.iter().map(|s| (*s).to_string()).collect());
        });
    }

    #[test]
    fn validate_accepts_register_recipient() {
        set_related_origins(&["https://id.ai"]);
        assert_smtp_ok(handle_smtp_request_validate(smtp_envelope(
            "register", "id.ai",
        )));
    }

    #[test]
    fn validate_accepts_recover_recipient() {
        set_related_origins(&["https://id.ai"]);
        assert_smtp_ok(handle_smtp_request_validate(smtp_envelope(
            "recover", "id.ai",
        )));
    }

    #[test]
    fn validate_accepts_case_insensitive() {
        // Source `related_origins` are already lowercased by the
        // host-extraction helper, so accepting an upper-case input
        // exercises the case-insensitive comparison on the SMTP
        // side.
        set_related_origins(&["https://id.ai"]);
        assert_smtp_ok(handle_smtp_request_validate(smtp_envelope(
            "REGISTER", "ID.AI",
        )));
        assert_smtp_ok(handle_smtp_request_validate(smtp_envelope(
            "Recover", "Id.Ai",
        )));
    }

    #[test]
    fn validate_accepts_any_related_origin_alias() {
        // All `related_origins` entries are equal aliases. A prod
        // deploy with id.ai + the *.icp0.io aliases must accept
        // mail at register@<any of them>.
        set_related_origins(&[
            "https://id.ai",
            "https://identity.ic0.app",
            "https://identity.internetcomputer.org",
        ]);
        assert_smtp_ok(handle_smtp_request_validate(smtp_envelope(
            "register",
            "identity.ic0.app",
        )));
        assert_smtp_ok(handle_smtp_request_validate(smtp_envelope(
            "recover",
            "identity.internetcomputer.org",
        )));
    }

    #[test]
    fn validate_accepts_beta_alias_only_when_configured() {
        // beta deploy: only beta.id.ai is on the allowlist.
        set_related_origins(&["https://beta.id.ai"]);
        assert_smtp_ok(handle_smtp_request_validate(smtp_envelope(
            "register",
            "beta.id.ai",
        )));
        // The bare `id.ai` is *not* on this deploy's list, so it's
        // bounced — the canister doesn't accept it just because the
        // string contains "id.ai".
        assert_smtp_err_code(
            handle_smtp_request_validate(smtp_envelope("register", "id.ai")),
            SMTP_ERR_MAILBOX_UNAVAILABLE,
        );
    }

    #[test]
    fn validate_rejects_unknown_user() {
        // Numeric anchor numbers (PoC postbox style) are not handled
        // by this canister anymore — the gateway should bounce them.
        set_related_origins(&["https://id.ai"]);
        assert_smtp_err_code(
            handle_smtp_request_validate(smtp_envelope("12345", "id.ai")),
            SMTP_ERR_MAILBOX_UNAVAILABLE,
        );
        assert_smtp_err_code(
            handle_smtp_request_validate(smtp_envelope("alice", "id.ai")),
            SMTP_ERR_MAILBOX_UNAVAILABLE,
        );
    }

    #[test]
    fn validate_rejects_known_user_wrong_domain() {
        // `register` at a domain not on the deploy's
        // `related_origins` must not slip through.
        set_related_origins(&["https://id.ai"]);
        assert_smtp_err_code(
            handle_smtp_request_validate(smtp_envelope("register", "evil.example")),
            SMTP_ERR_MAILBOX_UNAVAILABLE,
        );
    }

    #[test]
    fn validate_rejects_when_no_origins_configured() {
        // No `related_origins` set → no domains accepted. Defensive
        // path — real deploys always configure this.
        crate::state::persistent_state_mut(|p| p.related_origins = None);
        assert_smtp_err_code(
            handle_smtp_request_validate(smtp_envelope("register", "id.ai")),
            SMTP_ERR_MAILBOX_UNAVAILABLE,
        );
    }

    #[test]
    fn validate_rejects_missing_envelope() {
        let req = SmtpRequest {
            envelope: None,
            message: None,
            gateway_flags: None,
        };
        assert_smtp_err_code(handle_smtp_request_validate(req), SMTP_ERR_SYNTAX_ERROR);
    }

    // ============================================================
    // DNSSEC-path DKIM tag enforcement — `prepare_partial_verification`
    // must reject signatures that fail the signature-header-only
    // checks shared with the DoH path. Regression guards for the
    // four gaps the parity audit surfaced. The DNS-record-dependent
    // checks (`i=` AUID alignment, `t=y` testing) belong to
    // `submit_dkim_leaf::run_submit` and are covered by the unit
    // tests on the helpers themselves.
    // ============================================================

    fn dnssec_snapshot(claimed: &str, zone: &str) -> PendingSnapshot {
        PendingSnapshot {
            kind: SnapshotKind::Setup { anchor: 10_000 },
            claimed_address: claimed.into(),
            registered_domain: zone.into(),
            is_dnssec_path: true,
            cached_dmarc_txt: None,
            partial_set: false,
            already_terminal: false,
        }
    }

    /// Build an `SmtpRequest` with a single DKIM-Signature header
    /// whose tag content is `dkim_value`. `From:` is set to the same
    /// address every test uses, the body is `b"hi"`, and the envelope
    /// is shaped like a register-flow delivery.
    fn smtp_with_dkim(dkim_value: &str) -> SmtpRequest {
        use internet_identity_interface::internet_identity::types::smtp::{
            SmtpAddress, SmtpEnvelope, SmtpHeader, SmtpMessage,
        };
        use serde_bytes::ByteBuf;
        SmtpRequest {
            envelope: Some(SmtpEnvelope {
                from: SmtpAddress {
                    user: "alice".into(),
                    domain: "example.com".into(),
                },
                to: vec![SmtpAddress {
                    user: "register".into(),
                    domain: "id.ai".into(),
                }],
            }),
            message: Some(SmtpMessage {
                headers: vec![
                    SmtpHeader {
                        name: "From".into(),
                        value: "alice@example.com".into(),
                    },
                    SmtpHeader {
                        name: "DKIM-Signature".into(),
                        value: dkim_value.into(),
                    },
                ],
                body: ByteBuf::from(b"hi".to_vec()),
            }),
            gateway_flags: None,
        }
    }

    /// Now-pinned at the same value `now_secs()` returns under
    /// `cfg(test)`. Tests choose `t=` / `x=` relative to this so a
    /// future skew adjustment doesn't silently break them.
    const TEST_NOW: u64 = 1_700_000_000;

    #[test]
    fn dnssec_prepare_rejects_future_dated_t() {
        // t = now + 1000s, well beyond the shared 60s skew window.
        let dkim_value = "v=1; a=rsa-sha256; d=example.com; s=mail; \
                          c=relaxed/relaxed; h=From:Subject; \
                          t=1700001000; bh=MTIzNDU2; b=YWJj";
        let req = smtp_with_dkim(dkim_value);
        let snap = dnssec_snapshot("alice@example.com", "example.com");
        match prepare_partial_verification(&req, &snap, TEST_NOW) {
            Err(EmailRecoveryError::EmailVerificationFailed(msg)) => {
                assert!(
                    msg.contains("SignatureFutureDated"),
                    "DNSSEC path must reject future-dated t=; got {msg}"
                );
            }
            other => panic!("expected SignatureFutureDated, got {other:?}"),
        }
    }

    #[test]
    fn dnssec_prepare_accepts_future_t_within_skew() {
        // t = now + 30s, inside the 60s window. The signature-header
        // checks must pass; we expect later failure on body-hash (the
        // bh= fixture is intentionally a non-match).
        let dkim_value = "v=1; a=rsa-sha256; d=example.com; s=mail; \
                          c=relaxed/relaxed; h=From:Subject; \
                          t=1700000030; bh=MTIzNDU2; b=YWJj";
        let req = smtp_with_dkim(dkim_value);
        let snap = dnssec_snapshot("alice@example.com", "example.com");
        let result = prepare_partial_verification(&req, &snap, TEST_NOW);
        if let Err(EmailRecoveryError::EmailVerificationFailed(msg)) = &result {
            assert!(
                !msg.contains("SignatureFutureDated"),
                "t= within skew must not be rejected as future-dated; got {msg}"
            );
        }
        // Otherwise: fine — we only care that we didn't get the
        // SignatureFutureDated rejection. Body-hash fail (the bh=
        // fixture is intentionally a non-match) or any later failure
        // is acceptable for this regression guard.
    }

    #[test]
    fn dnssec_prepare_rejects_expired_x() {
        // x = now - 1, signature deadline has lapsed.
        let dkim_value = "v=1; a=rsa-sha256; d=example.com; s=mail; \
                          c=relaxed/relaxed; h=From:Subject; \
                          x=1699999999; bh=MTIzNDU2; b=YWJj";
        let req = smtp_with_dkim(dkim_value);
        let snap = dnssec_snapshot("alice@example.com", "example.com");
        match prepare_partial_verification(&req, &snap, TEST_NOW) {
            Err(EmailRecoveryError::EmailVerificationFailed(msg)) => {
                assert!(
                    msg.contains("SignatureExpired"),
                    "DNSSEC path must reject signatures past x=; got {msg}"
                );
            }
            other => panic!("expected SignatureExpired, got {other:?}"),
        }
    }

    #[test]
    fn dnssec_prepare_rejects_missing_subject_in_h() {
        // h= covers From but not Subject. The challenge nonce lives in
        // Subject, so a signature that doesn't cover it would let a
        // MITM rewrite the nonce on a legitimately-signed email.
        let dkim_value = "v=1; a=rsa-sha256; d=example.com; s=mail; \
                          c=relaxed/relaxed; h=From; \
                          bh=MTIzNDU2; b=YWJj";
        let req = smtp_with_dkim(dkim_value);
        let snap = dnssec_snapshot("alice@example.com", "example.com");
        assert!(matches!(
            prepare_partial_verification(&req, &snap, TEST_NOW),
            Err(EmailRecoveryError::SubjectNotSigned)
        ));
    }

    #[test]
    fn validate_rejects_multi_recipient_envelope_even_when_one_is_known() {
        // Recovery emails only ever target a single `register@…` /
        // `recover@…` mailbox. Additional recipients alongside one of
        // ours can only come from a phishy forwarder trying to BCC
        // itself a copy of the user's canister-signed challenge
        // nonce — refuse the SMTP transaction outright with 551
        // ("User not local"), which the gateway can tell apart from
        // the per-recipient 550.
        set_related_origins(&["https://id.ai"]);
        assert_smtp_err_code(
            handle_smtp_request_validate(smtp_envelope_with_recipients(&[
                ("register", "id.ai"),
                ("someone-else", "example.com"),
            ])),
            SMTP_ERR_USER_NOT_LOCAL,
        );
    }

    #[test]
    fn validate_rejects_multi_recipient_even_when_all_unknown() {
        // Belt and suspenders: the single-recipient rule fires before
        // the per-recipient match, so multiple unknown recipients
        // bounce with the same 551 the mixed case gets — *not* the
        // 550 we'd return for a single unknown recipient. Keeps the
        // response surface predictable for the gateway.
        set_related_origins(&["https://id.ai"]);
        assert_smtp_err_code(
            handle_smtp_request_validate(smtp_envelope_with_recipients(&[
                ("someone-else", "example.com"),
                ("alice", "id.ai"),
            ])),
            SMTP_ERR_USER_NOT_LOCAL,
        );
    }

    #[test]
    fn validate_rejects_empty_recipient_list() {
        // An envelope with no recipients can't address anyone on this
        // canister — same 551 as the multi-recipient case (the
        // single-recipient rule also fires on `to.len() == 0`).
        set_related_origins(&["https://id.ai"]);
        assert_smtp_err_code(
            handle_smtp_request_validate(smtp_envelope_with_recipients(&[])),
            SMTP_ERR_USER_NOT_LOCAL,
        );
    }
}
