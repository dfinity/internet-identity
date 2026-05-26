//! Typestate ladder from API-level `SmtpRequest` (untrusted, decoded
//! from the SMTP gateway) to a fully DKIM-certified, DMARC-aligned
//! [`VerifiedSmtpRequest`].
//!
//! Every stage's fields are private. The only way to construct
//! [`UnverifiedSmtpRequest`], a [`SignedSmtpRequestProjection`] vector,
//! or a [`VerifiedSmtpRequest`] is to call the appropriate fallible
//! `TryFrom` — so downstream callers can't hand a partially-validated
//! request to logic that assumes verification ran.
//!
//! # Stages
//!
//! **Stage 1 — `UnverifiedSmtpRequest`** (bounds + RFC 5322 §3.6 well-
//! formedness).
//!
//! - Subsumes everything `validate_smtp_request` currently checks
//!   (envelope presence, address/header/body bounds, recipient count).
//! - Adds single-occurrence enforcement on the message headers:
//!   `From`/`Date`/`Subject` exactly once when the message is present;
//!   `Sender`/`Reply-To`/`To`/`Cc`/`Bcc`/`Message-ID`/`In-Reply-To`/
//!   `References` at most once; `DKIM-Signature` at least once.
//! - Message remains optional so `smtp_request_validate` (envelope-only
//!   at RCPT TO time) can still produce a stage-1 value.
//!
//! **Stage 2 — `SignedSmtpRequestProjection`** (one per `DKIM-Signature`).
//!
//! - Stage 1 → stage 2 parses every `DKIM-Signature` header into a
//!   [`DkimSignature`] and returns one projection per successfully
//!   parsed signature (`Vec<SignedSmtpRequestProjection>`).
//! - Each element's *public* surface is the three parsed DKIM tags an
//!   orchestrator needs to fetch the right key and check the right
//!   zone: `signature_domain()` (`d=`), `signature_selector()` (`s=`),
//!   and `signature_algorithm()` (`a=`). Message-level headers
//!   (`From`, `Subject`, body bytes) are deliberately not exposed —
//!   they are claimed-but-unverified at this point and the type wall
//!   stops a downstream reader from accidentally trusting them.
//! - No canonicalisation happens at stage 2; building the `h=`-scoped
//!   signed view and the `bh=`-canonical body is deferred to stage 3
//!   so cycles aren't spent on signatures that never get tried.
//! - Conversion fails (`DkimScopeError`) only when **every** present
//!   `DKIM-Signature` header fails to parse; the error aggregates the
//!   per-header parse diagnostics for telemetry.
//!
//! **Stage 3 — `VerifiedSmtpRequest`** (DKIM signature + DMARC alignment
//! both passed).
//!
//! - Iterates the stage-2 vector **bottom-up** (RFC 6376 §5.4
//!   convention: forwarders prepend signatures, so the originating-
//!   domain signer sits towards the bottom). For each candidate we run
//!   the per-signature tag contract, body-hash check, header-canon
//!   build, and signature verification, then check DMARC alignment
//!   against the From-domain extracted from the message. First passing
//!   candidate wins.
//! - The verified type's whole public surface is `header(name) ->
//!   Option<&str>` and `body() -> &[u8]`. Both read from the winning
//!   signature's `h=` projection. There is no escape hatch for the
//!   DKIM signing domain, the From-domain, the gateway flags, or any
//!   other metadata: once verification has run, the only thing
//!   downstream code should be reading is the bytes DKIM bound.

use std::rc::Rc;

use crate::dkim::{
    body_hash_sha256, build_header_hash_input, enforce_dns_record_tag_contract,
    enforce_signature_header_tag_contract, parse_dkim_signature, parse_dkim_txt, relaxed_body,
    simple_body, Algorithm, BodyCanon, DkimCheck, DkimCheckName, DkimCheckStatus, DkimSignature,
    HeaderCanon, VerificationFailReason, VerifyOutcome,
};
use crate::dmarc::{aligns, parse_dmarc_txt, AlignmentMode, DmarcOutcome};
use internet_identity_interface::internet_identity::types::smtp::{
    smtp_err, validate_smtp_request, SmtpHeader, SmtpMessage, SmtpRequest, SmtpResponse,
    SMTP_ERR_SYNTAX_ERROR,
};
// `validate_smtp_request` (in the wire-types crate) folds bounds and
// RFC 5322 §3.6 header-uniqueness into a single pass; stage 1 is the
// thin typestate marker over that contract.

// =========================================================================
// Stage 1 — UnverifiedSmtpRequest
// =========================================================================

/// API-level `SmtpRequest` that has cleared bounds checks and RFC 5322
/// §3.6 single-occurrence checks. Nothing is cryptographically certified
/// at this stage — `Unverified` is the contract.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct UnverifiedSmtpRequest {
    request: SmtpRequest,
}

impl UnverifiedSmtpRequest {
    /// Borrow the underlying validated request. `pub(crate)` so the
    /// canister boundary can pull envelope / gateway-flag metadata
    /// that doesn't pass through DKIM (e.g. recipient dispatch).
    /// Downstream consumers outside this crate go through
    /// [`VerifiedSmtpRequest`] instead.
    pub(crate) fn raw(&self) -> &SmtpRequest {
        &self.request
    }
}

impl TryFrom<SmtpRequest> for UnverifiedSmtpRequest {
    /// Stage 1's validation contract is whatever `validate_smtp_request`
    /// returns — bounds + RFC 5322 §3.6 header uniqueness, folded into
    /// one pass in the wire-types crate. The error type is the
    /// wire-shape [`SmtpResponse`] so the canister boundary can
    /// surface it back to the gateway verbatim.
    type Error = SmtpResponse;

    fn try_from(request: SmtpRequest) -> Result<Self, SmtpResponse> {
        validate_smtp_request(&request)?;
        Ok(UnverifiedSmtpRequest { request })
    }
}

// =========================================================================
// Stage 2 — SignedSmtpRequestProjection
// =========================================================================

/// One signature's claimed projection of the request. Carries the
/// parsed `DkimSignature` plus a shared owning reference to the
/// underlying validated message so stage 3 can canonicalise the
/// `h=`-listed headers and body once verification picks a winning
/// candidate.
///
/// **Stage 2 deliberately does NOT expose message-level headers.** The
/// `From`, `Subject`, etc. are claimed-but-unverified at this point;
/// the type wall is what stops a downstream reader from accidentally
/// trusting them before stage 3 has bound them with a cryptographic
/// signature check.
#[derive(Clone, Debug)]
pub struct SignedSmtpRequestProjection {
    /// The validated message body the signature claims to cover.
    /// Stage 1's `validate_smtp_request` already enforced "at least
    /// one DKIM-Signature when message present", so stage 2 only
    /// produces projections when a message actually exists — we
    /// encode that invariant in the type by storing the unwrapped
    /// `SmtpMessage` here instead of carrying around an
    /// `Option<SmtpMessage>` and unwrapping at each access. Shared
    /// across sibling projections produced by the same stage-1 →
    /// stage-2 conversion via `Rc` so the value type doesn't need a
    /// lifetime parameter — `TryFrom` requires owned outputs.
    message: Rc<SmtpMessage>,
    /// The exact `DKIM-Signature` header value this projection was
    /// parsed from. Stage 3 needs the original bytes (with `b=`
    /// blanked) to compute the canonical signed-data input per RFC 6376
    /// §3.7.
    signature_header_value: String,
    /// The parsed signature.
    signature: DkimSignature,
}

/// Stage 2 failure: every present `DKIM-Signature` header failed to
/// parse. Stage 1 has already guaranteed at least one is present when
/// the message is present, so this never fires on envelope-only input.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DkimScopeError {
    /// Per-header parse diagnostics — one entry per `DKIM-Signature`
    /// header that failed to parse. Aggregated so telemetry can show
    /// every signature's failure reason instead of just the last one.
    pub per_signature: Vec<String>,
}

impl From<DkimScopeError> for SmtpResponse {
    fn from(err: DkimScopeError) -> SmtpResponse {
        smtp_err(
            SMTP_ERR_SYNTAX_ERROR,
            format!(
                "every DKIM-Signature header failed to parse: {}",
                err.per_signature.join("; ")
            ),
        )
    }
}

impl TryFrom<UnverifiedSmtpRequest> for Vec<SignedSmtpRequestProjection> {
    type Error = DkimScopeError;

    fn try_from(unverified: UnverifiedSmtpRequest) -> Result<Self, DkimScopeError> {
        // Stage 1 enforces "DKIM-Signature must appear at least once
        // when message present", so an absent message here is the
        // envelope-only case — which doesn't have signatures to scope
        // and shouldn't have reached stage 2 in the first place.
        // Callers that legitimately want to stop after stage 1 (the
        // `smtp_request_validate` query) don't call through to stage
        // 2. The neutral wording keeps the message safe to surface
        // back to the gateway as SMTP 555 (`DkimScopeError →
        // SmtpResponse`) if a misconfigured caller does reach it.
        //
        // Destructure-let-else so the message moves out by value —
        // each projection then owns its share of an `Rc<SmtpMessage>`
        // with no further `Option` unwrapping anywhere downstream.
        let SmtpRequest {
            message: Some(message),
            ..
        } = unverified.request
        else {
            return Err(DkimScopeError {
                per_signature: vec!["missing message body".into()],
            });
        };

        let mut projections: Vec<(String, DkimSignature)> = Vec::new();
        let mut per_signature_diagnostics: Vec<String> = Vec::new();
        for header in message
            .headers
            .iter()
            .filter(|h| h.name.eq_ignore_ascii_case("DKIM-Signature"))
        {
            match parse_dkim_signature(&header.value) {
                Ok(sig) => projections.push((header.value.clone(), sig)),
                Err(e) => per_signature_diagnostics.push(e),
            }
        }

        if projections.is_empty() {
            return Err(DkimScopeError {
                per_signature: per_signature_diagnostics,
            });
        }

        let message = Rc::new(message);
        Ok(projections
            .into_iter()
            .map(|(hv, sig)| SignedSmtpRequestProjection {
                message: Rc::clone(&message),
                signature_header_value: hv,
                signature: sig,
            })
            .collect())
    }
}

impl SignedSmtpRequestProjection {
    /// The signature's `d=` tag — the signing domain. Lowercased per
    /// `parse_dkim_signature`.
    pub fn signature_domain(&self) -> &str {
        &self.signature.d
    }

    /// The signature's `s=` tag — the selector. Lowercased.
    pub fn signature_selector(&self) -> &str {
        &self.signature.s
    }

    /// The signature's `a=` tag — algorithm family.
    pub fn signature_algorithm(&self) -> Algorithm {
        self.signature.algorithm
    }

    /// Crate-private: the full parsed `DKIM-Signature`. Exposed so the
    /// canister-side DNSSEC partial-verification step (which runs
    /// everything except the signature math, because the public key
    /// isn't available until the FE submits the DKIM leaf) can compute
    /// the body hash and canonical signed-headers digest.
    ///
    /// **Hazard**: the values here are what the signature *claims* to
    /// cover; they aren't certified until stage 3 runs. Anything bound
    /// downstream on the basis of these reads is bound on an
    /// uncertified projection.
    pub(crate) fn parsed_signature(&self) -> &DkimSignature {
        &self.signature
    }

    /// Crate-private: the original (gateway-supplied) bytes of the
    /// `DKIM-Signature` header value. Needed so RFC 6376 §3.7's
    /// "blank the `b=` tag value while preserving surrounding bytes"
    /// step can run against the exact bytes the signer hashed.
    pub(crate) fn signature_header_raw(&self) -> &str {
        &self.signature_header_value
    }

    /// Crate-private: the validated message body the signature claims
    /// to cover. Same hazard as `parsed_signature()` — the body hash
    /// hasn't been verified yet.
    pub(crate) fn message(&self) -> &SmtpMessage {
        &self.message
    }
}

// =========================================================================
// Stage 3 — VerifiedSmtpRequest
// =========================================================================

/// Inputs stage 3 needs that don't come from the request itself: the
/// already-trusted DKIM TXT record bytes (sourced from the DNSSEC chain
/// or a DoH outcall), the optional DMARC TXT record, and the current
/// time. The async DNS fetch happens outside the `TryFrom`; the
/// conversion itself is sync and pure.
pub struct VerificationContext<'a> {
    /// Content of `<selector>._domainkey.<domain>` TXT record. Caller
    /// is responsible for trust establishment (DNSSEC validation or
    /// DoH quorum).
    pub dkim_txt: &'a str,
    /// Content of `_dmarc.<domain>` TXT record, or `None` if the
    /// domain publishes no DMARC record. When `None`, stage 3 falls
    /// back to a strict `d=` ↔ From-domain equality check.
    pub dmarc_txt: Option<&'a str>,
    /// Current Unix time in seconds (so tests can pin it).
    pub now_secs: u64,
}

/// Cryptographically certified, DMARC-aligned email. Stage-3 output and
/// the only shape downstream callers (credential binding, recovery
/// dispatch) should consume.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct VerifiedSmtpRequest {
    /// Subset of the message headers covered by the winning signature's
    /// `h=` list, in source order. We materialise only the
    /// signature-bound headers so `header()` cannot accidentally hand
    /// out an unverified header value.
    signed_headers: Vec<SmtpHeader>,
    /// Raw body bytes — the same bytes whose canonical form the winning
    /// signature's `bh=` validated. Returned by `body()` so callers can
    /// parse the message body content (e.g. extract nonces, render
    /// rich content).
    body: Vec<u8>,
    /// DKIM signing domain (`d=`). `pub(crate)` so the `dkim`/`dmarc`
    /// test-vector modules can assert which signer was admitted. Not
    /// exposed publicly — downstream code outside the crate reads
    /// message content via `header()` and `body()` only.
    pub(crate) dkim_domain: String,
    /// From-domain extracted from the signed view. `pub(crate)` for
    /// the same reason as `dkim_domain`.
    pub(crate) from_domain: String,
    /// DMARC outcome (Aligned / NoRecord-with-equality / etc).
    /// `pub(crate)` for the DMARC test-vector assertions.
    pub(crate) dmarc_outcome: DmarcOutcome,
    /// Per-step DKIM checks. `pub(crate)`
    /// so the DKIM test-vector module can pin individual check
    /// statuses (e.g. `SignatureNotFromFuture = Pass`).
    pub(crate) checks: Vec<DkimCheck>,
}

/// Stage 3 failure: no signature verified, or none aligned. Aggregates
/// per-signature failure reasons so telemetry can show every attempt.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct VerificationError {
    /// `(reverse_index, reason, per_step_checks)` for every signature
    /// tried. `reverse_index` is the position in the stage-2 vector
    /// **as encountered during bottom-up iteration**, so a single
    /// signature is index 0, multi-signature emails get index 0 for
    /// the last-encountered (bottom-most) signature.
    pub per_signature: Vec<(usize, VerificationFailReason, Vec<DkimCheck>)>,
    /// Best-fit reason from the most-recently-attempted signature.
    /// Best-fit reason for surface-level error reporting: the last
    /// signature attempted (i.e., the top-most one, since stage 3
    /// iterates bottom-up). Mirrors the pre-typestate "first-pass
    /// wins, last-fail surfaces" convention.
    pub last_reason: VerificationFailReason,
    /// Flattened per-step trail across all attempts. Concatenates each
    /// candidate signature's check trail in attempt order so telemetry
    /// can render the full audit history.
    pub combined_checks: Vec<DkimCheck>,
}

impl VerifiedSmtpRequest {
    /// The value DKIM signed for `name` under the winning signature.
    /// `None` if the signer's `h=` didn't list it, or the message
    /// didn't have it — both mean "DKIM does not vouch for this
    /// header."
    ///
    /// Comparison is case-insensitive on the header name (RFC 5322
    /// §1.2.2). The returned value is the original (uncanonicalised)
    /// header value bytes; canonicalisation was an internal step of
    /// the signature math, not what the caller wants.
    ///
    /// When the winning signature's `h=` lists the same name multiple
    /// times the DKIM math picks the latest matching header bottom-up
    /// (RFC 6376 §5.4); for the recovery flow's headers (`From`,
    /// `Subject`, `Date`, ...) stage 1 already enforces single-
    /// occurrence so there is at most one match here.
    pub fn header(&self, name: &str) -> Option<&str> {
        // `signed_headers` already contains only the headers covered by
        // h=, so a name miss here means "not signed."
        self.signed_headers
            .iter()
            .rev()
            .find(|h| h.name.eq_ignore_ascii_case(name))
            .map(|h| h.value.as_str())
    }

    /// The body bytes the winning signature's `bh=` validated.
    pub fn body(&self) -> &[u8] {
        &self.body
    }
}

impl<'a> TryFrom<(Vec<SignedSmtpRequestProjection>, &VerificationContext<'a>)>
    for VerifiedSmtpRequest
{
    type Error = VerificationError;

    fn try_from(
        (projections, ctx): (Vec<SignedSmtpRequestProjection>, &VerificationContext<'a>),
    ) -> Result<Self, VerificationError> {
        let mut per_signature: Vec<(usize, VerificationFailReason, Vec<DkimCheck>)> = Vec::new();
        let mut combined_checks: Vec<DkimCheck> = Vec::new();

        // Iterate bottom-up. RFC 6376 §5.4 selection convention:
        // forwarders prepend signatures, so the originating-domain
        // signer sits towards the bottom. `enumerate()` over the
        // reverse iterator numbers attempts 0, 1, 2 ... in bottom-up
        // order so `per_signature[0]` is always the bottom attempt.
        for (attempt_idx, proj) in projections.iter().rev().enumerate() {
            match verify_signature(proj, ctx.dkim_txt, ctx.now_secs) {
                Ok((from_domain, checks)) => {
                    let dkim_domain = proj.signature.d.clone();
                    let dmarc_outcome =
                        compute_dmarc_outcome(&dkim_domain, &from_domain, ctx.dmarc_txt);
                    let accepted = match &dmarc_outcome {
                        DmarcOutcome::Aligned { .. } => true,
                        DmarcOutcome::NoRecord => dkim_domain == from_domain,
                        DmarcOutcome::Misaligned { .. } | DmarcOutcome::Malformed(_) => false,
                    };
                    if accepted {
                        return Ok(build_verified(
                            proj,
                            from_domain,
                            dkim_domain,
                            dmarc_outcome,
                            checks,
                        ));
                    }
                    let reason = match &dmarc_outcome {
                        DmarcOutcome::Malformed(e) => {
                            VerificationFailReason::DmarcMalformed(e.clone())
                        }
                        _ => VerificationFailReason::DmarcMisaligned,
                    };
                    per_signature.push((attempt_idx, reason.clone(), checks.clone()));
                    combined_checks.extend(checks);
                }
                Err((reason, checks)) => {
                    per_signature.push((attempt_idx, reason, checks.clone()));
                    combined_checks.extend(checks);
                }
            }
        }

        let last_reason = per_signature
            .last()
            .map(|(_, r, _)| r.clone())
            .unwrap_or(VerificationFailReason::NoSignature);
        Err(VerificationError {
            per_signature,
            last_reason,
            combined_checks,
        })
    }
}

/// Verify one signature: tag contract, DNS record parse, DNS tag
/// contract, body-hash, signed-headers canonicalisation, signature
/// math, From-header extraction. Returns the From-domain on success.
fn verify_signature(
    proj: &SignedSmtpRequestProjection,
    dkim_txt: &str,
    now_secs: u64,
) -> Result<(String, Vec<DkimCheck>), (VerificationFailReason, Vec<DkimCheck>)> {
    let mut checks: Vec<DkimCheck> = Vec::new();
    checks.push(check(
        DkimCheckName::DkimSignaturePresent,
        DkimCheckStatus::Pass,
        None,
    ));
    // The signature was parsed by stage 1 → stage 2, so SignatureParsed
    // and AlgorithmSupported are implicitly Pass at this point.
    checks.push(check(
        DkimCheckName::SignatureParsed,
        DkimCheckStatus::Pass,
        None,
    ));
    checks.push(check(
        DkimCheckName::AlgorithmSupported,
        DkimCheckStatus::Pass,
        None,
    ));

    let sig = &proj.signature;

    // (c=) Reject simple/* on the header side (design §5.2).
    if sig.c_header != HeaderCanon::Relaxed {
        checks.push(check(
            DkimCheckName::CanonicalizationSupported,
            DkimCheckStatus::Fail,
            Some("header canonicalisation must be relaxed (c=relaxed/...)".into()),
        ));
        return Err((VerificationFailReason::UnsupportedCanonicalization, checks));
    }
    checks.push(check(
        DkimCheckName::CanonicalizationSupported,
        DkimCheckStatus::Pass,
        None,
    ));

    // Signature-header-only tag contract (x=, t=, Subject ∈ h=).
    match enforce_signature_header_tag_contract(sig, now_secs) {
        Ok(mut trail) => checks.append(&mut trail),
        Err((reason, mut trail)) => {
            checks.append(&mut trail);
            return Err((reason, checks));
        }
    }

    // Parse the (already-trusted) DKIM TXT record.
    let dns = match parse_dkim_txt(dkim_txt) {
        Ok(r) => r,
        Err(e) => {
            checks.push(check(
                DkimCheckName::DnsRecordParsed,
                DkimCheckStatus::Fail,
                Some(e.clone()),
            ));
            return Err((VerificationFailReason::DnsRecordMalformed(e), checks));
        }
    };
    checks.push(check(
        DkimCheckName::DnsRecordParsed,
        DkimCheckStatus::Pass,
        None,
    ));

    // DNS-record-dependent tag contract (t=y, i= alignment).
    match enforce_dns_record_tag_contract(&sig.i, &sig.d, &dns) {
        Ok(mut trail) => checks.append(&mut trail),
        Err((reason, mut trail)) => {
            checks.append(&mut trail);
            return Err((reason, checks));
        }
    }

    // DNS k= must match a= family.
    if !dns.key_type.matches_signature_alg(sig.algorithm) {
        checks.push(check(
            DkimCheckName::PublicKeyTypeMatches,
            DkimCheckStatus::Fail,
            Some(format!(
                "DNS k= {:?} does not match signature a= {:?}",
                dns.key_type, sig.algorithm
            )),
        ));
        return Err((VerificationFailReason::AlgorithmKeyTypeMismatch, checks));
    }
    checks.push(check(
        DkimCheckName::PublicKeyTypeMatches,
        DkimCheckStatus::Pass,
        None,
    ));

    // Body hash.
    let message = proj.message();
    let canonical_body = match sig.c_body {
        BodyCanon::Relaxed => relaxed_body(&message.body),
        BodyCanon::Simple => simple_body(&message.body),
    };
    let computed_bh = body_hash_sha256(&canonical_body, sig.l);
    if computed_bh.as_slice() != sig.bh.as_slice() {
        checks.push(check(
            DkimCheckName::BodyHashValid,
            DkimCheckStatus::Fail,
            Some("computed body hash does not match bh=".into()),
        ));
        return Err((VerificationFailReason::BodyHashMismatch, checks));
    }
    checks.push(check(
        DkimCheckName::BodyHashValid,
        DkimCheckStatus::Pass,
        None,
    ));

    // Build the canonical signed-headers input, then verify. The
    // full path here avoids a name clash with this module's own
    // `verify_signature` orchestration helper.
    let signed_data = build_header_hash_input(&message.headers, sig, &proj.signature_header_value);
    let outcome = crate::dkim::verify_signature(
        sig.algorithm,
        dns.key_type,
        &dns.public_key,
        &signed_data,
        &sig.b,
    );
    match outcome {
        VerifyOutcome::Valid => {
            checks.push(check(
                DkimCheckName::SignatureValid,
                DkimCheckStatus::Pass,
                None,
            ));
        }
        VerifyOutcome::BadSignature => {
            checks.push(check(
                DkimCheckName::SignatureValid,
                DkimCheckStatus::Fail,
                Some("signature did not validate against public key".into()),
            ));
            return Err((VerificationFailReason::SignatureInvalid, checks));
        }
        VerifyOutcome::MalformedKey(e) => {
            checks.push(check(
                DkimCheckName::SignatureValid,
                DkimCheckStatus::Fail,
                Some(format!("malformed key: {e}")),
            ));
            return Err((VerificationFailReason::DnsRecordMalformed(e), checks));
        }
        VerifyOutcome::MalformedSignature(e) => {
            checks.push(check(
                DkimCheckName::SignatureValid,
                DkimCheckStatus::Fail,
                Some(format!("malformed signature: {e}")),
            ));
            return Err((VerificationFailReason::SignatureMalformed(e), checks));
        }
        VerifyOutcome::AlgorithmMismatch => {
            checks.push(check(
                DkimCheckName::SignatureValid,
                DkimCheckStatus::Fail,
                Some("algorithm/key-type mismatch".into()),
            ));
            return Err((VerificationFailReason::AlgorithmKeyTypeMismatch, checks));
        }
        VerifyOutcome::RsaKeyTooSmall(bits) => {
            checks.push(check(
                DkimCheckName::SignatureValid,
                DkimCheckStatus::Fail,
                Some(format!("RSA key only {bits} bits")),
            ));
            return Err((VerificationFailReason::RsaKeyTooSmall(bits), checks));
        }
    }

    // Extract the From-domain from the message. The signature's `h=`
    // already lists `From` (the parser enforces it), so what we read
    // here is the From the verifier just bound — even if a forwarder
    // prepended a different unsigned From, the bottom-up scan in
    // `build_header_hash_input` consumed the bottommost (originator)
    // From, and that's what we extract here.
    let from_domain = match crate::dmarc::extract_from_domain(message) {
        Ok(d) => d,
        Err(e) => {
            return Err((VerificationFailReason::MalformedFromHeader(e), checks));
        }
    };

    Ok((from_domain, checks))
}

/// Construct the final `VerifiedSmtpRequest` from the winning
/// signature's projection.
///
/// Header selection mirrors `build_header_hash_input` (RFC 6376 §5.4
/// bottom-up): for each `h=` entry we pick the latest matching
/// (not-yet-consumed) header in the message. The selected set is then
/// emitted in **message source order** so `header(name)`'s reverse
/// scan walks bottom-up over the actual message and returns the
/// instance the first `h=` entry consumed — i.e., the same bytes the
/// signer hashed.
///
/// Worst case is O(|h| · |headers|): a `Vec<bool>` mask gives O(1)
/// per-index "already consumed" checks, replacing the prior
/// `Vec<usize>::contains` linear scan.
fn build_verified(
    proj: &SignedSmtpRequestProjection,
    from_domain: String,
    dkim_domain: String,
    dmarc_outcome: DmarcOutcome,
    checks: Vec<DkimCheck>,
) -> VerifiedSmtpRequest {
    let message = proj.message();

    let mut consumed = vec![false; message.headers.len()];
    let mut selected: Vec<usize> = Vec::with_capacity(proj.signature.h.len());
    for h_name in &proj.signature.h {
        for (idx, hdr) in message.headers.iter().enumerate().rev() {
            if !consumed[idx] && hdr.name.eq_ignore_ascii_case(h_name) {
                consumed[idx] = true;
                selected.push(idx);
                break;
            }
        }
        // h= name absent from the message contributes nothing — DKIM
        // signed the empty string for it; from the caller's perspective
        // `header()` will return None, which matches "DKIM does not
        // vouch for this header."
    }
    // Sort ascending so `signed_headers` is in true message source
    // order. `.iter().rev()` in `header()` then walks bottom-up, and
    // for a name covered N times by `h=` it finds the bottom-most
    // surviving instance — the one the first `h=` entry consumed.
    selected.sort_unstable();
    let signed_headers: Vec<SmtpHeader> = selected
        .into_iter()
        .map(|idx| message.headers[idx].clone())
        .collect();

    VerifiedSmtpRequest {
        signed_headers,
        body: message.body.to_vec(),
        dkim_domain,
        from_domain,
        dmarc_outcome,
        checks,
    }
}

/// Compute the DMARC alignment outcome given the DKIM `d=`, the From-
/// header domain, and the optional published DMARC record. `pub(crate)`
/// so the DMARC unit tests can pin its behaviour without going through
/// the cryptographic stage-3 round-trip.
pub(crate) fn compute_dmarc_outcome(
    dkim_domain: &str,
    from_domain: &str,
    dmarc_txt: Option<&str>,
) -> DmarcOutcome {
    let txt = match dmarc_txt {
        None => return DmarcOutcome::NoRecord,
        Some(t) => t,
    };
    let record = match parse_dmarc_txt(txt) {
        Ok(r) => r,
        Err(e) => return DmarcOutcome::Malformed(e),
    };
    let adkim: AlignmentMode = record.adkim;
    if aligns(dkim_domain, from_domain, adkim) {
        DmarcOutcome::Aligned {
            policy: record.policy,
            alignment_mode: adkim,
        }
    } else {
        DmarcOutcome::Misaligned {
            policy: record.policy,
        }
    }
}

fn check(name: DkimCheckName, status: DkimCheckStatus, detail: Option<String>) -> DkimCheck {
    DkimCheck {
        name,
        status,
        detail,
    }
}

// =========================================================================
// Tests
// =========================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use internet_identity_interface::internet_identity::types::smtp::{
        SmtpAddress, SmtpEnvelope, SmtpMessage,
    };
    use serde_bytes::ByteBuf;

    fn addr(user: &str, domain: &str) -> SmtpAddress {
        SmtpAddress {
            user: user.into(),
            domain: domain.into(),
        }
    }

    fn header(name: &str, value: &str) -> SmtpHeader {
        SmtpHeader {
            name: name.into(),
            value: value.into(),
        }
    }

    fn envelope_only_request() -> SmtpRequest {
        SmtpRequest {
            envelope: Some(SmtpEnvelope {
                from: addr("alice", "example.com"),
                to: vec![addr("recover", "id.ai")],
            }),
            message: None,
            gateway_flags: None,
        }
    }

    /// A message with the minimum RFC 5322 headers stage 1 requires.
    fn well_formed_message(extra_headers: &[(&str, &str)]) -> SmtpMessage {
        let mut headers = vec![
            header("From", "alice@example.com"),
            header("Date", "Mon, 1 Jan 2024 00:00:00 +0000"),
            header("Subject", "II-Recovery-deadbeefcafe1234"),
            // Stage 1 requires at least one DKIM-Signature when the
            // message is present.
            header(
                "DKIM-Signature",
                "v=1; a=rsa-sha256; d=example.com; s=mail; \
                 c=relaxed/relaxed; h=From:Subject:Date; bh=MTIzNDU2; b=YWJj",
            ),
        ];
        for (n, v) in extra_headers {
            headers.push(header(n, v));
        }
        SmtpMessage {
            headers,
            body: ByteBuf::from(b"hi".to_vec()),
        }
    }

    fn well_formed_request() -> SmtpRequest {
        SmtpRequest {
            envelope: Some(SmtpEnvelope {
                from: addr("alice", "example.com"),
                to: vec![addr("recover", "id.ai")],
            }),
            message: Some(well_formed_message(&[])),
            gateway_flags: None,
        }
    }

    // --- Stage 1 tests ---

    #[test]
    fn stage1_envelope_only_request_succeeds() {
        // smtp_request_validate sends envelope-only requests at RCPT TO
        // time; stage 1 must accept them so the validate path still
        // works.
        let unverified = UnverifiedSmtpRequest::try_from(envelope_only_request())
            .expect("envelope-only must pass stage 1");
        assert!(unverified.raw().message.is_none());
    }

    #[test]
    fn stage1_well_formed_request_succeeds() {
        UnverifiedSmtpRequest::try_from(well_formed_request()).expect("well-formed must pass");
    }

    /// Stage 1's full bounds + RFC 5322 §3.6 contract lives in
    /// `validate_smtp_request` (see the wire-types crate for the
    /// per-rule unit tests). Here we just exercise the propagation:
    /// a validator failure must reach the caller as `SmtpResponse::Err`
    /// with the SMTP 555 code, so the canister boundary can return it
    /// to the gateway without further mapping.
    #[test]
    fn stage1_propagates_validator_failure_as_smtp_555() {
        use internet_identity_interface::internet_identity::types::smtp::SMTP_ERR_SYNTAX_ERROR;
        let mut req = well_formed_request();
        req.message
            .as_mut()
            .unwrap()
            .headers
            .push(header("From", "evil@elsewhere.example"));
        match UnverifiedSmtpRequest::try_from(req).unwrap_err() {
            SmtpResponse::Err(e) => {
                assert_eq!(e.code, SMTP_ERR_SYNTAX_ERROR);
                assert!(
                    e.message.contains("'From'"),
                    "expected SMTP message to name 'From', got {:?}",
                    e.message
                );
            }
            other => panic!("expected SmtpResponse::Err, got {other:?}"),
        }
    }

    // --- Stage 2 tests ---

    #[test]
    fn stage2_yields_one_projection_per_dkim_signature() {
        let mut req = well_formed_request();
        // Add a second DKIM-Signature (different selector) — both parse.
        req.message.as_mut().unwrap().headers.push(header(
            "DKIM-Signature",
            "v=1; a=rsa-sha256; d=example.com; s=other; \
             c=relaxed/relaxed; h=From:Subject:Date; bh=MTIzNDU2; b=ZGVm",
        ));
        let unverified = UnverifiedSmtpRequest::try_from(req).unwrap();
        let projections: Vec<SignedSmtpRequestProjection> = unverified.try_into().unwrap();
        assert_eq!(
            projections.len(),
            2,
            "expected one projection per signature"
        );
        assert_eq!(projections[0].signature_selector(), "mail");
        assert_eq!(projections[1].signature_selector(), "other");
    }

    #[test]
    fn stage2_fails_only_when_every_signature_fails_to_parse() {
        // Replace the well-formed DKIM-Signature with two garbage ones.
        let mut req = well_formed_request();
        let msg = req.message.as_mut().unwrap();
        msg.headers
            .retain(|h| !h.name.eq_ignore_ascii_case("DKIM-Signature"));
        msg.headers
            .push(header("DKIM-Signature", "garbage no equals here"));
        msg.headers
            .push(header("DKIM-Signature", "v=99; a=invalid"));
        let unverified = UnverifiedSmtpRequest::try_from(req).unwrap();
        let err: DkimScopeError = Vec::<SignedSmtpRequestProjection>::try_from(unverified)
            .expect_err("expected stage 2 to fail");
        assert_eq!(
            err.per_signature.len(),
            2,
            "expected per-header diagnostics to be aggregated"
        );
    }

    #[test]
    fn stage2_succeeds_when_at_least_one_signature_parses() {
        // One garbage signature plus one good one — stage 2 should
        // succeed and yield a single projection from the good one.
        let mut req = well_formed_request();
        req.message
            .as_mut()
            .unwrap()
            .headers
            .push(header("DKIM-Signature", "garbage no equals here"));
        let unverified = UnverifiedSmtpRequest::try_from(req).unwrap();
        let projections: Vec<SignedSmtpRequestProjection> = unverified.try_into().unwrap();
        assert_eq!(projections.len(), 1);
        assert_eq!(projections[0].signature_domain(), "example.com");
    }

    #[test]
    fn stage2_exposes_only_signature_tags() {
        // Compile-level guarantee: the projection's public surface is
        // signature_domain / signature_selector / signature_algorithm.
        // This test exists to document the invariant; if a future
        // accidentally adds a `.header()` or `.body()` accessor to
        // stage 2 the reviewer should push back.
        let unverified = UnverifiedSmtpRequest::try_from(well_formed_request()).unwrap();
        let projections: Vec<SignedSmtpRequestProjection> = unverified.try_into().unwrap();
        let p = &projections[0];
        assert_eq!(p.signature_domain(), "example.com");
        assert_eq!(p.signature_selector(), "mail");
        assert_eq!(p.signature_algorithm(), Algorithm::RsaSha256);
    }

    // --- Stage 3 tests ---
    //
    // End-to-end cryptographic round-trips against real signed `.eml`
    // fixtures live in `crate::dkim::test_vectors` and
    // `crate::dmarc::test_vectors` — those modules drive this stage's
    // `TryFrom<(projections, ctx)>` directly. The tests below cover
    // the typestate-specific behaviours that don't need a signed
    // fixture: error aggregation across attempts and bottom-up
    // iteration order.

    #[test]
    fn stage3_verification_error_aggregates_per_signature_reasons() {
        // Two signatures, both with bh= mismatches. We expect
        // VerificationError.per_signature to carry both reasons.
        let mut req = well_formed_request();
        req.message.as_mut().unwrap().headers.push(header(
            "DKIM-Signature",
            "v=1; a=rsa-sha256; d=example.com; s=other; \
             c=relaxed/relaxed; h=From:Subject:Date; bh=MTIzNDU2; b=ZGVm",
        ));
        let unverified = UnverifiedSmtpRequest::try_from(req).unwrap();
        let projections: Vec<SignedSmtpRequestProjection> = unverified.try_into().unwrap();
        let ctx = VerificationContext {
            dkim_txt: "v=DKIM1; p=YWJj",
            dmarc_txt: None,
            now_secs: 1_700_000_000,
        };
        let err: VerificationError = VerifiedSmtpRequest::try_from((projections, &ctx))
            .expect_err("expected verification to fail");
        assert_eq!(
            err.per_signature.len(),
            2,
            "expected per-signature reasons to be aggregated; got {err:?}"
        );
    }

    // ---------------------------------------------------------------
    // `compute_dmarc_outcome` — DMARC alignment-outcome tests, lifted
    // out of the deleted `dmarc::verify::compute_outcome`. They drive
    // the alignment helper directly (no DKIM signature round-trip),
    // exercising the four `DmarcOutcome` variants.
    // ---------------------------------------------------------------

    use crate::dmarc::DmarcPolicy;

    #[test]
    fn dmarc_no_record_when_dkim_equals_from() {
        let outcome = compute_dmarc_outcome("example.com", "example.com", None);
        assert_eq!(outcome, DmarcOutcome::NoRecord);
    }

    #[test]
    fn dmarc_no_record_when_dkim_subdomain_of_from() {
        // No DMARC record + dkim is a subdomain → still NoRecord at
        // this layer. Stage 3's wrapper then rejects on the strict
        // equality fallback (only `dkim == from` is accepted when no
        // record is published).
        let outcome = compute_dmarc_outcome("mail.example.com", "example.com", None);
        assert_eq!(outcome, DmarcOutcome::NoRecord);
    }

    #[test]
    fn dmarc_aligned_under_relaxed_subdomain() {
        let txt = "v=DMARC1; p=reject"; // adkim defaults to relaxed
        let outcome = compute_dmarc_outcome("mail.example.com", "example.com", Some(txt));
        assert!(matches!(
            outcome,
            DmarcOutcome::Aligned {
                policy: DmarcPolicy::Reject,
                alignment_mode: AlignmentMode::Relaxed,
            }
        ));
    }

    #[test]
    fn dmarc_misaligned_under_strict_subdomain() {
        let txt = "v=DMARC1; p=reject; adkim=s";
        let outcome = compute_dmarc_outcome("mail.example.com", "example.com", Some(txt));
        assert!(matches!(outcome, DmarcOutcome::Misaligned { .. }));
    }

    #[test]
    fn dmarc_aligned_strict_exact_match() {
        let txt = "v=DMARC1; p=reject; adkim=s";
        let outcome = compute_dmarc_outcome("example.com", "example.com", Some(txt));
        assert!(matches!(
            outcome,
            DmarcOutcome::Aligned {
                alignment_mode: AlignmentMode::Strict,
                ..
            }
        ));
    }

    #[test]
    fn dmarc_malformed_record_surfaces_as_outcome() {
        let txt = "v=BOGUS";
        let outcome = compute_dmarc_outcome("example.com", "example.com", Some(txt));
        match outcome {
            DmarcOutcome::Malformed(e) => assert!(e.contains("BOGUS")),
            other => panic!("expected Malformed, got {other:?}"),
        }
    }

    #[test]
    fn stage3_iterates_bottom_up() {
        // Two DKIM-Signatures in source order. Each one fails for a
        // DIFFERENT reason (chosen so the per-signature trail of one
        // can't be confused with the other), so the iteration order
        // is genuinely observable from `VerificationError`:
        //
        // - TOP signature (`s=forwarder`) uses `c=simple/simple` →
        //   `UnsupportedCanonicalization` (rejected at the canon-
        //   check step before body-hash runs).
        // - BOTTOM signature (`s=mail`) uses `c=relaxed/relaxed` but
        //   a synthetic `bh=` that won't match the test body →
        //   `BodyHashMismatch`.
        //
        // Bottom-up iteration must visit `mail` FIRST, so
        // `per_signature[0].1` is `BodyHashMismatch`. If a future
        // refactor accidentally flipped iteration direction, the
        // assertions below would surface
        // `UnsupportedCanonicalization` in `per_signature[0].1`
        // instead — the test catches that mistake.
        let mut req = well_formed_request();
        let msg = req.message.as_mut().unwrap();
        // Replace the well-formed fixture's DKIM-Signature with the
        // bottom (`s=mail`) candidate. The signature header value is
        // a known body-hash mismatch (bh=MTIzNDU2 vs body "hi").
        msg.headers
            .retain(|h| !h.name.eq_ignore_ascii_case("DKIM-Signature"));
        msg.headers.push(header(
            "DKIM-Signature",
            "v=1; a=rsa-sha256; d=example.com; s=mail; \
             c=relaxed/relaxed; h=From:Subject:Date; bh=MTIzNDU2; b=YWJj",
        ));
        // Insert the forwarder (`c=simple/simple`) above the mail
        // signature. Source order is now: [forwarder, ..., mail].
        let mail_pos = msg
            .headers
            .iter()
            .position(|h| h.name.eq_ignore_ascii_case("DKIM-Signature"))
            .unwrap();
        msg.headers.insert(
            mail_pos,
            header(
                "DKIM-Signature",
                "v=1; a=rsa-sha256; d=example.com; s=forwarder; \
                 c=simple/simple; h=From:Subject:Date; bh=MTIzNDU2; b=ZGVm",
            ),
        );

        let unverified = UnverifiedSmtpRequest::try_from(req).unwrap();
        let projections: Vec<SignedSmtpRequestProjection> = unverified.try_into().unwrap();
        // Source-order sanity: forwarder appears before mail.
        assert_eq!(projections[0].signature_selector(), "forwarder");
        assert_eq!(projections[1].signature_selector(), "mail");

        let ctx = VerificationContext {
            dkim_txt: "v=DKIM1; p=YWJj",
            dmarc_txt: None,
            now_secs: 1_700_000_000,
        };
        let err: VerificationError = VerifiedSmtpRequest::try_from((projections, &ctx))
            .expect_err("expected verification to fail");

        assert_eq!(
            err.per_signature.len(),
            2,
            "expected both signatures attempted; got {err:?}"
        );

        // The observable order check: the FIRST entry's failure
        // reason must come from the BOTTOM signature (mail,
        // BodyHashMismatch). A top-down iteration would surface
        // UnsupportedCanonicalization here instead.
        assert_eq!(
            err.per_signature[0].1,
            VerificationFailReason::BodyHashMismatch,
            "first attempt must be the bottom signature; got {:?}",
            err.per_signature[0].1,
        );
        assert_eq!(
            err.per_signature[1].1,
            VerificationFailReason::UnsupportedCanonicalization,
            "second attempt must be the top signature; got {:?}",
            err.per_signature[1].1,
        );
        // `last_reason` mirrors the most-recent (top) attempt, also
        // observable here.
        assert_eq!(
            err.last_reason,
            VerificationFailReason::UnsupportedCanonicalization,
            "last_reason must reflect the final attempt (the top signature)",
        );
    }
}
