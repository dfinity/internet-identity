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
//! formedness, restructured into a typed message).
//!
//! - Subsumes the bounds checks `validate_smtp_request` runs at the
//!   wire boundary (envelope presence, address/header/body bounds,
//!   recipient count).
//! - Materialises RFC 5322 §3.6 single-occurrence headers into named
//!   fields of [`unverified::SmtpMessage`]: `From`/`Date`/`Subject`
//!   are required `String`s (exactly-once), the rest of the §3.6 set
//!   (`Sender`/`Reply-To`/`To`/`Cc`/`Bcc`/`Message-ID`/`In-Reply-To`/
//!   `References`) are `Option<String>` (at-most-once), and
//!   `DKIM-Signature` is a `Vec<String>` (at-least-once when the
//!   message is present). The duplicate/missing checks downstream
//!   code used to need are now type-system invariants.
//! - Message remains optional so `smtp_request_validate` (envelope-
//!   only at RCPT TO time) can still produce a stage-1 value.
//!
//! **Stage 2 — `SignedSmtpRequestProjection`** (one per
//! `DKIM-Signature`).
//!
//! - Stage 1 → stage 2 parses every `DKIM-Signature` header into a
//!   [`DkimSignature`] and returns one projection per successfully
//!   parsed signature (`Vec<SignedSmtpRequestProjection>`).
//! - Each element's *public* surface is the three parsed DKIM tags an
//!   orchestrator needs to fetch the right key and check the right
//!   zone: `signature_domain()` (`d=`), `signature_selector()`
//!   (`s=`), and `signature_algorithm()` (`a=`). Message-level
//!   headers stay hidden — they are claimed-but-unverified at this
//!   point and the type wall is what stops a downstream reader from
//!   accidentally trusting them.
//! - No canonicalisation happens at stage 2; building the `h=`-scoped
//!   signed view and the `bh=`-canonical body is deferred to stage 3
//!   so cycles aren't spent on signatures that never get tried.
//! - Conversion fails (`DkimScopeError`) only when **every** present
//!   `DKIM-Signature` header fails to parse; the error aggregates the
//!   per-header parse diagnostics for telemetry.
//!
//! **Stage 3 — `VerifiedSmtpRequest`** (DKIM signature + DMARC
//! alignment both passed).
//!
//! - Iterates the stage-2 vector **bottom-up** (RFC 6376 §5.4
//!   convention: forwarders prepend signatures, so the originating-
//!   domain signer sits towards the bottom). For each candidate we
//!   call `dkim::run_signature_check`, then check DMARC alignment via
//!   `dmarc::compute_outcome`. First passing candidate wins.
//! - The verified type's whole public surface is `header(name) ->
//!   Option<&str>` and `body() -> &[u8]`. Both read from the winning
//!   signature's `h=` projection. The signing domain, From-domain,
//!   DMARC outcome and check trail are exposed via crate-private
//!   accessor methods (`dkim_domain()`, `from_domain()`,
//!   `dmarc_outcome()`, `checks()`) so the DKIM/DMARC test-vector
//!   modules can pin them without touching internal state.

use std::rc::Rc;

use crate::dkim::{
    parse_dkim_signature, Algorithm, DkimCheck, DkimSignature, VerificationFailReason,
};
use crate::dmarc::DmarcOutcome;
use internet_identity_interface::internet_identity::types::smtp::{
    smtp_err, validate_envelope, validate_message, SmtpHeader as WireHeader,
    SmtpMessage as WireMessage, SmtpRequest as WireRequest, SmtpResponse, SMTP_ERR_SYNTAX_ERROR,
};

pub use unverified::SmtpRequest as UnverifiedSmtpRequest;

// =========================================================================
// Stage 1 — UnverifiedSmtpRequest (lives in the `unverified` submodule
// so the typed `SmtpHeader` / `SmtpMessage` here don't clash with the
// wire-types names of the same shape).
// =========================================================================

pub mod unverified {
    use serde_bytes::ByteBuf;

    /// A header that wasn't one of the RFC 5322 §3.6 single-occurrence
    /// names. Same wire shape as [`crate::dkim::SmtpHeader`] but local
    /// to this module so the rest of the canister can't accidentally
    /// hand the typestate a Candid-decoded value that hasn't passed
    /// the stage-1 builder.
    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct SmtpHeader {
        pub name: String,
        pub value: String,
    }

    /// Stage-1 message. RFC 5322 §3.6 single-occurrence headers are
    /// destructured into typed fields, so duplicate/missing checks
    /// are type-system invariants rather than runtime predicates.
    ///
    /// Built via [`SmtpMessageBuilder`]; no other constructor exists
    /// (the fields are private).
    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct SmtpMessage {
        from: String,
        date: String,
        subject: String,
        sender: Option<String>,
        reply_to: Option<String>,
        to: Option<String>,
        cc: Option<String>,
        bcc: Option<String>,
        message_id: Option<String>,
        in_reply_to: Option<String>,
        references: Option<String>,
        /// At least one `DKIM-Signature` header value; each is the raw
        /// gateway-supplied bytes (not yet parsed). Per RFC 6376 §5.5
        /// a message MAY carry several.
        dkim_signatures: Vec<String>,
        /// Headers not in the §3.6 set and not `DKIM-Signature`,
        /// preserved in message source order. The DKIM `h=` selection
        /// in stage 3 walks this slice bottom-up for any name that
        /// isn't a typed §3.6 field.
        other_headers: Vec<SmtpHeader>,
        body: ByteBuf,
    }

    impl SmtpMessage {
        /// Value of the `From:` header. Named `mail_from` rather than
        /// `from` to avoid clippy's `wrong_self_convention` lint (the
        /// Rust `From` trait's `from()` constructor takes no `self`).
        /// Not the same as the SMTP envelope's `MAIL FROM` command,
        /// which the typestate's stage-1 layer doesn't carry.
        pub fn mail_from(&self) -> &str {
            &self.from
        }
        pub fn date(&self) -> &str {
            &self.date
        }
        pub fn subject(&self) -> &str {
            &self.subject
        }
        pub fn sender(&self) -> Option<&str> {
            self.sender.as_deref()
        }
        pub fn reply_to(&self) -> Option<&str> {
            self.reply_to.as_deref()
        }
        pub fn to(&self) -> Option<&str> {
            self.to.as_deref()
        }
        pub fn cc(&self) -> Option<&str> {
            self.cc.as_deref()
        }
        pub fn bcc(&self) -> Option<&str> {
            self.bcc.as_deref()
        }
        pub fn message_id(&self) -> Option<&str> {
            self.message_id.as_deref()
        }
        pub fn in_reply_to(&self) -> Option<&str> {
            self.in_reply_to.as_deref()
        }
        pub fn references(&self) -> Option<&str> {
            self.references.as_deref()
        }
        pub fn dkim_signatures(&self) -> &[String] {
            &self.dkim_signatures
        }
        pub fn other_headers(&self) -> &[SmtpHeader] {
            &self.other_headers
        }
        pub fn body(&self) -> &[u8] {
            &self.body
        }
    }

    /// Builder for [`SmtpMessage`]. Each `add_header` call routes the
    /// header to its typed slot (§3.6 fields) or into `other_headers`
    /// (everything else), failing on duplicates of single-occurrence
    /// names. `build` then enforces the "exactly once" / "at least
    /// once" rules.
    #[derive(Default)]
    pub struct SmtpMessageBuilder {
        from: Option<String>,
        date: Option<String>,
        subject: Option<String>,
        sender: Option<String>,
        reply_to: Option<String>,
        to: Option<String>,
        cc: Option<String>,
        bcc: Option<String>,
        message_id: Option<String>,
        in_reply_to: Option<String>,
        references: Option<String>,
        dkim_signatures: Vec<String>,
        other_headers: Vec<SmtpHeader>,
        body: Option<ByteBuf>,
    }

    /// Why the builder rejected a header or refused to build.
    #[derive(Clone, Debug, Eq, PartialEq)]
    pub enum BuilderError {
        /// A §3.6 single-occurrence header was supplied a second time.
        Duplicate(&'static str),
        /// A §3.6 exactly-once header was never supplied.
        Missing(&'static str),
        /// No `DKIM-Signature` header was supplied (RFC 6376 requires
        /// at least one when a message is present in the recovery
        /// flow).
        MissingDkimSignature,
    }

    impl std::fmt::Display for BuilderError {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::Duplicate(name) => {
                    write!(f, "header '{name}' must appear at most once")
                }
                Self::Missing(name) => {
                    write!(f, "header '{name}' must appear exactly once")
                }
                Self::MissingDkimSignature => {
                    write!(f, "header 'DKIM-Signature' must appear at least once")
                }
            }
        }
    }

    impl SmtpMessageBuilder {
        pub fn new() -> Self {
            Self::default()
        }

        pub fn body(mut self, body: ByteBuf) -> Self {
            self.body = Some(body);
            self
        }

        /// Route one header to its typed slot or to `other_headers`.
        /// Name matching is case-insensitive (RFC 5322 §1.2.2).
        pub fn add_header(&mut self, header: SmtpHeader) -> Result<(), BuilderError> {
            // Each `match_unique!` arm sets the field to Some and
            // returns Duplicate if it was already Some.
            macro_rules! match_unique {
                ($slot:ident, $label:literal) => {{
                    if self.$slot.is_some() {
                        return Err(BuilderError::Duplicate($label));
                    }
                    self.$slot = Some(header.value);
                    return Ok(());
                }};
            }
            let n = header.name.as_str();
            if n.eq_ignore_ascii_case("From") {
                match_unique!(from, "From")
            }
            if n.eq_ignore_ascii_case("Date") {
                match_unique!(date, "Date")
            }
            if n.eq_ignore_ascii_case("Subject") {
                match_unique!(subject, "Subject")
            }
            if n.eq_ignore_ascii_case("Sender") {
                match_unique!(sender, "Sender")
            }
            if n.eq_ignore_ascii_case("Reply-To") {
                match_unique!(reply_to, "Reply-To")
            }
            if n.eq_ignore_ascii_case("To") {
                match_unique!(to, "To")
            }
            if n.eq_ignore_ascii_case("Cc") {
                match_unique!(cc, "Cc")
            }
            if n.eq_ignore_ascii_case("Bcc") {
                match_unique!(bcc, "Bcc")
            }
            if n.eq_ignore_ascii_case("Message-ID") {
                match_unique!(message_id, "Message-ID")
            }
            if n.eq_ignore_ascii_case("In-Reply-To") {
                match_unique!(in_reply_to, "In-Reply-To")
            }
            if n.eq_ignore_ascii_case("References") {
                match_unique!(references, "References")
            }
            if n.eq_ignore_ascii_case("DKIM-Signature") {
                self.dkim_signatures.push(header.value);
                return Ok(());
            }
            self.other_headers.push(header);
            Ok(())
        }

        pub fn build(self) -> Result<SmtpMessage, BuilderError> {
            if self.dkim_signatures.is_empty() {
                return Err(BuilderError::MissingDkimSignature);
            }
            Ok(SmtpMessage {
                from: self.from.ok_or(BuilderError::Missing("From"))?,
                date: self.date.ok_or(BuilderError::Missing("Date"))?,
                subject: self.subject.ok_or(BuilderError::Missing("Subject"))?,
                sender: self.sender,
                reply_to: self.reply_to,
                to: self.to,
                cc: self.cc,
                bcc: self.bcc,
                message_id: self.message_id,
                in_reply_to: self.in_reply_to,
                references: self.references,
                dkim_signatures: self.dkim_signatures,
                other_headers: self.other_headers,
                body: self.body.unwrap_or_default(),
            })
        }
    }

    /// Stage-1 wrapper — `message` is `None` for envelope-only
    /// requests (`smtp_request_validate` at RCPT TO time) and
    /// `Some(SmtpMessage)` for full requests. The wire-type envelope
    /// is consumed by the canister boundary's recipient dispatch
    /// *before* this value is built, so it doesn't appear here.
    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct SmtpRequest {
        pub(super) message: Option<SmtpMessage>,
    }

    impl SmtpRequest {
        pub fn message(&self) -> Option<&SmtpMessage> {
            self.message.as_ref()
        }
    }
}

impl TryFrom<WireRequest> for UnverifiedSmtpRequest {
    /// Stage 1's validation contract is the wire-types bounds checks
    /// (`validate_envelope` + `validate_message`) plus the RFC 5322
    /// §3.6 well-formedness rules enforced by
    /// [`unverified::SmtpMessageBuilder`]. Failures map to SMTP 555
    /// (syntax error) via the wire-types [`SmtpResponse`].
    type Error = SmtpResponse;

    fn try_from(request: WireRequest) -> Result<Self, SmtpResponse> {
        let WireRequest {
            envelope,
            message,
            gateway_flags: _,
        } = request;

        let envelope =
            envelope.ok_or_else(|| smtp_err(SMTP_ERR_SYNTAX_ERROR, "Missing envelope"))?;
        validate_envelope(&envelope)?;

        let message = match message {
            None => None,
            Some(m) => Some(build_typed_message(m)?),
        };

        Ok(UnverifiedSmtpRequest { message })
    }
}

fn build_typed_message(wire: WireMessage) -> Result<unverified::SmtpMessage, SmtpResponse> {
    validate_message(&wire)?;
    let mut builder = unverified::SmtpMessageBuilder::new();
    for header in wire.headers {
        builder
            .add_header(unverified::SmtpHeader {
                name: header.name,
                value: header.value,
            })
            .map_err(builder_error_to_smtp_response)?;
    }
    builder
        .body(wire.body)
        .build()
        .map_err(builder_error_to_smtp_response)
}

fn builder_error_to_smtp_response(err: unverified::BuilderError) -> SmtpResponse {
    smtp_err(SMTP_ERR_SYNTAX_ERROR, format!("RFC 5322 §3.6: {err}"))
}

// =========================================================================
// Stage 2 — SignedSmtpRequestProjection
// =========================================================================

/// One signature's claimed projection of the request. Carries the
/// parsed `DkimSignature` plus a shared owning reference to the
/// stage-1 typed message so stage 3 can canonicalise the `h=`-listed
/// headers once verification picks a winning candidate.
///
/// **Stage 2 deliberately does NOT expose message-level headers.** The
/// `From`, `Subject`, etc. are claimed-but-unverified at this point;
/// the type wall is what stops a downstream reader from accidentally
/// trusting them before stage 3 has bound them with a cryptographic
/// signature check.
#[derive(Clone, Debug)]
pub struct SignedSmtpRequestProjection {
    message: Rc<unverified::SmtpMessage>,
    signature_header_value: String,
    signature: DkimSignature,
}

/// Stage 2 failure: every present `DKIM-Signature` header failed to
/// parse. Stage 1 has already guaranteed at least one is present when
/// the message is present, so this never fires on envelope-only input.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DkimScopeError {
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
        // Stage 1 enforced ≥1 DKIM-Signature when the message is
        // present, so an absent message means an envelope-only
        // request reached stage 2 — caller bug-ish, but surface a
        // neutral SMTP-555-safe wording.
        let Some(message) = unverified.message else {
            return Err(DkimScopeError {
                per_signature: vec!["missing message body".into()],
            });
        };

        let mut parsed: Vec<(String, DkimSignature)> = Vec::new();
        let mut diagnostics: Vec<String> = Vec::new();
        for value in message.dkim_signatures() {
            match parse_dkim_signature(value) {
                Ok(sig) => parsed.push((value.clone(), sig)),
                Err(e) => diagnostics.push(e),
            }
        }

        if parsed.is_empty() {
            return Err(DkimScopeError {
                per_signature: diagnostics,
            });
        }

        let message = Rc::new(message);
        Ok(parsed
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
    /// isn't available until the FE submits the DKIM leaf) can
    /// compute the body hash and canonical signed-headers digest.
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

    /// Crate-private: the stage-1 typed message this signature claims
    /// to cover.
    pub(crate) fn message(&self) -> &unverified::SmtpMessage {
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
    pub dkim_txt: &'a str,
    pub dmarc_txt: Option<&'a str>,
    pub now_secs: u64,
}

/// Cryptographically certified, DMARC-aligned email. Stage-3 output —
/// the only shape downstream callers should consume.
///
/// Field-level reads happen via accessor methods only; the struct's
/// fields are private so a future caller can't reach in and pull a
/// raw header value that wasn't covered by the winning signature.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct VerifiedSmtpRequest {
    /// Subset of the message headers covered by the winning
    /// signature's `h=` list, in message source order. `header()`
    /// walks this bottom-up and returns the bytes the first `h=` entry
    /// consumed (RFC 6376 §5.4 selection).
    signed_headers: Vec<WireHeader>,
    body: Vec<u8>,
    dkim_domain: String,
    from_domain: String,
    dmarc_outcome: DmarcOutcome,
    checks: Vec<DkimCheck>,
}

/// Stage 3 failure: no signature verified, or none aligned. Aggregates
/// per-signature failure reasons so telemetry can show every attempt.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct VerificationError {
    pub per_signature: Vec<(usize, VerificationFailReason, Vec<DkimCheck>)>,
    /// Best-fit reason for surface-level error reporting: the last
    /// signature attempted (i.e., the top-most one, since stage 3
    /// iterates bottom-up). Mirrors the pre-typestate "first-pass
    /// wins, last-fail surfaces" convention.
    pub last_reason: VerificationFailReason,
    /// Flattened per-step trail across all attempts.
    pub combined_checks: Vec<DkimCheck>,
}

impl VerifiedSmtpRequest {
    /// The value DKIM signed for `name` under the winning signature.
    /// `None` if the signer's `h=` didn't list it, or the message
    /// didn't have it — both mean "DKIM does not vouch for this
    /// header." Comparison on `name` is case-insensitive.
    pub fn header(&self, name: &str) -> Option<&str> {
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

    /// Crate-private: the winning signature's `d=`. Exposed for the
    /// dkim/dmarc test-vector modules' assertions; downstream code
    /// reads message content via `header()` / `body()` instead.
    pub(crate) fn dkim_domain(&self) -> &str {
        &self.dkim_domain
    }

    /// Crate-private: From-domain extracted from the signed view.
    /// `mail_from_domain` rather than `from_domain` to dodge clippy's
    /// `wrong_self_convention` lint on `from_*` method names.
    pub(crate) fn mail_from_domain(&self) -> &str {
        &self.from_domain
    }

    /// Crate-private: DMARC outcome (Aligned / NoRecord / etc).
    pub(crate) fn dmarc_outcome(&self) -> &DmarcOutcome {
        &self.dmarc_outcome
    }

    /// Crate-private: per-step DKIM checks for the winning signature.
    pub(crate) fn checks(&self) -> &[DkimCheck] {
        &self.checks
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
        // signer sits towards the bottom.
        for (attempt_idx, proj) in projections.iter().rev().enumerate() {
            match verify_signature(proj, ctx.dkim_txt, ctx.now_secs) {
                Ok((from_domain, checks)) => {
                    let dkim_domain = proj.signature.d.clone();
                    let dmarc_outcome =
                        crate::dmarc::compute_outcome(&dkim_domain, &from_domain, ctx.dmarc_txt);
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

/// Stage 3 orchestrator: hand the projection's DKIM math off to
/// `dkim::run_signature_check`, then read the From-domain straight
/// from the typed message (the signature's `h=` is enforced by the
/// parser to include `From`, so this read is the DKIM-bound view).
fn verify_signature(
    proj: &SignedSmtpRequestProjection,
    dkim_txt: &str,
    now_secs: u64,
) -> Result<(String, Vec<DkimCheck>), (VerificationFailReason, Vec<DkimCheck>)> {
    let message = proj.message();
    let headers = reconstruct_wire_headers(message, &proj.signature_header_value);
    let checks = crate::dkim::run_signature_check(
        &proj.signature,
        &headers,
        message.body(),
        &proj.signature_header_value,
        dkim_txt,
        now_secs,
    )?;
    let from_domain = from_domain_of(message).map_err(|e| {
        (
            VerificationFailReason::MalformedFromHeader(e),
            checks.clone(),
        )
    })?;
    Ok((from_domain, checks))
}

/// Rebuild a wire-shape header vec from the typed message for the
/// DKIM math layer to consume. Order: §3.6 fields in canonical
/// occurrence order, then DKIM-Signature headers (one per
/// `dkim_signatures` entry), then `other_headers` in source order.
///
/// `target_signature_value` is the raw `DKIM-Signature` value the
/// caller is verifying. The reconstructed slice places that signature
/// at the bottom of the DKIM-Signature block so bottom-up h= selection
/// picks it for self-reference (RFC 6376 §3.7 hashes the signature
/// header with `b=` blanked).
///
/// `pub(crate)` so the DNSSEC-path partial-verification step (which
/// runs the DKIM signed-headers digest without the public key) can
/// reuse exactly the same reconstruction the DoH-path's stage 3 uses.
pub(crate) fn reconstruct_wire_headers(
    message: &unverified::SmtpMessage,
    target_signature_value: &str,
) -> Vec<WireHeader> {
    let mut out: Vec<WireHeader> = Vec::new();
    push_field(&mut out, "From", Some(message.mail_from()));
    push_field(&mut out, "Date", Some(message.date()));
    push_field(&mut out, "Subject", Some(message.subject()));
    push_field(&mut out, "Sender", message.sender());
    push_field(&mut out, "Reply-To", message.reply_to());
    push_field(&mut out, "To", message.to());
    push_field(&mut out, "Cc", message.cc());
    push_field(&mut out, "Bcc", message.bcc());
    push_field(&mut out, "Message-ID", message.message_id());
    push_field(&mut out, "In-Reply-To", message.in_reply_to());
    push_field(&mut out, "References", message.references());
    // DKIM-Signatures other than the one being verified come first,
    // then the target — bottom-up h= selection picks the target when
    // it appears in `h=` (it shouldn't, per spec, but the
    // canonicalisation step still references the value).
    for value in message.dkim_signatures() {
        if value != target_signature_value {
            out.push(WireHeader {
                name: "DKIM-Signature".into(),
                value: value.clone(),
            });
        }
    }
    out.push(WireHeader {
        name: "DKIM-Signature".into(),
        value: target_signature_value.to_string(),
    });
    for header in message.other_headers() {
        out.push(WireHeader {
            name: header.name.clone(),
            value: header.value.clone(),
        });
    }
    out
}

fn push_field(out: &mut Vec<WireHeader>, name: &str, value: Option<&str>) {
    if let Some(v) = value {
        out.push(WireHeader {
            name: name.to_string(),
            value: v.to_string(),
        });
    }
}

/// Extract the From-domain from the typed message's `from` field.
/// The wire-types parser at `dmarc::from_header::extract_from_domain`
/// took an `SmtpMessage` and searched its headers; with typed
/// single-occurrence fields we read the field directly.
fn from_domain_of(message: &unverified::SmtpMessage) -> Result<String, String> {
    parse_single_mailbox_domain(message.mail_from())
}

/// Minimal copy of the dmarc-side mailbox parser, specialised for the
/// already-validated `from` field. Returns the lowercased domain on
/// success.
fn parse_single_mailbox_domain(value: &str) -> Result<String, String> {
    crate::dmarc::extract_from_value(value)
}

/// Build the final `VerifiedSmtpRequest` from the winning projection.
/// Selects exactly the headers the signature's `h=` covered (RFC 6376
/// §5.4 bottom-up), in true message source order.
fn build_verified(
    proj: &SignedSmtpRequestProjection,
    from_domain: String,
    dkim_domain: String,
    dmarc_outcome: DmarcOutcome,
    checks: Vec<DkimCheck>,
) -> VerifiedSmtpRequest {
    let message = proj.message();
    let headers = reconstruct_wire_headers(message, &proj.signature_header_value);

    let mut consumed = vec![false; headers.len()];
    let mut selected: Vec<usize> = Vec::with_capacity(proj.signature.h.len());
    for h_name in &proj.signature.h {
        for (idx, hdr) in headers.iter().enumerate().rev() {
            if !consumed[idx] && hdr.name.eq_ignore_ascii_case(h_name) {
                consumed[idx] = true;
                selected.push(idx);
                break;
            }
        }
    }
    selected.sort_unstable();
    let signed_headers: Vec<WireHeader> = selected
        .into_iter()
        .map(|idx| headers[idx].clone())
        .collect();

    VerifiedSmtpRequest {
        signed_headers,
        body: message.body().to_vec(),
        dkim_domain,
        from_domain,
        dmarc_outcome,
        checks,
    }
}

// =========================================================================
// Tests
// =========================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use internet_identity_interface::internet_identity::types::smtp::{
        SmtpAddress, SmtpEnvelope, SmtpMessage as TestWireMessage, SmtpRequest as TestWireRequest,
        SMTP_ERR_SYNTAX_ERROR,
    };
    use serde_bytes::ByteBuf;

    fn addr(user: &str, domain: &str) -> SmtpAddress {
        SmtpAddress {
            user: user.into(),
            domain: domain.into(),
        }
    }

    fn header(name: &str, value: &str) -> WireHeader {
        WireHeader {
            name: name.into(),
            value: value.into(),
        }
    }

    fn envelope_only_request() -> TestWireRequest {
        TestWireRequest {
            envelope: Some(SmtpEnvelope {
                from: addr("alice", "example.com"),
                to: vec![addr("recover", "id.ai")],
            }),
            message: None,
            gateway_flags: None,
        }
    }

    fn well_formed_message(extra_headers: &[(&str, &str)]) -> TestWireMessage {
        let mut headers = vec![
            header("From", "alice@example.com"),
            header("Date", "Mon, 1 Jan 2024 00:00:00 +0000"),
            header("Subject", "II-Recovery-deadbeefcafe1234"),
            header(
                "DKIM-Signature",
                "v=1; a=rsa-sha256; d=example.com; s=mail; \
                 c=relaxed/relaxed; h=From:Subject:Date; bh=MTIzNDU2; b=YWJj",
            ),
        ];
        for (n, v) in extra_headers {
            headers.push(header(n, v));
        }
        TestWireMessage {
            headers,
            body: ByteBuf::from(b"hi".to_vec()),
        }
    }

    fn well_formed_request() -> TestWireRequest {
        TestWireRequest {
            envelope: Some(SmtpEnvelope {
                from: addr("alice", "example.com"),
                to: vec![addr("recover", "id.ai")],
            }),
            message: Some(well_formed_message(&[])),
            gateway_flags: None,
        }
    }

    // --- Stage 1 ---

    #[test]
    fn stage1_envelope_only_request_succeeds() {
        let unverified = UnverifiedSmtpRequest::try_from(envelope_only_request())
            .expect("envelope-only must pass stage 1");
        assert!(unverified.message().is_none());
    }

    #[test]
    fn stage1_well_formed_request_succeeds() {
        let unverified =
            UnverifiedSmtpRequest::try_from(well_formed_request()).expect("well-formed must pass");
        let msg = unverified.message().unwrap();
        assert_eq!(msg.mail_from(), "alice@example.com");
        assert_eq!(msg.subject(), "II-Recovery-deadbeefcafe1234");
        assert_eq!(msg.dkim_signatures().len(), 1);
    }

    fn assert_555_with_substring(resp: SmtpResponse, substring: &str) {
        match resp {
            SmtpResponse::Err(e) => {
                assert_eq!(e.code, SMTP_ERR_SYNTAX_ERROR);
                assert!(
                    e.message.contains(substring),
                    "expected message to contain {substring:?}, got {:?}",
                    e.message,
                );
            }
            other => panic!("expected SmtpResponse::Err(555), got {other:?}"),
        }
    }

    #[test]
    fn stage1_rejects_duplicate_from() {
        let mut req = well_formed_request();
        req.message
            .as_mut()
            .unwrap()
            .headers
            .push(header("From", "evil@elsewhere.example"));
        let err = UnverifiedSmtpRequest::try_from(req).unwrap_err();
        assert_555_with_substring(err, "'From'");
    }

    #[test]
    fn stage1_rejects_missing_from() {
        let mut req = well_formed_request();
        req.message
            .as_mut()
            .unwrap()
            .headers
            .retain(|h| !h.name.eq_ignore_ascii_case("From"));
        let err = UnverifiedSmtpRequest::try_from(req).unwrap_err();
        assert_555_with_substring(err, "'From'");
    }

    #[test]
    fn stage1_rejects_duplicate_subject() {
        let mut req = well_formed_request();
        req.message
            .as_mut()
            .unwrap()
            .headers
            .push(header("Subject", "another subject"));
        let err = UnverifiedSmtpRequest::try_from(req).unwrap_err();
        assert_555_with_substring(err, "'Subject'");
    }

    #[test]
    fn stage1_rejects_duplicate_date() {
        let mut req = well_formed_request();
        req.message
            .as_mut()
            .unwrap()
            .headers
            .push(header("Date", "Tue, 2 Jan 2024 00:00:00 +0000"));
        let err = UnverifiedSmtpRequest::try_from(req).unwrap_err();
        assert_555_with_substring(err, "'Date'");
    }

    #[test]
    fn stage1_rejects_duplicate_message_id() {
        let mut req = well_formed_request();
        let msg = req.message.as_mut().unwrap();
        msg.headers.push(header("Message-ID", "<a@example.com>"));
        msg.headers.push(header("Message-ID", "<b@example.com>"));
        let err = UnverifiedSmtpRequest::try_from(req).unwrap_err();
        assert_555_with_substring(err, "'Message-ID'");
    }

    #[test]
    fn stage1_at_most_once_headers_allow_zero_or_one() {
        UnverifiedSmtpRequest::try_from(well_formed_request()).expect("Reply-To absent is fine");
        let mut req = well_formed_request();
        req.message
            .as_mut()
            .unwrap()
            .headers
            .push(header("Reply-To", "alice-reply@example.com"));
        UnverifiedSmtpRequest::try_from(req).expect("Reply-To once is fine");
    }

    #[test]
    fn stage1_rejects_missing_dkim_signature_when_message_present() {
        let mut req = well_formed_request();
        req.message
            .as_mut()
            .unwrap()
            .headers
            .retain(|h| !h.name.eq_ignore_ascii_case("DKIM-Signature"));
        let err = UnverifiedSmtpRequest::try_from(req).unwrap_err();
        assert_555_with_substring(err, "DKIM-Signature");
    }

    #[test]
    fn stage1_propagates_bounds_failure() {
        let req = TestWireRequest {
            envelope: None,
            message: None,
            gateway_flags: None,
        };
        let err = UnverifiedSmtpRequest::try_from(req).unwrap_err();
        assert_555_with_substring(err, "envelope");
    }

    #[test]
    fn stage1_header_count_uses_case_insensitive_match() {
        let mut req = well_formed_request();
        req.message
            .as_mut()
            .unwrap()
            .headers
            .push(header("from", "another@example.com"));
        let err = UnverifiedSmtpRequest::try_from(req).unwrap_err();
        assert_555_with_substring(err, "'From'");
    }

    // --- Stage 2 ---

    #[test]
    fn stage2_yields_one_projection_per_dkim_signature() {
        let mut req = well_formed_request();
        req.message.as_mut().unwrap().headers.push(header(
            "DKIM-Signature",
            "v=1; a=rsa-sha256; d=example.com; s=other; \
             c=relaxed/relaxed; h=From:Subject:Date; bh=MTIzNDU2; b=ZGVm",
        ));
        let unverified = UnverifiedSmtpRequest::try_from(req).unwrap();
        let projections: Vec<SignedSmtpRequestProjection> = unverified.try_into().unwrap();
        assert_eq!(projections.len(), 2);
        assert_eq!(projections[0].signature_selector(), "mail");
        assert_eq!(projections[1].signature_selector(), "other");
    }

    #[test]
    fn stage2_fails_only_when_every_signature_fails_to_parse() {
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
        assert_eq!(err.per_signature.len(), 2);
    }

    #[test]
    fn stage2_succeeds_when_at_least_one_signature_parses() {
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
        let unverified = UnverifiedSmtpRequest::try_from(well_formed_request()).unwrap();
        let projections: Vec<SignedSmtpRequestProjection> = unverified.try_into().unwrap();
        let p = &projections[0];
        assert_eq!(p.signature_domain(), "example.com");
        assert_eq!(p.signature_selector(), "mail");
        assert_eq!(p.signature_algorithm(), Algorithm::RsaSha256);
    }

    // --- Stage 3 ---

    #[test]
    fn stage3_verification_error_aggregates_per_signature_reasons() {
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
        assert_eq!(err.per_signature.len(), 2);
    }

    #[test]
    fn stage3_iterates_bottom_up() {
        // Two DKIM-Signatures with distinct failure reasons so the
        // iteration direction is genuinely observable.
        let mut req = well_formed_request();
        let msg = req.message.as_mut().unwrap();
        msg.headers
            .retain(|h| !h.name.eq_ignore_ascii_case("DKIM-Signature"));
        msg.headers.push(header(
            "DKIM-Signature",
            "v=1; a=rsa-sha256; d=example.com; s=mail; \
             c=relaxed/relaxed; h=From:Subject:Date; bh=MTIzNDU2; b=YWJj",
        ));
        // Insert a forwarder (c=simple/simple) BEFORE the mail
        // signature so source order is [forwarder, mail].
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
        assert_eq!(projections[0].signature_selector(), "forwarder");
        assert_eq!(projections[1].signature_selector(), "mail");

        let ctx = VerificationContext {
            dkim_txt: "v=DKIM1; p=YWJj",
            dmarc_txt: None,
            now_secs: 1_700_000_000,
        };
        let err = VerifiedSmtpRequest::try_from((projections, &ctx)).expect_err("expected fail");
        // Bottom-up: mail tried first (BodyHashMismatch), then
        // forwarder (UnsupportedCanonicalization).
        assert_eq!(
            err.per_signature[0].1,
            VerificationFailReason::BodyHashMismatch
        );
        assert_eq!(
            err.per_signature[1].1,
            VerificationFailReason::UnsupportedCanonicalization
        );
        assert_eq!(
            err.last_reason,
            VerificationFailReason::UnsupportedCanonicalization
        );
    }
}
