//! Typestate ladder from API-level `SmtpRequest` (untrusted, decoded
//! from the SMTP gateway) to a fully DKIM-certified, DMARC-aligned
//! [`VerifiedSmtpRequest`].
//!
//! Each stage's outcome has its own inner type whose **shape reflects
//! what got checked**: stage 1 stores §3.6 single-occurrence headers
//! in typed slots (so uniqueness is a type-system invariant, not a
//! runtime predicate); stage 2 wraps the per-signature projections in
//! a `first + rest` struct (so "at least one parsed signature" is a
//! type-system invariant); stage 3 exposes only the
//! signature-bound header view.
//!
//! Every stage's fields are private. The only way to construct
//! [`UnverifiedSmtpRequest`], [`SignedSmtpRequest`], or
//! [`VerifiedSmtpRequest`] is to call the appropriate fallible
//! `TryFrom`, so downstream callers can't hand a partially-validated
//! request to logic that assumes verification ran.
//!
//! # Stages
//!
//! **Stage 1 — [`UnverifiedSmtpRequest`]** (bounds + RFC 5322 §3.6
//! well-formedness).
//!
//! - Bounds (envelope/header/body sizes, recipient count) delegated to
//!   the wire-types crate's `validate_envelope` / `validate_message`.
//! - RFC 5322 §3.6 single-occurrence headers are routed into named
//!   fields of [`unverified::SmtpMessage`]: `From`/`Date` are required
//!   `String` (exactly-once), `Subject`/`Sender`/`Reply-To`/`To`/`Cc`/
//!   `Bcc`/`Message-ID`/`In-Reply-To`/`References` are `Option<String>`
//!   (at-most-once). Everything else (including `DKIM-Signature`,
//!   which §3.6 doesn't classify) lands in `other_headers` in source
//!   order.
//! - Duplicate detection is aggregated: every offending header name is
//!   collected into a single [`MessageError::DuplicateUniqueHeaders`]
//!   instead of failing on the first.
//! - Message remains optional so `smtp_request_validate` (envelope-
//!   only at RCPT TO time) can still produce a stage-1 value.
//!
//! **Stage 2 — [`SignedSmtpRequest`]** (≥1 parsed `DKIM-Signature`).
//!
//! - Walks `other_headers` for `DKIM-Signature` entries (case-
//!   insensitive), parses each into a [`DkimSignature`], and emits one
//!   [`SignedSmtpRequestProjection`] per success.
//! - The `first + rest` shape of [`signed::SmtpRequest`] reflects what
//!   the stage actually checked: at least one signature parsed.
//! - Each projection's *public* surface is the three DKIM tags an
//!   orchestrator needs to fetch the right key and check the right
//!   zone: `signature_domain()` (`d=`), `signature_selector()` (`s=`),
//!   and `signature_algorithm()` (`a=`). Message-level headers stay
//!   hidden — claimed-but-unverified until stage 3.
//! - No canonicalisation happens at stage 2; building the `h=`-scoped
//!   signed view and the `bh=`-canonical body is deferred to stage 3
//!   so cycles aren't spent on signatures that never get tried.
//! - Conversion fails ([`DkimScopeError`]) when no `DKIM-Signature`
//!   header is present, or when every present one fails to parse.
//!
//! **Stage 3 — [`VerifiedSmtpRequest`]** (DKIM signature + DMARC
//! alignment both passed).
//!
//! - Iterates [`SignedSmtpRequest::projections_rev`] (bottom-up per
//!   RFC 6376 §5.4 convention: forwarders prepend signatures, so the
//!   originating-domain signer sits towards the bottom). For each
//!   candidate we call `dkim::run_signature_check`, then check DMARC
//!   alignment via `dmarc::compute_outcome`. First passing candidate
//!   wins.
//! - The verified type's whole public surface is `header(name) ->
//!   Option<&str>` and `body() -> &[u8]`. Both read from the winning
//!   signature's `h=` projection. The signing domain, From-domain,
//!   DMARC outcome and check trail are exposed via crate-private
//!   accessor methods so the DKIM/DMARC test-vector modules can pin
//!   them without touching internal state.

use std::rc::Rc;

use crate::dkim::{parse_dkim_signature, DkimCheck, DkimSignature, VerificationFailReason};
use crate::dmarc::DmarcOutcome;
use internet_identity_interface::internet_identity::types::smtp::{
    smtp_err, validate_envelope, validate_message, validate_message_id,
    SmtpEnvelope as WireSmtpEnvelope, SmtpHeader as WireSmtpHeader, SmtpMessage as WireSmtpMessage,
    SmtpRequest as WireSmtpRequest, SmtpResponse, SMTP_ERR_SYNTAX_ERROR,
};

pub use signed::SignedSmtpRequestProjection;
pub use signed::SmtpRequest as SignedSmtpRequest;
pub use unverified::SmtpRequest as UnverifiedSmtpRequest;
pub use verified::SmtpRequest as VerifiedSmtpRequest;

// =========================================================================
// Stage 1 errors
// =========================================================================
//
// Internal Rust enums (not Candid) — the API-type vs inner-type split
// lets each variant carry exactly what the failure mode requires.
// `From<SmtpError> for SmtpResponse` at the canister boundary collapses
// everything to SMTP 555 with the right user-visible message.

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SmtpError {
    Envelope(EnvelopeError),
    Message(MessageError),
    /// The optional `message_id` correlation id exceeded its length
    /// bound. Pass-through of the wire-types crate's bounds response.
    MessageId(SmtpResponse),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum EnvelopeError {
    /// The wire request didn't carry an envelope at all. Required for
    /// every SMTP request (MAIL FROM / RCPT TO must be set).
    Missing,
    /// Wire-types crate's bounds check rejected the envelope (address
    /// length, recipient count). Passed through so the canister
    /// boundary keeps the wire-types crate's user-visible wording.
    Bounds(SmtpResponse),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum MessageError {
    /// Wire-types crate's bounds check rejected the message (header
    /// count, header name/value lengths, body length).
    Bounds(SmtpResponse),
    /// One or more §3.6 single-occurrence headers appeared more than
    /// once. The vec lists every offender so the SMTP 555 response
    /// can name them all in a single round-trip.
    DuplicateUniqueHeaders(Vec<&'static str>),
    /// A §3.6 exactly-once header (`From` or `Date`) was missing.
    MissingRequired(&'static str),
}

impl From<SmtpError> for SmtpResponse {
    fn from(err: SmtpError) -> SmtpResponse {
        match err {
            SmtpError::Envelope(EnvelopeError::Missing) => {
                smtp_err(SMTP_ERR_SYNTAX_ERROR, "Missing envelope")
            }
            SmtpError::Envelope(EnvelopeError::Bounds(resp))
            | SmtpError::Message(MessageError::Bounds(resp)) => resp,
            SmtpError::Message(MessageError::DuplicateUniqueHeaders(names)) => {
                let joined = names
                    .iter()
                    .map(|n| format!("'{n}'"))
                    .collect::<Vec<_>>()
                    .join(", ");
                smtp_err(
                    SMTP_ERR_SYNTAX_ERROR,
                    format!("RFC 5322 §3.6: header(s) must appear at most once: {joined}"),
                )
            }
            SmtpError::Message(MessageError::MissingRequired(name)) => smtp_err(
                SMTP_ERR_SYNTAX_ERROR,
                format!("RFC 5322 §3.6: header '{name}' must appear exactly once"),
            ),
            SmtpError::MessageId(resp) => resp,
        }
    }
}

// =========================================================================
// Stage 1 — UnverifiedSmtpRequest
// =========================================================================

pub mod unverified {
    use super::WireSmtpEnvelope;
    use serde_bytes::ByteBuf;

    /// A header that wasn't routed into one of the §3.6 typed slots.
    /// Same wire shape as the interface crate's `SmtpHeader`, but
    /// local to this module so the rest of the canister can't
    /// accidentally hand the typestate a Candid-decoded value that
    /// hasn't passed the stage-1 builder.
    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct SmtpHeader {
        pub name: String,
        pub value: String,
    }

    /// Stage-1 message. RFC 5322 §3.6 single-occurrence headers are
    /// destructured into typed fields; duplicate/missing checks are
    /// type-system invariants rather than runtime predicates.
    ///
    /// Built via [`SmtpMessageBuilder`]; no other constructor exists
    /// (fields are private).
    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct SmtpMessage {
        from: String,
        date: String,
        subject: Option<String>,
        sender: Option<String>,
        reply_to: Option<String>,
        to: Option<String>,
        cc: Option<String>,
        bcc: Option<String>,
        message_id: Option<String>,
        in_reply_to: Option<String>,
        references: Option<String>,
        /// Headers not in the §3.6 set, in message source order.
        /// **Includes `DKIM-Signature`** — DKIM isn't §3.6, so stage 1
        /// doesn't classify it specially. Stage 2 walks this slice to
        /// find every signature header.
        other_headers: Vec<SmtpHeader>,
        body: ByteBuf,
    }

    impl SmtpMessage {
        /// Value of the `From:` header. Named `mail_from` rather than
        /// `from` to avoid clippy's `wrong_self_convention` lint (the
        /// Rust `From` trait's `from()` constructor takes no `self`).
        /// Not the same as the SMTP envelope's `MAIL FROM` command —
        /// the typestate's stage-1 layer doesn't carry the envelope.
        pub fn mail_from(&self) -> &str {
            &self.from
        }
        pub fn date(&self) -> &str {
            &self.date
        }
        pub fn subject(&self) -> Option<&str> {
            self.subject.as_deref()
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
        pub fn other_headers(&self) -> &[SmtpHeader] {
            &self.other_headers
        }
        pub fn body(&self) -> &[u8] {
            &self.body
        }
    }

    /// Builder for [`SmtpMessage`]. Routes each header to a typed slot
    /// or into `other_headers`. On a duplicate-§3.6 header, the existing
    /// slot value is kept and a [`BuilderError::Duplicate`] is returned
    /// so the caller can keep walking and aggregate every offender into
    /// a single error.
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
        other_headers: Vec<SmtpHeader>,
        body: Option<ByteBuf>,
    }

    /// Why `add_header` rejected one entry.
    #[derive(Clone, Debug, Eq, PartialEq)]
    pub enum BuilderError {
        /// A §3.6 single-occurrence header was supplied a second time.
        Duplicate(&'static str),
    }

    /// Why `build()` refused to produce a message.
    #[derive(Clone, Debug, Eq, PartialEq)]
    pub enum BuildError {
        /// A §3.6 exactly-once header (`From` or `Date`) was missing.
        MissingRequired(&'static str),
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
            self.other_headers.push(header);
            Ok(())
        }

        pub fn build(self) -> Result<SmtpMessage, BuildError> {
            Ok(SmtpMessage {
                from: self.from.ok_or(BuildError::MissingRequired("From"))?,
                date: self.date.ok_or(BuildError::MissingRequired("Date"))?,
                subject: self.subject,
                sender: self.sender,
                reply_to: self.reply_to,
                to: self.to,
                cc: self.cc,
                bcc: self.bcc,
                message_id: self.message_id,
                in_reply_to: self.in_reply_to,
                references: self.references,
                other_headers: self.other_headers,
                body: self.body.unwrap_or_default(),
            })
        }
    }

    /// Stage-1 wrapper — the bounds-checked form of the whole inbound
    /// request. `message` is `None` for envelope-only requests
    /// (`smtp_request_validate` at RCPT TO time) and `Some` for full
    /// requests. The envelope is retained (still the wire shape, only
    /// bounds-validated) because recipient dispatch reads it *after*
    /// stage 1 runs; `message_id` rides along for diagnostics.
    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct SmtpRequest {
        pub(super) envelope: WireSmtpEnvelope,
        pub(super) message: Option<SmtpMessage>,
        pub(super) message_id: Option<String>,
    }

    impl SmtpRequest {
        pub fn envelope(&self) -> &WireSmtpEnvelope {
            &self.envelope
        }

        pub fn message(&self) -> Option<&SmtpMessage> {
            self.message.as_ref()
        }

        pub fn message_id(&self) -> Option<&str> {
            self.message_id.as_deref()
        }

        pub(super) fn take_message(self) -> Option<SmtpMessage> {
            self.message
        }
    }
}

impl TryFrom<WireSmtpRequest> for UnverifiedSmtpRequest {
    type Error = SmtpError;

    fn try_from(request: WireSmtpRequest) -> Result<Self, SmtpError> {
        let WireSmtpRequest {
            envelope,
            message,
            gateway_flags: _,
            message_id,
        } = request;

        let envelope = envelope.ok_or(SmtpError::Envelope(EnvelopeError::Missing))?;
        validate_envelope(&envelope).map_err(|e| SmtpError::Envelope(EnvelopeError::Bounds(e)))?;
        validate_message_id(message_id.as_deref()).map_err(SmtpError::MessageId)?;

        let message = match message {
            None => None,
            Some(m) => Some(build_typed_message(m)?),
        };

        Ok(UnverifiedSmtpRequest {
            envelope,
            message,
            message_id,
        })
    }
}

fn build_typed_message(wire: WireSmtpMessage) -> Result<unverified::SmtpMessage, SmtpError> {
    validate_message(&wire).map_err(|e| SmtpError::Message(MessageError::Bounds(e)))?;

    let mut builder = unverified::SmtpMessageBuilder::new();
    let mut duplicates: Vec<&'static str> = Vec::new();

    for header in wire.headers {
        match builder.add_header(unverified::SmtpHeader {
            name: header.name,
            value: header.value,
        }) {
            Ok(()) => {}
            Err(unverified::BuilderError::Duplicate(name)) => {
                if !duplicates.contains(&name) {
                    duplicates.push(name);
                }
            }
        }
    }

    if !duplicates.is_empty() {
        return Err(SmtpError::Message(MessageError::DuplicateUniqueHeaders(
            duplicates,
        )));
    }

    builder.body(wire.body).build().map_err(|e| match e {
        unverified::BuildError::MissingRequired(name) => {
            SmtpError::Message(MessageError::MissingRequired(name))
        }
    })
}

// =========================================================================
// Stage 2 — SignedSmtpRequest
// =========================================================================

pub mod signed {
    use std::rc::Rc;

    use super::unverified;
    use crate::dkim::{Algorithm, DkimSignature};

    /// Stage-2 outcome. The "≥1 parsed signature" invariant is
    /// expressed in the type itself (`first + rest`) rather than as a
    /// runtime check on a wrapped `Vec`.
    #[derive(Clone, Debug)]
    pub struct SmtpRequest {
        first: SignedSmtpRequestProjection,
        rest: Vec<SignedSmtpRequestProjection>,
    }

    impl SmtpRequest {
        pub(super) fn new(
            first: SignedSmtpRequestProjection,
            rest: Vec<SignedSmtpRequestProjection>,
        ) -> Self {
            Self { first, rest }
        }

        /// Walk projections in source order.
        pub fn projections(&self) -> impl DoubleEndedIterator<Item = &SignedSmtpRequestProjection> {
            std::iter::once(&self.first).chain(self.rest.iter())
        }
    }

    /// One signature's claimed projection of the request. Carries the
    /// parsed `DkimSignature` plus a shared owning reference to the
    /// stage-1 typed message so stage 3 can canonicalise the
    /// `h=`-listed headers once verification picks a winning candidate.
    ///
    /// **Stage 2 deliberately does NOT expose message-level headers.**
    /// `From`, `Subject`, etc. are claimed-but-unverified at this
    /// point; the type wall is what stops a downstream reader from
    /// accidentally trusting them before stage 3 has bound them with a
    /// cryptographic signature check.
    #[derive(Clone, Debug)]
    pub struct SignedSmtpRequestProjection {
        message: Rc<unverified::SmtpMessage>,
        signature_header_value: String,
        signature: DkimSignature,
    }

    impl SignedSmtpRequestProjection {
        pub(super) fn new(
            message: Rc<unverified::SmtpMessage>,
            signature_header_value: String,
            signature: DkimSignature,
        ) -> Self {
            Self {
                message,
                signature_header_value,
                signature,
            }
        }

        /// The signature's `d=` tag — the signing domain. Lowercased
        /// per `parse_dkim_signature`.
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

        /// Crate-private: the full parsed `DKIM-Signature`. Exposed so
        /// the canister-side DNSSEC partial-verification step (which
        /// runs everything except the signature math, because the
        /// public key isn't available until the FE submits the DKIM
        /// leaf) can compute the body hash and canonical signed-headers
        /// digest.
        pub(crate) fn parsed_signature(&self) -> &DkimSignature {
            &self.signature
        }

        /// Crate-private: the original (gateway-supplied) bytes of the
        /// `DKIM-Signature` header value. Needed so RFC 6376 §3.7's
        /// "blank the `b=` tag value while preserving surrounding
        /// bytes" step can run against the exact bytes the signer
        /// hashed.
        pub(crate) fn signature_header_raw(&self) -> &str {
            &self.signature_header_value
        }

        /// Crate-private: the stage-1 typed message this signature
        /// claims to cover.
        pub(crate) fn message(&self) -> &unverified::SmtpMessage {
            &self.message
        }
    }
}

/// Stage 2 failure: no `DKIM-Signature` header was present, or every
/// `DKIM-Signature` header failed to parse. The canister boundary
/// collapses both cases to SMTP 555 via `From<DkimScopeError> for
/// SmtpResponse`.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum DkimScopeError {
    /// No `DKIM-Signature` header present in the message (or the
    /// stage-1 message was absent — caller bug, since the canister
    /// boundary returns early for envelope-only requests before stage
    /// 2 runs).
    NoSignature,
    /// At least one `DKIM-Signature` was present, but every one of
    /// them failed to parse. The vec lists per-header parse
    /// diagnostics for telemetry.
    AllFailedToParse(Vec<String>),
}

impl From<DkimScopeError> for SmtpResponse {
    fn from(err: DkimScopeError) -> SmtpResponse {
        match err {
            DkimScopeError::NoSignature => {
                smtp_err(SMTP_ERR_SYNTAX_ERROR, "no DKIM-Signature header present")
            }
            DkimScopeError::AllFailedToParse(diagnostics) => smtp_err(
                SMTP_ERR_SYNTAX_ERROR,
                format!(
                    "every DKIM-Signature header failed to parse: {}",
                    diagnostics.join("; ")
                ),
            ),
        }
    }
}

impl TryFrom<UnverifiedSmtpRequest> for SignedSmtpRequest {
    type Error = DkimScopeError;

    fn try_from(unverified: UnverifiedSmtpRequest) -> Result<Self, DkimScopeError> {
        let Some(message) = unverified.take_message() else {
            return Err(DkimScopeError::NoSignature);
        };

        let mut parsed: Vec<(String, DkimSignature)> = Vec::new();
        let mut diagnostics: Vec<String> = Vec::new();
        let mut any_seen = false;
        for header in message.other_headers() {
            if !header.name.eq_ignore_ascii_case("DKIM-Signature") {
                continue;
            }
            any_seen = true;
            match parse_dkim_signature(&header.value) {
                Ok(sig) => parsed.push((header.value.clone(), sig)),
                Err(e) => diagnostics.push(e),
            }
        }

        if parsed.is_empty() {
            return Err(if any_seen {
                DkimScopeError::AllFailedToParse(diagnostics)
            } else {
                DkimScopeError::NoSignature
            });
        }

        let message = Rc::new(message);
        let mut iter = parsed.into_iter();
        let (first_hv, first_sig) = iter.next().expect("non-empty: checked above");
        let first =
            signed::SignedSmtpRequestProjection::new(Rc::clone(&message), first_hv, first_sig);
        let rest: Vec<_> = iter
            .map(|(hv, sig)| signed::SignedSmtpRequestProjection::new(Rc::clone(&message), hv, sig))
            .collect();
        Ok(signed::SmtpRequest::new(first, rest))
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

pub mod verified {
    use super::WireSmtpHeader;
    use crate::dkim::DkimCheck;
    use crate::dmarc::DmarcOutcome;

    /// Cryptographically certified, DMARC-aligned email. Stage-3
    /// output — the only shape downstream callers should consume.
    ///
    /// Field-level reads happen via accessor methods only; the
    /// struct's fields are private so a future caller can't reach in
    /// and pull a raw header value that wasn't covered by the winning
    /// signature.
    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct SmtpRequest {
        /// Subset of the message headers covered by the winning
        /// signature's `h=` list, in message source order. `header()`
        /// walks this bottom-up and returns the bytes the first `h=`
        /// entry consumed (RFC 6376 §5.4 selection).
        pub(super) signed_headers: Vec<WireSmtpHeader>,
        pub(super) body: Vec<u8>,
        pub(super) dkim_domain: String,
        pub(super) from_domain: String,
        pub(super) dmarc_outcome: DmarcOutcome,
        pub(super) checks: Vec<DkimCheck>,
    }

    impl SmtpRequest {
        /// The value DKIM signed for `name` under the winning
        /// signature. `None` if the signer's `h=` didn't list it, or
        /// the message didn't have it — both mean "DKIM does not vouch
        /// for this header." Comparison on `name` is case-insensitive.
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

        /// Crate-private: the winning signature's `d=`. Exposed for
        /// the dkim/dmarc test-vector modules' assertions; downstream
        /// code reads message content via `header()` / `body()`
        /// instead.
        pub(crate) fn dkim_domain(&self) -> &str {
            &self.dkim_domain
        }

        /// Crate-private: From-domain extracted from the signed view.
        /// `mail_from_domain` rather than `from_domain` to dodge
        /// clippy's `wrong_self_convention` lint on `from_*` names.
        pub(crate) fn mail_from_domain(&self) -> &str {
            &self.from_domain
        }

        /// Crate-private: DMARC outcome (Aligned / NoRecord / etc).
        pub(crate) fn dmarc_outcome(&self) -> &DmarcOutcome {
            &self.dmarc_outcome
        }

        /// Crate-private: per-step DKIM checks for the winning
        /// signature.
        pub(crate) fn checks(&self) -> &[DkimCheck] {
            &self.checks
        }
    }
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

impl<'a> TryFrom<(SignedSmtpRequest, &VerificationContext<'a>)> for VerifiedSmtpRequest {
    type Error = VerificationError;

    fn try_from(
        (signed, ctx): (SignedSmtpRequest, &VerificationContext<'a>),
    ) -> Result<Self, VerificationError> {
        let mut per_signature: Vec<(usize, VerificationFailReason, Vec<DkimCheck>)> = Vec::new();
        let mut combined_checks: Vec<DkimCheck> = Vec::new();

        // Bottom-up. RFC 6376 §5.4 selection convention: forwarders
        // prepend signatures, so the originating-domain signer sits
        // towards the bottom.
        let candidates: Vec<&SignedSmtpRequestProjection> = signed.projections().rev().collect();
        for (attempt_idx, proj) in candidates.into_iter().enumerate() {
            match verify_signature(proj, ctx.dkim_txt, ctx.now_secs) {
                Ok((from_domain, checks)) => {
                    let dkim_domain = proj.parsed_signature().d.clone();
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
    let headers = reconstruct_wire_headers(message, proj.signature_header_raw());
    let checks = crate::dkim::run_signature_check(
        proj.parsed_signature(),
        &headers,
        message.body(),
        proj.signature_header_raw(),
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
/// occurrence order, then `other_headers` (with the target
/// DKIM-Signature pulled to the end so RFC 6376 §3.7's b=-blanking
/// step lands on the right entry).
///
/// `pub(crate)` so the DNSSEC-path partial-verification step (which
/// runs the DKIM signed-headers digest without the public key) can
/// reuse exactly the same reconstruction the DoH-path's stage 3 uses.
pub(crate) fn reconstruct_wire_headers(
    message: &unverified::SmtpMessage,
    target_signature_value: &str,
) -> Vec<WireSmtpHeader> {
    let mut out: Vec<WireSmtpHeader> = Vec::new();
    push_field(&mut out, "From", Some(message.mail_from()));
    push_field(&mut out, "Date", Some(message.date()));
    push_field(&mut out, "Subject", message.subject());
    push_field(&mut out, "Sender", message.sender());
    push_field(&mut out, "Reply-To", message.reply_to());
    push_field(&mut out, "To", message.to());
    push_field(&mut out, "Cc", message.cc());
    push_field(&mut out, "Bcc", message.bcc());
    push_field(&mut out, "Message-ID", message.message_id());
    push_field(&mut out, "In-Reply-To", message.in_reply_to());
    push_field(&mut out, "References", message.references());

    // Non-target `other_headers` in source order. The target
    // DKIM-Signature is held back for the final push so the
    // bottom-up b=-blanking lookup picks it deterministically.
    let mut target_seen = false;
    for header in message.other_headers() {
        if !target_seen
            && header.name.eq_ignore_ascii_case("DKIM-Signature")
            && header.value == target_signature_value
        {
            target_seen = true;
            continue;
        }
        out.push(WireSmtpHeader {
            name: header.name.clone(),
            value: header.value.clone(),
        });
    }
    out.push(WireSmtpHeader {
        name: "DKIM-Signature".into(),
        value: target_signature_value.to_string(),
    });
    out
}

fn push_field(out: &mut Vec<WireSmtpHeader>, name: &str, value: Option<&str>) {
    if let Some(v) = value {
        out.push(WireSmtpHeader {
            name: name.to_string(),
            value: v.to_string(),
        });
    }
}

/// Extract the From-domain from the typed message's `from` field.
fn from_domain_of(message: &unverified::SmtpMessage) -> Result<String, String> {
    crate::dmarc::extract_from_value(message.mail_from())
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
    let headers = reconstruct_wire_headers(message, proj.signature_header_raw());

    let mut consumed = vec![false; headers.len()];
    let mut selected: Vec<usize> = Vec::with_capacity(proj.parsed_signature().h.len());
    for h_name in &proj.parsed_signature().h {
        for (idx, hdr) in headers.iter().enumerate().rev() {
            if !consumed[idx] && hdr.name.eq_ignore_ascii_case(h_name) {
                consumed[idx] = true;
                selected.push(idx);
                break;
            }
        }
    }
    selected.sort_unstable();
    let signed_headers: Vec<WireSmtpHeader> = selected
        .into_iter()
        .map(|idx| headers[idx].clone())
        .collect();

    verified::SmtpRequest {
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
    use crate::dkim::Algorithm;
    use internet_identity_interface::internet_identity::types::smtp::{
        SmtpAddress, SmtpEnvelope, SmtpMessage as TestWireMessage, SmtpRequest as TestWireRequest,
    };
    use serde_bytes::ByteBuf;

    fn addr(user: &str, domain: &str) -> SmtpAddress {
        SmtpAddress {
            user: user.into(),
            domain: domain.into(),
        }
    }

    fn header(name: &str, value: &str) -> WireSmtpHeader {
        WireSmtpHeader {
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
            message_id: None,
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
            message_id: None,
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
    fn stage1_carries_envelope_and_message_id() {
        let mut req = well_formed_request();
        req.message_id = Some("<id@gateway.example>".into());
        let unverified = UnverifiedSmtpRequest::try_from(req).expect("well-formed must pass");
        assert_eq!(unverified.envelope().to.len(), 1);
        assert_eq!(unverified.message_id(), Some("<id@gateway.example>"));
    }

    #[test]
    fn stage1_rejects_oversize_message_id() {
        use internet_identity_interface::internet_identity::types::smtp::MAX_MESSAGE_ID_BYTES;
        let mut req = well_formed_request();
        req.message_id = Some("x".repeat(MAX_MESSAGE_ID_BYTES + 1));
        let err = UnverifiedSmtpRequest::try_from(req).unwrap_err();
        assert!(matches!(err, SmtpError::MessageId(_)));
    }

    #[test]
    fn stage1_well_formed_request_succeeds() {
        let unverified =
            UnverifiedSmtpRequest::try_from(well_formed_request()).expect("well-formed must pass");
        let msg = unverified.message().unwrap();
        assert_eq!(msg.mail_from(), "alice@example.com");
        assert_eq!(msg.subject(), Some("II-Recovery-deadbeefcafe1234"));
        // DKIM-Signature lives in other_headers — stage 1 doesn't
        // route it specially because §3.6 doesn't classify it.
        assert_eq!(
            msg.other_headers()
                .iter()
                .filter(|h| h.name.eq_ignore_ascii_case("DKIM-Signature"))
                .count(),
            1
        );
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
        assert!(matches!(
            err,
            SmtpError::Message(MessageError::DuplicateUniqueHeaders(ref names))
                if names.contains(&"From")
        ));
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
        assert!(matches!(
            err,
            SmtpError::Message(MessageError::MissingRequired("From"))
        ));
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
        assert!(matches!(
            err,
            SmtpError::Message(MessageError::DuplicateUniqueHeaders(ref names))
                if names.contains(&"Subject")
        ));
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
        assert!(matches!(
            err,
            SmtpError::Message(MessageError::DuplicateUniqueHeaders(ref names))
                if names.contains(&"Date")
        ));
    }

    #[test]
    fn stage1_rejects_duplicate_message_id() {
        let mut req = well_formed_request();
        let msg = req.message.as_mut().unwrap();
        msg.headers.push(header("Message-ID", "<a@example.com>"));
        msg.headers.push(header("Message-ID", "<b@example.com>"));
        let err = UnverifiedSmtpRequest::try_from(req).unwrap_err();
        assert!(matches!(
            err,
            SmtpError::Message(MessageError::DuplicateUniqueHeaders(ref names))
                if names.contains(&"Message-ID")
        ));
    }

    #[test]
    fn stage1_aggregates_multiple_duplicates() {
        // Two distinct §3.6 violations in one message — both names
        // must appear in the single aggregated error so the SMTP 555
        // response can name every offender in one round-trip.
        let mut req = well_formed_request();
        let msg = req.message.as_mut().unwrap();
        msg.headers.push(header("From", "evil@elsewhere.example"));
        msg.headers.push(header("Subject", "another subject"));
        let err = UnverifiedSmtpRequest::try_from(req).unwrap_err();
        match err {
            SmtpError::Message(MessageError::DuplicateUniqueHeaders(names)) => {
                assert!(names.contains(&"From"));
                assert!(names.contains(&"Subject"));
            }
            other => panic!("expected DuplicateUniqueHeaders, got {other:?}"),
        }
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
    fn stage1_accepts_message_without_subject() {
        // Subject is at-most-once, not exactly-once (RFC 5322 §3.6).
        let mut req = well_formed_request();
        req.message
            .as_mut()
            .unwrap()
            .headers
            .retain(|h| !h.name.eq_ignore_ascii_case("Subject"));
        let unverified =
            UnverifiedSmtpRequest::try_from(req).expect("Subject-less message must pass stage 1");
        assert_eq!(unverified.message().unwrap().subject(), None);
    }

    #[test]
    fn stage1_accepts_message_without_dkim_signature() {
        // Stage 1 enforces §3.6 only; DKIM-Signature isn't §3.6.
        // The missing-signature case surfaces in stage 2.
        let mut req = well_formed_request();
        req.message
            .as_mut()
            .unwrap()
            .headers
            .retain(|h| !h.name.eq_ignore_ascii_case("DKIM-Signature"));
        UnverifiedSmtpRequest::try_from(req).expect("stage 1 doesn't enforce DKIM-Signature");
    }

    #[test]
    fn stage1_propagates_bounds_failure() {
        let req = TestWireRequest {
            envelope: None,
            message: None,
            gateway_flags: None,
            message_id: None,
        };
        let err = UnverifiedSmtpRequest::try_from(req).unwrap_err();
        assert!(matches!(err, SmtpError::Envelope(EnvelopeError::Missing)));
    }

    #[test]
    fn stage1_duplicate_detection_is_case_insensitive() {
        let mut req = well_formed_request();
        req.message
            .as_mut()
            .unwrap()
            .headers
            .push(header("from", "another@example.com"));
        let err = UnverifiedSmtpRequest::try_from(req).unwrap_err();
        assert!(matches!(
            err,
            SmtpError::Message(MessageError::DuplicateUniqueHeaders(ref names))
                if names.contains(&"From")
        ));
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
        let signed: SignedSmtpRequest = unverified.try_into().unwrap();
        let projections: Vec<&SignedSmtpRequestProjection> = signed.projections().collect();
        assert_eq!(projections.len(), 2);
        assert_eq!(projections[0].signature_selector(), "mail");
        assert_eq!(projections[1].signature_selector(), "other");
    }

    #[test]
    fn stage2_fails_when_no_dkim_signature_present() {
        // Stage 1 no longer requires DKIM-Signature; absence
        // surfaces in stage 2.
        let mut req = well_formed_request();
        req.message
            .as_mut()
            .unwrap()
            .headers
            .retain(|h| !h.name.eq_ignore_ascii_case("DKIM-Signature"));
        let unverified = UnverifiedSmtpRequest::try_from(req).unwrap();
        let err = SignedSmtpRequest::try_from(unverified).expect_err("expected stage 2 to fail");
        assert!(matches!(err, DkimScopeError::NoSignature));
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
        let err = SignedSmtpRequest::try_from(unverified).expect_err("expected stage 2 to fail");
        match err {
            DkimScopeError::AllFailedToParse(diagnostics) => {
                assert_eq!(diagnostics.len(), 2);
            }
            other => panic!("expected AllFailedToParse, got {other:?}"),
        }
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
        let signed: SignedSmtpRequest = unverified.try_into().unwrap();
        let projections: Vec<&SignedSmtpRequestProjection> = signed.projections().collect();
        assert_eq!(projections.len(), 1);
        assert_eq!(projections[0].signature_domain(), "example.com");
    }

    #[test]
    fn stage2_exposes_only_signature_tags() {
        let unverified = UnverifiedSmtpRequest::try_from(well_formed_request()).unwrap();
        let signed: SignedSmtpRequest = unverified.try_into().unwrap();
        let p = signed.projections().next().unwrap();
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
        let signed: SignedSmtpRequest = unverified.try_into().unwrap();
        let ctx = VerificationContext {
            dkim_txt: "v=DKIM1; p=YWJj",
            dmarc_txt: None,
            now_secs: 1_700_000_000,
        };
        let err =
            VerifiedSmtpRequest::try_from((signed, &ctx)).expect_err("expected verification fail");
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
        let signed: SignedSmtpRequest = unverified.try_into().unwrap();
        let projections: Vec<&SignedSmtpRequestProjection> = signed.projections().collect();
        assert_eq!(projections[0].signature_selector(), "forwarder");
        assert_eq!(projections[1].signature_selector(), "mail");

        let ctx = VerificationContext {
            dkim_txt: "v=DKIM1; p=YWJj",
            dmarc_txt: None,
            now_secs: 1_700_000_000,
        };
        let err = VerifiedSmtpRequest::try_from((signed, &ctx)).expect_err("expected fail");
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
