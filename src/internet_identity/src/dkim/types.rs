//! Type definitions for the DKIM verifier.
//!
//! Mirrors the result-shape design from `docs/ongoing/email-recovery.md`
//! §6.6: a [`DkimVerifyResult`] is the verifier's externally
//! visible verdict, carrying a per-step breakdown so the UI can show
//! *why* a signature failed when one did.

/// Header canonicalisation algorithm per RFC 6376 §3.4.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum HeaderCanon {
    /// Defined in §3.4.2: lowercase header names, unfold continuations,
    /// collapse runs of whitespace to a single SP, strip WSP around the
    /// colon and at the end of values.
    Relaxed,
    /// Defined in §3.4.1: signs the *exact original bytes* of every signed
    /// header line. We reject this — the parsed-pair gateway contract
    /// drops information `simple` requires (see design doc §5.2).
    Simple,
}

/// Body canonicalisation algorithm per RFC 6376 §3.4.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BodyCanon {
    /// §3.4.4: strip trailing whitespace from every line; reduce empty
    /// lines at the end to a single CRLF; remove the final CRLF iff the
    /// body is empty.
    Relaxed,
    /// §3.4.3: identity, except a non-empty body always ends in exactly
    /// one CRLF (any trailing all-empty lines are dropped except one).
    Simple,
}

/// DKIM signing/verification algorithm per RFC 6376 §3.3 + RFC 8463.
///
/// We support the RFC 8624 / RFC 8463 set: rsa-sha256 (the long-time
/// default) and ed25519-sha256 (the modern alternative). RSA-SHA1 was
/// withdrawn by RFC 8301 and is rejected here.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Algorithm {
    RsaSha256,
    Ed25519Sha256,
}

/// One per-step diagnostic emitted by [`verify`] regardless of the
/// overall outcome. Lets a UI render "what went wrong" as a checklist
/// rather than a single opaque error.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DkimCheck {
    pub name: DkimCheckName,
    pub status: DkimCheckStatus,
    /// Human-readable explanation: e.g. RSA signature length mismatch,
    /// the colon is missing in a tag, the body's hash didn't match.
    pub detail: Option<String>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DkimCheckName {
    DkimSignaturePresent,
    SignatureParsed,
    AlgorithmSupported,
    CanonicalizationSupported,
    SignatureNotExpired,
    SignatureNotFromFuture,
    SubjectSigned,
    AuidAlignsWithDomain,
    DnsRecordParsed,
    PublicKeyTypeMatches,
    BodyHashValid,
    SignatureValid,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DkimCheckStatus {
    Pass,
    Fail,
    /// The check is not relevant for this signature — for example a
    /// later-stage check that we never reached because an earlier one
    /// failed.
    Skipped,
}

/// Projection of an `SmtpMessage` exposing only the header values
/// DKIM's `h=` walk bound cryptographically. Returned alongside the
/// `Verified` verdict so post-verification code never has to read
/// the raw message - and so a header DKIM didn't sign (whether
/// duplicated, injected, or simply absent from `h=`) is unreachable
/// by construction.
///
/// **Why this exists.** RFC 6376 §5.4 walks header instances
/// bottom-up to pick which one to hash. A consumer that walks
/// top-down (e.g. `iter().find()`) will pick a different value when
/// a duplicate is present, and any decision rooted in that value
/// would be unsigned. The recovery surface had this exact bug for
/// `Subject` (the recovery-nonce extractor read the top instance
/// while DKIM signed the bottom one). Closing it with a typed view
/// stops the *class* of bug - adding a future reader against
/// `SignedSmtpMessage` automatically inherits "only signed bytes."
///
/// The view does NOT replace the wire-shape duplicate-header reject
/// in `validate_message`: that defence runs earlier and gives the
/// SMTP gateway a normal 555 decline. The view is the deeper
/// architectural layer that holds even if a future change ever
/// weakens the wire check.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SignedSmtpMessage {
    /// `SmtpHeader`s DKIM's bottom-up `h=` walk picked, in walk
    /// order. Same shape as `SmtpMessage::headers` - just the subset
    /// the signature actually covered, with everything else dropped.
    /// The distinct type (rather than re-using `SmtpMessage`) is the
    /// point: it stops a downstream reader from accidentally passing
    /// an unverified `SmtpMessage` where the verifier's projection
    /// is required. A name absent from `h=`, or listed in `h=` but
    /// not present in the message, does not appear. For an
    /// oversigned name (`h=Subject:Subject` with two instances),
    /// both values appear in walk order.
    headers: Vec<internet_identity_interface::internet_identity::types::smtp::SmtpHeader>,
}

impl SignedSmtpMessage {
    /// Build the view from the headers DKIM's bottom-up `h=` walk
    /// picked, in walk order. The only constructor - there is no
    /// setter or builder, so a `SignedSmtpMessage` in hand is always
    /// exactly the projection the verifier produced and cannot be
    /// mutated by downstream code.
    pub(crate) fn from_signed_walk(
        picked: Vec<internet_identity_interface::internet_identity::types::smtp::SmtpHeader>,
    ) -> Self {
        Self { headers: picked }
    }
    /// Empty view - used by callers that need to satisfy a type
    /// signature when DKIM signed nothing (test fixtures, fail-closed
    /// fallbacks). Every `header()` lookup against an empty view
    /// returns `None`, which is the safe answer.
    pub fn empty() -> Self {
        Self {
            headers: Vec::new(),
        }
    }
    /// The value DKIM signed for a single-instance header (RFC 5322
    /// §3.6 caps Subject/From/Date/etc at one). Returns `None` if
    /// the signer's `h=` didn't list this header, or the message
    /// didn't have it - both states mean "DKIM does not vouch for
    /// this header and no authentication decision may rest on it."
    ///
    /// For an oversigned header where the signer covered two
    /// instances, this returns the first value from DKIM's bottom-up
    /// walk; callers that need both should use [`Self::header_values`].
    pub fn header(&self, name: &str) -> Option<&str> {
        self.headers
            .iter()
            .find(|h| h.name.eq_ignore_ascii_case(name))
            .map(|h| h.value.as_str())
    }
    /// All values DKIM signed for `name`, in bottom-up walk order.
    /// Empty if `name` was not in `h=` or not in the message.
    pub fn header_values<'a>(&'a self, name: &str) -> impl Iterator<Item = &'a str> + 'a {
        let name_owned = name.to_string();
        self.headers.iter().filter_map(move |h| {
            if h.name.eq_ignore_ascii_case(&name_owned) {
                Some(h.value.as_str())
            } else {
                None
            }
        })
    }
}

/// Why a signature didn't verify, mapping straight to a UI message.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum VerificationFailReason {
    /// No `DKIM-Signature` header in the message. Distinct from the
    /// other variants so the frontend can render "this provider doesn't
    /// use DKIM" instead of a generic verification-failed message.
    NoSignature,
    /// At least one DKIM-Signature header was malformed (missing required
    /// tag, bad base64, unparseable folding, etc).
    SignatureMalformed(String),
    /// Signature uses an algorithm we don't support (RSA-SHA1, etc).
    UnsupportedAlgorithm(String),
    /// Header canonicalisation `c=simple/...` — see design §5.2 for why
    /// we constrain to relaxed only.
    UnsupportedCanonicalization,
    /// Signature's `x=` field is in the past.
    SignatureExpired,
    /// Signature's `t=` timestamp claims a signing time in the future
    /// beyond the configured clock-skew tolerance. RFC 6376 §3.5
    /// describes `t=` as "the time the signature was created"; a
    /// signature that claims it was signed in the future is either a
    /// misconfigured signer or a manipulated replay.
    SignatureFutureDated,
    /// `h=` doesn't include the `Subject` header. Required by the
    /// email-recovery flow (the challenge nonce lives in `Subject:`,
    /// see design §5.4); a signature that doesn't cover Subject would
    /// let a MITM rewrite the nonce on a legitimately-signed email.
    SubjectNotSigned,
    /// `i=` agent identifier doesn't end in `@d=` or `.d=` (and the DNS
    /// record didn't set the `t=s` flag that would soft-fail this).
    AuidMisaligned,
    /// DKIM TXT record at the queried selector was malformed.
    DnsRecordMalformed(String),
    /// Signature `a=` algorithm doesn't match the DNSKEY's `k=` type.
    AlgorithmKeyTypeMismatch,
    /// RSA key below the configured minimum (1024 bits, see §5.6).
    RsaKeyTooSmall(usize),
    /// Body hash in `bh=` doesn't match the actual body.
    BodyHashMismatch,
    /// Cryptographic signature check failed.
    SignatureInvalid,
    /// DNS record's `t=y` testing flag is set; the verifier treats every
    /// signature from such a record as inconclusive.
    TestingMode,
    /// `From:` header missing, malformed, or carrying more than one
    /// mailbox (RFC 7489 §3.1.1 requires exactly one). The string
    /// carries a parser diagnostic.
    MalformedFromHeader(String),
    /// DMARC TXT record was supplied but couldn't be parsed.
    DmarcMalformed(String),
    /// DKIM signed `d=` doesn't align with the From-header domain
    /// under the published `adkim=` mode (or, when no DMARC record
    /// exists, doesn't equal it exactly).
    DmarcMisaligned,
    /// The message violates RFC 5322 §3.6 (a single-instance header
    /// like `Subject` or `From` appears more than once). RFC 6376
    /// §8.15 explicitly authorises the verifier to refuse such
    /// messages: DKIM's bottom-up choice of which instance to sign
    /// would diverge from any consumer that walks headers top-down,
    /// turning the unsigned instance into an attacker-controlled
    /// channel for authentication-grade decisions.
    ///
    /// In normal operation `validate_message` (interface layer)
    /// catches this earlier with a 555. This reason exists as the
    /// architectural backstop inside the verifier itself.
    MalformedMessage(String),
}

/// DKIM-only verdict, the result of [`super::verify::verify`].
///
/// Per RFC 6376 §5.5 / design §5.5, an email may carry multiple
/// `DKIM-Signature` headers (e.g. original sender + mailing list
/// forwarder). The DKIM verifier accepts the email as soon as *any
/// one* signature passes the cryptographic check.
///
/// The combined DKIM + DMARC verdict (what callers actually want) is
/// [`crate::dmarc::EmailVerificationStatus`], returned by
/// `dmarc::verify_email`.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum DkimVerifyResult {
    /// At least one signature passed.
    Verified {
        /// The `d=` of the signature that verified.
        dkim_domain: String,
        /// Projection of the message exposing only the header values
        /// DKIM's `h=` walk hashed. Post-verification code must read
        /// headers through this view rather than the raw `SmtpMessage`
        /// - see [`SignedSmtpMessage`] for the threat model.
        signed: SignedSmtpMessage,
        /// Per-step checks for the winning signature.
        checks: Vec<DkimCheck>,
    },
    /// No signature passed.
    Unverified {
        /// Best-fit reason from the most-recently-attempted signature.
        reason: VerificationFailReason,
        /// Per-step checks across every signature attempted (each
        /// signature contributes one block of checks; multi-signature
        /// emails produce a flat concatenation).
        checks: Vec<DkimCheck>,
    },
}

impl DkimVerifyResult {
    pub fn is_verified(&self) -> bool {
        matches!(self, DkimVerifyResult::Verified { .. })
    }
}
