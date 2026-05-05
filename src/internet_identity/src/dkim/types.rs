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

/// Why a signature didn't verify, mapping straight to a UI message.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum VerificationFailReason {
    /// No `DKIM-Signature` header in the message.
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
