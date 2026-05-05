//! Type definitions for the DMARC alignment check.
//!
//! Mirrors `docs/ongoing/email-recovery.md` §6.6. The canister-side
//! verifier accepts a message only when [`DmarcOutcome`] is `Aligned`
//! *or* `NoRecord` with the DKIM `d=` matching the From-header domain
//! exactly (i.e. when the lack of a published DMARC record can still
//! be inferred from a coincidentally-aligned signature). Misaligned
//! mail is rejected outright; there is no "spoofing suspected" middle
//! state because the call has no value if the proof isn't a usable one.

/// DMARC published policy (`p=` tag, RFC 7489 §6.3).
///
/// We track the signer's stated intent so the failure path can
/// surface an informational reason ("the sending domain says this
/// kind of misaligned mail should be rejected"), but the canister's
/// recovery flow rejects misaligned mail regardless of policy. The
/// PoC postbox UI may still want the distinction.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DmarcPolicy {
    /// `p=none` — the domain owner asks receivers not to act on
    /// failed alignment, only to report.
    None,
    /// `p=quarantine` — failed mail should land in spam.
    Quarantine,
    /// `p=reject` — failed mail should be discarded outright.
    Reject,
}

/// DMARC alignment mode (`adkim=` tag, RFC 7489 §3.1.2).
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum AlignmentMode {
    /// `s` — DKIM `d=` must equal the From-header domain byte-for-byte
    /// (after ASCII-lowercasing).
    Strict,
    /// `r` — DKIM `d=` must equal the From-header domain or be a
    /// subdomain of it (or the From-header domain be a subdomain of
    /// the DKIM `d=`). Note: this is *stricter* than RFC-7489-compliant
    /// `adkim=r` which uses the Public Suffix List to compute the
    /// "organizational domain" — see design doc §6.4 for why we
    /// deliberately don't consult the PSL.
    Relaxed,
}

impl Default for AlignmentMode {
    /// `adkim=r` is the RFC default when the tag is absent.
    fn default() -> Self {
        AlignmentMode::Relaxed
    }
}

/// A parsed DMARC record. We track only the tags we honour at the
/// verifier; reporting tags (`fo=` / `rua=` / `ruf=` / `rf=`) are
/// accepted at parse time and dropped here.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DmarcRecord {
    /// `p=` — required, applies to the queried domain.
    pub policy: DmarcPolicy,
    /// `sp=` — applies to subdomains of the queried domain. Inherits
    /// from `p=` when absent.
    pub subdomain_policy: DmarcPolicy,
    /// `adkim=` — DKIM alignment mode.
    pub adkim: AlignmentMode,
    /// `aspf=` — SPF alignment mode. Tracked for completeness but
    /// not enforced; we don't check SPF (design doc §6.5).
    pub aspf: AlignmentMode,
    /// `pct=` — fraction of failing mail to which `p=` applies, 0–100.
    /// Defaults to 100 when absent.
    pub pct: u8,
}

/// Verifier output for the DMARC alignment check.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum DmarcOutcome {
    /// The DKIM-signed `d=` aligns with the From-header domain under
    /// the published `adkim=` mode.
    Aligned {
        policy: DmarcPolicy,
        alignment_mode: AlignmentMode,
    },
    /// A DMARC record exists, but the DKIM-signed `d=` does NOT align
    /// with the From-header domain. The message is rejected; the
    /// embedded policy is informational (so a UI can say "the sending
    /// domain says misaligned mail should be rejected").
    Misaligned { policy: DmarcPolicy },
    /// No DMARC record was supplied (caller passed `None`).
    NoRecord,
    /// A DMARC record was supplied but couldn't be parsed.
    Malformed(String),
}

/// Combined DKIM + DMARC verifier verdict — the public top-level
/// result of [`super::verify::verify_email`].
///
/// `Verified` carries the per-step DKIM checks plus the DMARC
/// outcome so a caller (or a reviewer-facing UI) can render the full
/// audit trail. `Unverified` collapses to a single best-fit reason
/// from whichever stage actually failed (DKIM signature, From-header
/// parsing, DMARC alignment, ...).
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum EmailVerificationStatus {
    /// DKIM passed and the DKIM `d=` aligns with the From-header
    /// domain (per DMARC, or de-facto when no DMARC record).
    Verified {
        /// The `d=` of the DKIM signature that verified.
        dkim_domain: String,
        /// The lowercased domain extracted from the `From:` header.
        from_domain: String,
        /// DMARC outcome — `Aligned` carries the published policy +
        /// mode, `NoRecord` indicates the caller didn't supply one.
        dmarc: DmarcOutcome,
        /// Per-step DKIM checks for the winning signature.
        checks: Vec<crate::dkim::DkimCheck>,
    },
    /// Either DKIM failed, the From-header was malformed, the DMARC
    /// record was malformed, or alignment failed.
    Unverified {
        /// Best-fit reason from the most-recent stage that failed.
        reason: crate::dkim::VerificationFailReason,
        /// Per-step DKIM checks across every signature attempted.
        checks: Vec<crate::dkim::DkimCheck>,
    },
}

impl EmailVerificationStatus {
    pub fn is_verified(&self) -> bool {
        matches!(self, EmailVerificationStatus::Verified { .. })
    }
}
