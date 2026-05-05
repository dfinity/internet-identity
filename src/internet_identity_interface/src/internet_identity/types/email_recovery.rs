//! Types for the email-based identity recovery flow.
//!
//! See `docs/ongoing/email-recovery.md` (PR #3836) for the full design.
//! At a high level:
//!
//! - A user binds an email address to an anchor via the *setup* flow
//!   (`email_recovery_credential_prepare_add` + a verified email to
//!   `register@id.ai`).
//! - When they later lose their other authn methods, they trigger the
//!   *recovery* flow (`email_recovery_prepare_delegation` + a verified
//!   email to `recover@id.ai`) and end up with a delegation bound to a
//!   fresh session keypair.
//!
//! Both flows hinge on the canister verifying a DKIM-signed email
//! whose DKIM key it trusts. The trust comes from one of two paths:
//!
//! 1. **DoH allowlist** — for major mailbox providers (Gmail, Outlook,
//!    iCloud, …) the canister fetches the DKIM TXT via a 3-of-5 quorum
//!    across independent DoH providers (`crate::doh::fetch_txt`).
//!    Restricted to a deploy-arg allowlist. **This is the only path
//!    supported in the initial cut.**
//! 2. **DNSSEC** *(deferred to a follow-up PR)* — the FE walks the
//!    DNSSEC chain from root to the DKIM TXT, the canister validates
//!    the chain synchronously at prepare time. Default for any zone
//!    that publishes DNSSEC, once shipped. The Candid type below
//!    leaves room for this variant.

use crate::internet_identity::types::{DnsProofBundle, Timestamp, UserKey};
use candid::{CandidType, Deserialize};
use serde::Serialize;
use serde_bytes::ByteBuf;

/// One bound recovery email, owned by a single anchor.
///
/// The credential lives directly on the anchor struct rather than in
/// a separate stable map: anchor storage uses `minicbor-derive`, which
/// is forward-compatible across optional-field additions. Old anchors
/// deserialize with `email_recovery: None`; no migration needed.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq, Serialize)]
pub struct EmailRecoveryCredential {
    /// Lowercased canonical form: `lowercase(local-part) + "@" +
    /// lowercase(domain)`. Stored verbatim (not hashed) so the user
    /// can see it in the management UI — exactly what they typed at
    /// registration.
    pub address: String,

    /// Nanoseconds since the Unix epoch (matches the rest of II's
    /// `Timestamp` field encoding).
    pub created_at: Timestamp,
    /// Same encoding as `created_at`; `None` until the credential
    /// is actually used to authorise a recovery.
    pub last_used: Option<Timestamp>,
}

/// Identifier the canister hands to the FE when it issues a challenge.
///
/// The same `nonce` the FE displays to the user (and the user types
/// into `Subject:`) is also the lookup key the canister uses to
/// match the inbound email and the FE uses to poll for status. There
/// is no separate `challenge_id`.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq, Serialize)]
pub struct EmailRecoveryChallenge {
    /// Human-typeable token the user must include verbatim in the
    /// `Subject:` of their challenge email. Format: `II-Recovery-`
    /// followed by 16 lowercase hex characters (8 random bytes from
    /// a canister-side ChaCha20Rng PRNG seeded once via `raw_rand`).
    pub nonce: String,
    /// Where the user should send the email — `register@id.ai` for
    /// setup, `recover@id.ai` for recovery. The canister identifies
    /// the challenge from the `nonce` in the `Subject:`, not from
    /// the recipient address.
    pub mailbox: String,
    /// Nanoseconds since the Unix epoch. 30 minutes after issue.
    pub expires_at: Timestamp,
}

/// What the FE submits at prepare time.
///
/// The FE never decides which verification path to use — it just
/// passes the address and selector it discovered via DoH probing.
/// The canister picks the path:
///
/// - If the FE was able to walk the DNSSEC delegation chain to root
///   for this domain, a follow-up PR will let it pass the resulting
///   bundle as an additional optional field on this record. The
///   canister then validates the chain synchronously and uses the
///   DKIM key from the bundle.
/// - Otherwise, the canister checks the registered domain against
///   `DohConfig.allowed_domains` (deploy-arg) and resolves the DKIM
///   TXT via `crate::doh::fetch_txt` at `smtp_request` time.
/// - If neither path applies, prepare fails with
///   `DomainNotSupported`.
///
/// The frontend deliberately doesn't need to know which domains are
/// on the DoH allowlist — that's operator config, not user-visible
/// state. It just calls prepare and reads back any `Domain*` error
/// to render an actionable message.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct EmailRecoveryDnsInput {
    /// Lowercased canonical form: `lowercase(local-part) + "@" +
    /// lowercase(domain)`.
    pub address: String,
    /// The DKIM selector the FE has discovered via DoH probing
    /// (`<selector>._domainkey.<domain>`). The canister uses this to
    /// know which TXT record to fetch via `doh::fetch_txt` when an
    /// email actually arrives.
    pub selector: String,
    /// Optional DNSSEC proof bundle. Present iff the FE was able to
    /// walk the DNSSEC delegation chain from root to the DKIM TXT.
    /// When supplied, the canister:
    ///
    /// 1. Synchronously validates the chain against its configured
    ///    `DnssecConfig.root_anchors` at prepare time.
    /// 2. Caches the verified DKIM public key + DMARC policy on the
    ///    pending challenge so `smtp_request` can reuse them
    ///    without an outcall.
    ///
    /// When absent, the canister falls back to the DoH-allowlist
    /// path (the registered domain must be in
    /// `DohConfig.allowed_domains`). The FE doesn't need to know
    /// which path the canister chose; it just submits whatever it
    /// could gather and reads back the typed error variants.
    pub dns_proof: Option<DnsProofBundle>,
}

/// Errors surfaced by every email-recovery flow method.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq, Serialize)]
pub enum EmailRecoveryError {
    /// The caller's principal isn't authorised for this anchor (setup
    /// flow only).
    Unauthorized(candid::Principal),
    /// No pending challenge exists for the supplied nonce. Either it
    /// was never issued, it expired, or it was already consumed.
    NonceUnknown,
    /// A pending challenge existed but its 30-minute TTL has lapsed.
    NonceExpired,
    /// The DoH path was requested but the registered domain isn't on
    /// the allowlist (`DohConfig.allowed_domains`).
    DomainNotAllowlisted(String),
    /// DoH path: `crate::doh::fetch_txt` couldn't reach quorum or
    /// reported a transport failure.
    DohFetchFailed(String),
    /// Catch-all for "this domain can't be verified by any supported
    /// path".
    DomainNotSupported(String),
    /// The verification pipeline rejected the email; the inner reason
    /// comes from `dkim::verify` / `dmarc::verify_email` — see the
    /// `EmailVerificationStatus` type in those modules.
    EmailVerificationFailed(String),
    /// The email was DKIM-signed under a different selector than the
    /// one in the prepare proof — usually the result of a provider
    /// rotating selectors between prepare and send.
    SelectorMismatch,
    /// `From:` did not match the address claimed at prepare time.
    AddressMismatch,
    /// DKIM signature didn't include `Subject:` in `h=` (see
    /// `docs/ongoing/email-recovery.md` §5.4).
    SubjectNotSigned,
    /// The address is already bound to a different anchor (setup flow
    /// only).
    AddressAlreadyRegistered,
    /// Tried to remove a credential the anchor doesn't have.
    AddressNotRegistered,
    InternalCanisterError(String),
}

/// Polling result for a pending challenge. The FE polls
/// `email_recovery_status(nonce)` at 1–5 s cadence until it sees a
/// terminal variant.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq, Serialize)]
pub enum EmailRecoveryStatus {
    /// Challenge is still waiting for the email to arrive at the
    /// gateway.
    Pending,
    /// Setup succeeded. The address is now bound to the anchor; the
    /// FE shows "all set" and ends the wizard.
    RegistrationSucceeded,
    /// Recovery succeeded. The FE follows up with
    /// `email_recovery_get_delegation(nonce, session_key, expiration)`
    /// to retrieve the actual `SignedDelegation`.
    RecoveryReady {
        user_key: UserKey,
        expiration: Timestamp,
    },
    /// Verification failed. The FE shows the granular reason and
    /// offers a retry.
    Failed(EmailRecoveryError),
    /// 30-minute TTL elapsed without an email matching the nonce.
    Expired,
}

/// Argument shape for `email_recovery_get_delegation` — mirrors the
/// existing `openid_get_delegation` query.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct EmailRecoveryGetDelegationArgs {
    pub nonce: String,
    pub session_key: ByteBuf,
    pub expiration: Timestamp,
}
