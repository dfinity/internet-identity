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
//! whose DKIM key it trusts. The trust comes from one of two paths,
//! picked by the canister per call:
//!
//! 1. **DNSSEC, two-phase** — when the FE supplies a `DnsProofBundle`
//!    skeleton at prepare time (DNSSEC chain rooted at IANA + optional
//!    DMARC leaf, **not** the DKIM leaf), the canister validates the
//!    chain synchronously against its configured
//!    `DnssecConfig.root_anchors` and caches the validated zone
//!    DNSKEY on the pending challenge. Once the email arrives the
//!    canister parses `s=` from the DKIM-Signature header and flips
//!    the polled status to `NeedDkimLeaf { selector }`; the FE walks
//!    DNSSEC for `<selector>._domainkey.<domain>` and submits the
//!    leaf via `email_recovery_submit_dkim_leaf` to finish the
//!    pipeline. See design doc §8.4 / §8.5 for the rationale (no
//!    selector probing — the selector is authoritative from the email
//!    itself).
//! 2. **DoH allowlist** — when no bundle is supplied, the canister
//!    checks the registered domain against `DohConfig.allowed_domains`
//!    (deploy-arg). Used for the major consumer mailbox providers
//!    (Gmail, Outlook, iCloud, …) that don't publish DNSSEC. The
//!    DKIM TXT is resolved via a 3-of-5 DoH quorum
//!    (`crate::doh::fetch_txt`) at `smtp_request` time, and
//!    verification finishes inside that one call — no `submit_leaf`
//!    follow-up needed.

use crate::internet_identity::types::{DnsProofBundle, SignedRRset, Timestamp, UserKey};
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
    /// Nanoseconds since the Unix epoch. 30 minutes after issue.
    pub expires_at: Timestamp,
}

// Note on the recipient mailbox: the canister doesn't include it in
// this response. All entries of the `related_origins` deploy arg
// are accepted as equal aliases (`register@<host>` /
// `recover@<host>` for any host), and the FE renders the
// user-facing label by pairing `register` / `recover` with
// `window.location.hostname`. That way each tab shows the user the
// alias matching the origin they're already on; the canister never
// has to single one out as canonical. See
// `crate::email_recovery::mailbox_domains` (canister-side) for the
// dispatch-side counterpart.

/// What the FE submits at prepare time.
///
/// The FE never decides which verification path to use — it just
/// submits the address and (when it could assemble one) a DNSSEC
/// **skeleton chain**. The canister picks the path:
///
/// - If `dns_proof` is `Some`, take the DNSSEC path: validate the
///   skeleton chain synchronously, cache the deepest-zone DNSKEY
///   for later leaf admission, and (if the FE included a DMARC
///   leaf in the bundle) cache the DMARC policy bytes.
/// - Otherwise, check the registered domain against
///   `DohConfig.allowed_domains` (deploy-arg) and resolve the DKIM
///   TXT via `crate::doh::fetch_txt` at `smtp_request` time.
/// - If neither path applies, prepare fails with
///   `DomainNotSupported`.
///
/// The skeleton chain deliberately omits the DKIM leaf: the DKIM
/// selector is published by the sender's mail provider and is only
/// authoritatively known once the email arrives (the
/// `DKIM-Signature: s=` tag). On the DNSSEC path the FE walks the
/// remaining leaf after the email is delivered and submits it via
/// `email_recovery_submit_dkim_leaf` — see design doc §8.4.
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
    /// Optional DNSSEC skeleton-chain bundle. Present iff the FE
    /// was able to walk the DNSSEC delegation chain from root to
    /// the registered domain's signed zone. The bundle's `leaf`
    /// field carries at most the DMARC TXT at
    /// `_dmarc.<registered_domain>` (optional). The DKIM leaf is
    /// *not* included; it lands later via
    /// `email_recovery_submit_dkim_leaf` once the email arrives
    /// and the selector is known.
    ///
    /// When supplied, the canister:
    ///
    /// 1. Synchronously validates the chain against its configured
    ///    `DnssecConfig.root_anchors` at prepare time.
    /// 2. Caches the validated deepest-zone DNSKEY RRset on the
    ///    pending challenge so a follow-up `submit_dkim_leaf` can
    ///    admit a single TXT leaf without re-walking the chain.
    /// 3. Caches the DMARC TXT bytes (if a DMARC leaf was present
    ///    and verified) so `submit_dkim_leaf` doesn't need to
    ///    fetch them via DoH.
    ///
    /// When absent, the canister falls back to the DoH-allowlist
    /// path (the registered domain must be in
    /// `DohConfig.allowed_domains`). The FE doesn't need to know
    /// which path the canister chose; it just submits whatever it
    /// could gather and reads back the typed error variants.
    pub dns_proof: Option<DnsProofBundle>,
}

/// What the FE submits at `email_recovery_submit_dkim_leaf` —
/// the DKIM leaf RRset for the selector embedded in the
/// just-arrived email's DKIM-Signature header.
///
/// The canister already cached the validated zone DNSKEY at prepare
/// time and the partial-verification record (the signed-headers
/// digest and the signature blob) at email-arrival time. This call
/// admits the leaf against the cached DNSKEY, parses the DKIM TXT
/// to recover the public key, and finishes the signature check
/// using the cached digest and signature. See design doc §8.4 /
/// §8.5.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct EmailRecoverySubmitDkimLeafArg {
    /// The challenge nonce from `email_recovery_credential_prepare_add`
    /// (or `email_recovery_prepare_delegation` for recovery). Both
    /// flows share the same submit-leaf surface — the canister
    /// dispatches by the pending entry's `kind`.
    pub nonce: String,
    /// The DKIM TXT leaf at `<selector>._domainkey.<registered_domain>`,
    /// signed under the same zone the skeleton chain anchored at
    /// prepare time. The canister rejects with `DkimLeafMismatch`
    /// if the leaf's owner name doesn't end at the registered
    /// domain or if the selector doesn't match the
    /// `NeedDkimLeaf { selector }` status set when the email
    /// arrived.
    pub dkim_leaf: SignedRRset,
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
    /// The DKIM leaf submitted via `email_recovery_submit_dkim_leaf`
    /// didn't match what's needed to complete the pending challenge
    /// — either the leaf's owner name isn't `<expected_selector>.
    /// _domainkey.<registered_domain>`, the leaf's RRSIG didn't
    /// validate under the cached zone DNSKEY, or the selector in
    /// the leaf differs from the `s=` tag the email was signed
    /// under. The FE typically retries the DoH walk and resubmits.
    DkimLeafMismatch,
    /// The submit-leaf call arrived but the pending challenge isn't
    /// in the right state for it. Either the email hasn't arrived
    /// yet (status is still `Pending`), or it already advanced past
    /// `NeedDkimLeaf` (status is terminal). The FE should resume
    /// polling `email_recovery_status`.
    NoDkimLeafExpected,
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
/// terminal variant — or sees `NeedDkimLeaf`, which is the trigger
/// to walk the DKIM leaf and call `email_recovery_submit_dkim_leaf`.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq, Serialize)]
pub enum EmailRecoveryStatus {
    /// Challenge is still waiting for the email to arrive at the
    /// gateway.
    Pending,
    /// (DNSSEC path only.) The email arrived; the canister parsed
    /// the DKIM-Signature header, verified the body hash, and
    /// stashed a partial-verification record on the pending
    /// challenge. It now needs the FE to walk DNSSEC for
    /// `<selector>._domainkey.<registered_domain>` and submit the
    /// leaf via `email_recovery_submit_dkim_leaf`. The selector
    /// returned here is the one in the email's
    /// `DKIM-Signature: s=` tag (the canonical selector for that
    /// message); the FE uses it both to query DoH and as the
    /// leaf's owner-name component.
    NeedDkimLeaf { selector: String },
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
