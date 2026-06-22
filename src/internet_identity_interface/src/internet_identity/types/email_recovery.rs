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
//!    leaf via `email_challenge_submit_dkim_leaf` to finish the
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

use crate::internet_identity::types::{
    AnchorNumber, DelegationChain, DnsProofBundle, SignedRRset, Timestamp, UserKey,
};
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
pub struct EmailChallenge {
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
/// `email_challenge_submit_dkim_leaf` — see design doc §8.4.
///
/// The frontend deliberately doesn't need to know which domains are
/// on the DoH allowlist — that's operator config, not user-visible
/// state. It just calls prepare and reads back any `Domain*` error
/// to render an actionable message.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct EmailChallengeDnsInput {
    /// Lowercased canonical form: `lowercase(local-part) + "@" +
    /// lowercase(domain)`.
    pub address: String,
    /// Optional DNSSEC skeleton-chain bundle. Present iff the FE
    /// was able to walk the DNSSEC delegation chain from root to
    /// the registered domain's signed zone. The bundle's `leaf`
    /// field carries at most the DMARC TXT at
    /// `_dmarc.<registered_domain>` (optional). The DKIM leaf is
    /// *not* included; it lands later via
    /// `email_challenge_submit_dkim_leaf` once the email arrives
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

/// What the FE submits at `email_challenge_submit_dkim_leaf` —
/// the signed DKIM resolution chain `<selector>._domainkey.<d>`
/// → … → final TXT, plus any *additional* delegation chains for
/// signed zones the resolution crosses into.
///
/// Operational shape per provider category (see design doc §7.6):
///
/// - **Same-zone direct TXT** (Gmail, iCloud, most ESPs configured
///   directly): `hops = [TXT]`, `extra_chains = []`. The apex zone
///   was already chained at prepare time.
/// - **Cross-zone CNAME** (Proton: `proton.me` → `proton.ch`,
///   Tutanota: `tutanota.com` → `tutanota.de`, M365 custom
///   domains, …): `hops = [CNAME, …, TXT]`, `extra_chains` carries
///   one chain per zone touched that wasn't already covered by the
///   skeleton chain.
///
/// The canister already cached the root DNSKEY + apex zone DNSKEY at
/// prepare time, and the partial-verification record (the signed-
/// headers digest and the signature blob) at email-arrival time.
/// This call:
///
/// 1. Validates each `extra_chains` entry under the cached root
///    DNSKEY and merges the resulting `(zone → DNSKEY)` entries
///    into the cached map.
/// 2. Verifies each hop under the zone its RRSIG names
///    (`rrsig.signer_name`).
/// 3. Walks the hop sequence: `hops[0].name` must equal
///    `<selector>._domainkey.<registered_domain>`, each
///    intermediate hop must be a CNAME whose target equals the next
///    hop's owner, the final hop must be the TXT we want.
/// 4. Parses the final TXT for the DKIM public key and finishes the
///    signature check against the cached digest + signature blob.
///
/// See design doc §8.4 / §8.5.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct EmailChallengeSubmitDkimLeafArg {
    /// The challenge nonce from `email_recovery_credential_prepare_add`
    /// (or `email_recovery_prepare_delegation` for recovery). Both
    /// flows share the same submit-leaf surface — the canister
    /// dispatches by the pending entry's `kind`.
    pub nonce: String,
    /// The DKIM resolution chain in CNAME order, ending in a TXT.
    /// At least one hop required; bounded by
    /// `MAX_CNAME_HOPS = 4` at the canister side. For the Gmail-
    /// style direct-TXT case this is a single-element vec.
    pub hops: Vec<SignedRRset>,
    /// Delegation chains for signed zones touched by `hops` that
    /// weren't already covered by the skeleton chain anchored at
    /// prepare time. Empty for same-zone resolution. Each chain
    /// must anchor at the same root DNSKEY the prepare bundle did
    /// (verified internally by re-validating against the cached
    /// root DNSKEY RRset).
    pub extra_chains: Vec<DelegationChain>,
}

/// Argument to `email_challenge_resolve_via_doh` — the DoH-resolution
/// sibling of `email_challenge_submit_dkim_leaf`. Wrapped in a record
/// (like [`EmailChallengeSubmitDkimLeafArg`]) so the method can grow
/// fields without a breaking interface change; `nonce` is the lookup key
/// and is always required.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct EmailChallengeResolveViaDohArg {
    /// The challenge nonce from `email_recovery_credential_prepare_add`
    /// (or `email_recovery_prepare_delegation` for recovery). The method
    /// carries no leaf data; the canister resolves the DKIM key over its
    /// own allowlist-gated DoH path. The FE calls it repeatedly while the
    /// status is `ResolvingDoh`.
    pub nonce: String,
}

/// Why a DoH resolution failed, as a typed discriminant rather than a
/// free-form string. The FE reads this directly to segment the
/// `doh_reason` analytics property — no string parsing — and adding a
/// new cause is a compile error on the FE until it's handled.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq, Serialize)]
pub enum DohFailureReason {
    /// Every provider's outcall failed (network error / non-200 / etc).
    AllProvidersFailed,
    /// Outcalls succeeded but the responses didn't reach the quorum
    /// threshold of identical TXT bytes.
    QuorumFailed { agreeing: u32, total: u32 },
    /// A response was received but failed to parse as a DNS message
    /// with a valid TXT record (or the TXT wasn't valid UTF-8). The
    /// inner string is diagnostic detail.
    ResponseMalformed(String),
}

/// Errors surfaced by every email-recovery flow method.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq, Serialize)]
pub enum EmailChallengeError {
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
    /// reported a transport failure. The inner [`DohFailureReason`]
    /// names the specific cause.
    DohFetchFailed(DohFailureReason),
    /// Catch-all for "this domain can't be verified by any supported
    /// path".
    DomainNotSupported(String),
    /// The verification pipeline rejected the email; the inner reason
    /// comes from `dkim::verify` / `dmarc::verify_email` — see the
    /// `EmailVerificationStatus` type in those modules.
    EmailVerificationFailed(String),
    /// The DKIM leaf submitted via `email_challenge_submit_dkim_leaf`
    /// didn't match what's needed to complete the pending challenge
    /// — either the leaf's owner name isn't `<expected_selector>.
    /// _domainkey.<registered_domain>`, the leaf's RRSIG didn't
    /// validate under the cached zone DNSKEY, or the selector in
    /// the leaf differs from the `s=` tag the email was signed
    /// under. The FE typically retries the DoH walk and resubmits.
    DkimLeafMismatch,
    /// `email_challenge_submit_dkim_leaf` was called with an empty
    /// `hops` vector. A genuine DNSSEC submission always carries at
    /// least the final TXT hop; an FE that can't walk DNSSEC must call
    /// `email_challenge_resolve_via_doh` instead. Distinct from
    /// `DkimLeafMismatch` (which means a *non-empty* chain failed to
    /// validate) so the malformed-request case is unambiguous in
    /// telemetry and user-facing copy.
    EmptyDkimLeafHops,
    /// The submit-leaf call arrived but the pending challenge isn't
    /// in the right state for it. Either the email hasn't arrived
    /// yet (status is still `Pending`), or it already advanced past
    /// `NeedDkimLeaf` (status is terminal). The FE should resume
    /// polling `email_challenge_status`.
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

/// Stable, public reason code for an `EmailChallengeError`: the variant
/// **name** only, dropping every inner payload (`String` / `Principal`).
///
/// Used to build [`EmailChallengeDiagnostics::reason_code`] for the
/// user-copyable support blob. Returning the bare discriminant is what
/// keeps that blob strictly public — variants like
/// `EmailVerificationFailed(String)`, `InternalCanisterError(String)`,
/// `DomainNotAllowlisted(String)` and `Unauthorized(Principal)` carry
/// payloads (domains, principals, free-form internal detail) that must
/// never leak into it. Mirrors the FE's existing `failureReason()`,
/// which already treats the variant name as the stable reason.
pub fn error_code_name(error: &EmailChallengeError) -> &'static str {
    match error {
        EmailChallengeError::Unauthorized(_) => "Unauthorized",
        EmailChallengeError::NonceUnknown => "NonceUnknown",
        EmailChallengeError::NonceExpired => "NonceExpired",
        EmailChallengeError::DomainNotAllowlisted(_) => "DomainNotAllowlisted",
        EmailChallengeError::DohFetchFailed(_) => "DohFetchFailed",
        EmailChallengeError::DomainNotSupported(_) => "DomainNotSupported",
        EmailChallengeError::EmailVerificationFailed(_) => "EmailVerificationFailed",
        EmailChallengeError::DkimLeafMismatch => "DkimLeafMismatch",
        EmailChallengeError::EmptyDkimLeafHops => "EmptyDkimLeafHops",
        EmailChallengeError::NoDkimLeafExpected => "NoDkimLeafExpected",
        EmailChallengeError::AddressMismatch => "AddressMismatch",
        EmailChallengeError::SubjectNotSigned => "SubjectNotSigned",
        EmailChallengeError::AddressAlreadyRegistered => "AddressAlreadyRegistered",
        EmailChallengeError::AddressNotRegistered => "AddressNotRegistered",
        EmailChallengeError::InternalCanisterError(_) => "InternalCanisterError",
    }
}

/// Polling result for a pending challenge. The FE polls
/// `email_challenge_status(nonce)` at 1–5 s cadence until it sees a
/// terminal variant — or sees `NeedDkimLeaf`, which is the trigger
/// to walk the DKIM leaf and call `email_challenge_submit_dkim_leaf`.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq, Serialize)]
pub enum EmailChallengeStatus {
    /// Challenge is still waiting for the email to arrive at the
    /// gateway.
    Pending,
    /// (DNSSEC path only.) The email arrived; the canister parsed
    /// the DKIM-Signature header, verified the body hash, and
    /// stashed a partial-verification record on the pending
    /// challenge. It now needs the FE to walk DNSSEC for
    /// `<selector>._domainkey.<registered_domain>` and submit the
    /// leaf via `email_challenge_submit_dkim_leaf`. The selector
    /// returned here is the one in the email's
    /// `DKIM-Signature: s=` tag (the canonical selector for that
    /// message); the FE uses it both to query DoH and as the
    /// leaf's owner-name component.
    NeedDkimLeaf { selector: String },
    /// The email arrived and the DKIM key is being resolved over DoH. The
    /// FE drives `email_challenge_resolve_via_doh` while this is set: each
    /// call reads the canister's DoH cache and either finishes or leaves the
    /// status here to poll again. Reached on the DoH path (`smtp_request`
    /// for a non-DNSSEC domain) and on the DNSSEC path's DoH fallback (when
    /// the DKIM leaf CNAMEs into an unsigned zone). Flips to
    /// `RegistrationSucceeded` / `RecoveryReady` / `Failed`. The DNSSEC
    /// leaf-walk path reports `NeedDkimLeaf` first.
    ResolvingDoh,
    /// Setup succeeded. The address is now bound to the anchor; the
    /// FE shows "all set" and ends the wizard.
    RegistrationSucceeded,
    /// Recovery succeeded. The FE follows up with
    /// `email_recovery_get_delegation(nonce, session_key, expiration)`
    /// to retrieve the actual `SignedDelegation`. The `anchor_number`
    /// is included so the FE can seed its auth store without making a
    /// separate lookup call — the canister already resolved it from
    /// the verified `From:` at submit-leaf time.
    RecoveryReady {
        user_key: UserKey,
        expiration: Timestamp,
        anchor_number: AnchorNumber,
    },
    /// Verification failed. The FE shows the granular reason and
    /// offers a retry.
    Failed(EmailChallengeError),
    /// 30-minute TTL elapsed without an email matching the nonce.
    Expired,
}

/// Which trust path the canister used (or will use) to verify the
/// challenge email. Decided at prepare time: a supplied DNSSEC skeleton
/// bundle → `Dnssec`; otherwise the DoH allowlist → `Doh`. Public — the
/// FE already chose the branch and the deploy config is public, so
/// surfacing it leaks nothing.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq, Serialize)]
pub enum VerificationPath {
    Doh,
    Dnssec,
}

/// Strictly-public, user-copyable diagnostics for one pending
/// challenge, returned by `email_challenge_diagnostics(nonce)`.
///
/// Intended to be pasted into a support ticket so the case can be lined
/// up against the SMTP gateway logs and the canister's production logs
/// via `message_id`. Every field here is deliberately
/// **non-sensitive**: there is NO email address, NO anchor number, NO
/// principal, NO delegation/seed material, and NO inner error string —
/// `reason_code` is the failing variant's *name* only (see
/// [`error_code_name`]). Readable only by whoever holds the 64-bit
/// nonce, exactly like `EmailChallengeStatus`.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq, Serialize)]
pub struct EmailChallengeDiagnostics {
    /// The gateway-supplied `SmtpRequest.message_id`, verbatim. `None`
    /// until an email bearing this nonce reaches the canister (or if
    /// the gateway omitted it). This is the cross-log correlation id.
    pub message_id: Option<String>,
    /// Coarse, stable status/failure code: the current lifecycle state
    /// (`"Pending"`, `"NeedDkimLeaf"`, `"Succeeded"`, `"Expired"`), or
    /// for a failed challenge `"Failed:<Variant>"` (variant name only,
    /// never a payload).
    pub reason_code: String,
    /// The trust path this challenge is on.
    pub verification_path: VerificationPath,
    /// Challenge creation time, nanoseconds since the Unix epoch
    /// (matches this crate's `Timestamp` encoding). Bounds the incident
    /// to the ~30-minute challenge window for log lookup.
    pub created_at: Timestamp,
}

/// Argument shape for `email_recovery_get_delegation` — mirrors the
/// existing `openid_get_delegation` query.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct EmailRecoveryGetDelegationArgs {
    pub nonce: String,
    pub session_key: ByteBuf,
    pub expiration: Timestamp,
}
