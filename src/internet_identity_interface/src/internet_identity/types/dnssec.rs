use candid::{CandidType, Deserialize};
use serde_bytes::ByteBuf;

/// Canister-wide DNSSEC verification configuration. Set on every `init` /
/// `post_upgrade`. Held in `PersistentState` so it survives upgrades when an
/// upgrade arg omits the field.
///
/// Not specific to email recovery. Any feature that needs DNSSEC-verified
/// DNS records (the email-recovery DKIM/DMARC flow today, future DANE/ACME
/// hooks, etc.) consumes the same set of trust anchors.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq, Default)]
pub struct DnssecConfig {
    /// IANA root KSK trust anchors. Multiple are accepted simultaneously so
    /// rollover is a one-line change in the next upgrade arg — during a
    /// transition both the retiring and incoming KSK digests live here.
    pub root_anchors: Vec<DnssecRootAnchor>,
}

/// A single IANA root KSK trust anchor, in the same shape IANA publishes at
/// `data.iana.org/root-anchors/root-anchors.xml`.
///
/// We only ship `digest_type = 2` (SHA-256) — the legacy SHA-1 form is
/// declined at the boundary by the verifier (see PR #1b TODO list).
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct DnssecRootAnchor {
    pub key_tag: u16,
    pub algorithm: u8,
    pub digest_type: u8,
    pub digest: ByteBuf,
}
