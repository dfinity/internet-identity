//! Test-vector loader: deserialize captured DNSSEC chains into a
//! `DnsProofBundle`.
//!
//! These captures are frozen wire-format DoH responses recorded during
//! PR development; they're checked into `test_vectors/dnssec/` and
//! pulled in here via `include_str!`. Each capture's
//! `_meta.captured_at_unix` field is used as the frozen `now` for the
//! freshness check, so the tests remain stable indefinitely even after
//! the captured RRSIGs roll out of their original validity windows.
//!
//! Algorithm coverage across the bundled chains (algorithm names as
//! used in RFC 8624 §3.1):
//!
//! - `cloudflare-com` — RSA-SHA256 root → ECDSA-P256-SHA256 com →
//!   ECDSA-P256-SHA256 leaf (TXT).
//! - `proton.me` — RSA-SHA256 root → RSA-SHA256 com → RSA-SHA256
//!   leaf (TXT).
//! - `protonmail.com` — RSA-SHA256 root → ECDSA-P256-SHA256 com →
//!   ECDSA-P256-SHA256 leaf (TXT).
//! - `tutanota.com` — RSA-SHA256 root → ECDSA-P256-SHA256 com →
//!   ECDSA-P256-SHA256 leaf (TXT).
//! - `ed25519.nl` — RSA-SHA256 root → ECDSA-P256-SHA256 nl →
//!   Ed25519 leaf (A).
//!
//! Together they exercise every algorithm in our RFC 8624 MUST set
//! `{RSA-SHA256, ECDSA-P256-SHA256, Ed25519}` with real captured
//! data, plus the four production email zones the recovery feature
//! targets (proton.me, protonmail.com, tutanota.com — gmail.com /
//! icloud.com / outlook.com / fastmail.com are unsigned and rely on
//! the DoH-fallback path landing in PR 4).
//!
//! This module is gated `#[cfg(test)]` at its declaration in
//! `dnssec/mod.rs`, so no inner gate is needed here.

use super::types::{
    DelegationChain, DelegationLink, DnsName, DnsProofBundle, Rrsig, SignedRRset,
};
use internet_identity_interface::internet_identity::types::DnssecRootAnchor;
use serde::Deserialize;
use serde_bytes::ByteBuf;

#[derive(Deserialize)]
struct WireRrsig {
    type_covered: u16,
    algorithm: u8,
    labels: u8,
    original_ttl: u32,
    expiration: u32,
    inception: u32,
    key_tag: u16,
    signer_name: String,
    signature_hex: String,
}

#[derive(Deserialize)]
struct WireSignedRRset {
    name_hex: String,
    rtype: u16,
    rdata_hex: Vec<String>,
    ttl: u32,
    rrsig: WireRrsig,
}

#[derive(Deserialize)]
struct WireDelegationLink {
    child_ds: WireSignedRRset,
    child_dnskey: WireSignedRRset,
}

#[derive(Deserialize)]
struct WireDelegationChain {
    links: Vec<WireDelegationLink>,
}

#[derive(Deserialize)]
struct WireBundle {
    root_dnskey: WireSignedRRset,
    chains: Vec<WireDelegationChain>,
    hops: Vec<WireSignedRRset>,
}

fn hex_to_vec(s: &str) -> Vec<u8> {
    hex::decode(s).expect("test vector contains malformed hex")
}

impl From<WireRrsig> for Rrsig {
    fn from(w: WireRrsig) -> Self {
        // The wire format stores `signer_name` as the canonical wire
        // form (length-prefixed labels + null) hex-encoded; the
        // captured field carries the same form.
        Rrsig {
            type_covered: w.type_covered,
            algorithm: w.algorithm,
            labels: w.labels,
            original_ttl: w.original_ttl,
            expiration: w.expiration,
            inception: w.inception,
            key_tag: w.key_tag,
            signer_name: DnsName(hex_to_vec(&w.signer_name)),
            signature: hex_to_vec(&w.signature_hex),
        }
    }
}

impl From<WireSignedRRset> for SignedRRset {
    fn from(w: WireSignedRRset) -> Self {
        SignedRRset {
            name: DnsName(hex_to_vec(&w.name_hex)),
            rtype: w.rtype,
            rdata: w.rdata_hex.iter().map(|h| hex_to_vec(h)).collect(),
            ttl: w.ttl,
            rrsig: w.rrsig.into(),
        }
    }
}

impl From<WireDelegationLink> for DelegationLink {
    fn from(w: WireDelegationLink) -> Self {
        DelegationLink {
            child_ds: w.child_ds.into(),
            child_dnskey: w.child_dnskey.into(),
        }
    }
}

impl From<WireDelegationChain> for DelegationChain {
    fn from(w: WireDelegationChain) -> Self {
        DelegationChain {
            links: w.links.into_iter().map(Into::into).collect(),
        }
    }
}

impl From<WireBundle> for DnsProofBundle {
    fn from(w: WireBundle) -> Self {
        DnsProofBundle {
            root_dnskey: w.root_dnskey.into(),
            chains: w.chains.into_iter().map(Into::into).collect(),
            hops: w.hops.into_iter().map(Into::into).collect(),
        }
    }
}

pub fn load_bundle(json: &str) -> DnsProofBundle {
    let wire: WireBundle = serde_json::from_str(json).expect("invalid bundle JSON");
    wire.into()
}

#[derive(Deserialize)]
struct WireAnchor {
    key_tag: u16,
    algorithm: u8,
    digest_type: u8,
    digest_hex: String,
}

#[derive(Deserialize)]
struct WireAnchorFile {
    anchors: Vec<WireAnchor>,
}

pub fn load_anchors(json: &str) -> Vec<DnssecRootAnchor> {
    let wire: WireAnchorFile = serde_json::from_str(json).expect("invalid anchors JSON");
    wire.anchors
        .into_iter()
        .map(|a| DnssecRootAnchor {
            key_tag: a.key_tag,
            algorithm: a.algorithm,
            digest_type: a.digest_type,
            digest: ByteBuf::from(hex_to_vec(&a.digest_hex)),
        })
        .collect()
}

/// Captured chain for `cloudflare.com TXT`. End-to-end algorithm
/// coverage: RSA-SHA256 at the root, ECDSA-P256-SHA256 from `com.`
/// downward.
pub const CLOUDFLARE_COM_CHAIN_JSON: &str =
    include_str!("../../../../test_vectors/dnssec/cloudflare-com-2026-05.json");

/// Captured chain for `proton.me TXT`. End-to-end RSA-SHA256, so
/// this is the only fixture that exercises RSA-SHA256 at the leaf.
pub const PROTON_ME_CHAIN_JSON: &str =
    include_str!("../../../../test_vectors/dnssec/proton.me-2026-05.json");

/// Captured chain for `protonmail.com TXT` — email-recovery target,
/// ECDSA-P256-SHA256 leaf.
pub const PROTONMAIL_COM_CHAIN_JSON: &str =
    include_str!("../../../../test_vectors/dnssec/protonmail.com-2026-05.json");

/// Captured chain for `tutanota.com TXT` — email-recovery target,
/// ECDSA-P256-SHA256 leaf.
pub const TUTANOTA_COM_CHAIN_JSON: &str =
    include_str!("../../../../test_vectors/dnssec/tutanota.com-2026-05.json");

/// Captured chain for `ed25519.nl A`. Provides real-data Ed25519 leaf
/// coverage — the rarest RFC 8624 MUST algorithm in production today.
/// The leaf is an `A` record because `ed25519.nl` doesn't publish a
/// signed apex `TXT`; for verifier purposes the leaf RTYPE is
/// irrelevant — the algorithm dispatch and signature check are the
/// same path.
pub const ED25519_NL_CHAIN_JSON: &str =
    include_str!("../../../../test_vectors/dnssec/ed25519-nl-2026-05.json");

pub const IANA_ROOT_ANCHORS_JSON: &str =
    include_str!("../../../../test_vectors/dnssec/iana-root-anchors-2026-05.json");
