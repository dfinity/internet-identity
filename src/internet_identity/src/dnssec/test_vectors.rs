//! Test-vector loader: deserialize captured DNSSEC chains into
//! `DnsProofBundle`.
//!
//! See `scripts/capture-dnssec-chain.py` for the producing side. The JSON
//! format mirrors `DnsProofBundle` field-for-field, with byte fields
//! hex-encoded (so the file is grep-friendly and diff-clean).

#![cfg(test)]

use super::types::{DelegationLink, DnsName, DnsProofBundle, Rrsig, SignedRRset};
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
struct WireBundle {
    root_dnskey: WireSignedRRset,
    chain: Vec<WireDelegationLink>,
    leaf: WireSignedRRset,
}

fn hex_to_vec(s: &str) -> Vec<u8> {
    hex::decode(s).expect("test vector contains malformed hex")
}

impl From<WireRrsig> for Rrsig {
    fn from(w: WireRrsig) -> Self {
        // The wire format puts `signer_name` as the canonical wire form
        // (length-prefixed labels + null) hex-encoded; the captured field
        // historically carries the same form.
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

impl From<WireBundle> for DnsProofBundle {
    fn from(w: WireBundle) -> Self {
        DnsProofBundle {
            root_dnskey: w.root_dnskey.into(),
            chain: w.chain.into_iter().map(Into::into).collect(),
            leaf: w.leaf.into(),
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

/// The captured chain for cloudflare.com TXT — exercises algorithms 8
/// (RSA-SHA256, root) and 13 (ECDSA-P256-SHA256, com → cloudflare.com →
/// leaf). See `test_vectors/dnssec/cloudflare-com-2026-05.json`.
pub const CLOUDFLARE_COM_CHAIN_JSON: &str =
    include_str!("../../../../test_vectors/dnssec/cloudflare-com-2026-05.json");

pub const IANA_ROOT_ANCHORS_JSON: &str =
    include_str!("../../../../test_vectors/dnssec/iana-root-anchors-2026-05.json");
