//! Wire-format constants used by more than one module.
//!
//! Constants used by a single function live next to that function;
//! this file is reserved for RDATA layout values that show up in both
//! `verify.rs` (chain walk / anchor matching) and `signature.rs` (DS
//! digest matching / DNSKEY public-key extraction) and that would
//! otherwise drift between the two if duplicated.

/// Length of the fixed DNSKEY RDATA header (RFC 4034 §2.1):
///
/// ```text
/// Flags (u16) | Protocol (u8) | Algorithm (u8) | Public Key (variable)
/// ```
///
/// Used to bounds-check RDATA before reading the algorithm byte or
/// slicing off the public-key sub-field.
pub const DNSKEY_RDATA_HEADER_LEN: usize = 4;

/// Length of the fixed DS RDATA header (RFC 4034 §5.1):
///
/// ```text
/// Key Tag (u16) | Algorithm (u8) | Digest Type (u8) | Digest (variable)
/// ```
pub const DS_RDATA_HEADER_LEN: usize = 4;

/// DS Digest Type 2 = SHA-256 (RFC 4509). The only digest type we
/// accept — SHA-1 (Digest Type 1) is MUST NOT for new deployments
/// per RFC 8624 §3.3, and the IANA root anchors are SHA-256 anyway.
pub const DS_DIGEST_TYPE_SHA256: u8 = 2;
