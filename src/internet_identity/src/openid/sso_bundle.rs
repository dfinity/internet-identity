//! Encoding for the certified SSO attribute bundle — a length-prefixed byte
//! string:
//!
//! ```text
//! b"ii-sso-attr"                       domain separator
//! u64-BE len || sso_domain bytes       SSO discovery domain
//! u64-BE len || origin bytes           certified dapp origin
//! u64-BE len || expiry_ns (u64 BE)     bundle expiry, ns since epoch
//! ```

use super::{calculate_delegation_seed, SSO_ATTR_BUNDLE_TTL_NS};
use crate::attributes::ICRC3_ATTRIBUTES_CERTIFICATION_DOMAIN;
use crate::{state, update_root_hash};
use candid::Principal;
use ic_canister_sig_creation::signature_map::CanisterSigInputs;
use ic_cdk::api::time;
use internet_identity_interface::internet_identity::types::openid::OpenIdDelegationError;
use internet_identity_interface::internet_identity::types::{AnchorNumber, Timestamp};

/// Domain separator for the SSO attribute bundle signature.
const SSO_ATTR_BUNDLE_DOMAIN: &[u8] = b"ii-sso-attr";

/// Max accepted size of an attached bundle payload (`sender_info`). A real
/// bundle is a few hundred bytes; anything larger is treated as no bundle,
/// bounding the decode allocation.
const MAX_SSO_BUNDLE_BYTES: usize = 4096;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SsoAttrBundle {
    /// The SSO discovery domain this session signed in through.
    pub sso_domain: String,
    /// The dapp origin II certified this session for.
    pub origin: String,
    /// Nanoseconds since the Unix epoch after which the bundle is invalid.
    pub expiry_ns: u64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SsoBundleDecodeError {
    BadDomain,
    Truncated,
    TrailingBytes,
    BadExpiryWidth,
    InvalidOriginUtf8,
    InvalidSsoDomainUtf8,
}

/// Append a length-prefixed field: `u64-BE length || data`.
fn write_field(blob: &mut Vec<u8>, data: &[u8]) {
    blob.extend_from_slice(&(data.len() as u64).to_be_bytes());
    blob.extend_from_slice(data);
}

pub fn encode_sso_attr_bundle(sso_domain: &str, origin: &str, expiry_ns: u64) -> Vec<u8> {
    let mut blob: Vec<u8> = Vec::new();
    blob.extend_from_slice(SSO_ATTR_BUNDLE_DOMAIN);
    write_field(&mut blob, sso_domain.as_bytes());
    write_field(&mut blob, origin.as_bytes());
    write_field(&mut blob, &expiry_ns.to_be_bytes());
    blob
}

/// A cursor reading length-prefixed fields; every read is bounds-checked.
struct FieldReader<'a> {
    bytes: &'a [u8],
    pos: usize,
}

impl<'a> FieldReader<'a> {
    fn new(bytes: &'a [u8]) -> Self {
        Self { bytes, pos: 0 }
    }

    fn read_field(&mut self) -> Result<&'a [u8], SsoBundleDecodeError> {
        let len_end = self
            .pos
            .checked_add(8)
            .ok_or(SsoBundleDecodeError::Truncated)?;
        let len_bytes = self
            .bytes
            .get(self.pos..len_end)
            .ok_or(SsoBundleDecodeError::Truncated)?;
        let len_u64 = u64::from_be_bytes(len_bytes.try_into().expect("slice is 8 bytes"));
        // A length that doesn't fit in usize can't address the buffer (usize is 32-bit on wasm32).
        let len = usize::try_from(len_u64).map_err(|_| SsoBundleDecodeError::Truncated)?;
        let data_end = len_end
            .checked_add(len)
            .ok_or(SsoBundleDecodeError::Truncated)?;
        let data = self
            .bytes
            .get(len_end..data_end)
            .ok_or(SsoBundleDecodeError::Truncated)?;
        self.pos = data_end;
        Ok(data)
    }

    fn is_exhausted(&self) -> bool {
        self.pos == self.bytes.len()
    }
}

/// Decode a bundle byte string produced by [`encode_sso_attr_bundle`].
pub fn decode_sso_attr_bundle(bytes: &[u8]) -> Result<SsoAttrBundle, SsoBundleDecodeError> {
    let rest = bytes
        .strip_prefix(SSO_ATTR_BUNDLE_DOMAIN)
        .ok_or(SsoBundleDecodeError::BadDomain)?;

    let mut reader = FieldReader::new(rest);
    let sso_domain_bytes = reader.read_field()?;
    let origin_bytes = reader.read_field()?;
    let expiry_bytes = reader.read_field()?;

    if !reader.is_exhausted() {
        return Err(SsoBundleDecodeError::TrailingBytes);
    }

    let sso_domain = String::from_utf8(sso_domain_bytes.to_vec())
        .map_err(|_| SsoBundleDecodeError::InvalidSsoDomainUtf8)?;
    let origin = String::from_utf8(origin_bytes.to_vec())
        .map_err(|_| SsoBundleDecodeError::InvalidOriginUtf8)?;
    let expiry_arr: [u8; 8] = expiry_bytes
        .try_into()
        .map_err(|_| SsoBundleDecodeError::BadExpiryWidth)?;
    let expiry_ns = u64::from_be_bytes(expiry_arr);

    Ok(SsoAttrBundle {
        sso_domain,
        origin,
        expiry_ns,
    })
}

fn msg_caller_info_data() -> Vec<u8> {
    let n = ic0::msg_caller_info_data_size();
    let mut b = vec![0u8; n];
    ic0::msg_caller_info_data_copy(&mut b, 0);
    b
}

fn msg_caller_info_signer() -> Option<Principal> {
    let n = ic0::msg_caller_info_signer_size();
    if n == 0 {
        return None;
    }
    let mut b = vec![0u8; n];
    ic0::msg_caller_info_signer_copy(&mut b, 0);
    Some(Principal::try_from(b.as_slice()).expect("valid principal"))
}

/// The certified SSO attribute bundle attached to this call, if signed by this
/// canister and not expired.
///
/// This is II's internal SSO-session bundle — signed and consumed by II — and is
/// distinct from the external ICRC-3 certified-attributes bundle that carries
/// arbitrary attributes to relying parties.
///
/// A `Some` result only proves II signed the bundle under the caller's credential
/// seed; the caller MUST still check `bundle.origin` matches the serving origin —
/// the bundle is bound to the identity, not the origin.
pub fn read_certified_sso_bundle() -> Option<SsoAttrBundle> {
    if msg_caller_info_signer()? != ic_cdk::id() {
        return None;
    }
    // A real bundle is a few hundred bytes; reject anything larger before allocating.
    if ic0::msg_caller_info_data_size() > MAX_SSO_BUNDLE_BYTES {
        return None;
    }
    let bundle = decode_sso_attr_bundle(&msg_caller_info_data()).ok()?;
    if time() > bundle.expiry_ns {
        return None;
    }
    Some(bundle)
}

/// Prepare the certified SSO attribute bundle: encode `(sso_domain, origin,
/// expiry)` and sign it under the credential seed. Returns the message bytes and
/// its expiry; [`get_sso_attr_bundle_signature`] witnesses the signature.
pub fn prepare_sso_attr_bundle(
    iss: &str,
    sub: &str,
    aud: &str,
    anchor_number: AnchorNumber,
    sso_domain: &str,
    origin: &str,
) -> (Vec<u8>, Timestamp) {
    let expiration = time().saturating_add(SSO_ATTR_BUNDLE_TTL_NS);
    let message = encode_sso_attr_bundle(sso_domain, origin, expiration);
    let seed = calculate_delegation_seed(
        &(iss.to_string(), sub.to_string(), aud.to_string()),
        anchor_number,
    );
    state::signature_map_mut(|sigs| {
        sigs.add_signature(&CanisterSigInputs {
            domain: ICRC3_ATTRIBUTES_CERTIFICATION_DOMAIN,
            seed: &seed,
            message: &message,
        });
    });
    update_root_hash();
    (message, expiration)
}

/// Witness the canister signature over an SSO attribute bundle `message`
/// prepared by [`prepare_sso_attr_bundle`].
pub fn get_sso_attr_bundle_signature(
    iss: &str,
    sub: &str,
    aud: &str,
    anchor_number: AnchorNumber,
    message: &[u8],
) -> Result<Vec<u8>, OpenIdDelegationError> {
    let seed = calculate_delegation_seed(
        &(iss.to_string(), sub.to_string(), aud.to_string()),
        anchor_number,
    );
    state::assets_and_signatures(|certified_assets, sigs| {
        let inputs = CanisterSigInputs {
            domain: ICRC3_ATTRIBUTES_CERTIFICATION_DOMAIN,
            seed: &seed,
            message,
        };
        sigs.get_signature_as_cbor(&inputs, Some(certified_assets.root_hash()))
    })
    .map_err(|_| OpenIdDelegationError::NoSuchDelegation)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn round_trips() {
        let bundle = SsoAttrBundle {
            sso_domain: "idp.example.com".to_string(),
            origin: "https://nice-name.com".to_string(),
            expiry_ns: 1_700_000_000_000_000_000,
        };
        let encoded = encode_sso_attr_bundle(&bundle.sso_domain, &bundle.origin, bundle.expiry_ns);
        assert_eq!(decode_sso_attr_bundle(&encoded), Ok(bundle));
    }

    #[test]
    fn round_trips_empty_and_unicode_fields() {
        for (sso_domain, origin) in [
            ("", ""),
            ("xn--tst-6la.example", "https://xn--tst-6la.example"),
            ("a", "b"),
        ] {
            let encoded = encode_sso_attr_bundle(sso_domain, origin, 0);
            let decoded = decode_sso_attr_bundle(&encoded).unwrap();
            assert_eq!(decoded.sso_domain, sso_domain);
            assert_eq!(decoded.origin, origin);
            assert_eq!(decoded.expiry_ns, 0);
        }
    }

    #[test]
    fn distinct_fields_do_not_alias() {
        let a = encode_sso_attr_bundle("ab", "c", 0);
        let b = encode_sso_attr_bundle("a", "bc", 0);
        assert_ne!(a, b);
    }

    #[test]
    fn rejects_unknown_domain() {
        let mut encoded = encode_sso_attr_bundle("idp", "https://a.com", 1);
        encoded[0] ^= 0xff;
        assert_eq!(
            decode_sso_attr_bundle(&encoded),
            Err(SsoBundleDecodeError::BadDomain)
        );
        let mut wrong_domain = b"xx-sso-attr".to_vec();
        wrong_domain.extend_from_slice(
            &encode_sso_attr_bundle("idp", "https://a.com", 1)[SSO_ATTR_BUNDLE_DOMAIN.len()..],
        );
        assert_eq!(
            decode_sso_attr_bundle(&wrong_domain),
            Err(SsoBundleDecodeError::BadDomain)
        );
    }

    #[test]
    fn rejects_trailing_bytes() {
        let mut encoded = encode_sso_attr_bundle("idp", "https://a.com", 1);
        encoded.push(0);
        assert_eq!(
            decode_sso_attr_bundle(&encoded),
            Err(SsoBundleDecodeError::TrailingBytes)
        );
    }

    #[test]
    fn rejects_truncated_field() {
        let encoded = encode_sso_attr_bundle("idp", "https://a.com", 1);
        let truncated = &encoded[..encoded.len() - 1];
        assert_eq!(
            decode_sso_attr_bundle(truncated),
            Err(SsoBundleDecodeError::Truncated)
        );
    }

    #[test]
    fn rejects_truncated_length_prefix() {
        let mut buf = SSO_ATTR_BUNDLE_DOMAIN.to_vec();
        buf.extend_from_slice(&[0, 0, 0]);
        assert_eq!(
            decode_sso_attr_bundle(&buf),
            Err(SsoBundleDecodeError::Truncated)
        );
    }

    #[test]
    fn rejects_oversized_length_prefix() {
        let mut buf = SSO_ATTR_BUNDLE_DOMAIN.to_vec();
        buf.extend_from_slice(&u64::MAX.to_be_bytes());
        buf.extend_from_slice(b"short");
        assert_eq!(
            decode_sso_attr_bundle(&buf),
            Err(SsoBundleDecodeError::Truncated)
        );
    }

    #[test]
    fn rejects_length_prefix_above_u32_max() {
        let mut buf = SSO_ATTR_BUNDLE_DOMAIN.to_vec();
        buf.extend_from_slice(&((u32::MAX as u64) + 1).to_be_bytes());
        buf.extend_from_slice(b"short");
        assert_eq!(
            decode_sso_attr_bundle(&buf),
            Err(SsoBundleDecodeError::Truncated)
        );
    }

    #[test]
    fn rejects_bad_expiry_width() {
        let mut buf = SSO_ATTR_BUNDLE_DOMAIN.to_vec();
        write_field(&mut buf, b"idp");
        write_field(&mut buf, b"https://a.com");
        write_field(&mut buf, &[0, 0, 0, 1]);
        assert_eq!(
            decode_sso_attr_bundle(&buf),
            Err(SsoBundleDecodeError::BadExpiryWidth)
        );
    }

    #[test]
    fn rejects_non_utf8_sso_domain() {
        let mut buf = SSO_ATTR_BUNDLE_DOMAIN.to_vec();
        write_field(&mut buf, &[0xff, 0xfe]);
        write_field(&mut buf, b"https://a.com");
        write_field(&mut buf, &0u64.to_be_bytes());
        assert_eq!(
            decode_sso_attr_bundle(&buf),
            Err(SsoBundleDecodeError::InvalidSsoDomainUtf8)
        );
    }

    #[test]
    fn rejects_non_utf8_origin() {
        let mut buf = SSO_ATTR_BUNDLE_DOMAIN.to_vec();
        write_field(&mut buf, b"idp");
        write_field(&mut buf, &[0xff, 0xfe]);
        write_field(&mut buf, &0u64.to_be_bytes());
        assert_eq!(
            decode_sso_attr_bundle(&buf),
            Err(SsoBundleDecodeError::InvalidOriginUtf8)
        );
    }
}
