//! Encoding for the certified SSO attribute bundle (IdP-side per-app gating).
//!
//! The bundle is the `info` payload II attaches to a session as `sender_info`
//! (via the SDK `AttributesIdentity`). It is signed by II as a canister
//! signature under the **openid-credential seed** — the same seed the caller's
//! delegation principal is derived from ([`crate::openid::calculate_delegation_seed`])
//! — so it is intrinsically bound to that principal and cannot be replayed onto
//! another identity. Because the credential seed is stable across delegation
//! refreshes (it binds `(iss, sub, aud, anchor)`, not `origin`), the same bundle
//! is reusable with any fresh openid delegation for that credential until its
//! own `expiry` passes.
//!
//! Since II is the only producer and the only consumer, the wire format is a
//! minimal length-prefixed byte string mirroring the seed encoding in
//! [`crate::openid`] — no self-describing candid / ICRC-3 `Value` shape is
//! needed:
//!
//! ```text
//! b"ii-sso-attr-v1"                    domain separator + version
//! u64-BE len || sso_domain bytes       the SSO discovery domain signed in through
//! u64-BE len || origin bytes           the certified dapp origin
//! u64-BE len || expiry_ns (u64 BE)     bundle expiry, nanoseconds since epoch
//! ```

/// Domain separator + version for the SSO attribute bundle. Bumping the version
/// suffix makes a future field change unambiguous.
const SSO_ATTR_BUNDLE_DOMAIN: &[u8] = b"ii-sso-attr-v1";

/// A decoded SSO attribute bundle.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SsoAttrBundle {
    /// The SSO discovery domain this session signed in through — the source for
    /// `sso:<domain>` attribute certification (§6.3).
    pub sso_domain: String,
    /// The dapp origin II certified this session for.
    pub origin: String,
    /// Nanoseconds since the Unix epoch after which the bundle is no longer
    /// valid.
    pub expiry_ns: u64,
}

/// Why decoding a bundle failed. Every case is terminal — the caller must
/// reject the bundle (fail closed).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SsoBundleDecodeError {
    /// The domain-separator prefix was missing or did not match.
    BadDomain,
    /// A length prefix claimed more bytes than remain in the buffer.
    Truncated,
    /// Bytes remained after the last declared field.
    TrailingBytes,
    /// The `expiry_ns` field was not exactly 8 bytes.
    BadExpiryWidth,
    /// The `origin` field was not valid UTF-8.
    InvalidOriginUtf8,
    /// The `sso_domain` field was not valid UTF-8.
    InvalidSsoDomainUtf8,
}

/// Append a length-prefixed field: `u64-BE length || data`. Mirrors the
/// `write_field` helper used by the delegation seed encoders in
/// [`crate::openid`].
fn write_field(blob: &mut Vec<u8>, data: &[u8]) {
    blob.extend_from_slice(&(data.len() as u64).to_be_bytes());
    blob.extend_from_slice(data);
}

/// Encode `(sso_domain, origin, expiry_ns)` into the canonical bundle byte
/// string.
pub fn encode_sso_attr_bundle(sso_domain: &str, origin: &str, expiry_ns: u64) -> Vec<u8> {
    let mut blob: Vec<u8> = Vec::new();
    blob.extend_from_slice(SSO_ATTR_BUNDLE_DOMAIN);
    write_field(&mut blob, sso_domain.as_bytes());
    write_field(&mut blob, origin.as_bytes());
    write_field(&mut blob, &expiry_ns.to_be_bytes());
    blob
}

/// A cursor over the bundle bytes that reads length-prefixed fields. Every read
/// is bounds-checked so a malformed buffer fails closed rather than panicking.
struct FieldReader<'a> {
    bytes: &'a [u8],
    pos: usize,
}

impl<'a> FieldReader<'a> {
    fn new(bytes: &'a [u8]) -> Self {
        Self { bytes, pos: 0 }
    }

    /// Read the next `u64-BE length || data` field, returning `data`.
    fn read_field(&mut self) -> Result<&'a [u8], SsoBundleDecodeError> {
        let len_end = self
            .pos
            .checked_add(8)
            .ok_or(SsoBundleDecodeError::Truncated)?;
        let len_bytes = self
            .bytes
            .get(self.pos..len_end)
            .ok_or(SsoBundleDecodeError::Truncated)?;
        let len = u64::from_be_bytes(len_bytes.try_into().expect("slice is 8 bytes")) as usize;
        let data_end = len_end.checked_add(len).ok_or(SsoBundleDecodeError::Truncated)?;
        let data = self
            .bytes
            .get(len_end..data_end)
            .ok_or(SsoBundleDecodeError::Truncated)?;
        self.pos = data_end;
        Ok(data)
    }

    /// True once every byte has been consumed.
    fn is_exhausted(&self) -> bool {
        self.pos == self.bytes.len()
    }
}

/// Decode a bundle byte string produced by [`encode_sso_attr_bundle`]. Strictly
/// the inverse: an unknown domain, a short/oversized field, a wrong-width
/// expiry, non-UTF-8 origin, or any trailing byte all reject.
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
    let origin =
        String::from_utf8(origin_bytes.to_vec()).map_err(|_| SsoBundleDecodeError::InvalidOriginUtf8)?;
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
        let encoded =
            encode_sso_attr_bundle(&bundle.sso_domain, &bundle.origin, bundle.expiry_ns);
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
        // A length-prefixed encoding must not let a domain/origin split shift:
        // swapping the two values yields a different byte string.
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
        // A different-version domain must not decode either.
        let mut wrong_version = b"ii-sso-attr-v2".to_vec();
        wrong_version.extend_from_slice(
            &encode_sso_attr_bundle("idp", "https://a.com", 1)[SSO_ATTR_BUNDLE_DOMAIN.len()..],
        );
        assert_eq!(
            decode_sso_attr_bundle(&wrong_version),
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
        // Drop the final byte of the expiry field.
        let truncated = &encoded[..encoded.len() - 1];
        assert_eq!(
            decode_sso_attr_bundle(truncated),
            Err(SsoBundleDecodeError::Truncated)
        );
    }

    #[test]
    fn rejects_truncated_length_prefix() {
        // Only the domain plus a partial (3-byte) length prefix.
        let mut buf = SSO_ATTR_BUNDLE_DOMAIN.to_vec();
        buf.extend_from_slice(&[0, 0, 0]);
        assert_eq!(
            decode_sso_attr_bundle(&buf),
            Err(SsoBundleDecodeError::Truncated)
        );
    }

    #[test]
    fn rejects_oversized_length_prefix() {
        // A length prefix claiming a huge field with no backing bytes must not
        // panic or over-read.
        let mut buf = SSO_ATTR_BUNDLE_DOMAIN.to_vec();
        buf.extend_from_slice(&u64::MAX.to_be_bytes());
        buf.extend_from_slice(b"short");
        assert_eq!(
            decode_sso_attr_bundle(&buf),
            Err(SsoBundleDecodeError::Truncated)
        );
    }

    #[test]
    fn rejects_bad_expiry_width() {
        // Hand-build a bundle whose expiry field is 4 bytes instead of 8.
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
