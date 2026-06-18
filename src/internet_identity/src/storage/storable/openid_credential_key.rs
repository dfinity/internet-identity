use crate::openid::{Aud, Iss, OpenIdCredentialKey, Sub};
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

/// Unbounded tuples are not supported yet in ic-stable-structures,
/// this struct wraps the `(iss, sub, aud)` key so it can be stored.
///
/// Serialized as a CBOR map `{0: iss, 1: sub, 2: aud}` via the `#[cbor(map)]`
/// derive. The legacy pre-`aud` `[iss, sub]` CBOR array shape has been migrated
/// away, so `from_bytes` only decodes the map shape (see there for how a stray
/// legacy key is reported).
#[derive(Encode, Decode, Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
#[cbor(map)]
pub struct StorableOpenIdCredentialKey {
    #[n(0)]
    pub iss: Iss,
    #[n(1)]
    pub sub: Sub,
    #[n(2)]
    pub aud: Aud,
}

impl Storable for StorableOpenIdCredentialKey {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableOpenIdCredentialKey");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        // The migration to the new `{0: iss, 1: sub, 2: aud}` CBOR map shape has
        // completed, so only that shape is decoded here. A legacy `[iss, sub]`
        // CBOR array key means this canister was upgraded past the migration
        // release without running it to completion (only reachable in non-prod
        // environments) — fail with an actionable message instead of a generic
        // CBOR decode error so operators can diagnose it quickly.
        let datatype = minicbor::Decoder::new(&bytes)
            .datatype()
            .expect("failed to read CBOR type for StorableOpenIdCredentialKey");
        if matches!(
            datatype,
            minicbor::data::Type::Array | minicbor::data::Type::ArrayIndef
        ) {
            panic!(
                "encountered an unmigrated legacy [iss, sub] OpenID credential key; this \
                 environment was upgraded past the OpenID credential key migration without \
                 running it to completion"
            );
        }
        minicbor::decode(&bytes).expect("failed to decode StorableOpenIdCredentialKey")
    }

    const BOUND: Bound = Bound::Unbounded;
}

impl From<StorableOpenIdCredentialKey> for OpenIdCredentialKey {
    fn from(value: StorableOpenIdCredentialKey) -> Self {
        (value.iss, value.sub, value.aud)
    }
}

impl From<OpenIdCredentialKey> for StorableOpenIdCredentialKey {
    fn from(value: OpenIdCredentialKey) -> Self {
        StorableOpenIdCredentialKey {
            iss: value.0,
            sub: value.1,
            aud: value.2,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ic_stable_structures::Storable;

    #[test]
    fn should_encode_and_decode_new_format() {
        let key = StorableOpenIdCredentialKey {
            iss: "https://accounts.google.com".to_string(),
            sub: "user123".to_string(),
            aud: "client456".to_string(),
        };
        let bytes = key.to_bytes();
        let decoded = StorableOpenIdCredentialKey::from_bytes(bytes);
        assert_eq!(decoded.iss, "https://accounts.google.com");
        assert_eq!(decoded.sub, "user123");
        assert_eq!(decoded.aud, "client456");
    }

    #[test]
    #[should_panic(expected = "unmigrated legacy")]
    fn should_trap_on_legacy_array_key() {
        // Encode a legacy 2-element CBOR array: [iss, sub]. After the migration
        // this shape no longer exists, so decoding must fail with the actionable
        // message rather than a generic CBOR decode error.
        let mut buffer = Vec::new();
        let mut encoder = minicbor::Encoder::new(&mut buffer);
        encoder.array(2).unwrap();
        encoder.str("https://accounts.google.com").unwrap();
        encoder.str("user123").unwrap();

        let _ = StorableOpenIdCredentialKey::from_bytes(std::borrow::Cow::Borrowed(&buffer));
    }

    #[test]
    fn should_roundtrip_through_storable() {
        let key = StorableOpenIdCredentialKey {
            iss: "https://login.microsoftonline.com/tid/v2.0".to_string(),
            sub: "sub123".to_string(),
            aud: "aud789".to_string(),
        };
        let bytes = key.to_bytes();
        let decoded = StorableOpenIdCredentialKey::from_bytes(bytes);
        assert_eq!(key, decoded);
    }

    #[test]
    fn should_convert_to_and_from_openid_credential_key() {
        let key = StorableOpenIdCredentialKey {
            iss: "iss".to_string(),
            sub: "sub".to_string(),
            aud: "aud".to_string(),
        };
        let tuple: OpenIdCredentialKey = key.clone().into();
        assert_eq!(
            tuple,
            ("iss".to_string(), "sub".to_string(), "aud".to_string())
        );
        let back: StorableOpenIdCredentialKey = tuple.into();
        assert_eq!(key, back);
    }
}
