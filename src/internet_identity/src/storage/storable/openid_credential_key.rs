use crate::openid::{Aud, Iss, OpenIdCredentialKey, Sub};
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use std::borrow::Cow;

/// Unbounded tuples are not supported yet in ic-stable-structures,
/// this file implements a struct to wrap it so it can be stored.
///
/// Encoding uses CBOR map format `{0: iss, 1: sub, 2: aud}`.
/// The decoder also handles legacy CBOR array format `[iss, sub]`
/// for backward compatibility during migration.
#[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct StorableOpenIdCredentialKey {
    pub iss: Iss,
    pub sub: Sub,
    pub aud: Aud,
}

impl StorableOpenIdCredentialKey {
    /// Returns true if this key was decoded from the legacy format (missing aud).
    pub fn is_legacy(&self) -> bool {
        self.aud.is_empty()
    }
}

// Encode as CBOR map: {0: iss, 1: sub, 2: aud}
impl<C> minicbor::Encode<C> for StorableOpenIdCredentialKey {
    fn encode<W: minicbor::encode::Write>(
        &self,
        e: &mut minicbor::Encoder<W>,
        _ctx: &mut C,
    ) -> Result<(), minicbor::encode::Error<W::Error>> {
        e.map(3)?;
        e.u32(0)?.str(&self.iss)?;
        e.u32(1)?.str(&self.sub)?;
        e.u32(2)?.str(&self.aud)?;
        Ok(())
    }
}

// Decode from either:
// - Legacy CBOR array format: [iss, sub] (aud defaults to "")
// - New CBOR map format: {0: iss, 1: sub, 2: aud}
impl<'b, C> minicbor::Decode<'b, C> for StorableOpenIdCredentialKey {
    fn decode(
        d: &mut minicbor::Decoder<'b>,
        _ctx: &mut C,
    ) -> Result<Self, minicbor::decode::Error> {
        match d.datatype()? {
            minicbor::data::Type::Array | minicbor::data::Type::ArrayIndef => {
                // Legacy array format: [iss, sub]
                let _len = d.array()?;
                let iss: &str = d.str()?;
                let sub: &str = d.str()?;
                Ok(StorableOpenIdCredentialKey {
                    iss: iss.to_string(),
                    sub: sub.to_string(),
                    aud: String::new(),
                })
            }
            minicbor::data::Type::Map | minicbor::data::Type::MapIndef => {
                // New map format: {0: iss, 1: sub, 2: aud}
                let len = d.map()?.unwrap_or(0);
                let mut iss = None;
                let mut sub = None;
                let mut aud = None;
                for _ in 0..len {
                    match d.u32()? {
                        0 => iss = Some(d.str()?.to_string()),
                        1 => sub = Some(d.str()?.to_string()),
                        2 => aud = Some(d.str()?.to_string()),
                        _ => {
                            d.skip()?;
                        }
                    }
                }
                Ok(StorableOpenIdCredentialKey {
                    iss: iss.ok_or_else(|| minicbor::decode::Error::message("missing iss"))?,
                    sub: sub.ok_or_else(|| minicbor::decode::Error::message("missing sub"))?,
                    aud: aud.unwrap_or_default(),
                })
            }
            other => Err(minicbor::decode::Error::type_mismatch(other)),
        }
    }
}

impl Storable for StorableOpenIdCredentialKey {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableOpenIdCredentialKey");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
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
        assert!(!decoded.is_legacy());
    }

    #[test]
    fn should_decode_legacy_array_format() {
        // Encode a legacy 2-element CBOR array: [iss, sub]
        let mut buffer = Vec::new();
        let mut encoder = minicbor::Encoder::new(&mut buffer);
        encoder.array(2).unwrap();
        encoder.str("https://accounts.google.com").unwrap();
        encoder.str("user123").unwrap();

        let decoded =
            StorableOpenIdCredentialKey::from_bytes(std::borrow::Cow::Borrowed(&buffer));
        assert_eq!(decoded.iss, "https://accounts.google.com");
        assert_eq!(decoded.sub, "user123");
        assert_eq!(decoded.aud, "");
        assert!(decoded.is_legacy());
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
        assert_eq!(tuple, ("iss".to_string(), "sub".to_string(), "aud".to_string()));
        let back: StorableOpenIdCredentialKey = tuple.into();
        assert_eq!(key, back);
    }
}
