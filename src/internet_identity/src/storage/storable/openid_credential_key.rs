use crate::openid::{Aud, Iss, OpenIdCredentialLookupKey, Sub};
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

/// Unbounded tuples are not supported yet in ic-stable-structures,
/// this struct wraps the `(iss, sub, aud, sso_domain)` key so it can be stored.
///
/// Serialized as a CBOR map `{0: iss, 1: sub, 2: aud, 3: sso_domain}` via the
/// `#[cbor(map)]` derive. `sso_domain` is optional, and minicbor omits a `None`
/// value from the map on encode and decodes an absent key back to `None`.
///
/// This gives a transparent dual-format decode for the `sso_domain` migration:
///
/// - **Legacy entries** — written before `sso_domain` was part of the key —
///   are CBOR maps with only keys `0`/`1`/`2` and decode with `sso_domain =
///   None` (see [`Self::is_legacy`]).
/// - **New entries** carry key `3` when (and only when) the credential is an
///   SSO credential (`Some(domain)`); direct-provider credentials keep
///   `sso_domain = None` and therefore encode to the exact same bytes as a
///   legacy entry — nothing to migrate for them.
///
/// Unlike the earlier `(iss, sub)` → `(iss, sub, aud)` migration (whose legacy
/// shape was a CBOR *array*, requiring a separate `Decode` struct and a
/// top-level type peek), both shapes here are CBOR *maps* differing only by the
/// presence of the optional key `3`, so a single `#[cbor(map)]` decode handles
/// both.
#[derive(Encode, Decode, Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
#[cbor(map)]
pub struct StorableOpenIdCredentialKey {
    #[n(0)]
    pub iss: Iss,
    #[n(1)]
    pub sub: Sub,
    #[n(2)]
    pub aud: Aud,
    #[n(3)]
    pub sso_domain: Option<String>,
}

impl StorableOpenIdCredentialKey {
    /// Returns true if this key lacks an `sso_domain` component, i.e. it was
    /// written before the `sso_domain` key migration or belongs to a direct
    /// provider (Google / Microsoft / Apple). The migration only re-keys
    /// entries whose backing credential resolves an `sso_domain`, so a `true`
    /// here does not by itself mean "unmigrated".
    pub fn is_legacy(&self) -> bool {
        self.sso_domain.is_none()
    }
}

impl Storable for StorableOpenIdCredentialKey {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableOpenIdCredentialKey");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        // Both the legacy (`{0, 1, 2}`) and the new (`{0, 1, 2, 3}`) shapes are
        // CBOR maps; an absent key `3` decodes to `sso_domain = None`, so a
        // single decode handles both formats.
        //
        // A CBOR *array* here would be a stray key from the long-completed
        // `(iss, sub)` → `(iss, sub, aud)` migration, only reachable in a
        // non-prod environment upgraded past that migration without running it.
        // Fail with an actionable message instead of a generic CBOR error.
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

impl From<StorableOpenIdCredentialKey> for OpenIdCredentialLookupKey {
    fn from(value: StorableOpenIdCredentialKey) -> Self {
        (value.iss, value.sub, value.aud, value.sso_domain)
    }
}

impl From<OpenIdCredentialLookupKey> for StorableOpenIdCredentialKey {
    fn from(value: OpenIdCredentialLookupKey) -> Self {
        StorableOpenIdCredentialKey {
            iss: value.0,
            sub: value.1,
            aud: value.2,
            sso_domain: value.3,
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
            iss: "https://auth.acme.com".to_string(),
            sub: "user123".to_string(),
            aud: "client456".to_string(),
            sso_domain: Some("acme.com".to_string()),
        };
        let bytes = key.to_bytes();
        let decoded = StorableOpenIdCredentialKey::from_bytes(bytes);
        assert_eq!(decoded.iss, "https://auth.acme.com");
        assert_eq!(decoded.sub, "user123");
        assert_eq!(decoded.aud, "client456");
        assert_eq!(decoded.sso_domain, Some("acme.com".to_string()));
        assert!(!decoded.is_legacy());
    }

    /// Keys written before `sso_domain` existed are CBOR maps with only keys
    /// `0`/`1`/`2` — they must decode with `sso_domain = None`. This is the
    /// exact on-disk shape the `sso_domain` key migration reads.
    #[test]
    fn should_decode_legacy_map_without_sso_domain() {
        // Encode the pre-`sso_domain` shape: a CBOR map with keys 0..=2 only.
        let mut buffer = Vec::new();
        let mut encoder = minicbor::Encoder::new(&mut buffer);
        encoder.map(3).unwrap();
        encoder
            .u8(0)
            .unwrap()
            .str("https://accounts.google.com")
            .unwrap();
        encoder.u8(1).unwrap().str("user123").unwrap();
        encoder.u8(2).unwrap().str("client456").unwrap();

        let decoded = StorableOpenIdCredentialKey::from_bytes(Cow::Borrowed(&buffer));
        assert_eq!(decoded.iss, "https://accounts.google.com");
        assert_eq!(decoded.sub, "user123");
        assert_eq!(decoded.aud, "client456");
        assert_eq!(decoded.sso_domain, None);
        assert!(decoded.is_legacy());
    }

    /// A direct-provider key (`sso_domain = None`) must encode to the same
    /// bytes as a legacy key, so the migration correctly treats it as needing
    /// no re-keying and the dual lookup fallback is a no-op for it.
    #[test]
    fn direct_provider_key_encodes_like_legacy() {
        let mut legacy = Vec::new();
        let mut encoder = minicbor::Encoder::new(&mut legacy);
        encoder.map(3).unwrap();
        encoder
            .u8(0)
            .unwrap()
            .str("https://accounts.google.com")
            .unwrap();
        encoder.u8(1).unwrap().str("user123").unwrap();
        encoder.u8(2).unwrap().str("client456").unwrap();

        let key = StorableOpenIdCredentialKey {
            iss: "https://accounts.google.com".to_string(),
            sub: "user123".to_string(),
            aud: "client456".to_string(),
            sso_domain: None,
        };
        assert_eq!(key.to_bytes().as_ref(), legacy.as_slice());
    }

    #[test]
    #[should_panic(expected = "unmigrated legacy")]
    fn should_trap_on_legacy_array_key() {
        // Encode a legacy 2-element CBOR array: [iss, sub]. This shape belongs
        // to the long-completed array→map migration and must fail with the
        // actionable message rather than a generic CBOR decode error.
        let mut buffer = Vec::new();
        let mut encoder = minicbor::Encoder::new(&mut buffer);
        encoder.array(2).unwrap();
        encoder.str("https://accounts.google.com").unwrap();
        encoder.str("user123").unwrap();

        let _ = StorableOpenIdCredentialKey::from_bytes(Cow::Borrowed(&buffer));
    }

    #[test]
    fn should_roundtrip_through_storable() {
        let key = StorableOpenIdCredentialKey {
            iss: "https://login.microsoftonline.com/tid/v2.0".to_string(),
            sub: "sub123".to_string(),
            aud: "aud789".to_string(),
            sso_domain: Some("corp.example".to_string()),
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
            sso_domain: Some("domain".to_string()),
        };
        let tuple: OpenIdCredentialLookupKey = key.clone().into();
        assert_eq!(
            tuple,
            (
                "iss".to_string(),
                "sub".to_string(),
                "aud".to_string(),
                Some("domain".to_string())
            )
        );
        let back: StorableOpenIdCredentialKey = tuple.into();
        assert_eq!(key, back);
    }
}
