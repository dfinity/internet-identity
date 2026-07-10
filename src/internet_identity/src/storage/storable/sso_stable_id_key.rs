use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

/// Key for the auxiliary SSO stable-id bridge (§6.5):
/// `(iss, primary_client_id, stable_id) -> primary_sub`.
///
/// The bridge maps a non-`sub` org's cross-client-stable identifier (e.g. Entra
/// `oid`) to the org's primary-client `sub`, so a later gated per-app login
/// (whose pairwise `sub` differs) resolves to the primary identity. The
/// `primary_client_id` component scopes each entry to its primary client: one
/// tenant exposed through two II discovery domains (different primary clients)
/// has a distinct pairwise `primary_sub` per client, so keying on `(iss,
/// stable_id)` alone would let one clobber the other (last-writer-wins). Adding
/// `primary_client_id` removes that collision and stops resting isolation on
/// `iss` being tenant-unique.
///
/// Serialized as a CBOR map `{0: iss, 1: primary_client_id, 2: stable_id}` via
/// the `#[cbor(map)]` derive, mirroring [`super::openid_credential_key`].
#[derive(Encode, Decode, Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
#[cbor(map)]
pub struct StorableSsoStableIdKey {
    #[n(0)]
    pub iss: String,
    #[n(1)]
    pub primary_client_id: String,
    #[n(2)]
    pub stable_id: String,
}

impl Storable for StorableSsoStableIdKey {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableSsoStableIdKey");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorableSsoStableIdKey")
    }

    const BOUND: Bound = Bound::Unbounded;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_encode_and_decode_roundtrip() {
        let key = StorableSsoStableIdKey {
            iss: "https://login.microsoftonline.com/tenant/v2.0".to_string(),
            primary_client_id: "primary-client".to_string(),
            stable_id: "entra-oid-0001".to_string(),
        };
        let bytes = key.to_bytes();
        let decoded = StorableSsoStableIdKey::from_bytes(bytes);
        assert_eq!(key, decoded);
    }

    #[test]
    fn different_primary_client_ids_produce_distinct_keys() {
        let a = StorableSsoStableIdKey {
            iss: "iss".to_string(),
            primary_client_id: "client-a".to_string(),
            stable_id: "oid-1".to_string(),
        };
        let b = StorableSsoStableIdKey {
            primary_client_id: "client-b".to_string(),
            ..a.clone()
        };
        assert_ne!(a.to_bytes(), b.to_bytes());
    }
}
