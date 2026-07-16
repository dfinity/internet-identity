use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

/// Key for the SSO stable-id index: `(iss, primary_client_id, stable_id) ->
/// AnchorNumber`. `primary_client_id` scopes each entry to its primary client
/// so distinct clients on the same `iss` don't collide. The value is
/// maintained by [`crate::storage::Storage::write`], reconciled from the
/// anchors' stored credentials.
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
