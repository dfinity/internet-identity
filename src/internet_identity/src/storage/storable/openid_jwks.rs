use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use identity_jose::jwk::Jwk;
use serde::{Deserialize, Serialize};
use std::borrow::Cow;

/// Stable-memory value holding the set of JWKs cached for a single OpenID
/// provider (keyed by the provider's `issuer` in the storage layer).
///
/// The keys are serialized as a JSON array — the exact `serde` shape used when
/// the keys are fetched from the provider's `jwks_uri` — so values round-trip
/// losslessly through `identity_jose::jwk::Jwk` and a seeded entry is
/// byte-for-byte identical to a fetched one.
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct StorableJwks {
    pub keys: Vec<Jwk>,
}

impl Storable for StorableJwks {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        Cow::Owned(serde_json::to_vec(self).expect("failed to serialize JWKs"))
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        serde_json::from_slice(&bytes).expect("failed to deserialize JWKs")
    }

    const BOUND: Bound = Bound::Unbounded;
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_jwk() -> Jwk {
        serde_json::from_str(
            r#"{"kty":"RSA","use":"sig","alg":"RS256","kid":"test-kid","n":"abc","e":"AQAB"}"#,
        )
        .unwrap()
    }

    #[test]
    fn should_round_trip_through_stable_bytes() {
        let original = StorableJwks {
            keys: vec![sample_jwk()],
        };
        let restored = StorableJwks::from_bytes(original.to_bytes());

        // `Jwk` does not implement `PartialEq`, so compare via their canonical
        // JSON serialization instead.
        assert_eq!(
            serde_json::to_string(&original).unwrap(),
            serde_json::to_string(&restored).unwrap()
        );
    }

    #[test]
    fn should_round_trip_empty() {
        let original = StorableJwks { keys: vec![] };
        let restored = StorableJwks::from_bytes(original.to_bytes());
        assert!(restored.keys.is_empty());
    }
}
