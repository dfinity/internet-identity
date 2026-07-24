use crate::openid::{Aud, Iss, OpenIdCredential, Sub};
use crate::storage::storable::metadata_v2::StorableMetadataEntryV2;
use crate::storage::storable::openid_credential_key::StorableOpenIdCredentialKey;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use internet_identity_interface::internet_identity::types::Timestamp;
use minicbor::{Decode, Encode};
use std::borrow::Cow;
use std::collections::HashMap;

#[derive(Encode, Decode, Clone)]
#[cbor(map)]
pub struct StorableOpenIdCredential {
    #[n(0)]
    pub iss: Iss,
    #[n(1)]
    pub sub: Sub,
    #[n(2)]
    pub aud: Aud,
    #[n(3)]
    pub last_usage_timestamp: Option<Timestamp>,
    #[n(4)]
    pub metadata: HashMap<String, StorableMetadataEntryV2>,
    /// SSO discovery domain this credential was created through, stamped at
    /// verification time. `None` for direct-provider credentials (Google /
    /// Microsoft / Apple) and for SSO credentials written before this field
    /// existed — the latter are backfilled by the `sso_credential_migration`
    /// upgrade arg (see `docs/ongoing/openid-sso-prod-readiness.md` §8.6).
    /// `Option` so credentials written under the previous schema decode
    /// cleanly — same pattern as the optional fields on `StorableAnchor`.
    #[n(5)]
    pub sso_domain: Option<String>,
    /// Human-readable SSO label from the domain's hop-1
    /// `ii-openid-configuration`, stamped alongside `sso_domain`. May be
    /// `None` even for SSO credentials — domains aren't required to publish
    /// a `name`.
    #[n(6)]
    pub sso_name: Option<String>,
    /// Cross-client-stable identifier (the `oid` claim value) for a non-`sub`
    /// SSO org, stamped only on the II-client-keyed credential at SSO write time.
    /// Backs the SSO stable-id index, which `write()` reconciles from the
    /// stored credentials — remove or move the credential and the index entry
    /// follows. Additive CBOR field: credentials written before it existed
    /// decode as `None`, so no migration is needed — same pattern as
    /// `sso_domain` / `sso_name` above. Storage-internal: never surfaced in
    /// the candid `OpenIdCredentialData`.
    #[n(7)]
    pub stable_id: Option<String>,
}

impl StorableOpenIdCredential {
    pub fn key(&self) -> StorableOpenIdCredentialKey {
        StorableOpenIdCredentialKey {
            iss: self.iss.clone(),
            sub: self.sub.clone(),
            aud: self.aud.clone(),
        }
    }
}

impl Storable for StorableOpenIdCredential {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableOpenIdCredential");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorableOpenIdCredential")
    }

    const BOUND: Bound = Bound::Unbounded;
}

impl From<StorableOpenIdCredential> for OpenIdCredential {
    fn from(value: StorableOpenIdCredential) -> Self {
        OpenIdCredential {
            iss: value.iss,
            sub: value.sub,
            aud: value.aud,
            last_usage_timestamp: value.last_usage_timestamp,
            metadata: value
                .metadata
                .into_iter()
                .map(|(k, v)| (k, v.into()))
                .collect(),
            sso_domain: value.sso_domain,
            sso_name: value.sso_name,
            stable_id: value.stable_id,
        }
    }
}

impl From<OpenIdCredential> for StorableOpenIdCredential {
    fn from(value: OpenIdCredential) -> Self {
        StorableOpenIdCredential {
            iss: value.iss,
            sub: value.sub,
            aud: value.aud,
            last_usage_timestamp: value.last_usage_timestamp,
            metadata: value
                .metadata
                .into_iter()
                .map(|(k, v)| (k, v.into()))
                .collect(),
            sso_domain: value.sso_domain,
            sso_name: value.sso_name,
            stable_id: value.stable_id,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn credential(
        sso_domain: Option<String>,
        sso_name: Option<String>,
    ) -> StorableOpenIdCredential {
        StorableOpenIdCredential {
            iss: "https://auth.acme.com".to_string(),
            sub: "user123".to_string(),
            aud: "client456".to_string(),
            last_usage_timestamp: Some(42),
            metadata: HashMap::new(),
            sso_domain,
            sso_name,
            stable_id: None,
        }
    }

    #[test]
    fn should_roundtrip_through_storable() {
        let mut credential =
            credential(Some("acme.com".to_string()), Some("Acme Corp".to_string()));
        credential.stable_id = Some("acme-oid-0001".to_string());
        let decoded = StorableOpenIdCredential::from_bytes(credential.to_bytes());
        assert_eq!(decoded.iss, credential.iss);
        assert_eq!(decoded.sub, credential.sub);
        assert_eq!(decoded.aud, credential.aud);
        assert_eq!(
            decoded.last_usage_timestamp,
            credential.last_usage_timestamp
        );
        assert_eq!(decoded.sso_domain, Some("acme.com".to_string()));
        assert_eq!(decoded.sso_name, Some("Acme Corp".to_string()));
        assert_eq!(decoded.stable_id, Some("acme-oid-0001".to_string()));
    }

    /// Credentials written before the `sso_domain` / `sso_name` / `stable_id`
    /// fields existed are CBOR maps without keys 5, 6 and 7 — they must decode
    /// with all three `None`.
    #[test]
    fn should_decode_credential_without_sso_fields() {
        // Encode the pre-`sso_domain` shape: a CBOR map with keys 0..=4 only.
        let mut buffer = Vec::new();
        let mut encoder = minicbor::Encoder::new(&mut buffer);
        encoder.map(5).unwrap();
        encoder.u8(0).unwrap().str("https://auth.acme.com").unwrap();
        encoder.u8(1).unwrap().str("user123").unwrap();
        encoder.u8(2).unwrap().str("client456").unwrap();
        encoder.u8(3).unwrap().u64(42).unwrap();
        encoder.u8(4).unwrap().map(0).unwrap();

        let decoded = StorableOpenIdCredential::from_bytes(Cow::Borrowed(&buffer));
        assert_eq!(decoded.iss, "https://auth.acme.com");
        assert_eq!(decoded.sub, "user123");
        assert_eq!(decoded.aud, "client456");
        assert_eq!(decoded.last_usage_timestamp, Some(42));
        assert_eq!(decoded.sso_domain, None);
        assert_eq!(decoded.sso_name, None);
        assert_eq!(decoded.stable_id, None);
    }

    /// Rollback safety: bytes written by the new encoder (with keys 5 and 6
    /// present) must remain decodable when the unknown keys are skipped —
    /// minicbor's `#[cbor(map)]` derive ignores unknown map keys, which is
    /// what a rolled-back (pre-`sso_domain`) wasm relies on. Cheaply
    /// approximated here by decoding new-encoder bytes and checking the
    /// pre-existing fields survive untouched.
    #[test]
    fn should_keep_preexisting_fields_when_sso_fields_present() {
        let credential = credential(Some("acme.com".to_string()), None);
        let decoded = StorableOpenIdCredential::from_bytes(credential.to_bytes());
        assert_eq!(decoded.iss, credential.iss);
        assert_eq!(decoded.sub, credential.sub);
        assert_eq!(decoded.aud, credential.aud);
        assert_eq!(decoded.sso_domain, Some("acme.com".to_string()));
        assert_eq!(decoded.sso_name, None);
    }
}
