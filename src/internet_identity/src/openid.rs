use candid::{CandidType, Deserialize, Principal};
use ic_certification::Hash;
use identity_jose::jws::Decoder;
use internet_identity_interface::internet_identity::types::{
    MetadataEntryV2, OpenIdConfig, Timestamp,
};
use sha2::{Digest, Sha256};
use std::cell::RefCell;
use std::collections::HashMap;

mod google;

pub type Iss = String;
pub type Sub = String;
pub type Aud = String;

#[derive(Debug, PartialEq, Eq, CandidType, Deserialize, Clone)]
pub struct OpenIdCredential {
    pub iss: Iss,
    pub sub: Sub,
    pub aud: Aud,
    pub delegation_principal: Principal,
    pub last_usage_timestamp: Timestamp,
    pub metadata: HashMap<String, MetadataEntryV2>,
}

trait OpenIdProvider {
    fn issuer(&self) -> &'static str;

    fn verify(&self, jwt: &str, salt: &[u8; 32]) -> Result<OpenIdCredential, String>;
}

#[derive(Deserialize)]
struct PartialClaims {
    iss: String,
}

thread_local! {
    static PROVIDERS: RefCell<Vec<Box<dyn OpenIdProvider >>> = RefCell::new(vec![]);
}

pub fn setup_google(config: OpenIdConfig) {
    PROVIDERS
        .with_borrow_mut(|providers| providers.push(Box::new(google::Provider::create(config))));
}

#[allow(unused)]
pub fn verify(jwt: &str, salt: &[u8; 32]) -> Result<OpenIdCredential, String> {
    let validation_item = Decoder::new()
        .decode_compact_serialization(jwt.as_bytes(), None)
        .map_err(|_| "Failed to decode JWT")?;
    let claims: PartialClaims =
        serde_json::from_slice(validation_item.claims()).map_err(|_| "Unable to decode claims")?;

    PROVIDERS.with_borrow(|providers| {
        match providers
            .iter()
            .find(|provider| provider.issuer() == claims.iss)
        {
            Some(provider) => provider.verify(jwt, salt),
            None => Err(format!("Unsupported issuer: {}", claims.iss)),
        }
    })
}

#[allow(clippy::cast_possible_truncation)]
fn calculate_seed(client_id: &str, iss: &Iss, sub: &Sub) -> Hash {
    let mut blob: Vec<u8> = vec![];
    blob.push(32);
    blob.extend_from_slice(&salt());

    blob.push(client_id.bytes().len() as u8);
    blob.extend(client_id.bytes());

    blob.push(iss.bytes().len() as u8);
    blob.extend(iss.bytes());

    blob.push(sub.bytes().len() as u8);
    blob.extend(sub.bytes());

    let mut hasher = Sha256::new();
    hasher.update(blob);
    hasher.finalize().into()
}

#[cfg(not(test))]
fn salt() -> [u8; 32] {
    crate::state::salt()
}

#[cfg(test)]
fn salt() -> [u8; 32] {
    [0; 32]
}

#[cfg(not(test))]
fn get_delegation_principal(client_id: &str, iss: &Iss, sub: &Sub) -> Principal {
    let seed = calculate_seed(client_id, iss, sub);
    let public_key = crate::delegation::der_encode_canister_sig_key(seed.to_vec());
    Principal::self_authenticating(public_key)
}

/// Skip `der_encode_canister_sig_key` in tests and create (invalid) Principal from seed data
#[cfg(test)]
fn get_delegation_principal(client_id: &str, iss: &Iss, sub: &Sub) -> Principal {
    Principal::self_authenticating(calculate_seed(client_id, iss, sub))
}

#[cfg(test)]
struct ExampleProvider;

#[cfg(test)]
impl OpenIdProvider for ExampleProvider {
    fn issuer(&self) -> &'static str {
        "https://example.com"
    }

    fn verify(&self, _: &str, _: &[u8; 32]) -> Result<OpenIdCredential, String> {
        Ok(self.credential())
    }
}

#[cfg(test)]
impl ExampleProvider {
    fn credential(&self) -> OpenIdCredential {
        OpenIdCredential {
            iss: self.issuer().into(),
            sub: "example-sub".into(),
            aud: "example-aud".into(),
            delegation_principal: Principal::anonymous(),
            last_usage_timestamp: 0,
            metadata: HashMap::new(),
        }
    }
}

#[test]
fn should_return_credential() {
    let provider = ExampleProvider {};
    let credential = provider.credential();
    PROVIDERS.replace(vec![Box::new(provider)]);
    let jwt = "eyJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJodHRwczovL2V4YW1wbGUuY29tIn0.SBeD7pV65F98wStsBuC_VRn-yjLoyf6iojJl9Y__wN0";

    assert_eq!(verify(jwt, &[0u8; 32]), Ok(credential));
}

#[test]
fn should_return_error_unsupported_issuer() {
    PROVIDERS.replace(vec![]);
    let jwt = "eyJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJodHRwczovL2V4YW1wbGUuY29tIn0.SBeD7pV65F98wStsBuC_VRn-yjLoyf6iojJl9Y__wN0";

    assert_eq!(
        verify(jwt, &[0u8; 32]),
        Err("Unsupported issuer: https://example.com".into())
    );
}

#[test]
fn should_return_error_when_encoding_invalid() {
    let invalid_jwt = "invalid-jwt";

    assert_eq!(
        verify(invalid_jwt, &[0u8; 32]),
        Err("Failed to decode JWT".to_string())
    );
}

#[test]
fn should_return_error_when_claims_invalid() {
    let jwt_without_issuer = "eyJhbGciOiJIUzI1NiJ9.e30.ZRrHA1JJJW8opsbCGfG_HACGpVUMN_a9IV7pAx_Zmeo";

    assert_eq!(
        verify(jwt_without_issuer, &[0u8; 32]),
        Err("Unable to decode claims".to_string())
    );
}
