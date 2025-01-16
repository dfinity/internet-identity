use candid::{Deserialize, Principal};
use identity_jose::jws::Decoder;
use internet_identity_interface::internet_identity::types::{
    MetadataEntryV2, OpenIdConfig, Timestamp,
};
use std::cell::RefCell;
use std::collections::HashMap;

mod google;

#[derive(Debug, PartialEq)]
pub struct OpenIdCredential {
    pub iss: String,
    pub sub: String,
    pub aud: String,
    pub principal: Principal,
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
    static OPEN_ID_PROVIDERS: RefCell<Vec<Box<dyn OpenIdProvider >>> = RefCell::new(vec![]);
}

pub fn setup_google(config: OpenIdConfig) {
    OPEN_ID_PROVIDERS
        .with_borrow_mut(|providers| providers.push(Box::new(google::Provider::create(config))));
}

#[allow(unused)]
pub fn verify(jwt: &str, salt: &[u8; 32]) -> Result<OpenIdCredential, String> {
    let validation_item = Decoder::new()
        .decode_compact_serialization(jwt.as_bytes(), None)
        .map_err(|_| "Failed to decode JWT")?;
    let claims: PartialClaims =
        serde_json::from_slice(validation_item.claims()).map_err(|_| "Unable to decode claims")?;

    OPEN_ID_PROVIDERS.with_borrow(|providers| {
        match providers
            .iter()
            .find(|provider| provider.issuer() == claims.iss)
        {
            Some(provider) => provider.verify(jwt, salt),
            None => Err(format!("Unsupported issuer: {}", claims.iss)),
        }
    })
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
            principal: Principal::anonymous(),
            last_usage_timestamp: 0,
            metadata: HashMap::new(),
        }
    }
}

#[test]
fn should_return_credential() {
    let provider = ExampleProvider {};
    let credential = provider.credential();
    OPEN_ID_PROVIDERS.replace(vec![Box::new(provider)]);
    let jwt = "eyJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJodHRwczovL2V4YW1wbGUuY29tIn0.SBeD7pV65F98wStsBuC_VRn-yjLoyf6iojJl9Y__wN0";

    assert_eq!(verify(jwt, &[0u8; 32]), Ok(credential));
}

#[test]
fn should_return_error_unsupported_issuer() {
    OPEN_ID_PROVIDERS.replace(vec![]);
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
