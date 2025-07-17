use crate::delegation::{add_delegation_signature, der_encode_canister_sig_key};
use crate::MINUTE_NS;
use crate::{state, update_root_hash};
use candid::{CandidType, Deserialize, Principal};
use ic_canister_sig_creation::{
    delegation_signature_msg, signature_map::CanisterSigInputs, DELEGATION_SIG_DOMAIN,
};
use ic_cdk::api::time;
use ic_certification::Hash;
use identity_jose::jws::Decoder;
use internet_identity_interface::internet_identity::types::openid::{
    OpenIdCredentialAddError, OpenIdDelegationError,
};
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, Delegation, IdRegFinishError, MetadataEntryV2, OpenIdConfig, PublicKey,
    SessionKey, SignedDelegation, Timestamp, UserKey,
};
use serde_bytes::ByteBuf;
use sha2::{Digest, Sha256};
use std::{cell::RefCell, collections::HashMap};

mod google;

const OPENID_SESSION_DURATION_NS: u64 = 30 * MINUTE_NS;

pub type OpenIdCredentialKey = (Iss, Sub);
pub type Iss = String;
pub type Sub = String;
pub type Aud = String;

#[derive(PartialEq, Eq, CandidType, Deserialize, Clone, Debug)]
pub enum OpenIDJWTVerificationError {
    GenericError(String),
    JWTExpired,
}

// Implementation of From trait to convert OpenIDJWTVerificationError to OpenIdCredentialAddError
impl From<OpenIDJWTVerificationError> for OpenIdCredentialAddError {
    fn from(error: OpenIDJWTVerificationError) -> Self {
        match error {
            OpenIDJWTVerificationError::JWTExpired => OpenIdCredentialAddError::JwtExpired,
            OpenIDJWTVerificationError::GenericError(_) => {
                OpenIdCredentialAddError::JwtVerificationFailed
            }
        }
    }
}

// Implementation of From trait to convert OpenIDJWTVerificationError to OpenIdDelegationError
impl From<OpenIDJWTVerificationError> for OpenIdDelegationError {
    fn from(error: OpenIDJWTVerificationError) -> Self {
        match error {
            OpenIDJWTVerificationError::JWTExpired => OpenIdDelegationError::JwtExpired,
            OpenIDJWTVerificationError::GenericError(_) => {
                OpenIdDelegationError::JwtVerificationFailed
            }
        }
    }
}

// Implementation of From trait to convert OpenIDJWTVerificationError to IdRegFinishError
impl From<OpenIDJWTVerificationError> for IdRegFinishError {
    fn from(error: OpenIDJWTVerificationError) -> Self {
        match error {
            OpenIDJWTVerificationError::JWTExpired => {
                IdRegFinishError::InvalidAuthnMethod("JWT expired".to_string())
            }
            OpenIDJWTVerificationError::GenericError(msg) => {
                IdRegFinishError::InvalidAuthnMethod(format!("JWT verification failed: {msg}"))
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, CandidType, Deserialize, Clone)]
pub struct OpenIdCredential {
    pub iss: Iss,
    pub sub: Sub,
    pub aud: Aud,
    pub last_usage_timestamp: Option<Timestamp>,
    pub metadata: HashMap<String, MetadataEntryV2>,
}

impl OpenIdCredential {
    pub fn key(&self) -> OpenIdCredentialKey {
        (self.iss.clone(), self.sub.clone())
    }

    pub fn principal(&self, anchor_number: AnchorNumber) -> Principal {
        let seed = calculate_delegation_seed(&self.aud, &self.key(), anchor_number);
        let public_key: PublicKey = der_encode_canister_sig_key(seed.to_vec()).into();
        Principal::self_authenticating(public_key)
    }

    pub async fn prepare_jwt_delegation(
        &self,
        session_key: SessionKey,
        anchor_number: AnchorNumber,
    ) -> (UserKey, Timestamp) {
        state::ensure_salt_set().await;

        let expiration = time().saturating_add(OPENID_SESSION_DURATION_NS);
        let seed = calculate_delegation_seed(&self.aud, &self.key(), anchor_number);

        state::signature_map_mut(|sigs| {
            add_delegation_signature(sigs, session_key, seed.as_ref(), expiration);
        });
        update_root_hash();

        (
            ByteBuf::from(der_encode_canister_sig_key(seed.to_vec())),
            expiration,
        )
    }

    pub fn get_jwt_delegation(
        &self,
        session_key: SessionKey,
        expiration: Timestamp,
        anchor_number: AnchorNumber,
    ) -> Result<SignedDelegation, OpenIdDelegationError> {
        state::assets_and_signatures(|certified_assets, sigs| {
            let inputs = CanisterSigInputs {
                domain: DELEGATION_SIG_DOMAIN,
                seed: &calculate_delegation_seed(&self.aud, &self.key(), anchor_number),
                message: &delegation_signature_msg(&session_key, expiration, None),
            };

            match sigs.get_signature_as_cbor(&inputs, Some(certified_assets.root_hash())) {
                Ok(signature) => Ok(SignedDelegation {
                    delegation: Delegation {
                        pubkey: session_key,
                        expiration,
                        targets: None,
                    },
                    signature: ByteBuf::from(signature),
                }),
                Err(_) => Err(OpenIdDelegationError::NoSuchDelegation),
            }
        })
    }
}

pub trait OpenIdProvider {
    fn issuer(&self) -> &'static str;

    /// Verify JWT and bound nonce with salt, return `OpenIdCredential` if successful
    ///
    /// # Arguments
    ///
    /// * `jwt`: The JWT returned by the OpenID authentication flow with the OpenID provider
    /// * `salt`: The random salt that was used to bind the nonce to the caller principal
    fn verify(
        &self,
        jwt: &str,
        salt: &[u8; 32],
    ) -> Result<OpenIdCredential, OpenIDJWTVerificationError>;

    fn metadata_name(&self, metadata: HashMap<String, MetadataEntryV2>) -> Option<String>;
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

pub fn with_provider<F, R>(jwt: &str, callback: F) -> Result<R, OpenIDJWTVerificationError>
where
    F: FnOnce(&dyn OpenIdProvider) -> Result<R, OpenIDJWTVerificationError>,
{
    let validation_item = Decoder::new()
        .decode_compact_serialization(jwt.as_bytes(), None)
        .map_err(|_| {
            OpenIDJWTVerificationError::GenericError("Failed to decode JWT".to_string())
        })?;

    let claims: PartialClaims = serde_json::from_slice(validation_item.claims()).map_err(|_| {
        OpenIDJWTVerificationError::GenericError("Unable to decode claims".to_string())
    })?;

    PROVIDERS.with_borrow(|providers| {
        providers
            .iter()
            .find(|provider| provider.issuer() == claims.iss)
            .ok_or_else(|| {
                OpenIDJWTVerificationError::GenericError(format!(
                    "Unsupported issuer: {}",
                    claims.iss
                ))
            })
            .and_then(|provider| callback(provider.as_ref()))
    })
}

/// Create `Hash` used for a delegation that can make calls on behalf of a `OpenIdCredential`
///
/// # Arguments
///
/// * `client_id`: The client id for which the `OpenIdCredential` was created
/// * `(iss, sub)`: The key of the `OpenIdCredential` to create a `Hash` from
/// * `anchor_number`: The anchor number the credential is assigned to
#[allow(clippy::cast_possible_truncation)]
fn calculate_delegation_seed(
    client_id: &str,
    (iss, sub): &OpenIdCredentialKey,
    anchor_number: AnchorNumber,
) -> Hash {
    let mut blob: Vec<u8> = vec![];
    blob.push(32);
    blob.extend_from_slice(&salt());
    blob.push(client_id.len() as u8);
    blob.extend(client_id.bytes());

    blob.push(iss.len() as u8);
    blob.extend(iss.bytes());

    blob.push(sub.len() as u8);
    blob.extend(sub.bytes());

    blob.push(anchor_number.to_be_bytes().len() as u8);
    blob.extend(anchor_number.to_le_bytes());

    let mut hasher = Sha256::new();
    hasher.update(blob);
    hasher.finalize().into()
}

/// Get salt unique to this II canister instance, used to make the `Hash` (and thus `Principal`)
/// unique between instances for the same `OpenIdCredential`, intentionally isolating the instances.
#[cfg(not(test))]
fn salt() -> [u8; 32] {
    state::salt()
}

/// Skip getting salt from state in tests, instead return a fixed salt
#[cfg(test)]
fn salt() -> [u8; 32] {
    [0; 32]
}

#[cfg(test)]
struct ExampleProvider;

#[cfg(test)]
impl OpenIdProvider for ExampleProvider {
    fn issuer(&self) -> &'static str {
        "https://example.com"
    }

    fn verify(
        &self,
        _: &str,
        _: &[u8; 32],
    ) -> Result<OpenIdCredential, OpenIDJWTVerificationError> {
        Ok(self.credential())
    }

    fn metadata_name(&self, _metadata: HashMap<String, MetadataEntryV2>) -> Option<String> {
        None
    }
}

#[cfg(test)]
impl ExampleProvider {
    fn credential(&self) -> OpenIdCredential {
        OpenIdCredential {
            iss: self.issuer().into(),
            sub: "example-sub".into(),
            aud: "example-aud".into(),
            last_usage_timestamp: None,
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

    assert_eq!(
        with_provider(jwt, |provider| provider.verify(jwt, &[0u8; 32])),
        Ok(credential)
    );
}

#[test]
fn should_return_error_unsupported_issuer() {
    PROVIDERS.replace(vec![]);
    let jwt = "eyJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJodHRwczovL2V4YW1wbGUuY29tIn0.SBeD7pV65F98wStsBuC_VRn-yjLoyf6iojJl9Y__wN0";

    assert_eq!(
        with_provider(jwt, |provider| provider.verify(jwt, &[0u8; 32])),
        Err(OpenIDJWTVerificationError::GenericError(
            "Unsupported issuer: https://example.com".to_string()
        ))
    );
}

#[test]
fn should_return_error_when_encoding_invalid() {
    let invalid_jwt = "invalid-jwt";

    assert_eq!(
        with_provider(invalid_jwt, |provider| provider
            .verify(invalid_jwt, &[0u8; 32])),
        Err(OpenIDJWTVerificationError::GenericError(
            "Failed to decode JWT".to_string()
        ))
    );
}

#[test]
fn should_return_error_when_claims_invalid() {
    let jwt_without_issuer = "eyJhbGciOiJIUzI1NiJ9.e30.ZRrHA1JJJW8opsbCGfG_HACGpVUMN_a9IV7pAx_Zmeo";

    assert_eq!(
        with_provider(jwt_without_issuer, |provider| provider
            .verify(jwt_without_issuer, &[0u8; 32])),
        Err(OpenIDJWTVerificationError::GenericError(
            "Unable to decode claims".to_string()
        ))
    );
}
