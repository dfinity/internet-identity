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
use internet_identity_interface::internet_identity::types::attributes::AttributeScope;
use internet_identity_interface::internet_identity::types::openid::{
    OpenIdCredentialAddError, OpenIdDelegationError,
};
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, Delegation, DiscoverableOidcConfig, IdRegFinishError, MetadataEntryV2,
    OidcConfig, OpenIdConfig, OpenIdEmailVerificationScheme, PublicKey, SessionKey,
    SignedDelegation, Timestamp, UserKey,
};
use serde_bytes::ByteBuf;
use sha2::{Digest, Sha256};
use std::{cell::RefCell, collections::HashMap};

pub(crate) mod generic;

// Re-export the SSO-metadata lookup so the storage-layer
// `OpenIdCredential → OpenIdCredentialData` conversion can call it
// without making the whole `generic` module public.
pub use generic::sso_fields_for;

pub const OPENID_SESSION_DURATION_NS: u64 = 30 * MINUTE_NS;

pub type OpenIdCredentialKey = (Iss, Sub, Aud);
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
        (self.iss.clone(), self.sub.clone(), self.aud.clone())
    }

    pub fn principal(&self, anchor_number: AnchorNumber) -> Principal {
        let seed = calculate_delegation_seed(&self.key(), anchor_number);
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
        let seed = calculate_delegation_seed(&self.key(), anchor_number);

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
                seed: &calculate_delegation_seed(&self.key(), anchor_number),
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

    /// Helper method to find the matching provider for this credential and execute a callback with it
    fn with_provider<F, R>(&self, f: F) -> Option<R>
    where
        F: FnOnce(&dyn OpenIdProvider) -> Option<R>,
    {
        PROVIDERS.with_borrow(|providers| {
            providers
                .iter()
                .find(|provider| {
                    // Skip providers whose discovery is still pending.
                    let Some(template) = provider.issuer() else {
                        return false;
                    };
                    let Some(provider_aud) = provider.client_id() else {
                        return false;
                    };
                    let issuer_placeholders = get_issuer_placeholders(&template);
                    let mut issuer_claims: Vec<(String, String)> = vec![];
                    for key in issuer_placeholders {
                        if let Some(MetadataEntryV2::String(value)) = self.metadata.get(&key) {
                            issuer_claims.push((key, value.to_string()));
                        } else {
                            return false;
                        }
                    }
                    let effective_issuer = replace_issuer_placeholders(&template, &issuer_claims);
                    effective_issuer == self.iss && provider_aud == self.aud
                })
                .and_then(|provider| f(provider.as_ref()))
        })
    }

    /// Find current config for stored credential
    pub fn config_issuer(&self) -> Option<String> {
        self.with_provider(|provider| provider.issuer())
    }

    /// Returns the `discovery_domain` of the SSO provider that currently
    /// matches this credential, if any. `None` for credentials matched to a
    /// hardcoded (non-discoverable) `Provider` — those are addressable via
    /// the `openid:<issuer>` attribute scope instead.
    ///
    /// Used to route attribute requests to the `sso:<domain>` scope with
    /// exclusive semantics: a credential with `Some(domain)` here is
    /// reachable via `sso:<domain>` only, never via `openid:<issuer>`.
    pub fn discovery_domain(&self) -> Option<String> {
        self.with_provider(|provider| provider.discovery_domain())
    }

    /// Returns the single `AttributeScope` this credential is addressable
    /// under, computed with one provider lookup. This is the source of truth
    /// for scope exclusivity:
    ///
    /// - `Some(Sso { domain })`     for credentials matched to a `DiscoverableProvider`
    /// - `Some(OpenId { issuer })`  for credentials matched to a hardcoded provider
    /// - `None`                     when no provider currently matches (discovery
    ///   pending or provider unregistered) — credential is unreachable.
    ///
    /// Prefer this over calling `discovery_domain()` + `config_issuer()`
    /// separately, which would perform two `with_provider` scans per
    /// credential in hot paths.
    pub fn matched_attribute_scope(&self) -> Option<AttributeScope> {
        self.with_provider(|provider| match provider.discovery_domain() {
            Some(domain) => Some(AttributeScope::Sso { domain }),
            None => provider
                .issuer()
                .map(|issuer| AttributeScope::OpenId { issuer }),
        })
    }

    fn read_attribute_as_string(&self, attribute_name: &str) -> Option<String> {
        let MetadataEntryV2::String(value) = self.metadata.get(attribute_name)? else {
            return None;
        };

        Some(value.clone())
    }

    pub fn get_name(&self) -> Option<String> {
        self.read_attribute_as_string("name")
    }

    pub fn get_email(&self) -> Option<String> {
        self.read_attribute_as_string("email")
    }

    fn get_google_verified_email(&self) -> Option<String> {
        let email_verified = self.read_attribute_as_string("email_verified")?;

        if !email_verified.eq_ignore_ascii_case("true") {
            return None;
        }

        self.get_email()
    }

    fn get_microsoft_verified_email(&self) -> Option<String> {
        // For Microsoft, check if tid matches the personal account tenant ID
        // (services like Xbox, Teams for Life, or Outlook.com)
        //
        // See Microsoft identity platform documentation:
        // See https://learn.microsoft.com/en-us/entra/identity-platform/id-token-claims-reference#payload-claims
        const MICROSOFT_PERSONAL_ACCOUNT_TENANT_ID: &str = "9188040d-6c67-4c5b-b112-36a304b66dad";

        let tid = self.read_attribute_as_string("tid")?;

        if tid != MICROSOFT_PERSONAL_ACCOUNT_TENANT_ID {
            return None;
        }

        self.get_email()
    }

    /// Return the verified email for this credential, if available
    pub fn get_verified_email(&self) -> Option<String> {
        self.with_provider(|provider| {
            use OpenIdEmailVerificationScheme::*;

            let verification_scheme = provider.email_verification_scheme()?;

            match verification_scheme {
                Unknown => None,
                Google => self.get_google_verified_email(),
                Microsoft => self.get_microsoft_verified_email(),
            }
        })
    }
}

pub trait OpenIdProvider {
    /// Issuer template for this provider. `None` means discovery is still pending —
    /// matching and verification skip such providers. May contain placeholders like
    /// `{tid}` that are filled from JWT claims before comparison.
    fn issuer(&self) -> Option<String>;

    /// Client id (the expected JWT `aud`) for this provider. `None` means discovery
    /// is still pending.
    fn client_id(&self) -> Option<String>;

    fn email_verification_scheme(&self) -> Option<OpenIdEmailVerificationScheme>;

    /// SSO discovery domain for this provider, if any. Returns `Some(domain)` only
    /// for providers created from a `DiscoverableOidcConfig` (the two-hop SSO
    /// discovery flow). All other providers (Google, Microsoft, hardcoded
    /// generic OIDC) inherit the default `None`.
    ///
    /// Credentials whose matched provider reports `Some(domain)` are reachable
    /// via the `sso:<domain>` attribute scope and are *not* reachable via
    /// `openid:<issuer>` — see `OpenIdCredential::discovery_domain` and
    /// `Anchor::prepare_openid_attributes` / `prepare_sso_attributes`.
    fn discovery_domain(&self) -> Option<String> {
        None
    }

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
}

#[derive(Deserialize)]
struct PartialClaims {
    iss: String,
    aud: AudClaim,
}

/// JWT `aud` claim — per RFC 7519 may be a single string or an array of strings
/// (Microsoft sometimes ships it as an array). Matching treats an array as a
/// set: any element equal to the provider's configured client id is accepted.
#[derive(Deserialize, Clone)]
#[serde(untagged)]
pub(super) enum AudClaim {
    Single(String),
    Multiple(Vec<String>),
}

impl AudClaim {
    /// Returns `true` if `expected` is the single value or contained in the array.
    pub(super) fn matches(&self, expected: &str) -> bool {
        match self {
            AudClaim::Single(s) => s == expected,
            AudClaim::Multiple(v) => v.iter().any(|s| s == expected),
        }
    }

    /// Returns `true` if the claim carries no audience at all.
    pub(super) fn is_empty(&self) -> bool {
        match self {
            AudClaim::Single(s) => s.is_empty(),
            AudClaim::Multiple(v) => v.is_empty(),
        }
    }
}

thread_local! {
    static PROVIDERS: RefCell<Vec<Box<dyn OpenIdProvider >>> = RefCell::new(vec![]);
    static OIDC_CONFIGS: RefCell<Vec<DiscoverableOidcConfig>> = const { RefCell::new(vec![]) };
}

pub fn setup(configs: Vec<OpenIdConfig>) {
    PROVIDERS.with_borrow_mut(|providers| {
        for config in configs {
            providers.push(Box::new(generic::Provider::create(config)));
        }
    });
}

pub fn setup_oidc(configs: Vec<DiscoverableOidcConfig>) {
    for config in configs {
        add_oidc_config_internal(config);
    }
    #[cfg(not(test))]
    generic::init_discovery_timers();
}

pub fn add_oidc_config(config: DiscoverableOidcConfig) {
    if !generic::is_allowed_discovery_domain(&config.discovery_domain) {
        ic_cdk::trap(&format!(
            "discovery_domain '{}' is not on the canary allowlist",
            config.discovery_domain
        ));
    }

    // Canonicalize the domain to lowercase. DNS hostnames are
    // case-insensitive and `is_allowed_discovery_domain` already accepts
    // case-insensitively via `eq_ignore_ascii_case`. Storing a canonical form
    // keeps `OIDC_CONFIGS`, `DISCOVERY_TASKS`, `DiscoverableProvider`, and
    // downstream attribute-scope matching (`sso:<domain>`) all in sync.
    let config = DiscoverableOidcConfig {
        discovery_domain: config.discovery_domain.to_ascii_lowercase(),
    };

    // Skip if already registered
    let already_exists = OIDC_CONFIGS.with_borrow(|stored| {
        stored
            .iter()
            .any(|c| c.discovery_domain == config.discovery_domain)
    });
    if already_exists {
        return;
    }

    add_oidc_config_internal(config.clone());

    // Persist to state so it survives upgrades
    state::persistent_state_mut(|persistent_state| {
        let configs = persistent_state.oidc_configs.get_or_insert_with(Vec::new);
        configs.push(config);
    });

    #[cfg(not(test))]
    generic::init_discovery_timers();
}

fn add_oidc_config_internal(config: DiscoverableOidcConfig) {
    // Canonicalize so `OIDC_CONFIGS`, `DISCOVERY_TASKS` (in `DiscoverableProvider`),
    // and downstream `sso:<domain>` scope matching stay in sync. `add_oidc_config`
    // already canonicalizes, but `setup_oidc` replays persisted configs at
    // post-upgrade time which may contain values from before this change.
    let config = DiscoverableOidcConfig {
        discovery_domain: config.discovery_domain.to_ascii_lowercase(),
    };
    OIDC_CONFIGS.with_borrow_mut(|stored| {
        stored.push(config.clone());
    });
    PROVIDERS.with_borrow_mut(|providers| {
        providers.push(Box::new(generic::DiscoverableProvider::create(config)));
    });
}

pub fn get_discovered_oidc_configs() -> Vec<OidcConfig> {
    OIDC_CONFIGS.with_borrow(|configs| {
        configs
            .iter()
            .map(|config| {
                let (client_id, openid_configuration, issuer) =
                    generic::discovered_state_for(&config.discovery_domain);
                OidcConfig {
                    discovery_domain: config.discovery_domain.clone(),
                    client_id,
                    openid_configuration,
                    issuer,
                }
            })
            .collect()
    })
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

    let PartialClaims { iss, aud } =
        serde_json::from_slice(validation_item.claims()).map_err(|_| {
            OpenIDJWTVerificationError::GenericError("Unable to decode claims".to_string())
        })?;
    if aud.is_empty() {
        return Err(OpenIDJWTVerificationError::GenericError(
            "JWT has empty aud claim".to_string(),
        ));
    }

    PROVIDERS.with_borrow(|providers| {
        providers
            .iter()
            .find(|provider| {
                // Skip providers whose discovery is still pending.
                let Some(template) = provider.issuer() else {
                    return false;
                };
                let Some(provider_aud) = provider.client_id() else {
                    return false;
                };
                let issuer_placeholders = get_issuer_placeholders(&template);
                let issuer_claims = get_all_claims(validation_item.claims(), issuer_placeholders);
                let effective_issuer = replace_issuer_placeholders(&template, &issuer_claims);
                effective_issuer == iss && aud.matches(&provider_aud)
            })
            .ok_or_else(|| {
                // If any provider is still waiting on discovery, surface a "pending" error
                // rather than the generic "unsupported issuer" so the frontend can retry.
                let has_pending = providers
                    .iter()
                    .any(|p| p.issuer().is_none() || p.client_id().is_none());
                if has_pending {
                    OpenIDJWTVerificationError::GenericError(
                        "OIDC provider discovery is still in progress, please try again shortly"
                            .to_string(),
                    )
                } else {
                    OpenIDJWTVerificationError::GenericError(format!("Unsupported issuer: {}", iss))
                }
            })
            .and_then(|provider| callback(provider.as_ref()))
    })
}

/// As seen in <https://login.microsoftonline.com/common/v2.0/.well-known/openid-configuration>,
/// the Microsoft issuer uri is dynamic based on placeholders, this method returns them e.g. `["tid"]`.
pub fn get_issuer_placeholders(template: &str) -> Vec<String> {
    let mut keys: Vec<String> = vec![];
    let mut remaining = template;

    // TODO: Simplify by using Regex once we are not constrained by WASM size
    while let Some(open_pos) = remaining.find('{') {
        remaining = &remaining[open_pos + 1..];

        if let Some(close_pos) = remaining.find('}') {
            let key = &remaining[..close_pos];
            keys.push(key.to_string());
            // Move past '}'
            remaining = &remaining[close_pos + 1..];
        } else {
            // No closing '}', return the empty vector
            return vec![];
        }
    }

    keys
}

/// Either get all claims for the given keys or nothing.
pub fn get_all_claims(claims_bytes: &[u8], keys: Vec<String>) -> Vec<(String, String)> {
    let Ok(claims) = serde_json::from_slice::<serde_json::Value>(claims_bytes) else {
        return vec![]; // If claims cannot be decoded, return empty vector
    };
    let mut result: Vec<(String, String)> = vec![];
    for key in keys {
        if let Some(claim) = claims.get(key.clone()).and_then(|v| v.as_str()) {
            result.push((key.to_string(), claim.to_string()));
        } else {
            return vec![];
        }
    }
    result
}

/// As seen in <https://login.microsoftonline.com/common/v2.0/.well-known/openid-configuration>,
/// the Microsoft issuer uri is dynamic based on the `tid` claim, this method makes sure to
/// replace the placeholders like e.g. `tid` in the issuer uri with the corresponding claims.
pub fn replace_issuer_placeholders(template: &str, claims: &Vec<(String, String)>) -> String {
    let mut result = template.to_string();
    for (key, value) in claims {
        result = result.replace(&format!("{{{key}}}"), value);
    }
    result
}

/// Create `Hash` used for a delegation that can make calls on behalf of a `OpenIdCredential`.
///
/// All three key components (`iss`, `sub`, `aud`) participate in the seed so that the
/// same user at the same provider with different OIDC clients derives distinct
/// principals — this is the security property that makes SSO safe.
///
/// # Arguments
///
/// * `(iss, sub, aud)`: The key of the `OpenIdCredential` to derive the `Hash` from
/// * `anchor_number`: The anchor number the credential is assigned to
#[allow(clippy::cast_possible_truncation)]
fn calculate_delegation_seed(
    (iss, sub, aud): &OpenIdCredentialKey,
    anchor_number: AnchorNumber,
) -> Hash {
    let mut blob: Vec<u8> = vec![];
    blob.push(32);
    blob.extend_from_slice(&salt());
    blob.push(aud.len() as u8);
    blob.extend(aud.bytes());

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
struct ExampleProvider {
    issuer: Option<String>,
    client_id: Option<String>,
}

#[cfg(test)]
impl Default for ExampleProvider {
    fn default() -> Self {
        Self {
            issuer: Some("https://example.com".into()),
            client_id: Some("example-aud".into()),
        }
    }
}

#[cfg(test)]
impl OpenIdProvider for ExampleProvider {
    fn issuer(&self) -> Option<String> {
        self.issuer.clone()
    }

    fn client_id(&self) -> Option<String> {
        self.client_id.clone()
    }

    fn email_verification_scheme(&self) -> Option<OpenIdEmailVerificationScheme> {
        None
    }

    fn verify(
        &self,
        _: &str,
        _: &[u8; 32],
    ) -> Result<OpenIdCredential, OpenIDJWTVerificationError> {
        Ok(self.credential())
    }
}

#[cfg(test)]
impl ExampleProvider {
    fn credential(&self) -> OpenIdCredential {
        OpenIdCredential {
            iss: self
                .issuer
                .clone()
                .unwrap_or_else(|| "https://example.com".into()),
            sub: "example-sub".into(),
            aud: self
                .client_id
                .clone()
                .unwrap_or_else(|| "example-aud".into()),
            last_usage_timestamp: None,
            metadata: HashMap::new(),
        }
    }
}

// Example JWT with {"iss":"https://example.com","aud":"example-aud"}.
#[cfg(test)]
const EXAMPLE_JWT: &str = "eyJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJodHRwczovL2V4YW1wbGUuY29tIiwiYXVkIjoiZXhhbXBsZS1hdWQifQ.SBeD7pV65F98wStsBuC_VRn-yjLoyf6iojJl9Y__wN0";

#[test]
fn should_return_credential() {
    let provider = ExampleProvider::default();
    let credential = provider.credential();
    PROVIDERS.replace(vec![Box::new(provider)]);

    assert_eq!(
        with_provider(EXAMPLE_JWT, |provider| provider
            .verify(EXAMPLE_JWT, &[0u8; 32])),
        Ok(credential)
    );
}

#[test]
fn should_return_error_unsupported_issuer() {
    PROVIDERS.replace(vec![]);

    assert_eq!(
        with_provider(EXAMPLE_JWT, |provider| provider
            .verify(EXAMPLE_JWT, &[0u8; 32])),
        Err(OpenIDJWTVerificationError::GenericError(
            "Unsupported issuer: https://example.com".to_string()
        ))
    );
}

#[test]
fn should_skip_provider_when_discovery_pending() {
    // Provider with no discovered issuer/client_id — must be skipped and the
    // surface error should be the "discovery in progress" message.
    let pending = ExampleProvider {
        issuer: None,
        client_id: None,
    };
    PROVIDERS.replace(vec![Box::new(pending)]);

    assert_eq!(
        with_provider(EXAMPLE_JWT, |provider| provider
            .verify(EXAMPLE_JWT, &[0u8; 32])),
        Err(OpenIDJWTVerificationError::GenericError(
            "OIDC provider discovery is still in progress, please try again shortly".to_string()
        ))
    );
}

#[test]
fn should_disambiguate_providers_by_aud() {
    // Two providers sharing the same iss but different aud — only the one whose
    // client_id matches the JWT's aud claim should verify.
    let matching = ExampleProvider {
        issuer: Some("https://example.com".into()),
        client_id: Some("example-aud".into()),
    };
    let expected = matching.credential();
    let other = ExampleProvider {
        issuer: Some("https://example.com".into()),
        client_id: Some("other-aud".into()),
    };
    PROVIDERS.replace(vec![Box::new(other), Box::new(matching)]);

    let returned = with_provider(EXAMPLE_JWT, |provider| {
        provider.verify(EXAMPLE_JWT, &[0u8; 32])
    })
    .expect("expected matching provider");
    assert_eq!(returned.aud, expected.aud);
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

#[test]
fn should_replace_placeholders_in_issuer() {
    let issuer = "https://login.microsoftonline.com/{tid}/v2.0";

    let issuer_placeholders = get_issuer_placeholders(issuer);
    assert_eq!(issuer_placeholders, vec!["tid"]);

    let issuer_claims = get_all_claims(
        r#"{ "tid": "9188040d-6c67-4c5b-b112-36a304b66dad" }"#.as_bytes(),
        issuer_placeholders,
    );
    assert_eq!(
        issuer_claims,
        vec![(
            "tid".to_string(),
            "9188040d-6c67-4c5b-b112-36a304b66dad".to_string()
        )]
    );

    let effective_issuer = replace_issuer_placeholders(issuer, &issuer_claims);
    assert_eq!(
        effective_issuer,
        "https://login.microsoftonline.com/9188040d-6c67-4c5b-b112-36a304b66dad/v2.0"
    );
}

#[test]
fn should_ignore_placeholders_in_issuer_that_dont_have_claim() {
    let issuer = "https://login.microsoftonline.com/{tid}/v2.0";

    let issuer_placeholders = get_issuer_placeholders(issuer);
    assert_eq!(issuer_placeholders, vec!["tid"]);

    let issuer_claims = get_all_claims(
        r#"{ "sub": "MdNi5RU6AxYZr7-2_F83sTswMq_2fvaK6rj8x3fbE9c" }"#.as_bytes(),
        issuer_placeholders,
    );
    assert!(issuer_claims.is_empty());

    let effective_issuer = replace_issuer_placeholders(issuer, &issuer_claims);
    assert_eq!(
        effective_issuer,
        "https://login.microsoftonline.com/{tid}/v2.0"
    );
}

#[test]
fn should_ignore_unclosed_placeholders_in_issuer() {
    let issuer = "https://login.microsoftonline.com/{tid/v2.0";

    let issuer_placeholders = get_issuer_placeholders(issuer);
    assert!(issuer_placeholders.is_empty());

    let issuer_claims = get_all_claims(
        r#"{ "tid": "MdNi5RU6AxYZr7-2_F83sTswMq_2fvaK6rj8x3fbE9c" }"#.as_bytes(),
        issuer_placeholders,
    );
    assert!(issuer_claims.is_empty());

    let effective_issuer = replace_issuer_placeholders(issuer, &issuer_claims);
    assert_eq!(
        effective_issuer,
        "https://login.microsoftonline.com/{tid/v2.0"
    );
}

#[test]
fn should_retain_issuer_without_placeholders_as_is() {
    let issuer = "https://accounts.google.com";

    let issuer_placeholders = get_issuer_placeholders(issuer);
    assert!(issuer_placeholders.is_empty());

    let issuer_claims = get_all_claims(
        r#"{ "sub": "MdNi5RU6AxYZr7-2_F83sTswMq_2fvaK6rj8x3fbE9c" }"#.as_bytes(),
        issuer_placeholders,
    );
    assert!(issuer_claims.is_empty());

    let effective_issuer = replace_issuer_placeholders(issuer, &issuer_claims);
    assert_eq!(effective_issuer, issuer);
}

#[test]
fn should_replace_multiple_placeholders_in_issuer() {
    let issuer = "https://login.microsoftonline.com/{tid}/{sub}/v2.0";

    let issuer_placeholders = get_issuer_placeholders(issuer);
    assert_eq!(issuer_placeholders, vec!["tid", "sub"]);

    let issuer_claims = get_all_claims(
        r#"{ "tid": "9188040d-6c67-4c5b-b112-36a304b66dad", "sub": "MdNi5RU6AxYZr7-2_F83sTswMq_2fvaK6rj8x3fbE9c" }"#.as_bytes(),
        issuer_placeholders,
    );
    assert_eq!(
        issuer_claims,
        vec![
            (
                "tid".to_string(),
                "9188040d-6c67-4c5b-b112-36a304b66dad".to_string(),
            ),
            (
                "sub".to_string(),
                "MdNi5RU6AxYZr7-2_F83sTswMq_2fvaK6rj8x3fbE9c".to_string()
            )
        ]
    );

    let effective_issuer = replace_issuer_placeholders(issuer, &issuer_claims);
    assert_eq!(
        effective_issuer,
        "https://login.microsoftonline.com/9188040d-6c67-4c5b-b112-36a304b66dad/MdNi5RU6AxYZr7-2_F83sTswMq_2fvaK6rj8x3fbE9c/v2.0"
    );
}

#[cfg(test)]
struct ExamplePlaceholderProvider;

#[cfg(test)]
impl OpenIdProvider for ExamplePlaceholderProvider {
    fn issuer(&self) -> Option<String> {
        Some("https://login.microsoftonline.com/{tid}/v2.0".into())
    }

    fn client_id(&self) -> Option<String> {
        Some("example-aud".into())
    }

    fn email_verification_scheme(&self) -> Option<OpenIdEmailVerificationScheme> {
        None
    }

    fn verify(
        &self,
        _: &str,
        _: &[u8; 32],
    ) -> Result<OpenIdCredential, OpenIDJWTVerificationError> {
        Ok(self.credential())
    }
}

#[cfg(test)]
impl ExamplePlaceholderProvider {
    fn credential(&self) -> OpenIdCredential {
        OpenIdCredential {
            iss: "https://login.microsoftonline.com/9188040d-6c67-4c5b-b112-36a304b66dad/v2.0"
                .into(),
            sub: "example-sub".into(),
            aud: "example-aud".into(),
            last_usage_timestamp: None,
            metadata: HashMap::from([(
                "tid".into(),
                MetadataEntryV2::String("9188040d-6c67-4c5b-b112-36a304b66dad".into()),
            )]),
        }
    }
}

#[test]
fn find_config_issuer_from_credential() {
    let provider = ExampleProvider::default();
    let placeholder_provider = ExamplePlaceholderProvider {};
    let config_issuer = provider.issuer();
    let placeholder_config_issuer = placeholder_provider.issuer();
    PROVIDERS.replace(vec![Box::new(provider), Box::new(placeholder_provider)]);

    assert_eq!(
        ExampleProvider::default().credential().config_issuer(),
        config_issuer
    );
    assert_eq!(
        ExamplePlaceholderProvider.credential().config_issuer(),
        placeholder_config_issuer
    );

    let mut unknown_credential = ExampleProvider::default().credential();
    unknown_credential.iss = "https://example.com/unknown".to_string();
    assert_eq!(unknown_credential.config_issuer(), None);
}
