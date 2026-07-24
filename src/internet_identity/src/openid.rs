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
    AnchorNumber, Delegation, IdRegFinishError, MetadataEntryV2, OpenIdConfig,
    OpenIdEmailVerificationScheme, PublicKey, SessionKey, SignedDelegation, SsoDiscovery,
    SsoDiscoveryStatus, Timestamp, UserKey,
};
use serde_bytes::ByteBuf;
use sha2::{Digest, Sha256};
use std::collections::HashMap;
use std::fmt::Display;

mod configured;

#[cfg(test)]
pub(crate) use configured::{clear_for_test, setup_for_test};
mod jwks;
mod provider;
mod sso;
mod sso_bundle;
mod sso_gating;
mod verify;

pub use crate::single_flight_cache::Cached;
pub use sso_bundle::{
    get_sso_attr_bundle_signature, prepare_sso_attr_bundle, read_certified_sso_bundle,
};
pub use sso_gating::{resolve_ii_client_identity, verify_sso_for_registration, verify_sso_jwt};

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
    /// SSO discovery domain this credential was verified through, stamped at
    /// verification time. `None` for direct-provider credentials (Google /
    /// Microsoft / Apple).
    pub sso_domain: Option<String>,
    /// Human-readable SSO label from the domain's hop-1
    /// `ii-openid-configuration`. May be `None` even for SSO credentials —
    /// domains aren't required to publish a `name`.
    pub sso_name: Option<String>,
    /// Cross-client-stable identifier (the `oid` claim value) for a non-`sub`
    /// SSO org, stamped only on the anchor's II-client-keyed credential at SSO
    /// write time. Drives the storage-maintained SSO stable-id index: the
    /// index entry `(iss, aud, stable_id) -> anchor` is reconciled from this
    /// field on every anchor `write()`, so it self-cleans when the credential
    /// is removed or moved. `None` for `sub` orgs, direct providers, and any
    /// non-II-client credential. Storage-internal only — deliberately absent
    /// from the candid `OpenIdCredentialData` so the `oid` never crosses the
    /// candid boundary.
    pub stable_id: Option<String>,
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
            // Unrestricted: the OpenID flow has no read-only option.
            add_delegation_signature(sigs, session_key, seed.as_ref(), expiration, None);
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
                        permissions: None,
                    },
                    signature: ByteBuf::from(signature),
                }),
                Err(_) => Err(OpenIdDelegationError::NoSuchDelegation),
            }
        })
    }

    /// Find current config issuer for stored credential. Returns the configured
    /// provider's (template) issuer, or `None` for SSO credentials (which are
    /// addressable via `sso:<domain>`, not `openid:<issuer>`) and for
    /// credentials matching no configured provider.
    pub fn config_issuer(&self) -> Option<String> {
        if self.sso_domain.is_some() {
            return None;
        }
        configured::config_issuer_for(self)
    }

    /// The SSO discovery domain this credential was verified through, if any.
    pub fn discovery_domain(&self) -> Option<String> {
        self.sso_domain.clone()
    }

    /// The single `AttributeScope` this credential is addressable under — the
    /// source of truth for scope exclusivity:
    ///
    /// - `Some(Sso { domain })`     for SSO credentials (stamped `sso_domain`),
    /// - `Some(OpenId { issuer })`  for configured-provider credentials,
    /// - `None`                     when no configured provider matches and the
    ///   credential isn't SSO — credential is unreachable.
    pub fn matched_attribute_scope(&self) -> Option<AttributeScope> {
        if let Some(domain) = self.sso_domain.clone() {
            return Some(AttributeScope::Sso { domain });
        }
        configured::config_issuer_for(self).map(|issuer| AttributeScope::OpenId { issuer })
    }

    pub(super) fn read_metadata_string(&self, attribute_name: &str) -> Option<String> {
        let MetadataEntryV2::String(value) = self.metadata.get(attribute_name)? else {
            return None;
        };

        Some(value.clone())
    }

    pub fn get_name(&self) -> Option<String> {
        self.read_metadata_string("name")
    }

    pub fn get_email(&self) -> Option<String> {
        self.read_metadata_string("email")
    }

    fn get_google_verified_email(&self) -> Option<String> {
        let email_verified = self.read_metadata_string("email_verified")?;

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

        let tid = self.read_metadata_string("tid")?;

        if tid != MICROSOFT_PERSONAL_ACCOUNT_TENANT_ID {
            return None;
        }

        self.get_email()
    }

    /// Return the verified email for this credential, if available. Direct
    /// providers opt into a hardcoded scheme; SSO credentials never do.
    pub fn get_verified_email(&self) -> Option<String> {
        if self.sso_domain.is_some() {
            return None;
        }
        use OpenIdEmailVerificationScheme::*;
        match configured::email_scheme_for(self)? {
            Unknown => None,
            Google => self.get_google_verified_email(),
            Microsoft => self.get_microsoft_verified_email(),
        }
    }
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

impl Display for AudClaim {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AudClaim::Single(s) => write!(f, "{}", s),
            AudClaim::Multiple(v) => write!(f, "[{}]", v.join(", ")),
        }
    }
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

/// Install the configured (hardcoded) OpenID providers from init args.
pub fn setup(configs: Vec<OpenIdConfig>) {
    configured::setup(configs);
}

/// Canonicalize an untrusted SSO discovery domain received as a canister-call
/// argument: trim surrounding whitespace and lowercase ASCII. Domains are
/// case-insensitive (DNS), but the value is stamped onto the credential as
/// `sso_domain` and used as the equality key for `sso:<domain>` scope routing,
/// so it must be canonical the moment it crosses the trust boundary. Every
/// endpoint taking a `discovery_domain` runs this before any further use.
pub fn canonical_discovery_domain(domain: &str) -> String {
    domain.trim().to_ascii_lowercase()
}

/// [`canonical_discovery_domain`] over an optional argument (the JWT endpoints,
/// where the domain selects an SSO vs configured provider).
pub fn canonical_discovery_domain_opt(domain: Option<String>) -> Option<String> {
    domain.map(|domain| canonical_discovery_domain(&domain))
}

/// Drive the on-demand SSO discovery / JWKS fetches for `domain` forward. Only
/// an update may call this — it spawns the outcalls the fills make. A no-op for
/// `None` (a configured provider needs no fetch) and for a disallowed domain.
/// After calling this, [`verify_jwt`] / [`discover_sso`] read the result once
/// the cache is warm.
pub fn prefetch_sso(domain: Option<&str>) {
    if let Some(domain) = domain {
        sso::prefetch(domain);
    }
}

/// Drive the SSO discovery fetch for `domain` (the discovery cache only). The
/// sign-in initiation poll calls this from an update when [`get_sso_discovery_status`]
/// reads `Pending`. A no-op for a disallowed domain (the query reports
/// `NotAllowed`).
pub fn discover_sso(domain: &str) {
    sso::drive_discovery(domain);
}

/// Read the status of `domain`'s SSO discovery: the resolved config, or still
/// pending.
///
/// With `origin`, `resolved_client_id` is that origin's client, or `None` when
/// the origin is gated out.
pub fn get_sso_discovery_status(domain: &str, origin: Option<&str>) -> SsoDiscoveryStatus {
    // Peek-only: `Pending` until the fill (driven by `prefetch_sso`) resolves.
    // A malformed domain never resolves (the fill rejects non-bare-authorities),
    // so it simply reads `Pending` until the frontend times out.
    match sso::peek_discovery(domain) {
        Cached::Ready(discovery_config) => {
            let resolved_client_id = match origin {
                None => Some(discovery_config.client_id.clone()),
                Some(origin) => discovery_config.resolve_client_for_origin(origin).ok(),
            };
            SsoDiscoveryStatus::Resolved(SsoDiscovery {
                discovery_domain: domain.to_ascii_lowercase(),
                client_id: discovery_config.client_id,
                issuer: discovery_config.issuer,
                authorization_endpoint: discovery_config.authorization_endpoint,
                scopes: discovery_config.scopes,
                name: discovery_config.name,
                resolved_client_id,
            })
        }
        Cached::Pending => SsoDiscoveryStatus::Pending,
    }
}

/// Decode a JWT's issuer, audience, and raw claims bytes. Rejects an empty
/// audience.
pub(super) fn decode_iss_aud_claims(
    jwt: &str,
) -> Result<(String, AudClaim, Vec<u8>), OpenIDJWTVerificationError> {
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
    Ok((iss, aud, validation_item.claims().to_vec()))
}

/// Verify a JWT and bind the nonce to the caller via the salt, returning the
/// resulting credential. `discovery_domain` selects the provider kind: `None`
/// resolves a configured provider by `(iss, aud)`; `Some(domain)` resolves an
/// SSO provider from the discovery cache.
///
/// Reads the SSO caches without spawning, so it's safe from both updates and
/// queries. `Ok(Cached::Pending)` means SSO discovery or JWKS isn't cached yet;
/// an update must call [`prefetch_sso`] to drive the fetch, then retry. A
/// configured provider never yields `Pending`.
pub fn verify_jwt(
    jwt: &str,
    salt: &[u8; 32],
    discovery_domain: Option<&str>,
) -> Result<Cached<OpenIdCredential>, OpenIDJWTVerificationError> {
    let (iss, aud, claims) = decode_iss_aud_claims(jwt)?;

    let resolved = match provider::resolve(&iss, &aud, &claims, discovery_domain)? {
        Cached::Pending => return Ok(Cached::Pending),
        Cached::Ready(resolved) => resolved,
    };

    let keys = match jwks::read_jwks(&resolved.jwk_source) {
        Cached::Pending => return Ok(Cached::Pending),
        Cached::Ready(keys) => keys,
    };

    verify::verify_and_build(jwt, &resolved.descriptor, &keys, salt).map(Cached::Ready)
}

/// As seen in <https://login.microsoftonline.com/common/v2.0/.well-known/openid-configuration>,
/// the Microsoft issuer uri is dynamic based on placeholders, this method returns them e.g. `["tid"]`.
pub(super) fn get_issuer_placeholders(template: &str) -> Vec<String> {
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
pub(super) fn get_all_claims(claims_bytes: &[u8], keys: Vec<String>) -> Vec<(String, String)> {
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
pub(super) fn replace_issuer_placeholders(
    template: &str,
    claims: &Vec<(String, String)>,
) -> String {
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
pub(super) fn calculate_delegation_seed(
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

/// How long a certified SSO attribute bundle is accepted after it is minted
/// (`sso_bundle.rs`) — the bundle acceptance window, not a session or delegation
/// lifetime. Both the mint and the expiry check use II's own IC time, so the
/// window needn't budget for client clock skew: 5 minutes covers the
/// post-prepare ceremony (IdP 2FA/redirect, `sso_get_delegation`, the dapp's
/// attribute call).
pub const SSO_ATTR_BUNDLE_TTL_NS: u64 = 5 * MINUTE_NS;

#[cfg(test)]
mod tests {
    use super::*;
    use internet_identity_interface::internet_identity::types::OpenIdConfig;

    pub(crate) const TEST_AUD: &str =
        "45431994619-cbbfgtn7o0pp0dpfcg2l66bc4rcg7qbu.apps.googleusercontent.com";

    fn google_config() -> OpenIdConfig {
        OpenIdConfig {
            name: "Google".to_string(),
            logo: String::new(),
            issuer: "https://accounts.google.com".to_string(),
            client_id: TEST_AUD.into(),
            jwks_uri: "https://www.googleapis.com/oauth2/v3/certs".to_string(),
            auth_uri: "https://accounts.google.com/o/oauth2/v2/auth".to_string(),
            auth_scope: vec!["openid".into(), "profile".into(), "email".into()],
            fedcm_uri: Some("https://accounts.google.com/gsi/fedcm.json".into()),
            email_verification: None,
            seed_jwks: None,
        }
    }

    pub(crate) fn test_certs() -> Vec<identity_jose::jwk::Jwk> {
        #[derive(serde::Deserialize)]
        struct Certs {
            keys: Vec<identity_jose::jwk::Jwk>,
        }
        serde_json::from_str::<Certs>(r#"{"keys":[{"n": "jwstqI4w2drqbTTVRDriFqepwVVI1y05D5TZCmGvgMK5hyOsVW0tBRiY9Jk9HKDRue3vdXiMgarwqZEDOyOA0rpWh-M76eauFhRl9lTXd5gkX0opwh2-dU1j6UsdWmMa5OpVmPtqXl4orYr2_3iAxMOhHZ_vuTeD0KGeAgbeab7_4ijyLeJ-a8UmWPVkglnNb5JmG8To77tSXGcPpBcAFpdI_jftCWr65eL1vmAkPNJgUTgI4sGunzaybf98LSv_w4IEBc3-nY5GfL-mjPRqVCRLUtbhHO_5AYDpqGj6zkKreJ9-KsoQUP6RrAVxkNuOHV9g1G-CHihKsyAifxNN2Q","use": "sig","kty": "RSA","alg": "RS256","kid": "dd125d5f462fbc6014aedab81ddf3bcedab70847","e": "AQAB"}]}"#)
            .unwrap()
            .keys
    }

    // Real Google-signed JWT (matches `test_certs`); nonce bound to the fixed
    // test caller + `test_salt()`. Expired in real time, valid against the
    // verify module's test clock.
    pub(crate) const VALID_JWT: &str = "eyJhbGciOiJSUzI1NiIsImtpZCI6ImRkMTI1ZDVmNDYyZmJjNjAxNGFlZGFiODFkZGYzYmNlZGFiNzA4NDciLCJ0eXAiOiJKV1QifQ.eyJpc3MiOiJodHRwczovL2FjY291bnRzLmdvb2dsZS5jb20iLCJhenAiOiI0NTQzMTk5NDYxOS1jYmJmZ3RuN28wcHAwZHBmY2cybDY2YmM0cmNnN3FidS5hcHBzLmdvb2dsZXVzZXJjb250ZW50LmNvbSIsImF1ZCI6IjQ1NDMxOTk0NjE5LWNiYmZndG43bzBwcDBkcGZjZzJsNjZiYzRyY2c3cWJ1LmFwcHMuZ29vZ2xldXNlcmNvbnRlbnQuY29tIiwic3ViIjoiMTE1MTYwNzE2MzM4ODEzMDA2OTAyIiwiaGQiOiJkZmluaXR5Lm9yZyIsImVtYWlsIjoidGhvbWFzLmdsYWRkaW5lc0BkZmluaXR5Lm9yZyIsImVtYWlsX3ZlcmlmaWVkIjp0cnVlLCJub25jZSI6ImV0aURhTEdjUmRtNS1yY3FlMFpRVWVNZ3BmcDR2OVRPT1lVUGJoUng3bkkiLCJuYmYiOjE3MzY3OTM4MDIsIm5hbWUiOiJUaG9tYXMgR2xhZGRpbmVzIiwicGljdHVyZSI6Imh0dHBzOi8vbGgzLmdvb2dsZXVzZXJjb250ZW50LmNvbS9hL0FDZzhvY0lTTWxja0M1RjZxaGlOWnpfREZtWGp5OTY4LXlPaEhPTjR4TGhRdXVNSDNuQlBXQT1zOTYtYyIsImdpdmVuX25hbWUiOiJUaG9tYXMiLCJmYW1pbHlfbmFtZSI6IkdsYWRkaW5lcyIsImlhdCI6MTczNjc5NDEwMiwiZXhwIjoxNzM2Nzk3NzAyLCJqdGkiOiIwMWM1NmYyMGM1MzFkNDhhYjU0ZDMwY2I4ZmRiNzU0MmM0ZjdmNjg4In0.f47b0HNskm-85sT5XtoRzORnfobK2nzVFG8jTH6eS_qAyu0ojNDqVsBtGN4A7HdjDDCOIMSu-R5e413xuGJIWLadKrLwXmguRFo3SzLrXeja-A-rP-axJsb5QUJZx1mwYd1vUNzLB9bQojU3Na6Hdvq09bMtTwaYdCn8Q9v3RErN-5VUxELmSbSXbf10A-IsS7jtzPjxHV6ueq687Ppeww6Q7AGGFB4t9H8qcDbI1unSdugX3-MfMWJLzVHbVxDgfAcLem1c2iAspvv_D5aPLeJF5HLRR2zg-Jil1BFTOoEPAAPFr1MEsvDMWSTt5jLyuMrnS4jiMGudGGPV4DDDww";

    pub(crate) fn test_salt() -> [u8; 32] {
        [
            143, 79, 158, 224, 218, 125, 157, 169, 98, 43, 205, 227, 243, 123, 173, 255, 132, 83,
            81, 139, 161, 18, 224, 243, 4, 129, 26, 123, 229, 242, 200, 189,
        ]
    }

    #[test]
    fn should_verify_configured_provider() {
        configured::clear_for_test();
        configured::setup_for_test(google_config(), test_certs());

        let result = verify_jwt(VALID_JWT, &test_salt(), None);
        match result {
            Ok(Cached::Ready(credential)) => {
                assert_eq!(credential.iss, "https://accounts.google.com");
                assert_eq!(credential.aud, TEST_AUD);
                assert_eq!(credential.sso_domain, None);
            }
            other => panic!("expected Ready credential, got {other:?}"),
        }
    }

    #[test]
    fn canonical_discovery_domain_trims_and_lowercases() {
        // Untrusted canister-call args: a mixed-case / padded domain that
        // passes the case-insensitive allowlist gate must be canonicalized so
        // the stamped `sso:<domain>` scope matches the allowlisted value, and
        // so the discovery endpoints (`discover_sso` / `get_sso_discovery_status`) gate
        // on the same canonical form as the JWT endpoints.
        assert_eq!(canonical_discovery_domain("  Example.ORG  "), "example.org");
        assert_eq!(canonical_discovery_domain("example.org"), "example.org");
        // Optional wrapper for the JWT endpoints: a configured provider supplies
        // no domain.
        assert_eq!(
            canonical_discovery_domain_opt(Some("  Example.ORG  ".to_string())),
            Some("example.org".to_string())
        );
        assert_eq!(canonical_discovery_domain_opt(None), None);
    }

    #[test]
    fn should_reject_unsupported_issuer() {
        configured::clear_for_test();
        // No providers registered → unsupported issuer.
        assert_eq!(
            verify_jwt(VALID_JWT, &test_salt(), None),
            Err(OpenIDJWTVerificationError::GenericError(
                "Unsupported issuer: https://accounts.google.com".to_string()
            ))
        );
    }

    #[test]
    fn should_reject_invalid_encoding() {
        configured::clear_for_test();
        configured::setup_for_test(google_config(), test_certs());
        assert_eq!(
            verify_jwt("invalid-jwt", &test_salt(), None),
            Err(OpenIDJWTVerificationError::GenericError(
                "Failed to decode JWT".to_string()
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

        let effective_issuer = replace_issuer_placeholders(issuer, &vec![]);
        assert_eq!(
            effective_issuer,
            "https://login.microsoftonline.com/{tid/v2.0"
        );
    }
}
