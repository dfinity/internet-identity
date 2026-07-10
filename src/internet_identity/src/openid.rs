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
    SsoDiscoveryState, Timestamp, UserKey,
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
mod verify;

pub use crate::single_flight_cache::Cached;

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
/// `sso_domain` and used as the equality key for `sso:<domain>` scope routing
/// and the allowlist gate, so it must be canonical the moment it crosses the
/// trust boundary. Every endpoint taking a `discovery_domain` runs this before
/// any further use, matching the `sso_discoverable_domains` config setter.
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
/// sign-in initiation poll calls this from an update when [`get_sso_discovery`]
/// reads `Pending`. A no-op for a disallowed domain (the query reports
/// `NotAllowed`).
pub fn discover_sso(domain: &str) {
    sso::drive_discovery(domain);
}

/// Read the state of `domain`'s SSO discovery: the resolved config, still
/// pending, or not on the allowlist.
///
/// When `origin` is `Some`, `resolved_client_id` reports the client the target
/// origin must run its ceremony against (IdP-side per-app gating, §6): the
/// per-app client if the origin is listed in `app_clients`, the primary client
/// if unlisted and `gate_all_apps` is off, or `None` when unlisted and
/// `gate_all_apps` is on (origin denied). Without an `origin` it mirrors the
/// primary `client_id`.
pub fn get_sso_discovery(domain: &str, origin: Option<&str>) -> SsoDiscoveryState {
    if !sso::is_allowed_discovery_domain(domain) {
        return SsoDiscoveryState::NotAllowed;
    }
    match sso::peek_discovery(domain) {
        Cached::Ready(cfg) => {
            let resolved_client_id = match origin {
                None => Some(cfg.client_id.clone()),
                Some(origin) => match cfg.resolve_client_for_origin(origin) {
                    sso::ClientResolution::PerApp(client)
                    | sso::ClientResolution::Primary(client) => Some(client),
                    sso::ClientResolution::NotAllowed => None,
                },
            };
            SsoDiscoveryState::Resolved(SsoDiscovery {
                discovery_domain: domain.to_ascii_lowercase(),
                client_id: cfg.client_id,
                issuer: cfg.issuer,
                authorization_endpoint: cfg.authorization_endpoint,
                scopes: cfg.scopes,
                name: cfg.name,
                resolved_client_id,
            })
        }
        Cached::Pending => SsoDiscoveryState::Pending,
    }
}

/// Decode a JWT's issuer, audience, and raw claims bytes. Rejects an empty
/// audience.
fn decode_iss_aud_claims(
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

// ===========================================================================
// IdP-side per-app SSO gating (see docs/ongoing/enterprise-sso-idp-side-gating.md)
//
// A dedicated `sso_prepare_delegation` / `sso_get_delegation` pair carries the
// SSO sign-in. `openid_prepare_delegation` / `openid_get_delegation` are left
// byte-identical (direct providers + the pre-feature flow). The gate is
// mint-or-refuse: `verify_sso_jwt` enforces `jwt.aud == the origin's declared
// client`, and the minted delegation's seed binds `(iss, sub, sso_domain,
// origin, anchor)`. The origin-scoped calls recompute that principal
// (`matching_sso_session`), so a refused gate leaves nothing to authenticate
// with. Identity always resolves to the primary client.
// ===========================================================================

/// Tighter session lifetime for the SSO sign-in path so certified SSO
/// attributes reflect a *recent* SSO ceremony, independent of the general /
/// passkey session lifetime (§6.3).
pub const SSO_SESSION_DURATION_NS: u64 = 30 * MINUTE_NS;

/// Verified + gated SSO login. `credential.aud` is the **resolved** client (the
/// per-app client for a gated origin, else the primary); `primary_client_id` is
/// always the org's primary client, on which identity is keyed (§6.1).
#[derive(Debug)]
pub struct SsoVerification {
    pub credential: OpenIdCredential,
    pub primary_client_id: String,
    /// True when the origin is listed in `app_clients` (served by a per-app
    /// client); false when it resolved to the primary client.
    pub gated: bool,
    pub stable_identifier_claim: String,
    /// The cross-client-stable identifier, extracted when
    /// `stable_identifier_claim != "sub"` (§6.5). `None` when the claim is
    /// `sub` (the credential's own `sub` is already stable).
    pub stable_id: Option<String>,
}

// In-heap auxiliary lookup `(iss, stable_id) -> primary sub` for orgs whose
// stable identifier isn't `sub` (Entra `oid`, §6.5). Additive, non-persistent
// (rebuilt on the users' next primary login after an upgrade); a miss fails
// safe (no anchor found → the user signs in normally first). Never a
// migration and never mutates a stored credential/key.
thread_local! {
    static SSO_STABLE_ID_INDEX: std::cell::RefCell<HashMap<(String, String), String>> =
        std::cell::RefCell::new(HashMap::new());
}

fn aux_stable_id_insert(iss: &str, stable_id: &str, primary_sub: &str) {
    SSO_STABLE_ID_INDEX.with_borrow_mut(|m| {
        m.insert(
            (iss.to_string(), stable_id.to_string()),
            primary_sub.to_string(),
        );
    });
}

fn aux_stable_id_lookup(iss: &str, stable_id: &str) -> Option<String> {
    SSO_STABLE_ID_INDEX.with_borrow(|m| m.get(&(iss.to_string(), stable_id.to_string())).cloned())
}

/// Extract a single string claim from a JWT's raw claims (used for the
/// configured `stable_identifier_claim`). `None` if absent or non-string.
fn extract_string_claim(jwt: &str, claim: &str) -> Option<String> {
    let (_, _, claims_bytes) = decode_iss_aud_claims(jwt).ok()?;
    let value = serde_json::from_slice::<serde_json::Value>(&claims_bytes).ok()?;
    value.get(claim)?.as_str().map(str::to_string)
}

/// Verify an SSO JWT for `(discovery_domain, origin)` and enforce the gate
/// (§6.2). `Ok(Cached::Pending)` means discovery / JWKS isn't cached yet — an
/// update must drive it via [`prefetch_sso`], then retry.
///
/// **The gate is mint-or-refuse:** the JWT's `aud` must equal the client the
/// origin resolves to (per-app if listed, primary if not, DENY under
/// `gate_all_apps`). A wrong `aud` fails verification, so no delegation is ever
/// minted for a token issued for a different app.
pub fn verify_sso_jwt(
    jwt: &str,
    salt: &[u8; 32],
    discovery_domain: &str,
    origin: &str,
) -> Result<Cached<SsoVerification>, OpenIDJWTVerificationError> {
    if !sso::is_allowed_discovery_domain(discovery_domain) {
        return Err(OpenIDJWTVerificationError::GenericError(format!(
            "SSO discovery domain not allowed: {discovery_domain}"
        )));
    }
    let cfg = match sso::peek_discovery(discovery_domain) {
        Cached::Pending => return Ok(Cached::Pending),
        Cached::Ready(cfg) => cfg,
    };

    // THE GATE: resolve the origin to its declared client, then require the
    // JWT's aud to match it (enforced inside `verify_and_build`).
    let (expected_client, gated) = match cfg.resolve_client_for_origin(origin) {
        sso::ClientResolution::PerApp(client) => (client, true),
        sso::ClientResolution::Primary(client) => (client, false),
        sso::ClientResolution::NotAllowed => {
            return Err(OpenIDJWTVerificationError::GenericError(format!(
                "origin '{origin}' is denied for domain '{discovery_domain}' (gate_all_apps)"
            )));
        }
    };

    let keys = match sso::read_jwks(&cfg.jwks_uri) {
        Cached::Pending => return Ok(Cached::Pending),
        Cached::Ready(keys) => keys,
    };

    let descriptor = verify::Descriptor {
        issuer: cfg.issuer.clone(),
        client_id: expected_client.clone(),
        stamp: verify::Stamp::Sso {
            domain: discovery_domain.to_string(),
            name: cfg.name.clone(),
        },
    };
    let credential = verify::verify_and_build(jwt, &descriptor, &keys, salt)?;

    let stable_id = if cfg.stable_identifier_claim == sso::DEFAULT_STABLE_IDENTIFIER_CLAIM {
        None
    } else {
        extract_string_claim(jwt, &cfg.stable_identifier_claim)
    };

    Ok(Cached::Ready(SsoVerification {
        credential,
        primary_client_id: cfg.client_id,
        gated,
        stable_identifier_claim: cfg.stable_identifier_claim,
        stable_id,
    }))
}

/// The primary-client-keyed identity a verified SSO login resolves to (§6.1):
/// the `(iss, sub, aud)` of the org's primary credential, with the token's
/// metadata + SSO stamp. Anchor resolution and any credential refresh happen at
/// the call site (`sso_prepare_delegation` in `main.rs`).
pub struct SsoPrimaryIdentity {
    /// Primary-keyed credential: `aud` == primary client, `sub` == the primary
    /// credential's sub (the stable id for `sub` orgs; bridged via the aux
    /// lookup otherwise).
    pub credential: OpenIdCredential,
}

/// Resolve a verified SSO login to the primary identity (§6.1, §6.5). Does not
/// touch storage; the caller looks the anchor up by `credential.key()`.
///
/// `Err(NoSuchAnchor)` for a non-`sub` gated login whose stable id has no aux
/// entry yet — the user must sign in normally (primary) first (§6.5).
pub fn resolve_primary_identity(
    verification: &SsoVerification,
) -> Result<SsoPrimaryIdentity, OpenIdDelegationError> {
    let is_sub = verification.stable_identifier_claim == sso::DEFAULT_STABLE_IDENTIFIER_CLAIM;
    let primary_sub = if is_sub {
        // `sub` is stable: the token's sub is the primary credential's sub.
        verification.credential.sub.clone()
    } else {
        let stable_id = verification.stable_id.clone().ok_or(
            // A non-`sub` org whose token omits the configured stable claim
            // can't be resolved to a stable identity.
            OpenIdDelegationError::JwtVerificationFailed,
        )?;
        if verification.gated {
            // Per-app login: bridge (iss, stable_id) -> primary sub via the aux
            // index. A miss fails safe (§6.5).
            aux_stable_id_lookup(&verification.credential.iss, &stable_id)
                .ok_or(OpenIdDelegationError::NoSuchAnchor)?
        } else {
            // Primary login: the token's sub *is* the primary sub, and it also
            // carries the alternate stable id — so it populates the aux index.
            aux_stable_id_insert(
                &verification.credential.iss,
                &stable_id,
                &verification.credential.sub,
            );
            verification.credential.sub.clone()
        }
    };

    let credential = OpenIdCredential {
        aud: verification.primary_client_id.clone(),
        sub: primary_sub,
        ..verification.credential.clone()
    };
    Ok(SsoPrimaryIdentity { credential })
}

/// Record the non-`sub` aux bridge after a normal (un-gated) primary-client SSO
/// login (§6.5): `(iss, stable_id) -> primary sub`. This is the login that
/// carries BOTH the alternate stable id (e.g. Entra `oid`) and the primary
/// `sub`, so a later gated per-app login (whose pairwise sub differs) can be
/// bridged to the primary identity. No-op for `sub` orgs and for direct
/// providers (no `sso_domain`). Additive, in-heap; never mutates a credential.
///
/// Called from the primary-token registration / credential-add paths (which use
/// `verify_jwt`, not the gated `resolve_primary_identity` that already inserts
/// on the delegation path), so a new non-`sub` user who signs in normally gets
/// their bridge and a subsequent gated login resolves.
pub fn note_primary_sso_login(jwt: &str, credential: &OpenIdCredential) {
    let Some(domain) = credential.sso_domain.as_deref() else {
        return;
    };
    let Cached::Ready(cfg) = sso::peek_discovery(domain) else {
        return;
    };
    if cfg.stable_identifier_claim == sso::DEFAULT_STABLE_IDENTIFIER_CLAIM {
        return;
    }
    if let Some(stable_id) = extract_string_claim(jwt, &cfg.stable_identifier_claim) {
        aux_stable_id_insert(&credential.iss, &stable_id, &credential.sub);
    }
}

/// Verify an SSO JWT for *registration* — the registration analogue of the
/// `sso_prepare_delegation` gate (§6.1/§6.2). Enforces the SAME gate for
/// `(discovery_domain, origin)` (via [`verify_sso_jwt`]) and returns the
/// PRIMARY-keyed credential to store (via [`resolve_primary_identity`]'s
/// substitution), so a first *gated* login registers directly under the org's
/// primary client — never a per-app credential. Reuses the delegation gate
/// helpers verbatim; the credential shape is identical to a normal primary SSO
/// credential (no new key, no migration).
///
/// **Fails safe.** A non-`sub` org's per-app token carries a pairwise sub with
/// no aux bridge yet, so no primary sub can be derived — this returns an error
/// and the caller creates NOTHING (the frontend routes the user to a normal
/// primary-client sign-in first, then retries). Also denies a wrong-`aud` /
/// `gate_all_apps`-blocked token exactly as the delegation gate does.
pub fn verify_sso_for_registration(
    jwt: &str,
    salt: &[u8; 32],
    discovery_domain: &str,
    origin: &str,
) -> Result<Cached<OpenIdCredential>, IdRegFinishError> {
    let verification = match verify_sso_jwt(jwt, salt, discovery_domain, origin) {
        Ok(Cached::Pending) => return Ok(Cached::Pending),
        Ok(Cached::Ready(verification)) => verification,
        // Gate mismatch / wrong `aud` / `gate_all_apps` deny / expired / bad
        // signature: a clear handled verification error (never a silent
        // fallthrough that looks like success).
        Err(err) => return Err(err.into()),
    };
    match resolve_primary_identity(&verification) {
        Ok(identity) => Ok(Cached::Ready(identity.credential)),
        // Non-`sub` org, first gated login: the per-app token's pairwise sub has
        // no aux bridge yet, so no primary sub can be derived. Create nothing and
        // signal the FE to run a normal primary-client sign-in first (§6.5).
        Err(OpenIdDelegationError::NoSuchAnchor) => Err(IdRegFinishError::SsoNormalLoginRequired),
        // Any other resolution failure (e.g. a non-`sub` token missing the
        // configured stable claim) is a malformed request, not a bridgeable one.
        Err(_) => Err(IdRegFinishError::InvalidAuthnMethod(
            "SSO identity could not be resolved".to_string(),
        )),
    }
}

/// Seed for an SSO-session delegation, binding `(iss, sub, sso_domain, origin,
/// anchor)` (§6.2). A dedicated domain separator and length-prefixed fields
/// keep it disjoint from every [`calculate_delegation_seed`] output, so an SSO
/// session principal can never collide with an OpenID-credential principal.
fn calculate_sso_delegation_seed(
    iss: &str,
    sub: &str,
    sso_domain: &str,
    origin: &str,
    anchor_number: AnchorNumber,
) -> Hash {
    fn write_field(blob: &mut Vec<u8>, data: &[u8]) {
        blob.extend_from_slice(&(data.len() as u64).to_be_bytes());
        blob.extend_from_slice(data);
    }
    let mut blob: Vec<u8> = Vec::new();
    blob.extend_from_slice(b"ii-sso-per-app-delegation");
    write_field(&mut blob, &salt());
    write_field(&mut blob, iss.as_bytes());
    write_field(&mut blob, sub.as_bytes());
    write_field(&mut blob, sso_domain.as_bytes());
    write_field(&mut blob, origin.as_bytes());
    write_field(&mut blob, &anchor_number.to_be_bytes());

    let mut hasher = Sha256::new();
    hasher.update(blob);
    hasher.finalize().into()
}

/// The self-authenticating principal of an SSO-session delegation.
pub fn sso_session_principal(
    iss: &str,
    sub: &str,
    sso_domain: &str,
    origin: &str,
    anchor_number: AnchorNumber,
) -> Principal {
    let seed = calculate_sso_delegation_seed(iss, sub, sso_domain, origin, anchor_number);
    let public_key: PublicKey = der_encode_canister_sig_key(seed.to_vec()).into();
    Principal::self_authenticating(public_key)
}

/// Mint an SSO-session delegation seeded `(iss, sub, sso_domain, origin,
/// anchor)` with the tighter SSO expiry.
pub async fn prepare_sso_delegation(
    iss: &str,
    sub: &str,
    sso_domain: &str,
    origin: &str,
    anchor_number: AnchorNumber,
    session_key: SessionKey,
) -> (UserKey, Timestamp) {
    state::ensure_salt_set().await;
    let expiration = time().saturating_add(SSO_SESSION_DURATION_NS);
    let seed = calculate_sso_delegation_seed(iss, sub, sso_domain, origin, anchor_number);
    state::signature_map_mut(|sigs| {
        add_delegation_signature(sigs, session_key, seed.as_ref(), expiration, None);
    });
    update_root_hash();
    (
        ByteBuf::from(der_encode_canister_sig_key(seed.to_vec())),
        expiration,
    )
}

/// Fetch a previously minted SSO-session delegation (query side).
pub fn get_sso_delegation(
    iss: &str,
    sub: &str,
    sso_domain: &str,
    origin: &str,
    anchor_number: AnchorNumber,
    session_key: SessionKey,
    expiration: Timestamp,
) -> Result<SignedDelegation, OpenIdDelegationError> {
    let seed = calculate_sso_delegation_seed(iss, sub, sso_domain, origin, anchor_number);
    state::assets_and_signatures(|certified_assets, sigs| {
        let inputs = CanisterSigInputs {
            domain: DELEGATION_SIG_DOMAIN,
            seed: &seed,
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

/// Config-free cert-time check (§6.3): if `caller` is the SSO-session principal
/// for one of `credentials` at `origin`, return that credential's `sso_domain`.
/// Only SSO credentials (`sso_domain.is_some()`) participate, and the match is
/// exact — a passkey / `openid_prepare_delegation` session yields a different
/// principal, so it matches nothing and gets no SSO attributes.
pub fn matching_sso_session<'a>(
    credentials: impl Iterator<Item = &'a OpenIdCredential>,
    anchor_number: AnchorNumber,
    origin: &str,
    caller: Principal,
) -> Option<String> {
    for credential in credentials {
        let Some(sso_domain) = credential.sso_domain.as_ref() else {
            continue;
        };
        let principal = sso_session_principal(
            &credential.iss,
            &credential.sub,
            sso_domain,
            origin,
            anchor_number,
        );
        if principal == caller {
            return Some(sso_domain.clone());
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use internet_identity_interface::internet_identity::types::OpenIdConfig;

    const TEST_AUD: &str =
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

    fn test_certs() -> Vec<identity_jose::jwk::Jwk> {
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
    const VALID_JWT: &str = "eyJhbGciOiJSUzI1NiIsImtpZCI6ImRkMTI1ZDVmNDYyZmJjNjAxNGFlZGFiODFkZGYzYmNlZGFiNzA4NDciLCJ0eXAiOiJKV1QifQ.eyJpc3MiOiJodHRwczovL2FjY291bnRzLmdvb2dsZS5jb20iLCJhenAiOiI0NTQzMTk5NDYxOS1jYmJmZ3RuN28wcHAwZHBmY2cybDY2YmM0cmNnN3FidS5hcHBzLmdvb2dsZXVzZXJjb250ZW50LmNvbSIsImF1ZCI6IjQ1NDMxOTk0NjE5LWNiYmZndG43bzBwcDBkcGZjZzJsNjZiYzRyY2c3cWJ1LmFwcHMuZ29vZ2xldXNlcmNvbnRlbnQuY29tIiwic3ViIjoiMTE1MTYwNzE2MzM4ODEzMDA2OTAyIiwiaGQiOiJkZmluaXR5Lm9yZyIsImVtYWlsIjoidGhvbWFzLmdsYWRkaW5lc0BkZmluaXR5Lm9yZyIsImVtYWlsX3ZlcmlmaWVkIjp0cnVlLCJub25jZSI6ImV0aURhTEdjUmRtNS1yY3FlMFpRVWVNZ3BmcDR2OVRPT1lVUGJoUng3bkkiLCJuYmYiOjE3MzY3OTM4MDIsIm5hbWUiOiJUaG9tYXMgR2xhZGRpbmVzIiwicGljdHVyZSI6Imh0dHBzOi8vbGgzLmdvb2dsZXVzZXJjb250ZW50LmNvbS9hL0FDZzhvY0lTTWxja0M1RjZxaGlOWnpfREZtWGp5OTY4LXlPaEhPTjR4TGhRdXVNSDNuQlBXQT1zOTYtYyIsImdpdmVuX25hbWUiOiJUaG9tYXMiLCJmYW1pbHlfbmFtZSI6IkdsYWRkaW5lcyIsImlhdCI6MTczNjc5NDEwMiwiZXhwIjoxNzM2Nzk3NzAyLCJqdGkiOiIwMWM1NmYyMGM1MzFkNDhhYjU0ZDMwY2I4ZmRiNzU0MmM0ZjdmNjg4In0.f47b0HNskm-85sT5XtoRzORnfobK2nzVFG8jTH6eS_qAyu0ojNDqVsBtGN4A7HdjDDCOIMSu-R5e413xuGJIWLadKrLwXmguRFo3SzLrXeja-A-rP-axJsb5QUJZx1mwYd1vUNzLB9bQojU3Na6Hdvq09bMtTwaYdCn8Q9v3RErN-5VUxELmSbSXbf10A-IsS7jtzPjxHV6ueq687Ppeww6Q7AGGFB4t9H8qcDbI1unSdugX3-MfMWJLzVHbVxDgfAcLem1c2iAspvv_D5aPLeJF5HLRR2zg-Jil1BFTOoEPAAPFr1MEsvDMWSTt5jLyuMrnS4jiMGudGGPV4DDDww";

    fn test_salt() -> [u8; 32] {
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
        // so the discovery endpoints (`discover_sso` / `get_sso_discovery`) gate
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

    // ── IdP-side per-app SSO gating ──────────────────────────────────────

    const SSO_DOMAIN: &str = "example.org";
    const PER_APP_CLIENT: &str = "0oaPAYROLL";
    const GATED_ORIGIN: &str = "https://payroll.com";

    /// A discovery config whose primary client is `TEST_AUD` (so `VALID_JWT`,
    /// which carries `aud == TEST_AUD`, verifies as a primary/ungated login) and
    /// whose issuer matches the JWT's Google issuer.
    fn sso_gate_config(
        app_clients: Vec<sso::AppClient>,
        gate_all_apps: bool,
    ) -> sso::DiscoveredConfig {
        sso::DiscoveredConfig {
            issuer: "https://accounts.google.com".to_string(),
            client_id: TEST_AUD.to_string(),
            jwks_uri: "https://accounts.google.com/jwks".to_string(),
            authorization_endpoint: "https://accounts.google.com/authorize".to_string(),
            scopes: vec!["openid".to_string()],
            name: Some("Example".to_string()),
            app_clients,
            gate_all_apps,
            stable_identifier_claim: "sub".to_string(),
        }
    }

    #[test]
    fn sso_gate_ungated_login_mints() {
        // Ungated origin (not in app_clients, gate off) resolves to the primary
        // client; `VALID_JWT`'s aud == primary, so the gate passes and a
        // credential is built, stamped with the SSO domain.
        sso::test_setup_discovery(SSO_DOMAIN, sso_gate_config(vec![], false), test_certs());
        match verify_sso_jwt(VALID_JWT, &test_salt(), SSO_DOMAIN, "https://public.app") {
            Ok(Cached::Ready(v)) => {
                assert_eq!(v.credential.aud, TEST_AUD);
                assert_eq!(v.credential.sso_domain, Some(SSO_DOMAIN.to_string()));
                assert!(!v.gated);
            }
            other => panic!("expected Ready verification, got {other:?}"),
        }
    }

    #[test]
    fn sso_gate_refuses_token_minted_for_another_app() {
        // The gated origin's declared per-app client differs from the token's
        // aud (a token minted for the primary / another client). The gate is
        // mint-or-refuse: verification fails, so no delegation can be minted.
        let app_clients = vec![sso::AppClient {
            key: sso::AppClientKey::Cleartext(GATED_ORIGIN.to_string()),
            client_id: PER_APP_CLIENT.to_string(),
        }];
        sso::test_setup_discovery(
            SSO_DOMAIN,
            sso_gate_config(app_clients, false),
            test_certs(),
        );
        assert!(verify_sso_jwt(VALID_JWT, &test_salt(), SSO_DOMAIN, GATED_ORIGIN).is_err());
    }

    #[test]
    fn sso_gate_all_apps_denies_unlisted_origin() {
        sso::test_setup_discovery(SSO_DOMAIN, sso_gate_config(vec![], true), test_certs());
        assert!(
            verify_sso_jwt(VALID_JWT, &test_salt(), SSO_DOMAIN, "https://unlisted.app").is_err()
        );
    }

    #[test]
    fn sso_session_seed_binds_all_inputs() {
        // The SSO-session seed binds (iss, sub, sso_domain, origin, anchor):
        // changing any input changes the seed, so a session minted for one
        // (domain, origin) can never authenticate another. (`sso_session_principal`
        // / `matching_sso_session` wrap this seed; they call `ic_cdk::id()` and
        // are exercised end-to-end in the integration tests.)
        let base = calculate_sso_delegation_seed("iss", "sub", SSO_DOMAIN, GATED_ORIGIN, 42);
        // Same inputs -> same seed.
        assert_eq!(
            base,
            calculate_sso_delegation_seed("iss", "sub", SSO_DOMAIN, GATED_ORIGIN, 42)
        );
        // Each field is load-bearing.
        assert_ne!(
            base,
            calculate_sso_delegation_seed("iss2", "sub", SSO_DOMAIN, GATED_ORIGIN, 42)
        );
        assert_ne!(
            base,
            calculate_sso_delegation_seed("iss", "sub2", SSO_DOMAIN, GATED_ORIGIN, 42)
        );
        assert_ne!(
            base,
            calculate_sso_delegation_seed("iss", "sub", "other.org", GATED_ORIGIN, 42)
        );
        assert_ne!(
            base,
            calculate_sso_delegation_seed("iss", "sub", SSO_DOMAIN, "https://other.app", 42)
        );
        assert_ne!(
            base,
            calculate_sso_delegation_seed("iss", "sub", SSO_DOMAIN, GATED_ORIGIN, 43)
        );
        // Distinct from the OpenID/direct-credential seed for the same identity —
        // the domain separator keeps SSO-session principals disjoint.
        let openid_seed = calculate_delegation_seed(
            &("iss".to_string(), "sub".to_string(), "aud".to_string()),
            42,
        );
        assert_ne!(base, openid_seed);
    }

    fn sso_verification(
        gated: bool,
        claim: &str,
        sub: &str,
        stable_id: Option<&str>,
    ) -> SsoVerification {
        SsoVerification {
            credential: OpenIdCredential {
                iss: "https://idp".to_string(),
                sub: sub.to_string(),
                aud: if gated { "per-app" } else { "primary" }.to_string(),
                last_usage_timestamp: None,
                metadata: HashMap::new(),
                sso_domain: Some(SSO_DOMAIN.to_string()),
                sso_name: None,
            },
            primary_client_id: "primary".to_string(),
            gated,
            stable_identifier_claim: claim.to_string(),
            stable_id: stable_id.map(str::to_string),
        }
    }

    #[test]
    fn resolve_primary_identity_sub_uses_token_sub() {
        let v = sso_verification(true, "sub", "sub-123", None);
        let identity = resolve_primary_identity(&v).expect("sub resolves directly");
        assert_eq!(identity.credential.sub, "sub-123");
        assert_eq!(identity.credential.aud, "primary");
    }

    #[test]
    fn resolve_primary_identity_non_sub_gated_needs_normal_login_first() {
        SSO_STABLE_ID_INDEX.with_borrow_mut(|m| m.clear());
        // First gated login for an Entra-style (oid) org, before any primary
        // login populated the aux index: no anchor can be resolved (§6.5).
        let gated = sso_verification(true, "oid", "per-app-sub", Some("oid-1"));
        assert!(matches!(
            resolve_primary_identity(&gated),
            Err(OpenIdDelegationError::NoSuchAnchor)
        ));

        // A normal (primary) login carries both the primary sub and the stable
        // id, populating the aux index.
        let primary = sso_verification(false, "oid", "primary-sub", Some("oid-1"));
        let identity = resolve_primary_identity(&primary).expect("primary login resolves");
        assert_eq!(identity.credential.sub, "primary-sub");

        // Now the gated login bridges (iss, oid) -> primary sub.
        let identity =
            resolve_primary_identity(&gated).expect("gated resolves after aux populated");
        assert_eq!(identity.credential.sub, "primary-sub");
    }
}
