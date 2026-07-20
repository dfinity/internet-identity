//! SSO per-app gating and primary-identity resolution: verify an SSO JWT
//! against the origin's resolved client and bridge per-app logins to the
//! primary identity via the storage-maintained SSO stable-id index (the
//! `stable_id` stamped on the primary credential, which the anchor `write()`
//! reconciles into the index).

use super::{
    decode_iss_aud_claims, sso, verify, Cached, OpenIDJWTVerificationError, OpenIdCredential,
};
use crate::state;
use internet_identity_interface::internet_identity::types::openid::OpenIdDelegationError;
use internet_identity_interface::internet_identity::types::{AnchorNumber, IdRegFinishError};

/// Maximum length of the configured stable-identifier claim value.
const MAX_STABLE_IDENTIFIER_LENGTH: usize = 255;

/// A verified SSO login. `credential.aud` is the resolved (per-app or primary)
/// client; `primary_client_id` is always the primary, on which identity is keyed.
#[derive(Debug)]
pub struct SsoVerification {
    pub credential: OpenIdCredential,
    pub primary_client_id: String,
    /// True when the origin resolved to a per-app client.
    pub gated: bool,
    pub stable_identifier_claim: String,
    /// The cross-client-stable identifier; `None` when the claim is `sub`.
    pub stable_id: Option<String>,
}

/// Extract a string claim from a JWT's raw claims; `None` if absent or non-string.
fn extract_string_claim(jwt: &str, claim: &str) -> Option<String> {
    let (_, _, claims_bytes) = decode_iss_aud_claims(jwt).ok()?;
    let value = serde_json::from_slice::<serde_json::Value>(&claims_bytes).ok()?;
    value.get(claim)?.as_str().map(str::to_string)
}

/// Verify an SSO JWT and enforce the gate: the JWT's `aud` must match the client
/// the origin resolves to, so no delegation is minted for a token issued for a
/// different app. `Ok(Cached::Pending)` means discovery/JWKS isn't cached yet —
/// drive it via [`super::prefetch_sso`], then retry.
pub fn verify_sso_jwt(
    jwt: &str,
    salt: &[u8; 32],
    discovery_domain: &str,
    origin: &str,
) -> Result<Cached<SsoVerification>, OpenIDJWTVerificationError> {
    sso::validate_allowed_discovery_domain(discovery_domain)
        .map_err(OpenIDJWTVerificationError::GenericError)?;
    let cfg = match sso::peek_discovery(discovery_domain) {
        Cached::Pending => return Ok(Cached::Pending),
        Cached::Ready(cfg) => cfg,
    };

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
    if stable_id
        .as_ref()
        .is_some_and(|id| id.len() > MAX_STABLE_IDENTIFIER_LENGTH)
    {
        return Err(OpenIDJWTVerificationError::GenericError(
            "stable identifier claim too long".to_string(),
        ));
    }

    Ok(Cached::Ready(SsoVerification {
        credential,
        primary_client_id: cfg.client_id,
        gated,
        stable_identifier_claim: cfg.stable_identifier_claim,
        stable_id,
    }))
}

pub struct SsoPrimaryIdentity {
    /// Primary-keyed credential to store: `aud` is the primary client. For a
    /// non-`sub` org it carries `stable_id = Some(oid)` so the anchor `write()`
    /// (re)establishes the SSO stable-id index entry; the gated resolve below
    /// sets the same value so re-writing the primary credential leaves the
    /// index unchanged.
    pub credential: OpenIdCredential,
}

/// Resolve a verified SSO login to the primary identity. Reads only (safe from a
/// query context): a gated non-`sub` login is resolved through the
/// storage-maintained stable-id index. `Err(NoSuchAnchor)`: a non-`sub` gated
/// login whose stable id has no index entry — either the user never signed in
/// normally, or the primary credential was removed (the index self-cleans on
/// `write()`), so this fails safe. The caller persists `credential` through the
/// normal anchor `write()`, which reconciles the index.
pub fn resolve_primary_identity(
    verification: &SsoVerification,
) -> Result<SsoPrimaryIdentity, OpenIdDelegationError> {
    let is_sub = verification.stable_identifier_claim == sso::DEFAULT_STABLE_IDENTIFIER_CLAIM;
    if is_sub {
        // `sub` orgs key identity on the token `sub` directly; no bridging.
        let credential = OpenIdCredential {
            aud: verification.primary_client_id.clone(),
            sub: verification.credential.sub.clone(),
            stable_id: None,
            ..verification.credential.clone()
        };
        return Ok(SsoPrimaryIdentity { credential });
    }

    let stable_id = verification
        .stable_id
        .clone()
        .ok_or(OpenIdDelegationError::JwtVerificationFailed)?;

    let primary_sub = if verification.gated {
        let anchor_number = state::storage_borrow(|storage| {
            storage.lookup_anchor_by_sso_stable_id(
                &verification.credential.iss,
                &verification.primary_client_id,
                &stable_id,
            )
        })
        .ok_or(OpenIdDelegationError::NoSuchAnchor)?;
        primary_sub_on_anchor(
            anchor_number,
            &verification.credential.iss,
            &verification.primary_client_id,
        )
        .ok_or(OpenIdDelegationError::NoSuchAnchor)?
    } else {
        verification.credential.sub.clone()
    };

    let credential = OpenIdCredential {
        aud: verification.primary_client_id.clone(),
        sub: primary_sub,
        stable_id: Some(stable_id),
        ..verification.credential.clone()
    };
    Ok(SsoPrimaryIdentity { credential })
}

/// Load `anchor_number` and return the `sub` of its primary-keyed OpenID
/// credential for `(iss, primary_client_id)`, if present. `None` when the
/// anchor is gone or no longer carries that credential — the index pointed
/// here but the credential has since been removed, so the gated login fails
/// safe.
fn primary_sub_on_anchor(
    anchor_number: AnchorNumber,
    iss: &str,
    primary_client_id: &str,
) -> Option<String> {
    state::storage_borrow(|storage| {
        let anchor = storage.read(anchor_number).ok()?;
        anchor
            .openid_credentials()
            .iter()
            .find(|cred| cred.iss == iss && cred.aud == primary_client_id)
            .map(|cred| cred.sub.clone())
    })
}

/// Stamp the non-`sub` stable id (the `oid` claim) onto a primary-keyed SSO
/// credential so the anchor `write()` establishes its SSO stable-id index
/// entry. Used on the direct primary-login registration path, which builds the
/// credential from [`verify::verify_and_build`] rather than
/// [`resolve_primary_identity`]. No-op for `sub` orgs and direct providers (no
/// `sso_domain`).
pub fn stamp_primary_sso_stable_id(jwt: &str, credential: &mut OpenIdCredential) {
    let Some(domain) = credential.sso_domain.as_deref() else {
        return;
    };
    let Cached::Ready(cfg) = sso::peek_discovery(domain) else {
        return;
    };
    if cfg.stable_identifier_claim == sso::DEFAULT_STABLE_IDENTIFIER_CLAIM {
        return;
    }
    // `credential` is primary-keyed here, so `aud` is the primary client.
    credential.stable_id = extract_string_claim(jwt, &cfg.stable_identifier_claim);
}

/// Verify an SSO JWT for registration, returning the primary-keyed credential to
/// store — a first gated login registers under the primary client, never a
/// per-app one. Fails safe: a non-`sub` per-app token with no bridge yet returns
/// an error and stores nothing.
pub fn verify_sso_for_registration(
    jwt: &str,
    salt: &[u8; 32],
    discovery_domain: &str,
    origin: &str,
) -> Result<Cached<OpenIdCredential>, IdRegFinishError> {
    let verification = match verify_sso_jwt(jwt, salt, discovery_domain, origin) {
        Ok(Cached::Pending) => return Ok(Cached::Pending),
        Ok(Cached::Ready(verification)) => verification,
        Err(err) => return Err(err.into()),
    };
    match resolve_primary_identity(&verification) {
        // The resolved credential carries `stable_id` for a non-`sub` primary
        // login; the registration `write()` reconciles the stable-id index.
        Ok(identity) => Ok(Cached::Ready(identity.credential)),
        Err(OpenIdDelegationError::NoSuchAnchor) => Err(IdRegFinishError::SsoNormalLoginRequired),
        Err(_) => Err(IdRegFinishError::InvalidAuthnMethod(
            "SSO identity could not be resolved".to_string(),
        )),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::openid::tests::{test_certs, test_salt, TEST_AUD, VALID_JWT};
    use std::collections::HashMap;

    const SSO_DOMAIN: &str = "example.org";
    const PER_APP_CLIENT: &str = "0oaPAYROLL";
    const GATED_ORIGIN: &str = "https://payroll.com";

    /// Discovery config whose primary client is `TEST_AUD`, matching `VALID_JWT`'s `aud`.
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

    fn sso_verification(
        gated: bool,
        claim: &str,
        sub: &str,
        stable_id: Option<&str>,
    ) -> SsoVerification {
        sso_verification_with_client(gated, claim, sub, stable_id, "primary")
    }

    fn sso_verification_with_client(
        gated: bool,
        claim: &str,
        sub: &str,
        stable_id: Option<&str>,
        primary_client_id: &str,
    ) -> SsoVerification {
        SsoVerification {
            credential: OpenIdCredential {
                iss: "https://idp".to_string(),
                sub: sub.to_string(),
                aud: if gated {
                    "per-app".to_string()
                } else {
                    primary_client_id.to_string()
                },
                last_usage_timestamp: None,
                metadata: HashMap::new(),
                sso_domain: Some(SSO_DOMAIN.to_string()),
                sso_name: None,
                stable_id: None,
            },
            primary_client_id: primary_client_id.to_string(),
            gated,
            stable_identifier_claim: claim.to_string(),
            stable_id: stable_id.map(str::to_string),
        }
    }

    fn init_test_storage() {
        crate::state::storage_replace(crate::storage::Storage::new(
            (0, 10_000),
            ic_stable_structures::VectorMemory::default(),
        ));
    }

    /// Persist a resolved primary-keyed credential on a fresh anchor exactly as
    /// the production write path does (allocate → add → `write()`), so the
    /// anchor `write()` reconciles the SSO stable-id index. Returns the anchor.
    fn store_primary_credential(credential: OpenIdCredential) -> AnchorNumber {
        crate::state::storage_borrow_mut(|storage| {
            let mut anchor = storage.allocate_anchor(0).unwrap();
            anchor.add_openid_credential(credential).unwrap();
            let anchor_number = anchor.anchor_number();
            storage.write(anchor).unwrap();
            anchor_number
        })
    }

    /// Remove the given credential from `anchor_number` and `write()`, so the
    /// index self-cleans.
    fn remove_credential(anchor_number: AnchorNumber, key: &crate::openid::OpenIdCredentialKey) {
        crate::state::storage_borrow_mut(|storage| {
            let mut anchor = storage.read(anchor_number).unwrap();
            anchor.remove_openid_credential(key).unwrap();
            storage.write(anchor).unwrap();
        });
    }

    #[test]
    fn resolve_primary_identity_sub_uses_token_sub() {
        let v = sso_verification(true, "sub", "sub-123", None);
        let identity = resolve_primary_identity(&v).expect("sub resolves directly");
        assert_eq!(identity.credential.sub, "sub-123");
        assert_eq!(identity.credential.aud, "primary");
        // `sub` orgs never bridge, so the primary credential carries no stable id.
        assert_eq!(identity.credential.stable_id, None);
    }

    #[test]
    fn resolve_primary_identity_non_sub_gated_needs_normal_login_first() {
        init_test_storage();
        let gated = sso_verification(true, "oid", "per-app-sub", Some("oid-1"));
        // No primary credential stored yet: the index has no entry, fail safe.
        assert!(matches!(
            resolve_primary_identity(&gated),
            Err(OpenIdDelegationError::NoSuchAnchor)
        ));

        // A normal (non-gated) primary login resolves to a credential stamped
        // with the stable id; persisting it populates the index.
        let primary = sso_verification(false, "oid", "primary-sub", Some("oid-1"));
        let identity = resolve_primary_identity(&primary).expect("primary login resolves");
        assert_eq!(identity.credential.sub, "primary-sub");
        assert_eq!(identity.credential.aud, "primary");
        assert_eq!(identity.credential.stable_id, Some("oid-1".to_string()));
        let key = identity.credential.key();
        let anchor_number = store_primary_credential(identity.credential);

        // Now the gated login resolves to the primary sub via the index.
        let identity =
            resolve_primary_identity(&gated).expect("gated resolves after index populated");
        assert_eq!(identity.credential.sub, "primary-sub");
        assert_eq!(identity.credential.aud, "primary");
        // The gated resolve keeps the stable id so re-writing the primary
        // credential leaves the index entry in place.
        assert_eq!(identity.credential.stable_id, Some("oid-1".to_string()));

        // Removing the primary credential self-cleans the index: the gated
        // login fails safe again. This is the cleanup this redesign delivers.
        remove_credential(anchor_number, &key);
        assert!(matches!(
            resolve_primary_identity(&gated),
            Err(OpenIdDelegationError::NoSuchAnchor)
        ));
    }

    #[test]
    fn sso_stable_id_index_is_scoped_per_primary_client() {
        init_test_storage();
        // Same stable id (oid-9) under two distinct primary clients.
        let primary_a =
            sso_verification_with_client(false, "oid", "primary-sub-a", Some("oid-9"), "client-a");
        let primary_b =
            sso_verification_with_client(false, "oid", "primary-sub-b", Some("oid-9"), "client-b");
        store_primary_credential(resolve_primary_identity(&primary_a).unwrap().credential);
        store_primary_credential(resolve_primary_identity(&primary_b).unwrap().credential);

        let gated_a =
            sso_verification_with_client(true, "oid", "pairwise-a", Some("oid-9"), "client-a");
        let gated_b =
            sso_verification_with_client(true, "oid", "pairwise-b", Some("oid-9"), "client-b");
        assert_eq!(
            resolve_primary_identity(&gated_a).unwrap().credential.sub,
            "primary-sub-a",
            "client-a gated login must resolve to client-a's primary sub"
        );
        assert_eq!(
            resolve_primary_identity(&gated_b).unwrap().credential.sub,
            "primary-sub-b",
            "client-b gated login must resolve to client-b's primary sub (no clobber)"
        );
    }
}
