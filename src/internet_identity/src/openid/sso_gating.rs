//! SSO per-app gating and primary-identity resolution: verify an SSO JWT
//! against the origin's resolved client, bridge per-app logins to the primary
//! identity, and the stable-id aux bridge.

use super::{
    decode_iss_aud_claims, sso, verify, Cached, OpenIDJWTVerificationError, OpenIdCredential,
};
use crate::state;
use internet_identity_interface::internet_identity::types::openid::OpenIdDelegationError;
use internet_identity_interface::internet_identity::types::IdRegFinishError;

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

pub struct AuxRecord {
    pub iss: String,
    pub primary_client_id: String,
    pub stable_id: String,
    pub primary_sub: String,
}

fn aux_stable_id_insert(iss: &str, primary_client_id: &str, stable_id: &str, primary_sub: &str) {
    state::storage_borrow_mut(|storage| {
        storage.insert_sso_stable_id(iss, primary_client_id, stable_id, primary_sub);
    });
}

fn aux_stable_id_lookup(iss: &str, primary_client_id: &str, stable_id: &str) -> Option<String> {
    state::storage_borrow(|storage| storage.lookup_sso_stable_id(iss, primary_client_id, stable_id))
}

/// Persist an [`AuxRecord`]. Call only from update contexts, never a query.
pub fn record_primary_sso_bridge(record: &AuxRecord) {
    aux_stable_id_insert(
        &record.iss,
        &record.primary_client_id,
        &record.stable_id,
        &record.primary_sub,
    );
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
    if !sso::is_allowed_discovery_domain(discovery_domain) {
        return Err(OpenIDJWTVerificationError::GenericError(format!(
            "SSO discovery domain not allowed: {discovery_domain}"
        )));
    }
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

    Ok(Cached::Ready(SsoVerification {
        credential,
        primary_client_id: cfg.client_id,
        gated,
        stable_identifier_claim: cfg.stable_identifier_claim,
        stable_id,
    }))
}

pub struct SsoPrimaryIdentity {
    /// Primary-keyed credential: `aud` is the primary client.
    pub credential: OpenIdCredential,
    /// Bridge entry to persist; `None` for `sub` orgs and gated logins.
    pub aux_record: Option<AuxRecord>,
}

/// Resolve a verified SSO login to the primary identity. Pure w.r.t. writes:
/// reads the aux bridge but never writes it (surfaces `aux_record` instead), so a
/// query caller stays valid. `Err(NoSuchAnchor)`: a non-`sub` gated login whose
/// stable id has no bridge entry yet — sign in normally first.
pub fn resolve_primary_identity(
    verification: &SsoVerification,
) -> Result<SsoPrimaryIdentity, OpenIdDelegationError> {
    let is_sub = verification.stable_identifier_claim == sso::DEFAULT_STABLE_IDENTIFIER_CLAIM;
    let (primary_sub, aux_record) = if is_sub {
        (verification.credential.sub.clone(), None)
    } else {
        let stable_id = verification
            .stable_id
            .clone()
            .ok_or(OpenIdDelegationError::JwtVerificationFailed)?;
        if verification.gated {
            let primary_sub = aux_stable_id_lookup(
                &verification.credential.iss,
                &verification.primary_client_id,
                &stable_id,
            )
            .ok_or(OpenIdDelegationError::NoSuchAnchor)?;
            (primary_sub, None)
        } else {
            (
                verification.credential.sub.clone(),
                Some(AuxRecord {
                    iss: verification.credential.iss.clone(),
                    primary_client_id: verification.primary_client_id.clone(),
                    stable_id,
                    primary_sub: verification.credential.sub.clone(),
                }),
            )
        }
    };

    let credential = OpenIdCredential {
        aud: verification.primary_client_id.clone(),
        sub: primary_sub,
        ..verification.credential.clone()
    };
    Ok(SsoPrimaryIdentity {
        credential,
        aux_record,
    })
}

/// Record the non-`sub` aux bridge after a normal primary-client SSO login, so a
/// later gated per-app login (whose pairwise sub differs) can be bridged to the
/// primary identity. No-op for `sub` orgs and for direct providers (no
/// `sso_domain`).
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
        // credential is primary-keyed here, so `aud` is the primary client.
        aux_stable_id_insert(
            &credential.iss,
            &credential.aud,
            &stable_id,
            &credential.sub,
        );
    }
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
        Ok(identity) => {
            if let Some(record) = &identity.aux_record {
                record_primary_sso_bridge(record);
            }
            Ok(Cached::Ready(identity.credential))
        }
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

    #[test]
    fn resolve_primary_identity_sub_uses_token_sub() {
        let v = sso_verification(true, "sub", "sub-123", None);
        let identity = resolve_primary_identity(&v).expect("sub resolves directly");
        assert_eq!(identity.credential.sub, "sub-123");
        assert_eq!(identity.credential.aud, "primary");
        assert!(identity.aux_record.is_none());
    }

    #[test]
    fn resolve_primary_identity_non_sub_gated_needs_normal_login_first() {
        init_test_storage();
        let gated = sso_verification(true, "oid", "per-app-sub", Some("oid-1"));
        assert!(matches!(
            resolve_primary_identity(&gated),
            Err(OpenIdDelegationError::NoSuchAnchor)
        ));

        let primary = sso_verification(false, "oid", "primary-sub", Some("oid-1"));
        let identity = resolve_primary_identity(&primary).expect("primary login resolves");
        assert_eq!(identity.credential.sub, "primary-sub");
        let record = identity
            .aux_record
            .expect("primary login surfaces a bridge entry");
        assert_eq!(record.iss, "https://idp");
        assert_eq!(record.primary_client_id, "primary");
        assert_eq!(record.stable_id, "oid-1");
        assert_eq!(record.primary_sub, "primary-sub");
        assert!(matches!(
            resolve_primary_identity(&gated),
            Err(OpenIdDelegationError::NoSuchAnchor)
        ));

        record_primary_sso_bridge(&record);

        let identity =
            resolve_primary_identity(&gated).expect("gated resolves after bridge recorded");
        assert_eq!(identity.credential.sub, "primary-sub");
        assert!(identity.aux_record.is_none());
    }

    #[test]
    fn aux_bridge_is_scoped_per_primary_client() {
        init_test_storage();
        let primary_a =
            sso_verification_with_client(false, "oid", "primary-sub-a", Some("oid-9"), "client-a");
        let primary_b =
            sso_verification_with_client(false, "oid", "primary-sub-b", Some("oid-9"), "client-b");
        record_primary_sso_bridge(
            &resolve_primary_identity(&primary_a)
                .unwrap()
                .aux_record
                .unwrap(),
        );
        record_primary_sso_bridge(
            &resolve_primary_identity(&primary_b)
                .unwrap()
                .aux_record
                .unwrap(),
        );

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
