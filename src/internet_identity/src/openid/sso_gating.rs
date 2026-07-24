//! Per-app SSO gating. An org registers Internet Identity as one OIDC client
//! (the *II client*) — the one a user's anchor is keyed on — plus optional
//! *per-app* clients, one per gated dapp.
//!
//! The problem: a gated dapp signs in against its own per-app client, so the
//! token's identity (`aud`, and for pairwise orgs like Entra the `sub`) is
//! scoped to that client, not the II client the anchor is keyed on. A gated
//! login therefore can't be matched to the anchor directly.
//!
//! This file bridges the two: (a) it verifies the SSO JWT against the client the
//! origin resolves to (the gate), then (b) maps the login to the II-client
//! identity via the storage-maintained SSO stable-id index — keyed on the
//! II-client credential's cross-client-stable `stable_id`, which the anchor
//! `write()` reconciles into the index.

use super::{sso, verify, Cached, OpenIDJWTVerificationError, OpenIdCredential};
use crate::state;
use internet_identity_interface::internet_identity::types::openid::OpenIdDelegationError;
use internet_identity_interface::internet_identity::types::{AnchorNumber, IdRegFinishError};

/// A verified SSO login. `credential.aud` is the resolved (per-app or II)
/// client; `ii_client_id` is always the II client, on which identity is keyed.
#[derive(Debug)]
pub struct VerifiedSsoLogin {
    pub credential: OpenIdCredential,
    pub ii_client_id: String,
    pub stable_identifier_claim: String,
    /// The cross-client-stable identifier; `None` when the claim is `sub`.
    pub stable_id: Option<String>,
}

impl VerifiedSsoLogin {
    /// The login used a per-app client (its `sub` is scoped to that client and
    /// must be bridged via the stable-id index), not the org's II client.
    fn is_gated(&self) -> bool {
        self.credential.aud != self.ii_client_id
    }
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
) -> Result<Cached<VerifiedSsoLogin>, OpenIDJWTVerificationError> {
    sso::validate_discovery_domain(discovery_domain)
        .map_err(OpenIDJWTVerificationError::GenericError)?;
    let discovery_config = match sso::peek_discovery(discovery_domain) {
        Cached::Pending => return Ok(Cached::Pending),
        Cached::Ready(discovery_config) => discovery_config,
    };

    let expected_client = discovery_config
        .resolve_client_for_origin(origin)
        .map_err(|_| {
            OpenIDJWTVerificationError::GenericError(format!(
                "origin '{origin}' is denied for domain '{discovery_domain}' (gate_all_apps)"
            ))
        })?;

    let keys = match sso::read_jwks(&discovery_config.jwks_uri) {
        Cached::Pending => return Ok(Cached::Pending),
        Cached::Ready(keys) => keys,
    };

    let descriptor = verify::Descriptor {
        issuer: discovery_config.issuer.clone(),
        client_id: expected_client.clone(),
        sso: Some(verify::SsoProvider {
            domain: discovery_domain.to_string(),
            name: discovery_config.name.clone(),
            stable_identifier_claim: sso::non_default_stable_claim(
                &discovery_config.stable_identifier_claim,
            ),
        }),
    };
    // `verify_and_build` extracts the stable id (and enforces its length cap)
    // from the claim above, so the credential already carries it.
    let credential = verify::verify_and_build(jwt, &descriptor, &keys, salt)?;

    Ok(Cached::Ready(VerifiedSsoLogin {
        stable_id: credential.stable_id.clone(),
        credential,
        ii_client_id: discovery_config.client_id,
        stable_identifier_claim: discovery_config.stable_identifier_claim,
    }))
}

pub struct SsoResolvedIdentity {
    /// II-client-keyed credential to store: `aud` is the II client. For a
    /// non-`sub` org it carries `stable_id = Some(oid)` so the anchor `write()`
    /// (re)establishes the SSO stable-id index entry; the gated resolve below
    /// sets the same value so re-writing the II-client credential leaves the
    /// index unchanged.
    pub credential: OpenIdCredential,
}

/// Resolve a verified SSO login to the II-client identity. Reads only (safe from
/// a query context): a gated non-`sub` login is resolved through the
/// storage-maintained stable-id index. `Err(NoSuchAnchor)`: a non-`sub` gated
/// login whose stable id has no index entry — either the user never signed in
/// normally, or the II-client credential was removed (the index self-cleans on
/// `write()`), so this fails safe. The caller persists `credential` through the
/// normal anchor `write()`, which reconciles the index.
pub fn resolve_ii_client_identity(
    verification: &VerifiedSsoLogin,
) -> Result<SsoResolvedIdentity, OpenIdDelegationError> {
    let is_sub = verification.stable_identifier_claim == sso::DEFAULT_STABLE_IDENTIFIER_CLAIM;
    if is_sub {
        // `sub` orgs key identity on the token `sub` directly; no bridging.
        let credential = OpenIdCredential {
            aud: verification.ii_client_id.clone(),
            sub: verification.credential.sub.clone(),
            stable_id: None,
            ..verification.credential.clone()
        };
        return Ok(SsoResolvedIdentity { credential });
    }

    let stable_id = verification
        .stable_id
        .clone()
        .ok_or(OpenIdDelegationError::JwtVerificationFailed)?;

    let ii_client_sub = if verification.is_gated() {
        // Scope the lookup to the domain the login was discovered through, so a
        // gated login can only ever resolve to an II-client identity established
        // through that same domain (an SSO credential always carries it).
        let sso_domain = verification
            .credential
            .sso_domain
            .as_deref()
            .ok_or(OpenIdDelegationError::NoSuchAnchor)?;
        let anchor_number = state::storage_borrow(|storage| {
            storage.lookup_anchor_by_sso_stable_id(
                sso_domain,
                &verification.credential.iss,
                &verification.ii_client_id,
                &stable_id,
            )
        })
        .ok_or(OpenIdDelegationError::NoSuchAnchor)?;
        ii_client_sub_on_anchor(
            anchor_number,
            &verification.credential.iss,
            &verification.ii_client_id,
        )
        .ok_or(OpenIdDelegationError::NoSuchAnchor)?
    } else {
        verification.credential.sub.clone()
    };

    let credential = OpenIdCredential {
        aud: verification.ii_client_id.clone(),
        sub: ii_client_sub,
        stable_id: Some(stable_id),
        ..verification.credential.clone()
    };
    Ok(SsoResolvedIdentity { credential })
}

/// Load `anchor_number` and return the `sub` of its II-client-keyed OpenID
/// credential for `(iss, ii_client_id)`, if present. `None` when the
/// anchor is gone or no longer carries that credential — the index pointed
/// here but the credential has since been removed, so the gated login fails
/// safe.
fn ii_client_sub_on_anchor(
    anchor_number: AnchorNumber,
    iss: &str,
    ii_client_id: &str,
) -> Option<String> {
    state::storage_borrow(|storage| {
        let anchor = storage.read(anchor_number).ok()?;
        anchor
            .openid_credentials()
            .iter()
            .find(|cred| cred.iss == iss && cred.aud == ii_client_id)
            .map(|cred| cred.sub.clone())
    })
}

/// Verify an SSO JWT for registration, returning the II-client-keyed credential to
/// store — a first gated login registers under the II client, never a
/// per-app one. Fails safe: a non-`sub` per-app token with no bridge yet returns
/// an error and stores nothing.
pub fn verify_sso_for_registration(
    jwt: &str,
    salt: &[u8; 32],
    discovery_domain: &str,
    origin: &str,
) -> Result<Cached<OpenIdCredential>, IdRegFinishError> {
    let verification_outcome = match verify_sso_jwt(jwt, salt, discovery_domain, origin) {
        Ok(Cached::Pending) => return Ok(Cached::Pending),
        Ok(Cached::Ready(verification)) => verification,
        Err(err) => return Err(err.into()),
    };
    match resolve_ii_client_identity(&verification_outcome) {
        // The resolved credential carries `stable_id` for a non-`sub` II-client
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

    /// Discovery config whose II client is `TEST_AUD`, matching `VALID_JWT`'s `aud`.
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
                assert!(!v.is_gated());
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
    ) -> VerifiedSsoLogin {
        sso_verification_with_client(gated, claim, sub, stable_id, "ii-client")
    }

    fn sso_verification_with_client(
        gated: bool,
        claim: &str,
        sub: &str,
        stable_id: Option<&str>,
        ii_client_id: &str,
    ) -> VerifiedSsoLogin {
        VerifiedSsoLogin {
            credential: OpenIdCredential {
                iss: "https://idp".to_string(),
                sub: sub.to_string(),
                aud: if gated {
                    "per-app".to_string()
                } else {
                    ii_client_id.to_string()
                },
                last_usage_timestamp: None,
                metadata: HashMap::new(),
                sso_domain: Some(SSO_DOMAIN.to_string()),
                sso_name: None,
                stable_id: None,
            },
            ii_client_id: ii_client_id.to_string(),
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

    /// Persist a resolved II-client-keyed credential on a fresh anchor exactly as
    /// the production write path does (allocate → add → `write()`), so the
    /// anchor `write()` reconciles the SSO stable-id index. Returns the anchor.
    fn store_ii_client_credential(credential: OpenIdCredential) -> AnchorNumber {
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
    fn resolve_ii_client_identity_sub_uses_token_sub() {
        let v = sso_verification(true, "sub", "sub-123", None);
        let identity = resolve_ii_client_identity(&v).expect("sub resolves directly");
        assert_eq!(identity.credential.sub, "sub-123");
        assert_eq!(identity.credential.aud, "ii-client");
        // `sub` orgs never bridge, so the II-client credential carries no stable id.
        assert_eq!(identity.credential.stable_id, None);
    }

    #[test]
    fn resolve_ii_client_identity_non_sub_gated_needs_normal_login_first() {
        init_test_storage();
        let gated = sso_verification(true, "oid", "per-app-sub", Some("oid-1"));
        // No II-client credential stored yet: the index has no entry, fail safe.
        assert!(matches!(
            resolve_ii_client_identity(&gated),
            Err(OpenIdDelegationError::NoSuchAnchor)
        ));

        // A normal (non-gated) II-client login resolves to a credential stamped
        // with the stable id; persisting it populates the index.
        let ii_client_login = sso_verification(false, "oid", "ii-client-sub", Some("oid-1"));
        let identity =
            resolve_ii_client_identity(&ii_client_login).expect("II-client login resolves");
        assert_eq!(identity.credential.sub, "ii-client-sub");
        assert_eq!(identity.credential.aud, "ii-client");
        assert_eq!(identity.credential.stable_id, Some("oid-1".to_string()));
        let key = identity.credential.key();
        let anchor_number = store_ii_client_credential(identity.credential);

        // Now the gated login resolves to the II-client sub via the index.
        let identity =
            resolve_ii_client_identity(&gated).expect("gated resolves after index populated");
        assert_eq!(identity.credential.sub, "ii-client-sub");
        assert_eq!(identity.credential.aud, "ii-client");
        // The gated resolve keeps the stable id so re-writing the II-client
        // credential leaves the index entry in place.
        assert_eq!(identity.credential.stable_id, Some("oid-1".to_string()));

        // Removing the II-client credential self-cleans the index: the gated
        // login fails safe again. This is the cleanup this redesign delivers.
        remove_credential(anchor_number, &key);
        assert!(matches!(
            resolve_ii_client_identity(&gated),
            Err(OpenIdDelegationError::NoSuchAnchor)
        ));
    }

    #[test]
    fn sso_stable_id_index_is_scoped_per_ii_client() {
        init_test_storage();
        // Same stable id (oid-9) under two distinct II clients.
        let ii_client_login_a = sso_verification_with_client(
            false,
            "oid",
            "ii-client-sub-a",
            Some("oid-9"),
            "client-a",
        );
        let ii_client_login_b = sso_verification_with_client(
            false,
            "oid",
            "ii-client-sub-b",
            Some("oid-9"),
            "client-b",
        );
        store_ii_client_credential(
            resolve_ii_client_identity(&ii_client_login_a)
                .unwrap()
                .credential,
        );
        store_ii_client_credential(
            resolve_ii_client_identity(&ii_client_login_b)
                .unwrap()
                .credential,
        );

        let gated_a =
            sso_verification_with_client(true, "oid", "pairwise-a", Some("oid-9"), "client-a");
        let gated_b =
            sso_verification_with_client(true, "oid", "pairwise-b", Some("oid-9"), "client-b");
        assert_eq!(
            resolve_ii_client_identity(&gated_a).unwrap().credential.sub,
            "ii-client-sub-a",
            "client-a gated login must resolve to client-a's II-client sub"
        );
        assert_eq!(
            resolve_ii_client_identity(&gated_b).unwrap().credential.sub,
            "ii-client-sub-b",
            "client-b gated login must resolve to client-b's II-client sub (no clobber)"
        );
    }

    #[test]
    fn sso_stable_id_index_is_scoped_per_discovery_domain() {
        init_test_storage();
        // An II-client identity established through the org's real domain.
        let ii_client_login = sso_verification(false, "oid", "ii-client-sub", Some("oid-9"));
        store_ii_client_credential(
            resolve_ii_client_identity(&ii_client_login)
                .unwrap()
                .credential,
        );

        // A gated login carrying the same (iss, ii_client_id, stable_id) but
        // discovered through a *different* domain must not resolve to that
        // identity — the domain is part of the index key, so cross-domain
        // injection into a foreign namespace fails safe.
        let mut gated_other_domain = sso_verification(true, "oid", "pairwise", Some("oid-9"));
        gated_other_domain.credential.sso_domain = Some("attacker.example".to_string());
        assert!(matches!(
            resolve_ii_client_identity(&gated_other_domain),
            Err(OpenIdDelegationError::NoSuchAnchor)
        ));

        // The same login through the correct domain still resolves.
        let gated_same_domain = sso_verification(true, "oid", "pairwise", Some("oid-9"));
        assert_eq!(
            resolve_ii_client_identity(&gated_same_domain)
                .unwrap()
                .credential
                .sub,
            "ii-client-sub"
        );
    }
}
