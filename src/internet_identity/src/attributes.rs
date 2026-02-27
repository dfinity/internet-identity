use crate::{
    openid::{OpenIdCredential, OpenIdCredentialKey},
    state,
    storage::{account::Account, anchor::Anchor},
    update_root_hash, MINUTE_NS,
};
use ic_canister_sig_creation::signature_map::{CanisterSigInputs, SignatureMap};
use ic_representation_independent_hash::{representation_independent_hash, Value};
use internet_identity_interface::internet_identity::types::{
    attributes::{
        Attribute, AttributeKey, AttributeName, AttributeScope, CertifiedAttribute,
        CertifiedAttributes,
    },
    Timestamp,
};
use std::collections::{BTreeMap, BTreeSet};

/// Domain separator for attribute certification signatures. Clients need this to verify signatures.
const ATTRIBUTES_CERTIFICATION_DOMAIN: &[u8] = b"ii-request-attribute";

/// Duration for which attribute certifications are valid. Does not strictly need to be the same
/// as `OPENID_SESSION_DURATION_NS`, but for simplicity we keep them aligned for now.
const ATTRIBUTES_CERTIFICATION_SESSION_DURATION_NS: u64 = 30 * MINUTE_NS;

fn expiration_timestamp_ns(issued_at_timestamp_ns: Timestamp) -> Timestamp {
    issued_at_timestamp_ns.saturating_add(ATTRIBUTES_CERTIFICATION_SESSION_DURATION_NS)
}

impl Anchor {
    pub fn get_attributes(
        &self,
        mut attributes: BTreeMap<Option<AttributeScope>, BTreeSet<Attribute>>,
        account: Account,
        issued_at_timestamp_ns: Timestamp,
    ) -> CertifiedAttributes {
        let expires_at_timestamp_ns = expiration_timestamp_ns(issued_at_timestamp_ns);
        let seed = account.calculate_seed();
        let domain = ATTRIBUTES_CERTIFICATION_DOMAIN;

        // Process scope `openid` ...
        let openid_certified_attributes = state::assets_and_signatures(|certified_assets, sigs| {
            self.openid_credentials
                .iter()
                .flat_map(|openid_credential| {
                    let scope = Some(AttributeScope::OpenId {
                        issuer: openid_credential.iss.clone(),
                    });

                    let Some(attributes_to_fetch) = attributes.remove(&scope) else {
                        return BTreeSet::new();
                    };

                    attributes_to_fetch
                        .into_iter()
                        .filter_map(|attribute| {
                            let message =
                                attribute_signature_msg(&attribute, expires_at_timestamp_ns);

                            let inputs = CanisterSigInputs {
                                domain,
                                seed: &seed,
                                message: &message,
                            };

                            let signature = sigs
                                .get_signature_as_cbor(&inputs, Some(certified_assets.root_hash()))
                                .ok()?;

                            let Attribute { key, value } = attribute;

                            Some(CertifiedAttribute {
                                key: key.to_string(),
                                value,
                                signature,
                            })
                        })
                        .collect::<BTreeSet<_>>()
                })
                .collect()
        });

        CertifiedAttributes {
            certified_attributes: openid_certified_attributes,
            expires_at_timestamp_ns,
        }
    }

    /// Processes `requested_attributes` for all attribute scopes, prepares signatures for
    /// the (attribute_key, attribute_value) pairs that can be fulfilled for this (anchor, account).
    ///
    /// Returns the list of attribute keys certified with expiry `now_timestamp_ns + ATTRIBUTES_CERTIFICATION_SESSION_DURATION_NS`.
    pub fn prepare_attributes(
        &self,
        mut requested_attributes: BTreeMap<Option<AttributeScope>, BTreeSet<AttributeName>>,
        account: Account,
        now_timestamp_ns: Timestamp,
    ) -> Vec<Attribute> {
        let mut attributes = Vec::new();

        // Process scope `openid` ...
        let mut openid_attributes_to_certify =
            self.prepare_openid_attributes(&mut requested_attributes);

        for openid_credential in &self.openid_credentials {
            let Some(new_attributes) =
                openid_attributes_to_certify.remove(&openid_credential.key())
            else {
                continue;
            };

            openid_credential.prepare_attributes_no_root_hash_update(
                &account,
                &new_attributes,
                now_timestamp_ns,
            );

            attributes.extend(new_attributes.into_iter());
        }

        update_root_hash();

        attributes
    }

    /// Returns `(attribute_key, attribute_value)` pairs for attributes from `requested_attributes`
    /// for which we have values in our OpenID credentials' JWT.
    ///
    /// `requested_attributes` is mutated to remove the attributes for which values were found.
    fn prepare_openid_attributes(
        &self,
        requested_attributes: &mut BTreeMap<Option<AttributeScope>, BTreeSet<AttributeName>>,
    ) -> BTreeMap<OpenIdCredentialKey, Vec<Attribute>> {
        self.openid_credentials
            .iter()
            .filter_map(|openid_credential| {
                // E.g., `openid:https://accounts.google.com`
                let scope = Some(AttributeScope::OpenId {
                    issuer: openid_credential.config_issuer()?,
                });

                // Why we do not include `sub` / `aud` in the map lookup key:
                // --------------------------------------------------------------
                // Currently, an anchor can only have a single iss linked once. The storage layer
                // allows for duplicate iss, but the implementation has been restricted to enforce
                // the one-to-one relationship.
                let attributes = requested_attributes
                    .remove(&scope)?
                    .into_iter()
                    .filter_map(|attribute_name| {
                        use AttributeName::*;

                        let value = match attribute_name {
                            Name => openid_credential.get_name()?,
                            Email => openid_credential.get_email()?,
                            VerifiedEmail => openid_credential.get_verified_email()?,
                        };

                        let key = AttributeKey {
                            scope: scope.clone(),
                            attribute_name,
                        };
                        let value = value.into_bytes();

                        Some(Attribute { key, value })
                    })
                    .collect::<Vec<_>>();

                Some((openid_credential.key(), attributes))
            })
            .collect()
    }
}

impl OpenIdCredential {
    /// Does not ensure the salt is initialized.
    ///
    /// Does not update the root hash, intended to be used in contexts where multiple.
    fn prepare_attributes_no_root_hash_update(
        &self,
        account: &Account,
        attributes: &Vec<Attribute>,
        now_timestamp_ns: Timestamp,
    ) {
        let expiration_timestamp_ns =
            now_timestamp_ns.saturating_add(ATTRIBUTES_CERTIFICATION_SESSION_DURATION_NS);

        let seed = account.calculate_seed();

        state::signature_map_mut(|sigs| {
            for attribute in attributes {
                let message = attribute_signature_msg(attribute, expiration_timestamp_ns);
                add_attribute_signature(sigs, &seed, &message);
            }
        });
    }
}

// TODO: This should eventually be moved to a library function.
fn attribute_signature_msg(attribute: &Attribute, expiration_timestamp_ns: u64) -> Vec<u8> {
    let m: Vec<(String, Value)> = vec![
        ("expiration".into(), Value::Number(expiration_timestamp_ns)),
        (
            attribute.key.to_string(),
            Value::Bytes(attribute.value.clone()),
        ),
    ];
    representation_independent_hash(m.as_slice()).to_vec()
}

fn add_attribute_signature(sigs: &mut SignatureMap, seed: &[u8], message: &[u8]) {
    let domain = ATTRIBUTES_CERTIFICATION_DOMAIN;

    let inputs = CanisterSigInputs {
        domain,
        seed,
        message,
    };
    sigs.add_signature(&inputs);
}

#[cfg(test)]
mod tests {
    use super::*;
    use internet_identity_interface::internet_identity::types::MetadataEntryV2;
    use pretty_assertions::{assert_eq as pretty_assert_eq, assert_ne as pretty_assert_ne};
    use std::collections::{BTreeMap, BTreeSet, HashMap};

    const ISSUER: &str = "https://issuer.example";
    const SUBJECT: &str = "user-123";
    const ANCHOR_NUMBER: u64 = 4242;

    fn openid_attribute_scope() -> Option<AttributeScope> {
        Some(AttributeScope::OpenId {
            issuer: ISSUER.to_string(),
        })
    }

    fn anchor_with_credential(metadata: HashMap<String, MetadataEntryV2>) -> Anchor {
        let mut anchor = Anchor::new(ANCHOR_NUMBER, 0);
        anchor.openid_credentials = vec![OpenIdCredential {
            iss: ISSUER.to_string(),
            sub: SUBJECT.to_string(),
            aud: "https://audience.example".to_string(),
            last_usage_timestamp: None,
            metadata,
        }];
        anchor
    }

    fn anchor_with_multiple_credentials(
        credentials: Vec<(String, String, HashMap<String, MetadataEntryV2>)>,
    ) -> Anchor {
        let mut anchor = Anchor::new(ANCHOR_NUMBER, 0);
        anchor.openid_credentials = credentials
            .into_iter()
            .map(|(iss, sub, metadata)| OpenIdCredential {
                iss,
                sub,
                aud: "https://audience.example".to_string(),
                last_usage_timestamp: None,
                metadata,
            })
            .collect();
        anchor
    }

    fn requested_attribute_map(
        names: &[AttributeName],
    ) -> BTreeMap<Option<AttributeScope>, BTreeSet<AttributeName>> {
        let mut requested = BTreeMap::new();
        requested.insert(openid_attribute_scope(), names.iter().copied().collect());
        requested
    }

    fn requested_attributes_multi_scope(
        scopes: Vec<(Option<AttributeScope>, Vec<AttributeName>)>,
    ) -> BTreeMap<Option<AttributeScope>, BTreeSet<AttributeName>> {
        scopes
            .into_iter()
            .map(|(scope, names)| (scope, names.into_iter().collect()))
            .collect()
    }

    fn attribute_pairs(attributes: &[Attribute]) -> BTreeSet<(String, String)> {
        attributes
            .iter()
            .map(|attr| {
                (
                    attr.key.to_string(),
                    String::from_utf8_lossy(&attr.value).to_string(),
                )
            })
            .collect()
    }

    fn sample_attribute(name: AttributeName, value: &str) -> Attribute {
        Attribute {
            key: AttributeKey {
                scope: openid_attribute_scope(),
                attribute_name: name,
            },
            value: value.as_bytes().to_vec(),
        }
    }

    mod expiration_timestamp_ns_tests {
        use super::*;

        #[test]
        fn test_expiration_calculation() {
            let test_cases = vec![
                (
                    "adds session duration to normal timestamp",
                    5_000,
                    5_000 + ATTRIBUTES_CERTIFICATION_SESSION_DURATION_NS,
                ),
                ("saturates on u64::MAX", u64::MAX, u64::MAX),
                (
                    "handles zero timestamp",
                    0,
                    ATTRIBUTES_CERTIFICATION_SESSION_DURATION_NS,
                ),
                ("saturates near max value", u64::MAX - 100, u64::MAX),
                (
                    "handles typical timestamp",
                    1_000_000_000,
                    1_000_000_000 + ATTRIBUTES_CERTIFICATION_SESSION_DURATION_NS,
                ),
            ];

            for (label, input, expected) in test_cases {
                let result = expiration_timestamp_ns(input);
                pretty_assert_eq!(result, expected, "Failed case: {}", label);
            }
        }
    }

    mod attribute_signature_msg_tests {
        use super::*;

        #[test]
        fn test_signature_message_generation() {
            let attr_email = sample_attribute(AttributeName::Email, "user@example.com");
            let attr_name = sample_attribute(AttributeName::Name, "value");
            let attr_empty = sample_attribute(AttributeName::Email, "");
            let attr_long = sample_attribute(AttributeName::Email, &"x".repeat(10_000));

            // Test consistency
            let msg1 = attribute_signature_msg(&attr_email, 1_234_567);
            let msg2 = attribute_signature_msg(&attr_email, 1_234_567);
            pretty_assert_eq!(
                msg1,
                msg2,
                "Failed case: produces consistent hash for same inputs"
            );

            // Test representation independent hash format
            let message = attribute_signature_msg(&attr_email, 1_234_567);
            let expected = representation_independent_hash(&[
                ("expiration".into(), Value::Number(1_234_567)),
                (
                    attr_email.key.to_string(),
                    Value::Bytes(attr_email.value.clone()),
                ),
            ])
            .to_vec();
            pretty_assert_eq!(
                message,
                expected,
                "Failed case: matches representation independent hash format"
            );

            // Test cases for different outputs
            let difference_test_cases = vec![
                (
                    "different expirations",
                    attribute_signature_msg(&attr_email, 10),
                    attribute_signature_msg(&attr_email, 11),
                ),
                (
                    "different attribute values",
                    attribute_signature_msg(
                        &sample_attribute(AttributeName::Email, "user1@example.com"),
                        1_000,
                    ),
                    attribute_signature_msg(
                        &sample_attribute(AttributeName::Email, "user2@example.com"),
                        1_000,
                    ),
                ),
                (
                    "different attribute names",
                    attribute_signature_msg(&attr_email, 1_000),
                    attribute_signature_msg(&attr_name, 1_000),
                ),
            ];

            for (label, msg1, msg2) in difference_test_cases {
                pretty_assert_ne!(msg1, msg2, "Failed case: {}", label);
            }

            // Test edge cases produce non-empty messages
            let edge_case_tests = vec![
                (
                    "empty attribute value",
                    attribute_signature_msg(&attr_empty, 1_000),
                ),
                (
                    "long attribute value",
                    attribute_signature_msg(&attr_long, 1_000),
                ),
            ];

            for (label, message) in edge_case_tests {
                pretty_assert_ne!(message.len(), 0, "Failed case: {}", label);
            }

            // Test boundary expirations
            let zero_exp = attribute_signature_msg(&attr_email, 0);
            let expected_zero = representation_independent_hash(&[
                ("expiration".into(), Value::Number(0)),
                (
                    attr_email.key.to_string(),
                    Value::Bytes(attr_email.value.clone()),
                ),
            ])
            .to_vec();
            pretty_assert_eq!(
                zero_exp,
                expected_zero,
                "Failed case: handles zero expiration"
            );

            let max_exp = attribute_signature_msg(&attr_email, u64::MAX);
            let expected_max = representation_independent_hash(&[
                ("expiration".into(), Value::Number(u64::MAX)),
                (
                    attr_email.key.to_string(),
                    Value::Bytes(attr_email.value.clone()),
                ),
            ])
            .to_vec();
            pretty_assert_eq!(max_exp, expected_max, "Failed case: handles max expiration");
        }
    }

    mod prepare_openid_attributes_tests {
        use super::*;

        #[test]
        fn test_attribute_extraction_scenarios() {
            // Test case: (label, metadata, requested_attrs, expected_attr_count, expected_pairs_opt)
            #[allow(clippy::type_complexity)]
            let test_cases: Vec<(
                &str,
                HashMap<String, MetadataEntryV2>,
                Vec<AttributeName>,
                usize,
                Option<BTreeSet<(String, String)>>,
            )> = vec![
                (
                    "returns requested values that exist",
                    {
                        let mut m = HashMap::new();
                        m.insert(
                            "email".to_string(),
                            MetadataEntryV2::String("user@example.com".to_string()),
                        );
                        m.insert(
                            "name".to_string(),
                            MetadataEntryV2::String("Example User".to_string()),
                        );
                        m
                    },
                    vec![AttributeName::Email, AttributeName::Name],
                    2,
                    Some(BTreeSet::from([
                        (
                            format!("openid:{ISSUER}:email"),
                            "user@example.com".to_string(),
                        ),
                        (format!("openid:{ISSUER}:name"), "Example User".to_string()),
                    ])),
                ),
                (
                    "ignores requested attributes not in metadata",
                    {
                        let mut m = HashMap::new();
                        m.insert(
                            "email".to_string(),
                            MetadataEntryV2::String("user@example.com".to_string()),
                        );
                        m
                    },
                    vec![AttributeName::Email, AttributeName::Name],
                    1,
                    Some(BTreeSet::from([(
                        format!("openid:{ISSUER}:email"),
                        "user@example.com".to_string(),
                    )])),
                ),
                (
                    "skips non-string metadata values",
                    {
                        let mut m = HashMap::new();
                        m.insert(
                            "email".to_string(),
                            MetadataEntryV2::Bytes(vec![9, 9, 9].into()),
                        );
                        m
                    },
                    vec![AttributeName::Email],
                    0,
                    None,
                ),
                (
                    "returns empty when metadata is empty",
                    HashMap::new(),
                    vec![AttributeName::Email],
                    0,
                    None,
                ),
                (
                    "skips metadata with unknown attribute names",
                    {
                        let mut m = HashMap::new();
                        m.insert(
                            "unknown_field".to_string(),
                            MetadataEntryV2::String("value".to_string()),
                        );
                        m.insert(
                            "email".to_string(),
                            MetadataEntryV2::String("user@example.com".to_string()),
                        );
                        m
                    },
                    vec![AttributeName::Email],
                    1,
                    Some(BTreeSet::from([(
                        format!("openid:{ISSUER}:email"),
                        "user@example.com".to_string(),
                    )])),
                ),
            ];

            for (label, metadata, requested_attrs, _expected_count, _expected_pairs) in test_cases {
                let anchor = anchor_with_credential(metadata);
                let mut requested = requested_attribute_map(&requested_attrs);
                let result = anchor.prepare_openid_attributes(&mut requested);
                // ISSUER has no configured provider, so config_issuer() returns None
                // and the credential is skipped → requested is NOT drained.
                assert!(
                    !requested.is_empty(),
                    "Failed case: {} - requested should not be drained for unknown issuer",
                    label
                );
                pretty_assert_eq!(
                    result.len(),
                    0,
                    "Failed case: {} - unknown issuer yields no results",
                    label
                );
            }
        }

        #[test]
        fn test_empty_and_mismatch_scenarios() {
            // Returns empty when no requested attributes
            let mut metadata = HashMap::new();
            metadata.insert(
                "email".to_string(),
                MetadataEntryV2::String("user@example.com".to_string()),
            );
            let anchor = anchor_with_credential(metadata.clone());
            let mut empty_requested = BTreeMap::new();
            let result = anchor.prepare_openid_attributes(&mut empty_requested);
            pretty_assert_eq!(
                result.len(),
                0,
                "Failed case: returns empty when no requested attributes"
            );
            assert!(empty_requested.is_empty());

            // Returns none when scope does not match
            let anchor = anchor_with_credential(metadata);
            let mut requested = BTreeMap::new();
            requested.insert(
                Some(AttributeScope::OpenId {
                    issuer: "https://different-issuer.com".to_string(),
                }),
                BTreeSet::from([AttributeName::Email]),
            );
            let result = anchor.prepare_openid_attributes(&mut requested);
            pretty_assert_eq!(
                result.len(),
                0,
                "Failed case: returns none when scope does not match"
            );
            assert!(
                !requested.is_empty(),
                "requested should NOT be drained when no credential scope matches"
            );
        }

        #[test]
        fn test_multiple_credentials() {
            let mut google_metadata = HashMap::new();
            google_metadata.insert(
                "email".to_string(),
                MetadataEntryV2::String("google@example.com".to_string()),
            );

            let mut github_metadata = HashMap::new();
            github_metadata.insert(
                "email".to_string(),
                MetadataEntryV2::String("github@example.com".to_string()),
            );

            let anchor = anchor_with_multiple_credentials(vec![
                (
                    "https://google.com".to_string(),
                    "google-user".to_string(),
                    google_metadata,
                ),
                (
                    "https://github.com".to_string(),
                    "github-user".to_string(),
                    github_metadata,
                ),
            ]);

            let requested = requested_attributes_multi_scope(vec![
                (
                    Some(AttributeScope::OpenId {
                        issuer: "https://google.com".to_string(),
                    }),
                    vec![AttributeName::Email],
                ),
                (
                    Some(AttributeScope::OpenId {
                        issuer: "https://github.com".to_string(),
                    }),
                    vec![AttributeName::Email],
                ),
            ]);

            let mut requested = requested;
            let result = anchor.prepare_openid_attributes(&mut requested);
            // Fake issuers have no configured provider → credentials skipped
            pretty_assert_eq!(result.len(), 0, "Failed case: no results without providers");
            assert!(
                !requested.is_empty(),
                "requested should NOT be drained for unknown issuers"
            );
        }
    }

    mod verified_email_tests {
        use super::*;
        use internet_identity_interface::internet_identity::types::{
            OpenIdConfig, OpenIdEmailVerificationScheme,
        };

        const GOOGLE_ISSUER: &str = "https://accounts.google.com";
        const MICROSOFT_ISSUER_TEMPLATE: &str = "https://login.microsoftonline.com/{tid}/v2.0";
        const MICROSOFT_PERSONAL_TID: &str = "9188040d-6c67-4c5b-b112-36a304b66dad";
        const MICROSOFT_RESOLVED_ISSUER: &str =
            "https://login.microsoftonline.com/9188040d-6c67-4c5b-b112-36a304b66dad/v2.0";

        fn setup_providers() {
            crate::openid::setup(vec![
                OpenIdConfig {
                    name: "Google".to_string(),
                    logo: String::new(),
                    issuer: GOOGLE_ISSUER.to_string(),
                    client_id: "test-client-id".to_string(),
                    jwks_uri: String::new(),
                    auth_uri: String::new(),
                    auth_scope: vec![],
                    fedcm_uri: None,
                    email_verification: Some(OpenIdEmailVerificationScheme::Google),
                },
                OpenIdConfig {
                    name: "Microsoft".to_string(),
                    logo: String::new(),
                    issuer: MICROSOFT_ISSUER_TEMPLATE.to_string(),
                    client_id: "test-client-id".to_string(),
                    jwks_uri: String::new(),
                    auth_uri: String::new(),
                    auth_scope: vec![],
                    fedcm_uri: None,
                    email_verification: Some(OpenIdEmailVerificationScheme::Microsoft),
                },
            ]);
        }

        fn google_credential_with(metadata: HashMap<String, MetadataEntryV2>) -> OpenIdCredential {
            OpenIdCredential {
                iss: GOOGLE_ISSUER.to_string(),
                sub: "google-user-123".to_string(),
                aud: "test-client-id".to_string(),
                last_usage_timestamp: None,
                metadata,
            }
        }

        fn microsoft_credential_with(
            metadata: HashMap<String, MetadataEntryV2>,
        ) -> OpenIdCredential {
            OpenIdCredential {
                iss: MICROSOFT_RESOLVED_ISSUER.to_string(),
                sub: "ms-user-456".to_string(),
                aud: "test-client-id".to_string(),
                last_usage_timestamp: None,
                metadata,
            }
        }

        fn anchor_with_openid_credentials(credentials: Vec<OpenIdCredential>) -> Anchor {
            let mut anchor = Anchor::new(ANCHOR_NUMBER, 0);
            anchor.openid_credentials = credentials;
            anchor
        }

        fn request_verified_email_for(
            scope: Option<AttributeScope>,
        ) -> BTreeMap<Option<AttributeScope>, BTreeSet<AttributeName>> {
            BTreeMap::from([(scope, BTreeSet::from([AttributeName::VerifiedEmail]))])
        }

        fn google_scope() -> Option<AttributeScope> {
            Some(AttributeScope::OpenId {
                issuer: GOOGLE_ISSUER.to_string(),
            })
        }

        fn microsoft_scope() -> Option<AttributeScope> {
            Some(AttributeScope::OpenId {
                issuer: MICROSOFT_ISSUER_TEMPLATE.to_string(),
            })
        }

        // ── Google verified email tests ──────────────────────────────────

        #[test]
        fn test_google_returns_verified_email_when_email_verified_is_true() {
            setup_providers();

            let metadata = HashMap::from([
                (
                    "email".to_string(),
                    MetadataEntryV2::String("user@gmail.com".to_string()),
                ),
                (
                    "email_verified".to_string(),
                    MetadataEntryV2::String("true".to_string()),
                ),
            ]);
            let anchor = anchor_with_openid_credentials(vec![google_credential_with(metadata)]);
            let mut requested = request_verified_email_for(google_scope());
            let result = anchor.prepare_openid_attributes(&mut requested);
            assert!(requested.is_empty());

            let attrs = result
                .get(&(GOOGLE_ISSUER.to_string(), "google-user-123".to_string()))
                .expect("google verified email attributes should be present");

            pretty_assert_eq!(attrs.len(), 1);
            pretty_assert_eq!(
                attribute_pairs(attrs),
                BTreeSet::from([(
                    format!("openid:{GOOGLE_ISSUER}:verified_email"),
                    "user@gmail.com".to_string(),
                )])
            );
        }

        #[test]
        fn test_google_returns_nothing_when_email_verified_is_false() {
            setup_providers();

            let metadata = HashMap::from([
                (
                    "email".to_string(),
                    MetadataEntryV2::String("user@gmail.com".to_string()),
                ),
                (
                    "email_verified".to_string(),
                    MetadataEntryV2::String("false".to_string()),
                ),
            ]);
            let anchor = anchor_with_openid_credentials(vec![google_credential_with(metadata)]);
            let mut requested = request_verified_email_for(google_scope());
            let result = anchor.prepare_openid_attributes(&mut requested);
            assert!(requested.is_empty());

            let attrs = result
                .get(&(GOOGLE_ISSUER.to_string(), "google-user-123".to_string()))
                .expect("credential key should be present");
            pretty_assert_eq!(
                attrs.len(),
                0,
                "no verified email when email_verified is false"
            );
        }

        #[test]
        fn test_google_returns_nothing_when_email_verified_is_missing() {
            setup_providers();

            let metadata = HashMap::from([(
                "email".to_string(),
                MetadataEntryV2::String("user@gmail.com".to_string()),
            )]);
            let anchor = anchor_with_openid_credentials(vec![google_credential_with(metadata)]);
            let mut requested = request_verified_email_for(google_scope());
            let result = anchor.prepare_openid_attributes(&mut requested);
            assert!(requested.is_empty());

            let attrs = result
                .get(&(GOOGLE_ISSUER.to_string(), "google-user-123".to_string()))
                .expect("credential key should be present");
            pretty_assert_eq!(
                attrs.len(),
                0,
                "no verified email when email_verified metadata is absent"
            );
        }

        #[test]
        fn test_google_returns_nothing_when_email_is_missing() {
            setup_providers();

            let metadata = HashMap::from([(
                "email_verified".to_string(),
                MetadataEntryV2::String("true".to_string()),
            )]);
            let anchor = anchor_with_openid_credentials(vec![google_credential_with(metadata)]);
            let mut requested = request_verified_email_for(google_scope());
            let result = anchor.prepare_openid_attributes(&mut requested);
            assert!(requested.is_empty());

            let attrs = result
                .get(&(GOOGLE_ISSUER.to_string(), "google-user-123".to_string()))
                .expect("credential key should be present");
            pretty_assert_eq!(
                attrs.len(),
                0,
                "no verified email when email metadata is absent"
            );
        }

        #[test]
        fn test_google_email_verified_check_is_case_insensitive() {
            setup_providers();

            // "True" (capitalized) should pass the case-insensitive check
            let metadata = HashMap::from([
                (
                    "email".to_string(),
                    MetadataEntryV2::String("user@gmail.com".to_string()),
                ),
                (
                    "email_verified".to_string(),
                    MetadataEntryV2::String("True".to_string()),
                ),
            ]);
            let anchor = anchor_with_openid_credentials(vec![google_credential_with(metadata)]);
            let mut requested = request_verified_email_for(google_scope());
            let result = anchor.prepare_openid_attributes(&mut requested);
            assert!(requested.is_empty());

            let attrs = result
                .get(&(GOOGLE_ISSUER.to_string(), "google-user-123".to_string()))
                .expect("credential key should be present");
            pretty_assert_eq!(
                attrs.len(),
                1,
                "email_verified check should be case-insensitive"
            );
            pretty_assert_eq!(
                attribute_pairs(attrs),
                BTreeSet::from([(
                    format!("openid:{GOOGLE_ISSUER}:verified_email"),
                    "user@gmail.com".to_string(),
                )])
            );
        }

        #[test]
        fn test_google_returns_nothing_when_email_verified_is_non_string() {
            setup_providers();

            let metadata = HashMap::from([
                (
                    "email".to_string(),
                    MetadataEntryV2::String("user@gmail.com".to_string()),
                ),
                (
                    "email_verified".to_string(),
                    MetadataEntryV2::Bytes(b"true".to_vec().into()),
                ),
            ]);
            let anchor = anchor_with_openid_credentials(vec![google_credential_with(metadata)]);
            let mut requested = request_verified_email_for(google_scope());
            let result = anchor.prepare_openid_attributes(&mut requested);
            assert!(requested.is_empty());

            let attrs = result
                .get(&(GOOGLE_ISSUER.to_string(), "google-user-123".to_string()))
                .expect("credential key should be present");
            pretty_assert_eq!(
                attrs.len(),
                0,
                "no verified email when email_verified is stored as bytes"
            );
        }

        // ── Microsoft verified email tests ───────────────────────────────

        #[test]
        fn test_microsoft_returns_verified_email_with_personal_tenant() {
            setup_providers();

            let metadata = HashMap::from([
                (
                    "email".to_string(),
                    MetadataEntryV2::String("user@outlook.com".to_string()),
                ),
                (
                    "tid".to_string(),
                    MetadataEntryV2::String(MICROSOFT_PERSONAL_TID.to_string()),
                ),
            ]);
            let anchor = anchor_with_openid_credentials(vec![microsoft_credential_with(metadata)]);
            let mut requested = request_verified_email_for(microsoft_scope());
            let result = anchor.prepare_openid_attributes(&mut requested);
            assert!(requested.is_empty());

            let attrs = result
                .get(&(
                    MICROSOFT_RESOLVED_ISSUER.to_string(),
                    "ms-user-456".to_string(),
                ))
                .expect("microsoft verified email attributes should be present");

            pretty_assert_eq!(attrs.len(), 1);
            pretty_assert_eq!(
                attribute_pairs(attrs),
                BTreeSet::from([(
                    format!("openid:{MICROSOFT_ISSUER_TEMPLATE}:verified_email"),
                    "user@outlook.com".to_string(),
                )])
            );
        }

        #[test]
        fn test_microsoft_returns_nothing_with_non_personal_tenant() {
            setup_providers();

            let enterprise_tid = "164d0422-a01d-41d5-945a-37456ea80dbb";
            let enterprise_iss = format!("https://login.microsoftonline.com/{enterprise_tid}/v2.0");

            let metadata = HashMap::from([
                (
                    "email".to_string(),
                    MetadataEntryV2::String("user@contoso.com".to_string()),
                ),
                (
                    "tid".to_string(),
                    MetadataEntryV2::String(enterprise_tid.to_string()),
                ),
            ]);
            let credential = OpenIdCredential {
                iss: enterprise_iss.clone(),
                sub: "enterprise-user".to_string(),
                aud: "test-client-id".to_string(),
                last_usage_timestamp: None,
                metadata,
            };
            let anchor = anchor_with_openid_credentials(vec![credential]);
            let mut requested = request_verified_email_for(microsoft_scope());
            let result = anchor.prepare_openid_attributes(&mut requested);
            assert!(requested.is_empty());

            let attrs = result
                .get(&(enterprise_iss, "enterprise-user".to_string()))
                .expect("credential key should be present");
            pretty_assert_eq!(
                attrs.len(),
                0,
                "no verified email for non-personal (enterprise) tenant"
            );
        }

        #[test]
        fn test_microsoft_returns_nothing_when_tid_is_missing() {
            setup_providers();

            // Without `tid` in metadata, the Microsoft provider won't match
            // because the {tid} placeholder can't be resolved.
            let metadata = HashMap::from([(
                "email".to_string(),
                MetadataEntryV2::String("user@outlook.com".to_string()),
            )]);
            let credential = OpenIdCredential {
                iss: MICROSOFT_RESOLVED_ISSUER.to_string(),
                sub: "ms-user-456".to_string(),
                aud: "test-client-id".to_string(),
                last_usage_timestamp: None,
                metadata,
            };
            let anchor = anchor_with_openid_credentials(vec![credential]);
            let mut requested = request_verified_email_for(microsoft_scope());
            let result = anchor.prepare_openid_attributes(&mut requested);

            // config_issuer() returns None when tid is missing → credential is skipped entirely
            pretty_assert_eq!(
                result.len(),
                0,
                "credential is skipped when tid is missing (provider can't match)"
            );
            assert!(
                !requested.is_empty(),
                "requested should NOT be drained when credential is skipped"
            );
        }

        #[test]
        fn test_microsoft_returns_nothing_when_email_is_missing() {
            setup_providers();

            let metadata = HashMap::from([(
                "tid".to_string(),
                MetadataEntryV2::String(MICROSOFT_PERSONAL_TID.to_string()),
            )]);
            let anchor = anchor_with_openid_credentials(vec![microsoft_credential_with(metadata)]);
            let mut requested = request_verified_email_for(microsoft_scope());
            let result = anchor.prepare_openid_attributes(&mut requested);
            assert!(requested.is_empty());

            let attrs = result
                .get(&(
                    MICROSOFT_RESOLVED_ISSUER.to_string(),
                    "ms-user-456".to_string(),
                ))
                .expect("credential key should be present");
            pretty_assert_eq!(
                attrs.len(),
                0,
                "no verified email when email metadata is absent"
            );
        }

        // ── Combined provider tests ──────────────────────────────────────

        #[test]
        fn test_verified_email_for_both_google_and_microsoft() {
            setup_providers();

            let google_metadata = HashMap::from([
                (
                    "email".to_string(),
                    MetadataEntryV2::String("user@gmail.com".to_string()),
                ),
                (
                    "email_verified".to_string(),
                    MetadataEntryV2::String("true".to_string()),
                ),
            ]);
            let microsoft_metadata = HashMap::from([
                (
                    "email".to_string(),
                    MetadataEntryV2::String("user@outlook.com".to_string()),
                ),
                (
                    "tid".to_string(),
                    MetadataEntryV2::String(MICROSOFT_PERSONAL_TID.to_string()),
                ),
            ]);

            let anchor = anchor_with_openid_credentials(vec![
                google_credential_with(google_metadata),
                microsoft_credential_with(microsoft_metadata),
            ]);

            let mut requested = requested_attributes_multi_scope(vec![
                (google_scope(), vec![AttributeName::VerifiedEmail]),
                (microsoft_scope(), vec![AttributeName::VerifiedEmail]),
            ]);
            let result = anchor.prepare_openid_attributes(&mut requested);
            assert!(requested.is_empty());

            pretty_assert_eq!(result.len(), 2, "both credentials should be present");

            let google_attrs = result
                .get(&(GOOGLE_ISSUER.to_string(), "google-user-123".to_string()))
                .expect("google attributes");
            pretty_assert_eq!(
                attribute_pairs(google_attrs),
                BTreeSet::from([(
                    format!("openid:{GOOGLE_ISSUER}:verified_email"),
                    "user@gmail.com".to_string(),
                )])
            );

            let ms_attrs = result
                .get(&(
                    MICROSOFT_RESOLVED_ISSUER.to_string(),
                    "ms-user-456".to_string(),
                ))
                .expect("microsoft attributes");
            pretty_assert_eq!(
                attribute_pairs(ms_attrs),
                BTreeSet::from([(
                    format!("openid:{MICROSOFT_ISSUER_TEMPLATE}:verified_email"),
                    "user@outlook.com".to_string(),
                )])
            );
        }

        #[test]
        fn test_verified_email_alongside_other_attributes() {
            setup_providers();

            let metadata = HashMap::from([
                (
                    "email".to_string(),
                    MetadataEntryV2::String("user@gmail.com".to_string()),
                ),
                (
                    "email_verified".to_string(),
                    MetadataEntryV2::String("true".to_string()),
                ),
                (
                    "name".to_string(),
                    MetadataEntryV2::String("Test User".to_string()),
                ),
            ]);
            let anchor = anchor_with_openid_credentials(vec![google_credential_with(metadata)]);

            let mut requested = BTreeMap::from([(
                google_scope(),
                BTreeSet::from([
                    AttributeName::VerifiedEmail,
                    AttributeName::Email,
                    AttributeName::Name,
                ]),
            )]);
            let result = anchor.prepare_openid_attributes(&mut requested);
            assert!(requested.is_empty());

            let attrs = result
                .get(&(GOOGLE_ISSUER.to_string(), "google-user-123".to_string()))
                .expect("google attributes");

            pretty_assert_eq!(attrs.len(), 3, "all three attributes should be returned");
            let pairs = attribute_pairs(attrs);
            assert!(
                pairs.contains(&(
                    format!("openid:{GOOGLE_ISSUER}:verified_email"),
                    "user@gmail.com".to_string(),
                )),
                "should contain verified_email"
            );
            assert!(
                pairs.contains(&(
                    format!("openid:{GOOGLE_ISSUER}:email"),
                    "user@gmail.com".to_string(),
                )),
                "should contain email"
            );
            assert!(
                pairs.contains(&(
                    format!("openid:{GOOGLE_ISSUER}:name"),
                    "Test User".to_string(),
                )),
                "should contain name"
            );
        }

        #[test]
        fn test_no_verified_email_without_matching_provider() {
            // Use an issuer with no configured provider
            let unknown_issuer = "https://unknown-provider.example.com";
            let metadata = HashMap::from([
                (
                    "email".to_string(),
                    MetadataEntryV2::String("user@example.com".to_string()),
                ),
                (
                    "email_verified".to_string(),
                    MetadataEntryV2::String("true".to_string()),
                ),
            ]);
            let credential = OpenIdCredential {
                iss: unknown_issuer.to_string(),
                sub: "unknown-user".to_string(),
                aud: "test-client-id".to_string(),
                last_usage_timestamp: None,
                metadata,
            };
            let anchor = anchor_with_openid_credentials(vec![credential]);

            let scope = Some(AttributeScope::OpenId {
                issuer: unknown_issuer.to_string(),
            });
            let mut requested = request_verified_email_for(scope);
            let result = anchor.prepare_openid_attributes(&mut requested);

            // config_issuer() returns None → credential skipped
            pretty_assert_eq!(
                result.len(),
                0,
                "credential with unknown issuer is skipped entirely"
            );
            assert!(
                !requested.is_empty(),
                "requested should NOT be drained when credential is skipped"
            );
        }
    }
}
