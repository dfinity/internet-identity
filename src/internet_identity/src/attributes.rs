use crate::{
    openid::{OpenIdCredential, OpenIdCredentialKey, OPENID_SESSION_DURATION_NS},
    state,
    storage::{account::Account, anchor::Anchor},
    update_root_hash,
};
use ic_canister_sig_creation::signature_map::{CanisterSigInputs, SignatureMap};
use ic_representation_independent_hash::{representation_independent_hash, Value};
use internet_identity_interface::internet_identity::types::{
    attributes::{
        Attribute, AttributeKey, AttributeName, AttributeScope, CertifiedAttribute,
        CertifiedAttributes,
    },
    MetadataEntryV2, Timestamp,
};
use std::collections::{BTreeMap, BTreeSet};

/// Domain separator for attribute certification signatures. Clients need this to verify signatures.
const ATTRIBUTES_CERTIFICATION_DOMAIN: &[u8] = b"ii-request-attribute";

/// Duration for which attribute certifications are valid. Does not strictly need to be the same
/// as `OPENID_SESSION_DURATION_NS`, but for simplicity we keep them aligned for now.
const ATTRIBUTES_CERTIFICATION_SESSION_DURATION_NS: u64 = OPENID_SESSION_DURATION_NS;

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
        requested_attributes: BTreeMap<Option<AttributeScope>, BTreeSet<AttributeName>>,
        account: Account,
        now_timestamp_ns: Timestamp,
    ) -> Vec<Attribute> {
        let mut attributes = Vec::new();

        // Process scope `openid` ...
        let mut openid_attributes_to_certify = self.prepare_openid_attributes(requested_attributes);

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
        mut requested_attributes: BTreeMap<Option<AttributeScope>, BTreeSet<AttributeName>>,
    ) -> BTreeMap<OpenIdCredentialKey, Vec<Attribute>> {
        self.openid_credentials
            .iter()
            .filter_map(|openid_credential| {
                // E.g., `openid:google.com`
                let scope = Some(AttributeScope::OpenId {
                    issuer: openid_credential.iss.clone(),
                });
                // E.g., {`email`, `name`}
                //
                // Why we do not include `sub` / `aud` into the keys of this map:
                // --------------------------------------------------------------
                // Currently, an anchor can only have a single iss linked once. The storage layer
                // allows for duplicate iss, but the implementation has been restricted to enforce
                // the one-to-one relationship.
                let attribute_names = requested_attributes.remove(&scope)?;

                let mut attribute_keys: BTreeMap<AttributeName, AttributeKey> = attribute_names
                    .into_iter()
                    .map(|attribute_name| {
                        (
                            attribute_name,
                            AttributeKey {
                                scope: scope.clone(),
                                attribute_name,
                            },
                        )
                    })
                    .collect();

                let attributes = openid_credential
                    .metadata
                    .iter()
                    .filter_map(|(attribute_name_str, attribute_value)| {
                        // If the attribute exists in the OpenID JWT metadata, but isn't listed as
                        // an explicit case in `AttributeName`, it cannot be certified; we skip it.
                        let attribute_name =
                            AttributeName::try_from(attribute_name_str.as_str()).ok()?;

                        let key = attribute_keys.remove(&attribute_name)?;

                        let MetadataEntryV2::String(value) = attribute_value else {
                            return None;
                        };

                        let attribute = Attribute {
                            key,
                            value: value.as_bytes().to_vec(),
                        };

                        Some(attribute)
                    })
                    .collect::<Vec<Attribute>>();

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

            for (label, metadata, requested_attrs, expected_count, expected_pairs) in test_cases {
                let anchor = anchor_with_credential(metadata);
                let requested = requested_attribute_map(&requested_attrs);
                let result = anchor.prepare_openid_attributes(requested);

                if expected_count == 0 && expected_pairs.is_none() {
                    if let Some(attributes) = result.get(&(ISSUER.to_string(), SUBJECT.to_string()))
                    {
                        pretty_assert_eq!(attributes.len(), 0, "Failed case: {}", label);
                    }
                } else {
                    let attributes = result
                        .get(&(ISSUER.to_string(), SUBJECT.to_string()))
                        .unwrap_or_else(|| panic!("Failed case: {} - missing attributes", label));
                    pretty_assert_eq!(
                        attributes.len(),
                        expected_count,
                        "Failed case: {} - count",
                        label
                    );

                    if let Some(expected) = expected_pairs {
                        let actual = attribute_pairs(attributes);
                        pretty_assert_eq!(actual, expected, "Failed case: {}", label);
                    }
                }
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
            let result = anchor.prepare_openid_attributes(BTreeMap::new());
            pretty_assert_eq!(
                result.len(),
                0,
                "Failed case: returns empty when no requested attributes"
            );

            // Returns none when scope does not match
            let anchor = anchor_with_credential(metadata);
            let mut requested = BTreeMap::new();
            requested.insert(
                Some(AttributeScope::OpenId {
                    issuer: "https://different-issuer.com".to_string(),
                }),
                BTreeSet::from([AttributeName::Email]),
            );
            let result = anchor.prepare_openid_attributes(requested);
            pretty_assert_eq!(
                result.len(),
                0,
                "Failed case: returns none when scope does not match"
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

            let result = anchor.prepare_openid_attributes(requested);
            pretty_assert_eq!(result.len(), 2, "Failed case: handles multiple credentials");

            let google_attrs = result
                .get(&("https://google.com".to_string(), "google-user".to_string()))
                .expect("google attributes");
            pretty_assert_eq!(
                google_attrs.len(),
                1,
                "Failed case: google credential count"
            );

            let github_attrs = result
                .get(&("https://github.com".to_string(), "github-user".to_string()))
                .expect("github attributes");
            pretty_assert_eq!(
                github_attrs.len(),
                1,
                "Failed case: github credential count"
            );
        }
    }
}
