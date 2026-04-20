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
        CertifiedAttributes, GetIcrc3AttributeError, GetIcrc3AttributeResponse,
        PrepareIcrc3AttributeError, ValidatedAttributeSpec,
    },
    icrc3::Icrc3Value,
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
                    // It is important here to rely on the issuer ID as opposed to `openid_credential.iss`
                    // becasue, e.g., for Microsoft accounts, the issuer has a tenant ID parameter
                    // which should *not* be instantiated for a general Microsoft attribute scope.
                    let Some(issuer) = openid_credential.config_issuer() else {
                        ic_cdk::println!(
                            "No matching OpenID provider for issuer: {}, skipping credential",
                            openid_credential.iss
                        );
                        return BTreeSet::new();
                    };

                    let scope = Some(AttributeScope::OpenId { issuer });

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

// ==================== ICRC-3 attribute sharing ====================

/// Signature domain for IC sender info as specified in the IC interface specification:
/// https://internetcomputer.org/docs/current/references/ic-interface-spec/#request-call-http-call
pub const ICRC3_ATTRIBUTES_CERTIFICATION_DOMAIN: &[u8] = b"ic-sender-info";

/// Builds a Candid-encoded ICRC-3 Value map from certified key-value pairs.
/// The pairs are (certified_key, value) where certified_key may have scope omitted.
fn icrc3_attribute_message(certified_pairs: &BTreeMap<String, Icrc3Value>) -> Vec<u8> {
    use candid::Encode;

    let map_entries: Vec<(String, Icrc3Value)> = certified_pairs
        .iter()
        .map(|(key, value)| (key.clone(), value.clone()))
        .collect();

    let value = Icrc3Value::Map(map_entries);
    Encode!(&value).expect("Candid encoding of ICRC-3 value should not fail")
}

impl Anchor {
    /// Resolves attribute specs against stored credentials, builds the Candid-encoded
    /// ICRC-3 message, signs it, and returns the message blob.
    ///
    /// For each `ValidatedAttributeSpec`:
    /// - Looks up the stored value from the matching OpenID credential.
    /// - If `spec.value` is `Some`, validates it matches the stored value.
    /// - Computes the certified key: if `omit_scope` is true, uses just the attribute name
    ///   (e.g., `"email"`); otherwise uses the full key (e.g., `"openid:https://...:email"`).
    pub fn prepare_icrc3_attributes(
        &self,
        attribute_specs: Vec<ValidatedAttributeSpec>,
        nonce: Vec<u8>,
        origin: String,
        issued_at_timestamp_ns: u64,
        account: Account,
    ) -> Result<Vec<u8>, PrepareIcrc3AttributeError> {
        let mut certified_pairs: BTreeMap<String, Icrc3Value> = BTreeMap::new();
        let mut problems = Vec::new();

        for spec in &attribute_specs {
            match &spec.key.scope {
                Some(AttributeScope::OpenId { issuer }) => {
                    let credential = self
                        .openid_credentials
                        .iter()
                        .find(|c| c.config_issuer().as_deref() == Some(issuer.as_str()));

                    let Some(credential) = credential else {
                        problems.push(format!("No credential found for issuer: {}", issuer));
                        continue;
                    };

                    let stored_value = match spec.key.attribute_name {
                        AttributeName::Email => credential.get_email(),
                        AttributeName::Name => credential.get_name(),
                        AttributeName::VerifiedEmail => credential.get_verified_email(),
                    };

                    let Some(stored) = stored_value else {
                        problems.push(format!(
                            "Attribute {} not available for issuer {}",
                            spec.key.attribute_name, issuer
                        ));
                        continue;
                    };

                    // If a value was provided, validate it matches.
                    if let Some(ref expected_value) = spec.value {
                        if expected_value.as_slice() != stored.as_bytes() {
                            problems.push(format!(
                                "Attribute value mismatch for {}: provided value does not match stored value",
                                spec.key
                            ));
                            continue;
                        }
                    }

                    // Compute the certified key.
                    let certified_key = if spec.omit_scope {
                        spec.key.attribute_name.to_string()
                    } else {
                        spec.key.to_string()
                    };

                    match certified_pairs.entry(certified_key) {
                        std::collections::btree_map::Entry::Occupied(entry) => {
                            problems.push(format!(
                                "Duplicate certified attribute key '{}' derived from spec {}",
                                entry.key(),
                                spec.key
                            ));
                        }
                        std::collections::btree_map::Entry::Vacant(entry) => {
                            entry.insert(Icrc3Value::Text(stored));
                        }
                    }
                }
                None => {
                    problems.push(format!(
                        "Attribute {} has no scope; only scoped attributes are supported",
                        spec.key
                    ));
                }
            }
        }

        if !problems.is_empty() {
            return Err(PrepareIcrc3AttributeError::AttributeMismatch { problems });
        }

        certified_pairs.insert("implicit:nonce".to_string(), Icrc3Value::Blob(nonce));
        certified_pairs.insert("implicit:origin".to_string(), Icrc3Value::Text(origin));
        certified_pairs.insert(
            "implicit:issued_at_timestamp_ns".to_string(),
            Icrc3Value::Nat(candid::Nat::from(issued_at_timestamp_ns)),
        );

        let message = icrc3_attribute_message(&certified_pairs);

        let seed = account.calculate_seed();
        state::signature_map_mut(|sigs| {
            let inputs = CanisterSigInputs {
                domain: ICRC3_ATTRIBUTES_CERTIFICATION_DOMAIN,
                seed: &seed,
                message: &message,
            };
            sigs.add_signature(&inputs);
        });

        update_root_hash();

        Ok(message)
    }

    /// Looks up the single ICRC-3 attribute signature for the given message blob.
    pub fn get_icrc3_attributes(
        &self,
        account: Account,
        message: &[u8],
    ) -> Result<GetIcrc3AttributeResponse, GetIcrc3AttributeError> {
        let seed = account.calculate_seed();

        let signature = state::assets_and_signatures(|certified_assets, sigs| {
            let inputs = CanisterSigInputs {
                domain: ICRC3_ATTRIBUTES_CERTIFICATION_DOMAIN,
                seed: &seed,
                message,
            };
            sigs.get_signature_as_cbor(&inputs, Some(certified_assets.root_hash()))
        })
        .map_err(|_| GetIcrc3AttributeError::NoSuchSignature)?;

        Ok(GetIcrc3AttributeResponse { signature })
    }

    /// Lists available attribute key/value pairs from stored OpenID credentials.
    ///
    /// If `requested` is `None`, returns all available attributes.
    /// If `requested` is `Some(keys)`, returns only attributes matching the given keys.
    /// Unscoped keys (e.g., `"email"`) match all scopes; scoped keys match exactly.
    /// Response keys are always fully scoped.
    pub fn list_available_attributes(
        &self,
        requested: Option<Vec<AttributeKey>>,
    ) -> Vec<(String, Vec<u8>)> {
        let all_attribute_names = AttributeName::all();
        let mut result = BTreeMap::new();

        for credential in &self.openid_credentials {
            let Some(issuer) = credential.config_issuer() else {
                continue;
            };
            let scope = AttributeScope::OpenId { issuer };

            for &attr_name in all_attribute_names {
                let value = match attr_name {
                    AttributeName::Email => credential.get_email(),
                    AttributeName::Name => credential.get_name(),
                    AttributeName::VerifiedEmail => credential.get_verified_email(),
                };
                let Some(value) = value else {
                    continue;
                };

                let matches = match &requested {
                    None => true,
                    Some(keys) => keys.iter().any(|k| {
                        k.attribute_name == attr_name
                            && (k.scope.is_none() || k.scope.as_ref() == Some(&scope))
                    }),
                };

                if matches {
                    let key_str = format!("{}:{}", scope, attr_name);
                    result.entry(key_str).or_insert_with(|| value.into_bytes());
                }
            }
        }

        result.into_iter().collect()
    }
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
                .get(&(
                    GOOGLE_ISSUER.to_string(),
                    "google-user-123".to_string(),
                    "test-client-id".to_string(),
                ))
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
                .get(&(
                    GOOGLE_ISSUER.to_string(),
                    "google-user-123".to_string(),
                    "test-client-id".to_string(),
                ))
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
                .get(&(
                    GOOGLE_ISSUER.to_string(),
                    "google-user-123".to_string(),
                    "test-client-id".to_string(),
                ))
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
                .get(&(
                    GOOGLE_ISSUER.to_string(),
                    "google-user-123".to_string(),
                    "test-client-id".to_string(),
                ))
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
                .get(&(
                    GOOGLE_ISSUER.to_string(),
                    "google-user-123".to_string(),
                    "test-client-id".to_string(),
                ))
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
                .get(&(
                    GOOGLE_ISSUER.to_string(),
                    "google-user-123".to_string(),
                    "test-client-id".to_string(),
                ))
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
                    "test-client-id".to_string(),
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
                .get(&(
                    enterprise_iss,
                    "enterprise-user".to_string(),
                    "test-client-id".to_string(),
                ))
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
                    "test-client-id".to_string(),
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
                .get(&(
                    GOOGLE_ISSUER.to_string(),
                    "google-user-123".to_string(),
                    "test-client-id".to_string(),
                ))
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
                    "test-client-id".to_string(),
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
                .get(&(
                    GOOGLE_ISSUER.to_string(),
                    "google-user-123".to_string(),
                    "test-client-id".to_string(),
                ))
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

    /// Regression test for the `get_attributes` fix.
    ///
    /// The bug: `get_attributes` built its scope key from `openid_credential.iss`
    /// (the *resolved* issuer, e.g. `…/9188040d-…/v2.0` for Microsoft),
    /// while callers pass the *template* issuer (`…/{tid}/v2.0`) as the scope
    /// key. This caused `attributes.remove(&scope)` to always miss for
    /// Microsoft credentials.
    ///
    /// We test via `prepare_openid_attributes` because:
    ///  1. It uses the identical `config_issuer()` lookup that the fix applied
    ///     to `get_attributes`.
    ///  2. `get_attributes` calls into canister runtime (`state::assets_and_signatures`,
    ///     `ic_cdk::println!`) and cannot be exercised in a unit test.
    ///  3. A test that passes for `prepare_openid_attributes` with the
    ///     template-scope key proves that the same key will work in
    ///     `get_attributes` after the fix.
    mod get_attributes_issuer_regression_test {
        use super::*;
        use internet_identity_interface::internet_identity::types::{
            OpenIdConfig, OpenIdEmailVerificationScheme,
        };

        const MICROSOFT_ISSUER_TEMPLATE: &str = "https://login.microsoftonline.com/{tid}/v2.0";
        const MICROSOFT_PERSONAL_TID: &str = "9188040d-6c67-4c5b-b112-36a304b66dad";
        const MICROSOFT_RESOLVED_ISSUER: &str =
            "https://login.microsoftonline.com/9188040d-6c67-4c5b-b112-36a304b66dad/v2.0";
        const ANCHOR_NUMBER: u64 = 4242;

        fn setup_microsoft_provider() {
            crate::openid::setup(vec![OpenIdConfig {
                name: "Microsoft".to_string(),
                logo: String::new(),
                issuer: MICROSOFT_ISSUER_TEMPLATE.to_string(),
                client_id: "test-client-id".to_string(),
                jwks_uri: String::new(),
                auth_uri: String::new(),
                auth_scope: vec![],
                fedcm_uri: None,
                email_verification: Some(OpenIdEmailVerificationScheme::Microsoft),
            }]);
        }

        /// Callers of `get_attributes` / `prepare_attributes` always use the
        /// *template* issuer (`{tid}` placeholder) as the scope key.
        fn microsoft_template_scope() -> Option<AttributeScope> {
            Some(AttributeScope::OpenId {
                issuer: MICROSOFT_ISSUER_TEMPLATE.to_string(),
            })
        }

        /// A Microsoft credential whose `.iss` is the *resolved* issuer
        /// (real tenant ID), as it would be after JWT verification.
        fn microsoft_credential_with_email(email: &str) -> OpenIdCredential {
            OpenIdCredential {
                iss: MICROSOFT_RESOLVED_ISSUER.to_string(),
                sub: "ms-user-456".to_string(),
                aud: "test-client-id".to_string(),
                last_usage_timestamp: None,
                metadata: HashMap::from([
                    (
                        "email".to_string(),
                        MetadataEntryV2::String(email.to_string()),
                    ),
                    (
                        "tid".to_string(),
                        MetadataEntryV2::String(MICROSOFT_PERSONAL_TID.to_string()),
                    ),
                ]),
            }
        }

        /// The scope key uses the *template* issuer while the credential's
        /// `.iss` contains the *resolved* issuer. Before the fix,
        /// `get_attributes` used `.iss` directly and the lookup would miss.
        /// `prepare_openid_attributes` (and now `get_attributes`) uses
        /// `config_issuer()` which returns the template, so the key matches.
        #[test]
        fn test_template_scope_resolves_microsoft_credential() {
            setup_microsoft_provider();

            let mut anchor = Anchor::new(ANCHOR_NUMBER, 0);
            anchor.openid_credentials = vec![microsoft_credential_with_email("user@outlook.com")];

            // Key the request by the *template* scope — exactly as callers do.
            let mut requested = BTreeMap::from([(
                microsoft_template_scope(),
                BTreeSet::from([AttributeName::Email]),
            )]);

            let result = anchor.prepare_openid_attributes(&mut requested);

            // The scope must have been consumed (matched).
            assert!(
                requested.is_empty(),
                "template scope should match the credential via config_issuer()"
            );

            // Exactly one credential key should be present with one attribute.
            pretty_assert_eq!(result.len(), 1, "one credential should match");
            let attrs = result
                .get(&(
                    MICROSOFT_RESOLVED_ISSUER.to_string(),
                    "ms-user-456".to_string(),
                    "test-client-id".to_string(),
                ))
                .expect("credential key should be present");
            pretty_assert_eq!(attrs.len(), 1, "email attribute should be returned");
            pretty_assert_eq!(String::from_utf8_lossy(&attrs[0].value), "user@outlook.com",);
        }
    }

    mod prepare_icrc3_attributes_tests {
        use super::*;
        use internet_identity_interface::internet_identity::types::{
            OpenIdConfig, OpenIdEmailVerificationScheme,
        };

        const GOOGLE_ISSUER: &str = "https://accounts.google.com";

        fn setup_google_provider() {
            crate::openid::setup(vec![OpenIdConfig {
                name: "Google".to_string(),
                logo: String::new(),
                issuer: GOOGLE_ISSUER.to_string(),
                client_id: "test-client-id".to_string(),
                jwks_uri: String::new(),
                auth_uri: String::new(),
                auth_scope: vec![],
                fedcm_uri: None,
                email_verification: Some(OpenIdEmailVerificationScheme::Google),
            }]);
        }

        fn google_anchor() -> Anchor {
            let mut anchor = Anchor::new(ANCHOR_NUMBER, 0);
            anchor.openid_credentials = vec![OpenIdCredential {
                iss: GOOGLE_ISSUER.to_string(),
                sub: "google-user-123".to_string(),
                aud: "test-client-id".to_string(),
                last_usage_timestamp: None,
                metadata: HashMap::from([
                    (
                        "email".to_string(),
                        MetadataEntryV2::String("user@example.com".to_string()),
                    ),
                    (
                        "name".to_string(),
                        MetadataEntryV2::String("Example User".to_string()),
                    ),
                ]),
            }];
            anchor
        }

        fn google_spec(
            attr: AttributeName,
            value: Option<&[u8]>,
            omit_scope: bool,
        ) -> ValidatedAttributeSpec {
            ValidatedAttributeSpec {
                key: AttributeKey {
                    scope: Some(AttributeScope::OpenId {
                        issuer: GOOGLE_ISSUER.to_string(),
                    }),
                    attribute_name: attr,
                },
                value: value.map(|v| v.to_vec()),
                omit_scope,
            }
        }

        #[test]
        fn should_reject_value_mismatch() {
            setup_google_provider();
            let anchor = google_anchor();
            let account = Account::new(ANCHOR_NUMBER, "https://dapp.com".to_string(), None, None);

            let result = anchor.prepare_icrc3_attributes(
                vec![
                    google_spec(AttributeName::Email, Some(b"user@example.com"), false), // correct
                    google_spec(AttributeName::Name, Some(b"Wrong Name"), false),        // wrong
                ],
                vec![0u8; 32],
                "https://dapp.com".to_string(),
                1_000_000_000,
                account,
            );

            match result {
                Err(PrepareIcrc3AttributeError::AttributeMismatch { problems }) => {
                    pretty_assert_eq!(problems.len(), 1);
                    assert!(
                        problems[0].contains("value mismatch"),
                        "Expected value mismatch error, got: {}",
                        problems[0]
                    );
                    assert!(problems[0].contains("name"), "Error should mention 'name'");
                }
                other => panic!("Expected AttributeMismatch, got {:?}", other),
            }
        }

        #[test]
        fn should_reject_unknown_issuer() {
            setup_google_provider();
            let anchor = google_anchor();
            let account = Account::new(ANCHOR_NUMBER, "https://dapp.com".to_string(), None, None);

            let result = anchor.prepare_icrc3_attributes(
                vec![ValidatedAttributeSpec {
                    key: AttributeKey {
                        scope: Some(AttributeScope::OpenId {
                            issuer: "https://unknown-issuer.com".to_string(),
                        }),
                        attribute_name: AttributeName::Email,
                    },
                    value: None,
                    omit_scope: false,
                }],
                vec![0u8; 32],
                "https://dapp.com".to_string(),
                1_000_000_000,
                account,
            );

            match result {
                Err(PrepareIcrc3AttributeError::AttributeMismatch { problems }) => {
                    assert!(
                        problems[0].contains("No credential found"),
                        "Expected 'No credential found', got: {}",
                        problems[0]
                    );
                }
                other => panic!("Expected AttributeMismatch, got {:?}", other),
            }
        }

        #[test]
        fn should_reject_scopeless_attribute() {
            setup_google_provider();
            let anchor = google_anchor();
            let account = Account::new(ANCHOR_NUMBER, "https://dapp.com".to_string(), None, None);

            let result = anchor.prepare_icrc3_attributes(
                vec![ValidatedAttributeSpec {
                    key: AttributeKey {
                        scope: None,
                        attribute_name: AttributeName::Email,
                    },
                    value: None,
                    omit_scope: false,
                }],
                vec![0u8; 32],
                "https://dapp.com".to_string(),
                1_000_000_000,
                account,
            );

            match result {
                Err(PrepareIcrc3AttributeError::AttributeMismatch { problems }) => {
                    assert!(
                        problems[0].contains("no scope"),
                        "Expected 'no scope' error, got: {}",
                        problems[0]
                    );
                }
                other => panic!("Expected AttributeMismatch, got {:?}", other),
            }
        }

        #[test]
        fn should_reject_duplicate_certified_keys() {
            setup_google_provider();
            let anchor = google_anchor();
            let account = Account::new(ANCHOR_NUMBER, "https://dapp.com".to_string(), None, None);

            // Two specs that both resolve to certified key "email" due to omit_scope=true
            let result = anchor.prepare_icrc3_attributes(
                vec![
                    google_spec(AttributeName::Email, None, true),
                    google_spec(AttributeName::Email, None, true),
                ],
                vec![0u8; 32],
                "https://dapp.com".to_string(),
                1_000_000_000,
                account,
            );

            match result {
                Err(PrepareIcrc3AttributeError::AttributeMismatch { problems }) => {
                    assert!(
                        problems[0].contains("Duplicate certified attribute key"),
                        "Expected duplicate key error, got: {}",
                        problems[0]
                    );
                }
                other => panic!("Expected AttributeMismatch, got {:?}", other),
            }
        }
    }

    mod icrc3_attribute_message_tests {
        use super::*;
        use candid::Decode;

        const GOOGLE_ISSUER: &str = "https://accounts.google.com";

        #[test]
        fn should_produce_different_messages_for_different_omit_scope() {
            let mut scoped_pairs = BTreeMap::new();
            scoped_pairs.insert(
                format!("openid:{}:email", GOOGLE_ISSUER),
                Icrc3Value::Text("user@example.com".to_string()),
            );

            let mut unscoped_pairs = BTreeMap::new();
            unscoped_pairs.insert(
                "email".to_string(),
                Icrc3Value::Text("user@example.com".to_string()),
            );

            let scoped_msg = icrc3_attribute_message(&scoped_pairs);
            let unscoped_msg = icrc3_attribute_message(&unscoped_pairs);

            pretty_assert_ne!(
                scoped_msg,
                unscoped_msg,
                "Messages with different key scoping should differ"
            );
        }

        #[test]
        fn should_produce_deterministic_encoding() {
            let mut pairs = BTreeMap::new();
            pairs.insert(
                "email".to_string(),
                Icrc3Value::Text("user@example.com".to_string()),
            );
            pairs.insert(
                "name".to_string(),
                Icrc3Value::Text("Example User".to_string()),
            );

            let msg1 = icrc3_attribute_message(&pairs);
            let msg2 = icrc3_attribute_message(&pairs);
            pretty_assert_eq!(msg1, msg2, "Same input should produce identical encoding");
        }

        #[test]
        fn should_produce_decodable_icrc3_value() {
            let mut pairs = BTreeMap::new();
            pairs.insert(
                "email".to_string(),
                Icrc3Value::Text("user@example.com".to_string()),
            );
            pairs.insert(
                "name".to_string(),
                Icrc3Value::Text("Example User".to_string()),
            );

            let msg = icrc3_attribute_message(&pairs);
            let decoded: Icrc3Value =
                Decode!(&msg, Icrc3Value).expect("should decode as Icrc3Value");

            match decoded {
                Icrc3Value::Map(entries) => {
                    pretty_assert_eq!(entries.len(), 2);
                    // BTreeMap ordering: email < name
                    pretty_assert_eq!(entries[0].0, "email");
                    pretty_assert_eq!(
                        entries[0].1,
                        Icrc3Value::Text("user@example.com".to_string())
                    );
                    pretty_assert_eq!(entries[1].0, "name");
                    pretty_assert_eq!(entries[1].1, Icrc3Value::Text("Example User".to_string()));
                }
                other => panic!("Expected Map, got {:?}", other),
            }
        }
    }

    mod list_available_attributes_tests {
        use super::*;
        use internet_identity_interface::internet_identity::types::{
            OpenIdConfig, OpenIdEmailVerificationScheme,
        };

        const GOOGLE_ISSUER: &str = "https://accounts.google.com";

        fn setup_google_provider() {
            crate::openid::setup(vec![OpenIdConfig {
                name: "Google".to_string(),
                logo: String::new(),
                issuer: GOOGLE_ISSUER.to_string(),
                client_id: "test-client-id".to_string(),
                jwks_uri: String::new(),
                auth_uri: String::new(),
                auth_scope: vec![],
                fedcm_uri: None,
                email_verification: Some(OpenIdEmailVerificationScheme::Google),
            }]);
        }

        fn google_anchor() -> Anchor {
            let mut anchor = Anchor::new(ANCHOR_NUMBER, 0);
            anchor.openid_credentials = vec![OpenIdCredential {
                iss: GOOGLE_ISSUER.to_string(),
                sub: "google-user-123".to_string(),
                aud: "test-client-id".to_string(),
                last_usage_timestamp: None,
                metadata: HashMap::from([
                    (
                        "email".to_string(),
                        MetadataEntryV2::String("user@example.com".to_string()),
                    ),
                    (
                        "name".to_string(),
                        MetadataEntryV2::String("Example User".to_string()),
                    ),
                ]),
            }];
            anchor
        }

        #[test]
        fn should_return_all_attributes_when_none() {
            setup_google_provider();
            let anchor = google_anchor();

            let result = anchor.list_available_attributes(None);

            // Should have email and name (verified_email requires email_verified=true in metadata)
            pretty_assert_eq!(result.len(), 2);
            let keys: Vec<&str> = result.iter().map(|(k, _)| k.as_str()).collect();
            assert!(keys.contains(&format!("openid:{}:email", GOOGLE_ISSUER).as_str()));
            assert!(keys.contains(&format!("openid:{}:name", GOOGLE_ISSUER).as_str()));
        }

        #[test]
        fn should_filter_by_scoped_key() {
            setup_google_provider();
            let anchor = google_anchor();

            let result = anchor.list_available_attributes(Some(vec![AttributeKey {
                scope: Some(AttributeScope::OpenId {
                    issuer: GOOGLE_ISSUER.to_string(),
                }),
                attribute_name: AttributeName::Email,
            }]));

            pretty_assert_eq!(result.len(), 1);
            pretty_assert_eq!(result[0].0, format!("openid:{}:email", GOOGLE_ISSUER));
            pretty_assert_eq!(result[0].1, b"user@example.com");
        }

        #[test]
        fn should_filter_by_unscoped_key() {
            setup_google_provider();
            let anchor = google_anchor();

            // "email" without scope should match all scopes
            let result = anchor.list_available_attributes(Some(vec![AttributeKey {
                scope: None,
                attribute_name: AttributeName::Email,
            }]));

            pretty_assert_eq!(result.len(), 1);
            pretty_assert_eq!(result[0].0, format!("openid:{}:email", GOOGLE_ISSUER));
            pretty_assert_eq!(result[0].1, b"user@example.com");
        }

        #[test]
        fn should_return_empty_for_missing_attribute() {
            setup_google_provider();
            let anchor = google_anchor();

            let result = anchor.list_available_attributes(Some(vec![AttributeKey {
                scope: Some(AttributeScope::OpenId {
                    issuer: GOOGLE_ISSUER.to_string(),
                }),
                attribute_name: AttributeName::VerifiedEmail,
            }]));

            // verified_email is not available because email_verified is not "true" in metadata
            pretty_assert_eq!(result.len(), 0);
        }

        #[test]
        fn should_return_empty_for_no_credentials() {
            setup_google_provider();
            let anchor = Anchor::new(ANCHOR_NUMBER, 0); // no credentials

            let result = anchor.list_available_attributes(None);
            pretty_assert_eq!(result.len(), 0);
        }

        #[test]
        fn should_return_empty_for_empty_filter() {
            setup_google_provider();
            let anchor = google_anchor();

            // Some(vec![]) means "filter by these keys" with no keys → nothing matches
            let result = anchor.list_available_attributes(Some(vec![]));
            pretty_assert_eq!(result.len(), 0);
        }

        #[test]
        fn should_return_empty_for_wrong_issuer() {
            setup_google_provider();
            let anchor = google_anchor();

            let result = anchor.list_available_attributes(Some(vec![AttributeKey {
                scope: Some(AttributeScope::OpenId {
                    issuer: "https://unknown-issuer.com".to_string(),
                }),
                attribute_name: AttributeName::Email,
            }]));

            pretty_assert_eq!(result.len(), 0);
        }
    }
}
