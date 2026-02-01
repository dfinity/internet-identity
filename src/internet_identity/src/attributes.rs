use std::collections::{BTreeMap, BTreeSet};

use ic_canister_sig_creation::signature_map::{CanisterSigInputs, SignatureMap};
use ic_representation_independent_hash::{representation_independent_hash, Value};
use internet_identity_interface::internet_identity::types::{
    attributes::{
        Attribute, AttributeKey, AttributeName, AttributeScope, CertifiedAttribute,
        CertifiedAttributes,
    },
    MetadataEntryV2, Timestamp,
};

use crate::{
    openid::{OpenIdCredential, OpenIdCredentialKey, OPENID_SESSION_DURATION_NS},
    state,
    storage::{account::Account, anchor::Anchor},
    update_root_hash,
};

const ATTRIBUTES_CERTIFICATION_DOMAIN: &[u8] = b"ii-request-attribute";
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
    /// Returns the list of attribute keys certified with expiry `now_timestamp_ns + OPENID_SESSION_DURATION_NS`.
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
                            value: value.clone(),
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
        let expiration_timestamp_ns = now_timestamp_ns.saturating_add(OPENID_SESSION_DURATION_NS);

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
            Value::String(attribute.value.to_string()),
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
