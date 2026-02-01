use std::collections::{BTreeMap, BTreeSet};

use ic_canister_sig_creation::signature_map::{CanisterSigInputs, SignatureMap};
use ic_representation_independent_hash::{representation_independent_hash, Value};
use internet_identity_interface::internet_identity::types::{
    attributes::{AttributeField, AttributeRequest, AttributeScope},
    MetadataEntryV2, Timestamp,
};

use crate::{
    openid::{OpenIdCredential, OpenIdCredentialKey, OPENID_SESSION_DURATION_NS},
    state,
    storage::{account::Account, anchor::Anchor},
    update_root_hash,
};

impl Anchor {
    /// Returns `(attribute_key, attribute_value)` pairs for attributes from `requested_attributes`
    /// for which we have values in our OpenID credentials' JWT.
    ///
    /// `requested_attributes` is mutated to remove the attributes for which values were found.
    fn prepare_openid_attributes(
        &self,
        mut requested_attributes: BTreeMap<Option<AttributeScope>, BTreeSet<AttributeField>>,
    ) -> BTreeMap<OpenIdCredentialKey, Vec<(String, String)>> {
        self.openid_credentials
            .iter()
            .filter_map(|openid_credential| {
                // E.g., `openid:google.com`
                let scope = Some(AttributeScope::OpenId {
                    issuer: openid_credential.iss.clone(),
                });
                // E.g., {`email`, `name`}
                let Some(fields) = requested_attributes.remove(&scope) else {
                    return None;
                };

                let attribute_requests: BTreeMap<String, AttributeRequest> = fields
                    .into_iter()
                    .map(|field| {
                        let field_str = format!("{}", field);
                        let scope = scope.clone();
                        (field_str, AttributeRequest { scope, field })
                    })
                    .collect();

                let attributes = openid_credential
                    .metadata
                    .iter()
                    .filter_map(|(field_str, attribute_value)| {
                        let attribute_request = attribute_requests.get(field_str)?;

                        // E.g., `openid:google.com:email`
                        let attribute_key = format!("{}", attribute_request);

                        let MetadataEntryV2::String(attribute_value) = attribute_value else {
                            return None;
                        };

                        Some((attribute_key, attribute_value.clone()))
                    })
                    .collect::<Vec<(String, String)>>();

                Some((openid_credential.key(), attributes))
            })
            .collect()
    }

    /// Processes attribute requests from all attribute scopes, prepares signatures for
    /// the (attribute_key, attribute_value) pairs that can be fulfilled, and returns
    /// the list of issued attribute keys.
    pub fn prepare_attributes(
        &self,
        requested_attributes: BTreeMap<Option<AttributeScope>, BTreeSet<AttributeField>>,
        account: Account,
        now_timestamp_ns: Timestamp,
    ) -> Vec<String> {
        let mut certified_attributes = Vec::new();

        // Process scope `openid` ...
        let mut opedid_attributes_to_certify = self.prepare_openid_attributes(requested_attributes);

        for openid_credential in &self.openid_credentials {
            let Some(attributes) = opedid_attributes_to_certify.remove(&openid_credential.key())
            else {
                continue;
            };

            openid_credential.prepare_attributes_no_root_hash_update(
                &account,
                &attributes,
                now_timestamp_ns,
            );

            certified_attributes.extend(
                attributes
                    .into_iter()
                    .map(|(attribute_key, _)| attribute_key),
            );
        }

        update_root_hash();

        certified_attributes
    }
}

impl OpenIdCredential {
    /// Does not ensure the salt is initialized.
    ///
    /// Does not update the root hash, intended to be used in contexts where multiple.
    fn prepare_attributes_no_root_hash_update(
        &self,
        account: &Account,
        attributes: &Vec<(String, String)>,
        now_timestamp_ns: Timestamp,
    ) {
        let expiration_timestamp_ns = now_timestamp_ns.saturating_add(OPENID_SESSION_DURATION_NS);

        let seed = account.calculate_seed();

        state::signature_map_mut(|sigs| {
            for (attribute_key, attribute_value) in attributes {
                add_attribute_signature(
                    sigs,
                    &seed,
                    attribute_key,
                    attribute_value,
                    expiration_timestamp_ns,
                );
            }
        });
    }
}

fn add_attribute_signature(
    sigs: &mut SignatureMap,
    seed: &[u8],
    attribute_key: &str,
    attribute_value: &str,
    expiration_timestamp_ns: u64,
) {
    // TODO: This should eventually be moved to a library function.
    let attribute_signature_msg = {
        let m: Vec<(String, Value)> = vec![
            ("expiration".into(), Value::Number(expiration_timestamp_ns)),
            (
                attribute_key.into(),
                Value::String(attribute_value.to_string()),
            ),
        ];
        representation_independent_hash(m.as_slice()).to_vec()
    };

    let inputs = CanisterSigInputs {
        domain: b"ii-request-attribute",
        seed,
        message: &attribute_signature_msg,
    };
    sigs.add_signature(&inputs);
}
