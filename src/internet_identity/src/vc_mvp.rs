use crate::delegation::check_frontend_length;
use crate::{delegation, random_salt, state, update_root_hash, MINUTE_NS};
use candid::Principal;
use ic_canister_sig_creation::signature_map::CanisterSigInputs;
use ic_canister_sig_creation::CanisterSigPublicKey;
use ic_cdk::api::time;
use ic_certification::Hash;
use ic_verifiable_credentials::issuer_api::{ArgumentValue, CredentialSpec};
use ic_verifiable_credentials::{
    build_credential_jwt, canister_sig_pk_from_vc_signing_input, did_for_principal,
    vc_signing_input, vc_signing_input_to_jws, CredentialParams, II_CREDENTIAL_URL_PREFIX,
    II_ISSUER_URL, VC_SIGNING_INPUT_DOMAIN,
};
use internet_identity_interface::internet_identity::types::vc_mvp::{
    GetIdAliasError, IdAliasCredentials, PreparedIdAlias, SignedIdAlias,
};
use internet_identity_interface::internet_identity::types::{FrontendHostname, IdentityNumber};
use serde_bytes::ByteBuf;
use sha2::{Digest, Sha256};
use std::collections::HashMap;

pub struct AliasTuple {
    /// A temporary identity, used in attribute sharing flow.
    pub id_alias: Principal,
    /// An identity under which a user is known to a dapp.
    pub id_dapp: Principal,
    /// The derivation origin of the id_dapp
    pub derivation_origin: String,
}

// The expiration of id_alias verifiable credentials.
const ID_ALIAS_VC_EXPIRATION_PERIOD_NS: u64 = 15 * MINUTE_NS;

pub struct InvolvedDapps {
    pub(crate) relying_party: FrontendHostname,
    pub(crate) issuer: FrontendHostname,
}

pub async fn prepare_id_alias(
    identity_number: IdentityNumber,
    dapps: InvolvedDapps,
) -> PreparedIdAlias {
    state::ensure_salt_set().await;
    check_frontend_length(&dapps.relying_party);
    check_frontend_length(&dapps.issuer);

    let seed = random_salt().await;
    let canister_sig_pk = CanisterSigPublicKey::new(ic_cdk::id(), seed.to_vec());
    let id_alias_principal = Principal::self_authenticating(canister_sig_pk.to_der());

    let rp_tuple = AliasTuple {
        id_alias: id_alias_principal,
        id_dapp: delegation::get_principal(identity_number, dapps.relying_party.clone()),
        derivation_origin: dapps.relying_party.clone(),
    };
    let issuer_tuple = AliasTuple {
        id_alias: id_alias_principal,
        id_dapp: delegation::get_principal(identity_number, dapps.issuer.clone()),
        derivation_origin: dapps.issuer.clone(),
    };

    // rp_signing_input is sent to the relying party.
    // The relying party needs to have the derivation origin to confirm where the principal is derived from.
    let rp_signing_input = vc_signing_input(&id_alias_credential_jwt(&rp_tuple), &canister_sig_pk)
        .expect("failed getting signing_input");
    // issuer_signing_input is sent to the relying party.
    // The issuer needs to have the derivation origin to confirm where the principal is derived from.
    let issuer_signing_input =
        vc_signing_input(&id_alias_credential_jwt(&issuer_tuple), &canister_sig_pk)
            .expect("failed getting signing_input");
    state::signature_map_mut(|sigs| {
        sigs.add_signature(&CanisterSigInputs {
            domain: VC_SIGNING_INPUT_DOMAIN,
            seed: &seed,
            message: &rp_signing_input,
        });
        sigs.add_signature(&CanisterSigInputs {
            domain: VC_SIGNING_INPUT_DOMAIN,
            seed: &seed,
            message: &issuer_signing_input,
        });
    });
    update_root_hash();

    state::usage_metrics_mut(|metrics| {
        metrics.prepare_id_alias_counter += 1;
    });

    PreparedIdAlias {
        canister_sig_pk_der: ByteBuf::from(canister_sig_pk.to_der()),
        rp_id_alias_jwt: String::from_utf8(rp_signing_input).unwrap(),
        issuer_id_alias_jwt: String::from_utf8(issuer_signing_input).unwrap(),
    }
}

pub fn get_id_alias(
    identity_number: IdentityNumber,
    dapps: InvolvedDapps,
    rp_id_alias_jwt: &str,
    issuer_id_alias_jwt: &str,
) -> Result<IdAliasCredentials, GetIdAliasError> {
    check_frontend_length(&dapps.relying_party);
    check_frontend_length(&dapps.issuer);

    state::assets_and_signatures(|cert_assets, sigs| {
        let canister_sig_pk = canister_sig_pk_from_vc_signing_input(rp_id_alias_jwt.as_bytes())
            .map_err(GetIdAliasError::NoSuchCredentials)?;
        let seed = canister_sig_pk.seed.as_slice();

        let id_alias_principal = Principal::self_authenticating(canister_sig_pk.to_der());
        let id_rp = delegation::get_principal(identity_number, dapps.relying_party.clone());
        let id_issuer = delegation::get_principal(identity_number, dapps.issuer.clone());

        let rp_sig_inputs = CanisterSigInputs {
            domain: VC_SIGNING_INPUT_DOMAIN,
            seed,
            message: rp_id_alias_jwt.as_bytes(),
        };
        let rp_sig = sigs
            .get_signature_as_cbor(&rp_sig_inputs, Some(cert_assets.root_hash()))
            .map_err(|err| {
                GetIdAliasError::NoSuchCredentials(format!("rp_sig not found: {err}"))
            })?;
        let rp_jws = vc_signing_input_to_jws(rp_id_alias_jwt.as_bytes(), &rp_sig)
            .expect("failed constructing rp JWS");

        let issuer_sig_inputs = CanisterSigInputs {
            domain: VC_SIGNING_INPUT_DOMAIN,
            seed,
            message: issuer_id_alias_jwt.as_bytes(),
        };
        let issuer_sig = sigs
            .get_signature_as_cbor(&issuer_sig_inputs, Some(cert_assets.root_hash()))
            .map_err(|err| {
                GetIdAliasError::NoSuchCredentials(format!("issuer_sig not found: {err}"))
            })?;
        let issuer_jws = vc_signing_input_to_jws(issuer_id_alias_jwt.as_bytes(), &issuer_sig)
            .expect("failed constructing issuer JWS");

        Ok(IdAliasCredentials {
            rp_id_alias_credential: SignedIdAlias {
                id_alias: id_alias_principal,
                id_dapp: id_rp,
                credential_jws: rp_jws,
            },
            issuer_id_alias_credential: SignedIdAlias {
                id_alias: id_alias_principal,
                id_dapp: id_issuer,
                credential_jws: issuer_jws,
            },
        })
    })
}

fn id_alias_credential_jwt(alias_tuple: &AliasTuple) -> String {
    let expiration_timestamp_s: u32 =
        ((time() + ID_ALIAS_VC_EXPIRATION_PERIOD_NS) / 1_000_000_000) as u32;
    let params = CredentialParams {
        spec: id_alias_credential_spec(alias_tuple.id_alias, &alias_tuple.derivation_origin),
        subject_id: did_for_principal(alias_tuple.id_dapp),
        credential_id_url: prepare_credential_id_new(alias_tuple),
        issuer_url: II_ISSUER_URL.to_string(),
        expiration_timestamp_s,
    };
    build_credential_jwt(params)
}

fn id_alias_credential_spec(id_alias: Principal, origin: &FrontendHostname) -> CredentialSpec {
    let mut args = HashMap::new();
    args.insert(
        "hasIdAlias".to_string(),
        ArgumentValue::String(id_alias.to_text()),
    );
    args.insert(
        "derivationOrigin".to_string(),
        ArgumentValue::String(origin.to_string()),
    );
    CredentialSpec {
        credential_type: "InternetIdentityIdAlias".to_string(),
        arguments: Some(args),
    }
}

// Prepares a unique id for the given alias_tuple.
// The returned URL has the format: "data:text/plain;charset=UTF-8,timestamp_sec:...,alias_hash:..."
fn prepare_credential_id_new(alias_tuple: &AliasTuple) -> String {
    let timestamp = format!("timestamp_ns:{}", time());
    let mut hasher = Sha256::new();
    hasher.update("id_dapp=");
    hasher.update(alias_tuple.id_dapp.to_text());
    hasher.update(",id_alias=");
    hasher.update(alias_tuple.id_alias.to_text());
    let hash: Hash = hasher.finalize().into();
    let alias_hash = format!("alias_hash:{}", hex::encode(hash));
    format!("{II_CREDENTIAL_URL_PREFIX}{timestamp},{alias_hash}")
}
