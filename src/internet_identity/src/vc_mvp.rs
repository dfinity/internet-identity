use crate::assets::CertifiedAssets;
use crate::delegation::check_frontend_length;
use crate::{delegation, hash, state, update_root_hash, LABEL_SIG, MINUTE_NS};
use candid::Principal;
use ic_cdk::api::{data_certificate, time};
use ic_cdk::trap;
use ic_certified_map::{Hash, HashTree};
use internet_identity::signature_map::SignatureMap;
use internet_identity_interface::internet_identity::types::attribute_sharing_mvp::{
    GetIdAliasResponse, IdAliasCredentials, SignedIdAlias,
};
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, CanisterSigKey, FrontendHostname, IdentityNumber, Signature,
};
use serde::Serialize;
use serde_bytes::ByteBuf;
use std::collections::HashMap;

// The expiration used for signatures
#[allow(clippy::identity_op)]
const SIGNATURE_EXPIRATION_PERIOD_NS: u64 = 1 * MINUTE_NS;

pub struct InvolvedDapps {
    pub(crate) relying_party: FrontendHostname,
    pub(crate) issuer: FrontendHostname,
}

pub async fn prepare_id_alias(
    identity_number: IdentityNumber,
    dapps: InvolvedDapps,
) -> CanisterSigKey {
    state::ensure_salt_set().await;
    check_frontend_length(&dapps.relying_party);
    check_frontend_length(&dapps.issuer);

    let id_alias_principal = get_id_alias_principal(identity_number, &dapps);
    let rp_tuple = AliasTuple {
        id_alias: id_alias_principal,
        id_dapp: delegation::get_principal(identity_number, dapps.relying_party.clone()),
    };
    let issuer_tuple = AliasTuple {
        id_alias: id_alias_principal,
        id_dapp: delegation::get_principal(identity_number, dapps.issuer.clone()),
    };
    let seed = calculate_id_alias_seed(identity_number, &dapps);

    state::signature_map_mut(|sigs| {
        add_signature(sigs, rp_tuple, seed);
        add_signature(sigs, issuer_tuple, seed);
    });
    update_root_hash();
    ByteBuf::from(delegation::der_encode_canister_sig_key(seed.to_vec()))
}

pub fn get_id_alias(identity_number: IdentityNumber, dapps: InvolvedDapps) -> GetIdAliasResponse {
    check_frontend_length(&dapps.relying_party);
    check_frontend_length(&dapps.issuer);

    state::assets_and_signatures(|cert_assets, sigs| {
        let id_alias_principal = get_id_alias_principal(identity_number, &dapps);
        let seed = calculate_id_alias_seed(identity_number, &dapps);
        let rp_tuple = AliasTuple {
            id_alias: id_alias_principal,
            id_dapp: delegation::get_principal(identity_number, dapps.relying_party.clone()),
        };
        let issuer_tuple = AliasTuple {
            id_alias: id_alias_principal,
            id_dapp: delegation::get_principal(identity_number, dapps.issuer.clone()),
        };
        let maybe_sig = get_signature(cert_assets, sigs, seed, &rp_tuple);
        let rp_sig = if let Some(sig) = maybe_sig {
            sig
        } else {
            return GetIdAliasResponse::NoSuchCredentials("rp_sig not found".to_string());
        };

        let maybe_sig = get_signature(cert_assets, sigs, seed, &issuer_tuple);
        let issuer_sig = if let Some(sig) = maybe_sig {
            sig
        } else {
            return GetIdAliasResponse::NoSuchCredentials("issuer_sig not found".to_string());
        };
        GetIdAliasResponse::Ok(IdAliasCredentials {
            rp_id_alias_credential: SignedIdAlias {
                id_alias: rp_tuple.id_alias,
                id_dapp: rp_tuple.id_dapp,
                signature: Signature::from(rp_sig),
            },
            issuer_id_alias_credential: SignedIdAlias {
                id_alias: issuer_tuple.id_alias,
                id_dapp: issuer_tuple.id_dapp,
                signature: Signature::from(issuer_sig),
            },
        })
    })
}

fn get_signature(
    cert_assets: &CertifiedAssets,
    sigs: &SignatureMap,
    seed: Hash,
    alias_tuple: &AliasTuple,
) -> Option<Vec<u8>> {
    let certificate = data_certificate().unwrap_or_else(|| {
        trap("data certificate is only available in query calls");
    });
    let msg_hash = id_alias_signature_msg_hash(alias_tuple);
    let witness = sigs.witness(hash::hash_bytes(seed), msg_hash)?;

    let witness_hash = witness.reconstruct();
    let root_hash = sigs.root_hash();
    if witness_hash != root_hash {
        trap(&format!(
            "internal error: signature map computed an invalid hash tree, witness hash is {}, root hash is {}",
            hex::encode(witness_hash),
            hex::encode(root_hash)
        ));
    }

    let tree = ic_certified_map::fork(
        HashTree::Pruned(cert_assets.root_hash()),
        ic_certified_map::labeled(LABEL_SIG, witness),
    );

    #[derive(Serialize)]
    struct Sig<'a> {
        certificate: ByteBuf,
        tree: HashTree<'a>,
    }

    let sig = Sig {
        certificate: ByteBuf::from(certificate),
        tree,
    };

    let mut cbor = serde_cbor::ser::Serializer::new(Vec::new());
    cbor.self_describe().unwrap();
    sig.serialize(&mut cbor).unwrap();
    Some(cbor.into_inner())
}

struct AliasTuple {
    id_alias: Principal,
    id_dapp: Principal,
}

fn add_signature(sigs: &mut SignatureMap, alias_tuple: AliasTuple, seed: Hash) {
    let msg_hash = id_alias_signature_msg_hash(&alias_tuple);
    let expires_at = time().saturating_add(SIGNATURE_EXPIRATION_PERIOD_NS);
    sigs.put(hash::hash_bytes(seed), msg_hash, expires_at);
}

fn get_id_alias_principal(identity_number: AnchorNumber, dapps: &InvolvedDapps) -> Principal {
    check_frontend_length(&dapps.relying_party);
    check_frontend_length(&dapps.issuer);

    let seed = calculate_id_alias_seed(identity_number, dapps);
    let public_key = delegation::der_encode_canister_sig_key(seed.to_vec());
    Principal::self_authenticating(public_key)
}

fn calculate_id_alias_seed(identity_number: AnchorNumber, dapps: &InvolvedDapps) -> Hash {
    let salt = state::salt();

    let mut blob: Vec<u8> = vec![];
    blob.push(salt.len() as u8);
    blob.extend_from_slice(&salt);

    let identity_number_str = identity_number.to_string();
    let identity_number_blob = identity_number_str.bytes();
    blob.push(identity_number_blob.len() as u8);
    blob.extend(identity_number_blob);

    blob.push(dapps.relying_party.bytes().len() as u8);
    blob.extend(dapps.relying_party.bytes());

    blob.push(dapps.issuer.bytes().len() as u8);
    blob.extend(dapps.issuer.bytes());

    hash::hash_bytes(blob)
}

fn id_alias_signature_msg_hash(alias_tuple: &AliasTuple) -> Hash {
    use hash::Value;

    let mut m = HashMap::new();
    m.insert("id_alias", Value::Bytes(alias_tuple.id_alias.as_slice()));
    m.insert("id_dapp", Value::Bytes(alias_tuple.id_dapp.as_slice()));
    let map_hash = hash::hash_of_map(m);
    hash::hash_with_domain(b"ic-id-alias", &map_hash)
}
