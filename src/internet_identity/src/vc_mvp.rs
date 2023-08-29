use crate::assets::CertifiedAssets;
use crate::delegation::check_frontend_length;
use crate::{delegation, hash, state, update_root_hash, LABEL_SIG, MINUTE_NS};
use candid::Principal;
use ic_cdk::api::{data_certificate, time};
use ic_cdk::trap;
use ic_certified_map::{Hash, HashTree};
use identity_core::common::Url;
use identity_core::convert::FromJson;
use identity_credential::credential::{Credential, CredentialBuilder, Subject};
use identity_jose::jws::{CompactJwsEncoder, JwsAlgorithm, JwsHeader};

use internet_identity::signature_map::SignatureMap;
use internet_identity_interface::internet_identity::types::vc_mvp::{
    GetIdAliasResponse, IdAliasCredentials, PreparedIdAlias, SignedIdAlias,
};
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, FrontendHostname, IdentityNumber,
};
use serde::Serialize;
use serde_bytes::ByteBuf;
use serde_json::json;
use sha2::{Digest, Sha256};

// The expiration used for signatures
#[allow(clippy::identity_op)]
const SIGNATURE_EXPIRATION_PERIOD_NS: u64 = 1 * MINUTE_NS;
const II_CREDENTIAL_URL: &str = "https://internetcomputer.org/credential/internet-idenity";
const II_ISSUER_URL: &str = "https://internetcomputer.org/issuers/internet-idenity";

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

    let id_alias_principal = get_id_alias_principal(identity_number, &dapps);

    let rp_tuple = AliasTuple {
        id_alias: id_alias_principal,
        id_dapp: delegation::get_principal(identity_number, dapps.relying_party.clone()),
    };
    let issuer_tuple = AliasTuple {
        id_alias: id_alias_principal,
        id_dapp: delegation::get_principal(identity_number, dapps.issuer.clone()),
    };

    let (rp_id_alias_jwt, rp_msg_hash) = id_alias_jwt_and_msg_hash(&rp_tuple);
    let (issuer_id_alias_jwt, issuer_msg_hash) = id_alias_jwt_and_msg_hash(&issuer_tuple);

    let seed = calculate_id_alias_seed(identity_number, &dapps);

    state::signature_map_mut(|sigs| {
        add_signature(sigs, rp_msg_hash, seed);
        add_signature(sigs, issuer_msg_hash, seed);
    });
    update_root_hash();
    let canister_sig_pk = ByteBuf::from(delegation::der_encode_canister_sig_key(seed.to_vec()));
    PreparedIdAlias {
        canister_sig_pk,
        rp_id_alias_jwt,
        issuer_id_alias_jwt,
    }
}

fn id_alias_jwt_and_msg_hash(alias_tuple: &AliasTuple) -> (String, Hash) {
    let credential_jwt = prepare_id_alias_jwt(alias_tuple);
    let msg_hash = id_alias_signing_input_hash(&jwt_signing_input(&credential_jwt));
    (credential_jwt, msg_hash)
}

pub fn get_id_alias(
    identity_number: IdentityNumber,
    dapps: InvolvedDapps,
    rp_id_alias_jwt: &str,
    issuer_id_alias_jwt: &str,
) -> GetIdAliasResponse {
    check_frontend_length(&dapps.relying_party);
    check_frontend_length(&dapps.issuer);

    state::assets_and_signatures(|cert_assets, sigs| {
        let id_alias_principal = get_id_alias_principal(identity_number, &dapps);
        let seed = calculate_id_alias_seed(identity_number, &dapps);
        let id_rp = delegation::get_principal(identity_number, dapps.relying_party.clone());
        let id_issuer = delegation::get_principal(identity_number, dapps.issuer.clone());

        let encoder = jws_encoder(rp_id_alias_jwt);
        let msg_hash = id_alias_signing_input_hash(encoder.signing_input());
        let maybe_sig = get_signature(cert_assets, sigs, seed, msg_hash);
        let rp_sig = if let Some(sig) = maybe_sig {
            sig
        } else {
            return GetIdAliasResponse::NoSuchCredentials("rp_sig not found".to_string());
        };
        let rp_jws = encoder.into_jws(&rp_sig);

        let encoder = jws_encoder(issuer_id_alias_jwt);
        let msg_hash = id_alias_signing_input_hash(encoder.signing_input());
        let maybe_sig = get_signature(cert_assets, sigs, seed, msg_hash);
        let issuer_sig = if let Some(sig) = maybe_sig {
            sig
        } else {
            return GetIdAliasResponse::NoSuchCredentials("issuer_sig not found".to_string());
        };
        let issuer_jws = encoder.into_jws(&issuer_sig);

        GetIdAliasResponse::Ok(IdAliasCredentials {
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

fn get_signature(
    cert_assets: &CertifiedAssets,
    sigs: &SignatureMap,
    seed: Hash,
    msg_hash: Hash,
) -> Option<Vec<u8>> {
    let certificate = data_certificate().unwrap_or_else(|| {
        trap("data certificate is only available in query calls");
    });
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

fn add_signature(sigs: &mut SignatureMap, msg_hash: Hash, seed: Hash) {
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

fn did_for_principal(principal: &Principal) -> String {
    let mut did = String::from("did:web:");
    did.push_str(&principal.to_text());
    did
}

fn id_alias_credential(alias_tuple: &AliasTuple) -> Credential {
    let subject: Subject = Subject::from_json_value(json!({
        "id": did_for_principal(&alias_tuple.id_dapp),
        "has_id_alias": did_for_principal(&alias_tuple.id_alias),
    }))
    .expect("internal: failed building id_alias subject");

    let credential: Credential = CredentialBuilder::default()
        .id(Url::parse(II_CREDENTIAL_URL).expect("internal: bad credential id"))
        .issuer(Url::parse(II_ISSUER_URL).expect("internal: bad issuer url"))
        .type_("InternetIdentityIdAlias")
        .subject(subject)
        .build()
        .expect("internal: failed building id_alias credential");
    credential
}

fn prepare_id_alias_jwt(alias_tuple: &AliasTuple) -> String {
    let credential = id_alias_credential(alias_tuple);
    credential
        .serialize_jwt()
        .expect("internal: JWT serialization failure")
}

fn jws_encoder(credential_jwt: &str) -> CompactJwsEncoder {
    let mut header: JwsHeader = JwsHeader::new();
    header.set_alg(JwsAlgorithm::IcCs);
    let kid = "did:ic:ii-canister";
    header.set_kid(kid);

    // let claims_bytes: Vec<u8> = serde_json::to_vec(&claims).expect("internal error: JSON serialization failed");
    let encoder: CompactJwsEncoder<'_> = CompactJwsEncoder::new(credential_jwt.as_ref(), &header)
        .expect("internal error: JWS encoder failed");
    encoder
}

fn jwt_signing_input(credential_jwt: &str) -> Vec<u8> {
    let encoder = jws_encoder(credential_jwt);
    Vec::from(encoder.signing_input())
}

fn id_alias_signing_input_hash(signing_input: &[u8]) -> Hash {
    let sep = b"iccs_verifiable_credential";
    let mut hasher = Sha256::new();
    let buf = [sep.len() as u8];
    hasher.update(buf);
    hasher.update(sep);
    hasher.update(signing_input);
    hasher.finalize().into()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vc_mvp::AliasTuple;
    use canister_tests::framework::{principal_1, principal_2};
    use identity_jose::jws::Decoder;

    #[test]
    fn signing_input_encoder_decoder_test() {
        let alias_tuple = AliasTuple {
            id_alias: principal_1(),
            id_dapp: principal_2(),
        };
        let prepared_jwt = prepare_id_alias_jwt(&alias_tuple);
        let signing_input_1 = jwt_signing_input(&prepared_jwt);
        let input_hash_1 = id_alias_signing_input_hash(&signing_input_1);
        let encoder = jws_encoder(&prepared_jwt);
        let sig = vec![1, 2, 3, 4];
        let jws_bytes = encoder.into_jws(&sig);

        let decoder: Decoder = Decoder::new();
        let jws = decoder
            .decode_compact_serialization(jws_bytes.as_ref(), None)
            .expect("Failure decoding JWS credential");
        let signing_input_2 = jws.signing_input();
        let input_hash_2 = id_alias_signing_input_hash(&signing_input_2);
        assert_eq!(signing_input_1, signing_input_2);
        assert_eq!(input_hash_1, input_hash_2);
    }
}
