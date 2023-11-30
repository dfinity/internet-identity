use crate::delegation::check_frontend_length;
use crate::{delegation, hash, state, update_root_hash, LABEL_SIG, MINUTE_NS};
use candid::Principal;
use canister_sig_util::CanisterSigPublicKey;
use ic_cdk::api::{data_certificate, time};
use ic_cdk::trap;
use ic_certification::{fork, labeled, pruned, Hash, HashTree};
use identity_core::common::{Timestamp, Url};
use identity_core::convert::FromJson;
use identity_credential::credential::{Credential, CredentialBuilder, Subject};

use asset_util::CertifiedAssets;
use canister_sig_util::signature_map::SignatureMap;
use internet_identity_interface::internet_identity::types::vc_mvp::{
    GetIdAliasResponse, IdAliasCredentials, PreparedIdAlias, SignedIdAlias,
};
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, FrontendHostname, IdentityNumber,
};
use serde::Serialize;
use serde_bytes::ByteBuf;
use serde_json::json;
use vc_util::{
    did_for_principal, vc_jwt_to_jws, vc_signing_input, vc_signing_input_hash, AliasTuple,
    II_CREDENTIAL_URL_PREFIX, II_ISSUER_URL,
};

// The expiration used for signatures.
#[allow(clippy::identity_op)]
const SIGNATURE_EXPIRATION_PERIOD_NS: u64 = 1 * MINUTE_NS;

// The expiration of id_alias verfiable credentials.
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

    let seed = calculate_id_alias_seed(identity_number, &dapps);
    let canister_sig_pk = CanisterSigPublicKey::new(ic_cdk::id(), seed.to_vec());
    let id_alias_principal = Principal::self_authenticating(canister_sig_pk.to_der());

    let rp_tuple = AliasTuple {
        id_alias: id_alias_principal,
        id_dapp: delegation::get_principal(identity_number, dapps.relying_party.clone()),
    };
    let issuer_tuple = AliasTuple {
        id_alias: id_alias_principal,
        id_dapp: delegation::get_principal(identity_number, dapps.issuer.clone()),
    };

    let rp_id_alias_jwt = prepare_id_alias_jwt(&rp_tuple);
    let issuer_id_alias_jwt = prepare_id_alias_jwt(&issuer_tuple);

    state::signature_map_mut(|sigs| {
        add_signature(
            sigs,
            vc_jwt_signing_input_hash(&rp_id_alias_jwt, &canister_sig_pk),
            seed,
        );
        add_signature(
            sigs,
            vc_jwt_signing_input_hash(&issuer_id_alias_jwt, &canister_sig_pk),
            seed,
        );
    });
    update_root_hash();
    PreparedIdAlias {
        canister_sig_pk_der: ByteBuf::from(canister_sig_pk.to_der()),
        rp_id_alias_jwt,
        issuer_id_alias_jwt,
    }
}

fn vc_jwt_signing_input_hash(credential_jwt: &str, canister_sig_pk: &CanisterSigPublicKey) -> Hash {
    let signing_input =
        vc_signing_input(credential_jwt, canister_sig_pk).expect("failed getting signing_input");
    vc_signing_input_hash(&signing_input)
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
        let seed = calculate_id_alias_seed(identity_number, &dapps);
        let canister_sig_pk = CanisterSigPublicKey::new(ic_cdk::id(), seed.to_vec());
        let id_alias_principal = Principal::self_authenticating(canister_sig_pk.to_der());
        let id_rp = delegation::get_principal(identity_number, dapps.relying_party.clone());
        let id_issuer = delegation::get_principal(identity_number, dapps.issuer.clone());

        let signing_input = vc_signing_input(rp_id_alias_jwt, &canister_sig_pk)
            .expect("failed getting signing_input");
        let msg_hash = vc_signing_input_hash(&signing_input);
        let Some(rp_sig) = get_signature(cert_assets, sigs, seed, msg_hash) else {
            return GetIdAliasResponse::NoSuchCredentials("rp_sig not found".to_string());
        };
        let rp_jws = vc_jwt_to_jws(rp_id_alias_jwt, &canister_sig_pk, &rp_sig)
            .expect("failed constructing JWS");

        let signing_input = vc_signing_input(issuer_id_alias_jwt, &canister_sig_pk)
            .expect("failed getting signing_input");
        let msg_hash = vc_signing_input_hash(&signing_input);
        let Some(issuer_sig) = get_signature(cert_assets, sigs, seed, msg_hash) else {
            return GetIdAliasResponse::NoSuchCredentials("issuer_sig not found".to_string());
        };
        let issuer_jws = vc_jwt_to_jws(issuer_id_alias_jwt, &canister_sig_pk, &issuer_sig)
            .expect("failed constructing JWS");

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

    let witness_hash = witness.digest();
    let root_hash = sigs.root_hash();
    if witness_hash != root_hash {
        trap(&format!(
            "internal error: signature map computed an invalid hash tree, witness hash is {}, root hash is {}",
            hex::encode(witness_hash),
            hex::encode(root_hash)
        ));
    }

    let tree = fork(pruned(cert_assets.root_hash()), labeled(LABEL_SIG, witness));

    #[derive(Serialize)]
    struct Sig {
        certificate: ByteBuf,
        tree: HashTree,
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

fn add_signature(sigs: &mut SignatureMap, msg_hash: Hash, seed: Hash) {
    let expires_at = time().saturating_add(SIGNATURE_EXPIRATION_PERIOD_NS);
    sigs.put(hash::hash_bytes(seed), msg_hash, expires_at);
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

fn id_alias_credential(alias_tuple: &AliasTuple) -> Credential {
    let subject: Subject = Subject::from_json_value(json!({
        "id": did_for_principal(alias_tuple.id_dapp),
        "has_id_alias": did_for_principal(alias_tuple.id_alias),
    }))
    .expect("internal: failed building id_alias subject");
    let exp_timestamp_sec =
        Timestamp::from_unix(((time() + ID_ALIAS_VC_EXPIRATION_PERIOD_NS) / 1_000_000_000) as i64)
            .expect("internal: failed computing expiration timestamp");

    let credential: Credential = CredentialBuilder::default()
        .id(prepare_credential_id())
        .issuer(Url::parse(II_ISSUER_URL).expect("internal: bad issuer url"))
        .type_("InternetIdentityIdAlias")
        .subject(subject)
        .expiration_date(exp_timestamp_sec)
        .build()
        .expect("internal: failed building id_alias credential");
    credential
}

fn prepare_credential_id() -> Url {
    let url = Url::parse(II_CREDENTIAL_URL_PREFIX).expect("internal: bad credential id base url");
    url.join(time().to_string())
        .expect("internal: bad credential id extension")
}

fn prepare_id_alias_jwt(alias_tuple: &AliasTuple) -> String {
    let credential = id_alias_credential(alias_tuple);
    credential
        .serialize_jwt()
        .expect("internal: JWT serialization failure")
}
