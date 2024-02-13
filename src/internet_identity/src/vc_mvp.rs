use crate::delegation::check_frontend_length;
use crate::{delegation, random_salt, state, update_root_hash, MINUTE_NS};

use candid::Principal;
use canister_sig_util::CanisterSigPublicKey;
use ic_cdk::api::time;
use identity_core::common::{Timestamp, Url};
use identity_core::convert::FromJson;
use identity_credential::credential::{Credential, CredentialBuilder, Subject};
use identity_jose::jws::{CompactJwsEncoder, Decoder};
use internet_identity_interface::internet_identity::types::vc_mvp::{
    GetIdAliasError, IdAliasCredentials, PreparedIdAlias, SignedIdAlias,
};
use internet_identity_interface::internet_identity::types::{FrontendHostname, IdentityNumber};
use serde_bytes::ByteBuf;
use serde_json::json;
use vc_util::{
    did_for_principal, get_canister_sig_pk_raw, vc_signing_input, vc_signing_input_hash,
    AliasTuple, II_CREDENTIAL_URL_PREFIX, II_ISSUER_URL,
};

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
    };
    let issuer_tuple = AliasTuple {
        id_alias: id_alias_principal,
        id_dapp: delegation::get_principal(identity_number, dapps.issuer.clone()),
    };

    let rp_signing_input = vc_signing_input(&prepare_id_alias_jwt(&rp_tuple), &canister_sig_pk)
        .expect("failed getting signing_input");
    let issuer_signing_input =
        vc_signing_input(&prepare_id_alias_jwt(&issuer_tuple), &canister_sig_pk)
            .expect("failed getting signing_input");
    state::signature_map_mut(|sigs| {
        sigs.add_signature(seed.as_ref(), vc_signing_input_hash(&rp_signing_input));
        sigs.add_signature(seed.as_ref(), vc_signing_input_hash(&issuer_signing_input));
    });
    update_root_hash();
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
        let canister_sig_pk = canister_sig_pk_from_signing_input(rp_id_alias_jwt)
            .map_err(GetIdAliasError::NoSuchCredentials)?;
        let seed = canister_sig_pk.seed.as_slice();

        let id_alias_principal = Principal::self_authenticating(canister_sig_pk.to_der());
        let id_rp = delegation::get_principal(identity_number, dapps.relying_party.clone());
        let id_issuer = delegation::get_principal(identity_number, dapps.issuer.clone());

        let rp_alias_msg_hash = vc_signing_input_hash(rp_id_alias_jwt.as_bytes());
        let rp_sig = sigs
            .get_signature_as_cbor(seed, rp_alias_msg_hash, Some(cert_assets.root_hash()))
            .map_err(|err| {
                GetIdAliasError::NoSuchCredentials(format!("rp_sig not found: {}", err))
            })?;
        let rp_jws =
            add_sig_to_signing_input(rp_id_alias_jwt, &rp_sig).expect("failed constructing rp JWS");

        let issuer_id_alias_msg_hash = vc_signing_input_hash(issuer_id_alias_jwt.as_bytes());
        let issuer_sig = sigs
            .get_signature_as_cbor(
                seed,
                issuer_id_alias_msg_hash,
                Some(cert_assets.root_hash()),
            )
            .map_err(|err| {
                GetIdAliasError::NoSuchCredentials(format!("issuer_sig not found: {}", err))
            })?;
        let issuer_jws = add_sig_to_signing_input(issuer_id_alias_jwt, &issuer_sig)
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

/// Parses the canister signature public key from the given signing_input.
fn canister_sig_pk_from_signing_input(signing_input: &str) -> Result<CanisterSigPublicKey, String> {
    let decoder = Decoder::new();
    let bytes_with_separators = [signing_input.as_bytes(), &[b'.']].concat();
    let parsed_signing_input = decoder
        .decode_compact_serialization(&bytes_with_separators, None)
        .map_err(|e| format!("internal: failed parsing signing_input: {:?}", e))?;
    let header = parsed_signing_input
        .protected_header()
        .expect("internal: failed getting protected header");
    let canister_sig_pk_raw = get_canister_sig_pk_raw(header)
        .map_err(|e| format!("internal: failed getting canister_sig_pk_raw: {:?}", e))?;
    CanisterSigPublicKey::try_from_raw(&canister_sig_pk_raw)
        .map_err(|e| format!("internal: failed parsing canister_sig_pk: {}", e))
}

/// Constructs and returns a JWS (a signed JWT) from the given components.
fn add_sig_to_signing_input(signing_input: &str, sig: &[u8]) -> Result<String, String> {
    let decoder = Decoder::new();
    let bytes_with_separators = [signing_input.as_bytes(), &[b'.']].concat();
    let parsed_signing_input = decoder
        .decode_compact_serialization(&bytes_with_separators, None)
        .unwrap();
    let header = parsed_signing_input
        .protected_header()
        .expect("internal: failed getting protected header");

    let encoder: CompactJwsEncoder = CompactJwsEncoder::new(parsed_signing_input.claims(), header)
        .map_err(|e| format!("internal: failed creating JWS encoder: {:?}", e))?;
    Ok(encoder.into_jws(sig))
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
