use crate::state::AssetHashes;
use crate::{
    hash, secs_to_nanos, state, trap_if_not_authenticated, update_root_hash, LABEL_ASSETS,
    LABEL_SIG,
};
use candid::Principal;
use ic_cdk::api::{data_certificate, time};
use ic_cdk::{id, trap};
use ic_certified_map::AsHashTree;
use ic_certified_map::{Hash, HashTree};
use internet_identity::signature_map::SignatureMap;
use internet_identity_interface::*;
use serde::Serialize;
use serde_bytes::ByteBuf;
use std::collections::HashMap;

// 30 mins
const DEFAULT_EXPIRATION_PERIOD_NS: u64 = secs_to_nanos(30 * 60);
// 30 days
const MAX_EXPIRATION_PERIOD_NS: u64 = secs_to_nanos(30 * 24 * 60 * 60);
// 1 min
const DEFAULT_SIGNATURE_EXPIRATION_PERIOD_NS: u64 = secs_to_nanos(60);

pub async fn prepare_delegation(
    anchor_number: AnchorNumber,
    frontend: FrontendHostname,
    session_key: SessionKey,
    max_time_to_live: Option<u64>,
) -> (UserKey, Timestamp) {
    // must be called before the first await because it requires caller()
    trap_if_not_authenticated(&state::anchor(anchor_number));

    state::ensure_salt_set().await;
    prune_expired_signatures();
    check_frontend_length(&frontend);

    let delta = u64::min(
        max_time_to_live.unwrap_or(DEFAULT_EXPIRATION_PERIOD_NS),
        MAX_EXPIRATION_PERIOD_NS,
    );
    let expiration = time().saturating_add(delta);
    let seed = calculate_seed(anchor_number, &frontend);

    state::signature_map_mut(|sigs| {
        add_signature(sigs, session_key, seed, expiration);
    });
    update_root_hash();

    state::usage_metrics_mut(|metrics| {
        metrics.delegation_counter += 1;
    });
    (
        ByteBuf::from(der_encode_canister_sig_key(seed.to_vec())),
        expiration,
    )
}

pub fn get_delegation(
    anchor_number: AnchorNumber,
    frontend: FrontendHostname,
    session_key: SessionKey,
    expiration: Timestamp,
) -> GetDelegationResponse {
    check_frontend_length(&frontend);
    trap_if_not_authenticated(&state::anchor(anchor_number));

    state::asset_hashes_and_sigs(|asset_hashes, sigs| {
        match get_signature(
            asset_hashes,
            sigs,
            session_key.clone(),
            calculate_seed(anchor_number, &frontend),
            expiration,
        ) {
            Some(signature) => GetDelegationResponse::SignedDelegation(SignedDelegation {
                delegation: Delegation {
                    pubkey: session_key,
                    expiration,
                    targets: None,
                },
                signature: ByteBuf::from(signature),
            }),
            None => GetDelegationResponse::NoSuchDelegation,
        }
    })
}

pub fn get_principal(anchor_number: AnchorNumber, frontend: FrontendHostname) -> Principal {
    check_frontend_length(&frontend);

    trap_if_not_authenticated(&state::anchor(anchor_number));

    let seed = calculate_seed(anchor_number, &frontend);
    let public_key = der_encode_canister_sig_key(seed.to_vec());
    Principal::self_authenticating(public_key)
}

fn calculate_seed(anchor_number: AnchorNumber, frontend: &FrontendHostname) -> Hash {
    let salt = state::salt();

    let mut blob: Vec<u8> = vec![];
    blob.push(salt.len() as u8);
    blob.extend_from_slice(&salt);

    let anchor_number_str = anchor_number.to_string();
    let anchor_number_blob = anchor_number_str.bytes();
    blob.push(anchor_number_blob.len() as u8);
    blob.extend(anchor_number_blob);

    blob.push(frontend.bytes().len() as u8);
    blob.extend(frontend.bytes());

    hash::hash_bytes(blob)
}

fn der_encode_canister_sig_key(seed: Vec<u8>) -> Vec<u8> {
    let my_canister_id: Vec<u8> = id().as_ref().to_vec();

    let mut bitstring: Vec<u8> = vec![];
    bitstring.push(my_canister_id.len() as u8);
    bitstring.extend(my_canister_id);
    bitstring.extend(seed);

    let mut der: Vec<u8> = vec![];
    // sequence of length 17 + the bit string length
    der.push(0x30);
    der.push(17 + bitstring.len() as u8);
    der.extend(vec![
        // sequence of length 12 for the OID
        0x30, 0x0C, // OID 1.3.6.1.4.1.56387.1.2
        0x06, 0x0A, 0x2B, 0x06, 0x01, 0x04, 0x01, 0x83, 0xB8, 0x43, 0x01, 0x02,
    ]);
    // BIT string of given length
    der.push(0x03);
    der.push(1 + bitstring.len() as u8);
    der.push(0x00);
    der.extend(bitstring);
    der
}

fn delegation_signature_msg_hash(d: &Delegation) -> Hash {
    use hash::Value;

    let mut m = HashMap::new();
    m.insert("pubkey", Value::Bytes(d.pubkey.as_slice()));
    m.insert("expiration", Value::U64(d.expiration));
    if let Some(targets) = d.targets.as_ref() {
        let mut arr = Vec::with_capacity(targets.len());
        for t in targets.iter() {
            arr.push(Value::Bytes(t.as_ref()));
        }
        m.insert("targets", Value::Array(arr));
    }
    let map_hash = hash::hash_of_map(m);
    hash::hash_with_domain(b"ic-request-auth-delegation", &map_hash)
}

fn get_signature(
    asset_hashes: &AssetHashes,
    sigs: &SignatureMap,
    pk: PublicKey,
    seed: Hash,
    expiration: Timestamp,
) -> Option<Vec<u8>> {
    let certificate = data_certificate().unwrap_or_else(|| {
        trap("data certificate is only available in query calls");
    });
    let msg_hash = delegation_signature_msg_hash(&Delegation {
        pubkey: pk,
        expiration,
        targets: None,
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
        HashTree::Pruned(ic_certified_map::labeled_hash(
            LABEL_ASSETS,
            &asset_hashes.root_hash(),
        )),
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

fn add_signature(sigs: &mut SignatureMap, pk: PublicKey, seed: Hash, expiration: Timestamp) {
    let msg_hash = delegation_signature_msg_hash(&Delegation {
        pubkey: pk,
        expiration,
        targets: None,
    });
    let expires_at = time().saturating_add(DEFAULT_SIGNATURE_EXPIRATION_PERIOD_NS);
    sigs.put(hash::hash_bytes(seed), msg_hash, expires_at);
}

/// Removes a batch of expired signatures from the signature map.
///
/// This function is supposed to piggy back on update calls to
/// amortize the cost of tree pruning.  Each operation on the signature map
/// will prune at most MAX_SIGS_TO_PRUNE other signatures.
pub fn prune_expired_signatures() {
    const MAX_SIGS_TO_PRUNE: usize = 10;
    let num_pruned = state::signature_map_mut(|sigs| sigs.prune_expired(time(), MAX_SIGS_TO_PRUNE));
    if num_pruned > 0 {
        update_root_hash();
    }
}

fn check_frontend_length(frontend: &FrontendHostname) {
    const FRONTEND_HOSTNAME_LIMIT: usize = 255;

    let n = frontend.len();
    if frontend.len() > FRONTEND_HOSTNAME_LIMIT {
        trap(&format!(
            "frontend hostname {} exceeds the limit of {} bytes",
            n, FRONTEND_HOSTNAME_LIMIT,
        ));
    }
}
