use crate::assets::CertifiedAssets;
use crate::ii_domain::IIDomain;
use crate::state::persistent_state_mut;
use crate::{hash, state, update_root_hash, DAY_NS, LABEL_SIG, MINUTE_NS};
use candid::Principal;
use canister_sig_util::signature_map::SignatureMap;
use ic_cdk::api::{data_certificate, time};
use ic_cdk::{id, trap};
use ic_certified_map::{Hash, HashTree};
use internet_identity_interface::internet_identity::types::*;
use serde::Serialize;
use serde_bytes::ByteBuf;
use std::collections::HashMap;
use std::net::IpAddr;

// The expiration used for delegations if none is specified
// (calculated as now() + this)
const DEFAULT_EXPIRATION_PERIOD_NS: u64 = 30 * MINUTE_NS;

// The maximum expiration time for delegation
// (calculated as now() + this)
const MAX_EXPIRATION_PERIOD_NS: u64 = 30 * DAY_NS;

// The expiration used for signatures
#[allow(clippy::identity_op)]
const SIGNATURE_EXPIRATION_PERIOD_NS: u64 = 1 * MINUTE_NS;

pub async fn prepare_delegation(
    anchor_number: AnchorNumber,
    frontend: FrontendHostname,
    session_key: SessionKey,
    max_time_to_live: Option<u64>,
    ii_domain: &Option<IIDomain>,
) -> (UserKey, Timestamp) {
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

    delegation_bookkeeping(frontend, ii_domain);

    (
        ByteBuf::from(der_encode_canister_sig_key(seed.to_vec())),
        expiration,
    )
}

/// Update metrics and the list of latest front-end origins.
fn delegation_bookkeeping(frontend: FrontendHostname, ii_domain: &Option<IIDomain>) {
    state::usage_metrics_mut(|metrics| {
        metrics.delegation_counter += 1;
    });
    if ii_domain.is_some() && !is_dev_frontend(&frontend) {
        update_latest_delegation_origins(frontend);
    }
}

/// Filter out derivation origins that most likely point to development setups.
/// This is not bullet proof but given the data we collected so far it should be good for now.
fn is_dev_frontend(frontend: &FrontendHostname) -> bool {
    if frontend.starts_with("http://") || frontend.contains("localhost") {
        // we don't care about insecure origins or localhost
        return true;
    }

    // lets check for local IP addresses
    if let Some(hostname) = frontend
        .strip_prefix("https://")
        .and_then(|s| s.split(':').next())
    {
        return match hostname.parse::<IpAddr>() {
            Ok(IpAddr::V4(addr)) => addr.is_private() || addr.is_loopback(),
            Ok(IpAddr::V6(addr)) => addr.is_loopback(),
            Err(_) => false,
        };
    }
    false
}

/// Add the current front-end to the list of latest used front-end origins.
fn update_latest_delegation_origins(frontend: FrontendHostname) {
    let now_ns = time();

    persistent_state_mut(|persistent_state| {
        let latest_delegation_origins = persistent_state
            .latest_delegation_origins
            .get_or_insert(HashMap::new());

        if let Some(timestamp_ns) = latest_delegation_origins.get_mut(&frontend) {
            *timestamp_ns = now_ns;
        } else {
            latest_delegation_origins.insert(frontend, now_ns);
        };

        // drop entries older than 30 days
        latest_delegation_origins.retain(|_, timestamp_ns| now_ns - *timestamp_ns < 30 * DAY_NS);

        // if we still have too many entries, drop the oldest
        if latest_delegation_origins.len() as u64
            > persistent_state.max_num_latest_delegation_origins.unwrap()
        {
            // if this case is hit often (i.e. we routinely have more than 1000 entries), we should
            // consider using a more efficient data structure
            let mut values: Vec<_> = latest_delegation_origins.clone().into_iter().collect();
            values.sort_by(|(_, timestamp_1), (_, timestamp_2)| timestamp_1.cmp(timestamp_2));
            latest_delegation_origins.remove(&values[0].0);
        };
    });
}

pub fn get_delegation(
    anchor_number: AnchorNumber,
    frontend: FrontendHostname,
    session_key: SessionKey,
    expiration: Timestamp,
) -> GetDelegationResponse {
    check_frontend_length(&frontend);

    state::assets_and_signatures(|asset_hashes, sigs| {
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

pub(crate) fn der_encode_canister_sig_key(seed: Vec<u8>) -> Vec<u8> {
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
    assets: &CertifiedAssets,
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
        HashTree::Pruned(assets.root_hash()),
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
    let expires_at = time().saturating_add(SIGNATURE_EXPIRATION_PERIOD_NS);
    sigs.put(hash::hash_bytes(seed), msg_hash, expires_at);
}

/// Removes a batch of expired signatures from the signature map.
///
/// This function piggy-backs on update calls that create new signatures to
/// amortize the cost of tree pruning. Each operation on the signature map
/// will prune at most MAX_SIGS_TO_PRUNE other signatures.
pub fn prune_expired_signatures() {
    const MAX_SIGS_TO_PRUNE: usize = 50;
    let num_pruned = state::signature_map_mut(|sigs| sigs.prune_expired(time(), MAX_SIGS_TO_PRUNE));
    if num_pruned > 0 {
        update_root_hash();
    }
}

pub(crate) fn check_frontend_length(frontend: &FrontendHostname) {
    const FRONTEND_HOSTNAME_LIMIT: usize = 255;

    let n = frontend.len();
    if frontend.len() > FRONTEND_HOSTNAME_LIMIT {
        trap(&format!(
            "frontend hostname {n} exceeds the limit of {FRONTEND_HOSTNAME_LIMIT} bytes",
        ));
    }
}
