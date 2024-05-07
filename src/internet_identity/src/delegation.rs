use crate::ii_domain::IIDomain;
use crate::stats::event_stats::{
    update_event_based_stats, Event, EventData, PrepareDelegationEvent,
};
use crate::{hash, state, update_root_hash, DAY_NS, MINUTE_NS};
use candid::Principal;
use canister_sig_util::signature_map::SignatureMap;
use canister_sig_util::CanisterSigPublicKey;
use ic_cdk::api::time;
use ic_cdk::{id, trap};
use ic_certification::Hash;
use internet_identity_interface::internet_identity::types::*;
use serde_bytes::ByteBuf;
use std::collections::HashMap;
use std::net::IpAddr;

// The expiration used for delegations if none is specified
// (calculated as now() + this)
const DEFAULT_EXPIRATION_PERIOD_NS: u64 = 30 * MINUTE_NS;

// The maximum expiration time for delegation
// (calculated as now() + this)
const MAX_EXPIRATION_PERIOD_NS: u64 = 30 * DAY_NS;

pub async fn prepare_delegation(
    anchor_number: AnchorNumber,
    frontend: FrontendHostname,
    session_key: SessionKey,
    max_time_to_live: Option<u64>,
    ii_domain: &Option<IIDomain>,
) -> (UserKey, Timestamp) {
    state::ensure_salt_set().await;
    check_frontend_length(&frontend);

    let session_duration_ns = u64::min(
        max_time_to_live.unwrap_or(DEFAULT_EXPIRATION_PERIOD_NS),
        MAX_EXPIRATION_PERIOD_NS,
    );
    let expiration = time().saturating_add(session_duration_ns);
    let seed = calculate_seed(anchor_number, &frontend);

    state::signature_map_mut(|sigs| {
        add_delegation_signature(sigs, session_key, seed.as_ref(), expiration);
    });
    update_root_hash();

    delegation_bookkeeping(frontend, ii_domain.clone(), session_duration_ns);

    (
        ByteBuf::from(der_encode_canister_sig_key(seed.to_vec())),
        expiration,
    )
}

/// Update metrics and the list of latest front-end origins.
fn delegation_bookkeeping(
    frontend: FrontendHostname,
    ii_domain: Option<IIDomain>,
    session_duration_ns: u64,
) {
    state::usage_metrics_mut(|metrics| {
        metrics.delegation_counter += 1;
    });
    if !is_dev_frontend(&frontend) {
        update_event_based_stats(EventData {
            event: Event::PrepareDelegation(PrepareDelegationEvent {
                ii_domain: ii_domain.clone(),
                frontend: frontend.clone(),
                session_duration_ns,
            }),
        });
    }
}

/// Filter out derivation origins that most likely point to development setups.
/// This is not bulletproof but given the data we collected so far it should be good for now.
fn is_dev_frontend(frontend: &FrontendHostname) -> bool {
    if frontend.starts_with("http://") || frontend.contains("localhost") {
        // we don't care about insecure origins or localhost
        return true;
    }

    // let's check for local IP addresses
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

pub fn get_delegation(
    anchor_number: AnchorNumber,
    frontend: FrontendHostname,
    session_key: SessionKey,
    expiration: Timestamp,
) -> GetDelegationResponse {
    check_frontend_length(&frontend);

    state::assets_and_signatures(|certified_assets, sigs| {
        let message_hash = delegation_signature_msg_hash(&Delegation {
            pubkey: session_key.clone(),
            expiration,
            targets: None,
        });
        match sigs.get_signature_as_cbor(
            &calculate_seed(anchor_number, &frontend),
            message_hash,
            Some(certified_assets.root_hash()),
        ) {
            Ok(signature) => GetDelegationResponse::SignedDelegation(SignedDelegation {
                delegation: Delegation {
                    pubkey: session_key,
                    expiration,
                    targets: None,
                },
                signature: ByteBuf::from(signature),
            }),
            Err(_) => GetDelegationResponse::NoSuchDelegation,
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
    let my_canister_id = id();
    CanisterSigPublicKey::new(my_canister_id, seed).to_der()
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

fn add_delegation_signature(
    sigs: &mut SignatureMap,
    pk: PublicKey,
    seed: &[u8],
    expiration: Timestamp,
) {
    let msg_hash = delegation_signature_msg_hash(&Delegation {
        pubkey: pk,
        expiration,
        targets: None,
    });
    sigs.add_signature(seed, msg_hash);
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
