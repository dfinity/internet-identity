use crate::ii_domain::IIDomain;
use crate::stats::event_stats::{
    update_event_based_stats, Event, EventData, PrepareDelegationEvent,
};
use crate::{state, update_root_hash, DAY_NS, MINUTE_NS};
use candid::Principal;
use ic_canister_sig_creation::signature_map::{CanisterSigInputs, SignatureMap};
use ic_canister_sig_creation::{
    delegation_signature_msg, CanisterSigPublicKey, DELEGATION_SIG_DOMAIN,
};
use ic_cdk::api::time;
use ic_cdk::{id, trap};
use ic_certification::Hash;
use internet_identity_interface::internet_identity::types::*;
use serde_bytes::ByteBuf;
use sha2::{Digest, Sha256};
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
        let inputs = CanisterSigInputs {
            domain: DELEGATION_SIG_DOMAIN,
            seed: &calculate_seed(anchor_number, &frontend),
            message: &delegation_signature_msg(&session_key, expiration, None),
        };
        match sigs.get_signature_as_cbor(&inputs, Some(certified_assets.root_hash())) {
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

    hash_bytes(blob)
}

fn hash_bytes(value: impl AsRef<[u8]>) -> Hash {
    let mut hasher = Sha256::new();
    hasher.update(value.as_ref());
    hasher.finalize().into()
}

pub(crate) fn der_encode_canister_sig_key(seed: Vec<u8>) -> Vec<u8> {
    let my_canister_id = id();
    CanisterSigPublicKey::new(my_canister_id, seed).to_der()
}

fn add_delegation_signature(
    sigs: &mut SignatureMap,
    pk: PublicKey,
    seed: &[u8],
    expiration: Timestamp,
) {
    let inputs = CanisterSigInputs {
        domain: DELEGATION_SIG_DOMAIN,
        seed,
        message: &delegation_signature_msg(&pk, expiration, None),
    };
    sigs.add_signature(&inputs);
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
