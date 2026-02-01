use crate::ii_domain::IIDomain;
use crate::stats::event_stats::{
    update_event_based_stats, Event, EventData, PrepareDelegationEvent,
};
use crate::{state, DAY_NS, MINUTE_NS};
use candid::Principal;
use ic_canister_sig_creation::signature_map::{CanisterSigInputs, SignatureMap};
use ic_canister_sig_creation::{
    delegation_signature_msg, CanisterSigPublicKey, DELEGATION_SIG_DOMAIN,
};
use ic_cdk::{id, trap};
use ic_certification::Hash;
use internet_identity_interface::internet_identity::types::*;
use sha2::{Digest, Sha256};
use std::net::IpAddr;

// The expiration used for delegations if none is specified
// (calculated as now() + this)
pub const DEFAULT_EXPIRATION_PERIOD_NS: u64 = 30 * MINUTE_NS;

// The maximum expiration time for delegation
// (calculated as now() + this)
pub const MAX_EXPIRATION_PERIOD_NS: u64 = 30 * DAY_NS;

// The prefix used in all account seed calculations to avoid collisions
// with the primary acount seed calculations based on anchor number.
const ACCOUNT_SEED_PREFIX: &str = "<account>";

/// Update metrics and the list of latest front-end origins.
pub fn delegation_bookkeeping(
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

pub fn get_principal(anchor_number: AnchorNumber, frontend: FrontendHostname) -> Principal {
    check_frontend_length(&frontend);

    let seed = calculate_anchor_seed(anchor_number, &frontend);
    let public_key = der_encode_canister_sig_key(seed.to_vec());
    Principal::self_authenticating(public_key)
}

pub fn calculate_anchor_seed(anchor_number: AnchorNumber, frontend: &FrontendHostname) -> Hash {
    let salt = state::salt();

    let mut blob: Vec<u8> = vec![];
    blob.push(salt.len() as u8);
    blob.extend_from_slice(&salt);

    let anchor_number_str = anchor_number.to_string();
    let anchor_number_blob = anchor_number_str.bytes();
    blob.push(anchor_number_blob.len() as u8);
    blob.extend(anchor_number_blob);

    blob.push(frontend.len() as u8);
    blob.extend(frontend.bytes());

    hash_bytes(blob)
}

/// Calculate a seed only from an `AccountNumber` and `FrontendHostname`.
/// This is only called when we're not dealing with a default account.
/// The anchor number is not included because accounts are not tied to specific anchors.
pub fn calculate_account_seed(account_number: AccountNumber, frontend: &FrontendHostname) -> Hash {
    let salt = state::salt();

    let mut blob: Vec<u8> = vec![];
    blob.push(salt.len() as u8);
    blob.extend_from_slice(&salt);

    blob.push(ACCOUNT_SEED_PREFIX.len() as u8);
    blob.extend(ACCOUNT_SEED_PREFIX.bytes());

    let account_number_str = account_number.to_string();
    let account_number_blob = account_number_str.bytes();
    blob.push(account_number_blob.len() as u8);
    blob.extend(account_number_blob);

    blob.push(frontend.len() as u8);
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

pub fn add_delegation_signature(
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
