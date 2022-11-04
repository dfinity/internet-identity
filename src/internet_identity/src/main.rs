use crate::assets::init_assets;
use anchor_management::tentative_device_registration;
use assets::ContentType;
use candid::Principal;
use ic_cdk::api::{caller, set_certified_data, trap};
use ic_cdk_macros::{init, post_upgrade, query, update};
use ic_certified_map::AsHashTree;
use ic_stable_structures::DefaultMemoryImpl;
use storage::{Salt, Storage};

use internet_identity_interface::*;

mod anchor_management;
mod assets;
mod delegation;
mod hash;
mod http;
mod state;
mod storage;

const fn secs_to_nanos(secs: u64) -> u64 {
    secs * 1_000_000_000
}

const LABEL_ASSETS: &[u8] = b"http_assets";
const LABEL_SIG: &[u8] = b"sig";

#[update]
async fn init_salt() {
    state::init_salt().await;
}

#[update]
fn enter_device_registration_mode(user_number: UserNumber) -> Timestamp {
    tentative_device_registration::enter_device_registration_mode(user_number)
}

#[update]
fn exit_device_registration_mode(user_number: UserNumber) {
    tentative_device_registration::exit_device_registration_mode(user_number)
}

#[update]
async fn add_tentative_device(
    user_number: UserNumber,
    device_data: DeviceData,
) -> AddTentativeDeviceResponse {
    tentative_device_registration::add_tentative_device(user_number, device_data).await
}

#[update]
async fn verify_tentative_device(
    user_number: UserNumber,
    user_verification_code: DeviceVerificationCode,
) -> VerifyTentativeDeviceResponse {
    tentative_device_registration::verify_tentative_device(user_number, user_verification_code)
        .await
}

#[update]
async fn create_challenge() -> Challenge {
    anchor_management::registration::create_challenge().await
}

#[update]
async fn register(device_data: DeviceData, challenge_result: ChallengeAttempt) -> RegisterResponse {
    anchor_management::registration::register(device_data, challenge_result).await
}

#[update]
async fn add(user_number: UserNumber, device_data: DeviceData) {
    anchor_management::add(user_number, device_data).await
}

#[update]
async fn update(user_number: UserNumber, device_key: DeviceKey, device_data: DeviceData) {
    anchor_management::update(user_number, device_key, device_data).await
}

#[update]
async fn remove(user_number: UserNumber, device_key: DeviceKey) {
    anchor_management::remove(user_number, device_key).await
}

/// Returns all devices of the user (authentication and recovery) but no information about device registrations.
/// Note: Will be changed in the future to be more consistent with get_anchor_info.
#[query]
fn lookup(user_number: UserNumber) -> Vec<DeviceData> {
    state::storage(|storage| {
        storage
            .read(user_number)
            .unwrap_or_default()
            .into_iter()
            .map(DeviceData::from)
            .collect()
    })
}

#[update] // this is an update call because queries are not (yet) certified
fn get_anchor_info(user_number: UserNumber) -> IdentityAnchorInfo {
    anchor_management::get_anchor_info(user_number)
}

#[query]
fn get_principal(user_number: UserNumber, frontend: FrontendHostname) -> Principal {
    delegation::get_principal(user_number, frontend)
}

/// This makes this Candid service self-describing, so that for example Candid UI, but also other
/// tools, can seamlessly integrate with it. The concrete interface (method name etc.) is
/// provisional, but works.
#[query]
fn __get_candid_interface_tmp_hack() -> String {
    include_str!("../internet_identity.did").to_string()
}

#[update]
async fn prepare_delegation(
    user_number: UserNumber,
    frontend: FrontendHostname,
    session_key: SessionKey,
    max_time_to_live: Option<u64>,
) -> (UserKey, Timestamp) {
    delegation::prepare_delegation(user_number, frontend, session_key, max_time_to_live).await
}

#[query]
fn get_delegation(
    user_number: UserNumber,
    frontend: FrontendHostname,
    session_key: SessionKey,
    expiration: Timestamp,
) -> GetDelegationResponse {
    delegation::get_delegation(user_number, frontend, session_key, expiration)
}

#[query]
fn http_request(req: HttpRequest) -> HttpResponse {
    http::http_request(req)
}

#[query]
fn stats() -> InternetIdentityStats {
    state::storage(|storage| InternetIdentityStats {
        assigned_user_number_range: storage.assigned_user_number_range(),
        users_registered: storage.user_count() as u64,
    })
}

#[init]
fn init(maybe_arg: Option<InternetIdentityInit>) {
    init_assets();
    state::storage_mut(|storage| {
        if let Some(arg) = maybe_arg {
            *storage = Storage::new(arg.assigned_user_number_range, DefaultMemoryImpl::default());
        }
        storage.flush();
    });

    update_root_hash();
}

#[post_upgrade]
fn retrieve_data() {
    init_assets();
    state::initialize_from_stable_memory();

    // We drop all the signatures on upgrade, users will
    // re-request them if needed.
    update_root_hash();
}

fn update_root_hash() {
    use ic_certified_map::{fork_hash, labeled_hash};
    state::asset_hashes_and_sigs(|asset_hashes, sigs| {
        let prefixed_root_hash = fork_hash(
            // NB: Labels added in lexicographic order
            &labeled_hash(LABEL_ASSETS, &asset_hashes.root_hash()),
            &labeled_hash(LABEL_SIG, &sigs.root_hash()),
        );
        set_certified_data(&prefixed_root_hash[..]);
    })
}

// Checks if the caller is authenticated against any of the public keys provided
// and traps if not.
fn trap_if_not_authenticated<'a>(public_keys: impl Iterator<Item = &'a PublicKey>) {
    for pk in public_keys {
        if caller() == Principal::self_authenticating(pk) {
            return;
        }
    }
    trap(&format!("{} could not be authenticated.", caller()))
}

fn main() {}
