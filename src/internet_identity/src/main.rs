use crate::anchor_management::tentative_device_registration;
use crate::assets::init_assets;
use crate::state::Anchor;
use candid::Principal;
use ic_cdk::api::{caller, set_certified_data, trap};
use ic_cdk_macros::{init, post_upgrade, pre_upgrade, query, update};
use ic_certified_map::AsHashTree;
use serde_bytes::ByteBuf;
use storage::{Salt, Storage};

use crate::archive::ArchiveState;
use internet_identity_interface::*;

mod anchor_management;
mod archive;
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
            .devices
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
    let archive_info = state::persistent_state(|persistent_state| {
        if let ArchiveState::Created(ref data) = persistent_state.archive_info.state {
            ArchiveInfo {
                archive_canister: Some(data.archive_canister),
                expected_wasm_hash: persistent_state.archive_info.expected_module_hash,
            }
        } else {
            ArchiveInfo {
                archive_canister: None,
                expected_wasm_hash: persistent_state.archive_info.expected_module_hash,
            }
        }
    });

    let canister_creation_cycles_cost =
        state::persistent_state(|persistent_state| persistent_state.canister_creation_cycles_cost);

    state::storage(|storage| InternetIdentityStats {
        assigned_user_number_range: storage.assigned_user_number_range(),
        users_registered: storage.user_count() as u64,
        archive_info,
        canister_creation_cycles_cost,
        storage_layout_version: storage.version(),
    })
}

#[update]
async fn deploy_archive(wasm: ByteBuf) -> DeployArchiveResult {
    archive::deploy_archive(wasm).await
}

#[init]
fn init(maybe_arg: Option<InternetIdentityInit>) {
    init_assets();

    if let Some(arg) = maybe_arg {
        if let Some(range) = arg.assigned_user_number_range {
            state::storage_mut(|storage| {
                storage.set_user_number_range(range);
            });
        }
        if let Some(archive_hash) = arg.archive_module_hash {
            state::persistent_state_mut(|persistent_state| {
                persistent_state.archive_info.expected_module_hash = Some(archive_hash);
            })
        }
        if let Some(cost) = arg.canister_creation_cycles_cost {
            state::persistent_state_mut(|persistent_state| {
                persistent_state.canister_creation_cycles_cost = cost;
            })
        }
    }

    // make sure the fully initialized storage configuration is written to stable memory
    state::storage_mut(|storage| storage.flush());
    update_root_hash();
}

#[post_upgrade]
fn post_upgrade(maybe_arg: Option<InternetIdentityInit>) {
    init_assets();
    state::initialize_from_stable_memory();

    // We drop all the signatures on upgrade, users will
    // re-request them if needed.
    update_root_hash();
    // load the persistent state after initializing storage, otherwise the memory address to load it from cannot be calculated
    state::load_persistent_state();

    if let Some(arg) = maybe_arg {
        if let Some(range) = arg.assigned_user_number_range {
            let current_range = state::storage(|storage| storage.assigned_user_number_range());
            if range != current_range {
                trap(&format!(
                    "User number range cannot be changed. Current value: {:?}",
                    current_range
                ));
            }
        }
        if let Some(archive_hash) = arg.archive_module_hash {
            state::persistent_state_mut(|persistent_state| {
                persistent_state.archive_info.expected_module_hash = Some(archive_hash);
            })
        }
        if let Some(cost) = arg.canister_creation_cycles_cost {
            state::persistent_state_mut(|persistent_state| {
                persistent_state.canister_creation_cycles_cost = cost;
            })
        }
    }
}

#[pre_upgrade]
fn save_persistent_state() {
    state::save_persistent_state();
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

/// Checks if the caller is authenticated against the anchor provided and traps if not.
fn trap_if_not_authenticated(anchor: &Anchor) {
    for device in &anchor.devices {
        if caller() == Principal::self_authenticating(&device.pubkey) {
            return;
        }
    }
    trap(&format!("{} could not be authenticated.", caller()))
}

fn main() {}
