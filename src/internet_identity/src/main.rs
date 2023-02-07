use crate::anchor_management::tentative_device_registration;
use crate::archive::ArchiveState;
use crate::assets::init_assets;
use crate::storage::anchor::Anchor;
use candid::Principal;
use ic_cdk::api::{caller, set_certified_data, trap};
use ic_cdk_macros::{init, post_upgrade, pre_upgrade, query, update};
use ic_certified_map::AsHashTree;
use internet_identity_interface::archive::BufferedEntry;
use internet_identity_interface::*;
use serde_bytes::ByteBuf;
use storage::{Salt, Storage};

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
fn enter_device_registration_mode(anchor_number: AnchorNumber) -> Timestamp {
    trap_if_not_authenticated(&state::anchor(anchor_number));
    tentative_device_registration::enter_device_registration_mode(anchor_number)
}

#[update]
fn exit_device_registration_mode(anchor_number: AnchorNumber) {
    trap_if_not_authenticated(&state::anchor(anchor_number));
    tentative_device_registration::exit_device_registration_mode(anchor_number)
}

#[update]
async fn add_tentative_device(
    anchor_number: AnchorNumber,
    device_data: DeviceData,
) -> AddTentativeDeviceResponse {
    tentative_device_registration::add_tentative_device(anchor_number, device_data).await
}

#[update]
fn verify_tentative_device(
    anchor_number: AnchorNumber,
    user_verification_code: DeviceVerificationCode,
) -> VerifyTentativeDeviceResponse {
    trap_if_not_authenticated(&state::anchor(anchor_number));
    tentative_device_registration::verify_tentative_device(anchor_number, user_verification_code)
}

#[update]
async fn create_challenge() -> Challenge {
    anchor_management::registration::create_challenge().await
}

#[update]
fn register(device_data: DeviceData, challenge_result: ChallengeAttempt) -> RegisterResponse {
    anchor_management::registration::register(device_data, challenge_result)
}

#[update]
fn add(anchor_number: AnchorNumber, device_data: DeviceData) {
    trap_if_not_authenticated(&state::anchor(anchor_number));
    anchor_management::add(anchor_number, device_data)
}

#[update]
fn update(anchor_number: AnchorNumber, device_key: DeviceKey, device_data: DeviceData) {
    trap_if_not_authenticated(&state::anchor(anchor_number));
    anchor_management::update(anchor_number, device_key, device_data)
}

#[update]
fn replace(anchor_number: AnchorNumber, device_key: DeviceKey, device_data: DeviceData) {
    trap_if_not_authenticated(&state::anchor(anchor_number));
    anchor_management::replace(anchor_number, device_key, device_data)
}

#[update]
fn remove(anchor_number: AnchorNumber, device_key: DeviceKey) {
    trap_if_not_authenticated(&state::anchor(anchor_number));
    anchor_management::remove(anchor_number, device_key)
}

/// Returns all devices of the anchor (authentication and recovery) but no information about device registrations.
/// Note: Will be changed in the future to be more consistent with get_anchor_info.
#[query]
fn lookup(anchor_number: AnchorNumber) -> Vec<DeviceData> {
    state::storage(|storage| {
        storage
            .read(anchor_number)
            .unwrap_or_default()
            .into_devices()
            .into_iter()
            .map(DeviceData::from)
            .map(|mut d| {
                // This hides the alias from unauthenticated callers.
                d.alias = "".to_string();
                d
            })
            .collect()
    })
}

#[update] // this is an update call because queries are not (yet) certified
fn get_anchor_info(anchor_number: AnchorNumber) -> IdentityAnchorInfo {
    trap_if_not_authenticated(&state::anchor(anchor_number));
    anchor_management::get_anchor_info(anchor_number)
}

#[query]
fn get_principal(anchor_number: AnchorNumber, frontend: FrontendHostname) -> Principal {
    trap_if_not_authenticated(&state::anchor(anchor_number));
    delegation::get_principal(anchor_number, frontend)
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
    anchor_number: AnchorNumber,
    frontend: FrontendHostname,
    session_key: SessionKey,
    max_time_to_live: Option<u64>,
) -> (UserKey, Timestamp) {
    trap_if_not_authenticated(&state::anchor(anchor_number));
    delegation::prepare_delegation(anchor_number, frontend, session_key, max_time_to_live).await
}

#[query]
fn get_delegation(
    anchor_number: AnchorNumber,
    frontend: FrontendHostname,
    session_key: SessionKey,
    expiration: Timestamp,
) -> GetDelegationResponse {
    trap_if_not_authenticated(&state::anchor(anchor_number));
    delegation::get_delegation(anchor_number, frontend, session_key, expiration)
}

#[query]
fn http_request(req: HttpRequest) -> HttpResponse {
    http::http_request(req)
}

#[query]
fn stats() -> InternetIdentityStats {
    let archive_info = match state::archive_state() {
        ArchiveState::NotConfigured => ArchiveInfo {
            archive_canister: None,
            archive_config: None,
        },
        ArchiveState::Configured { config } | ArchiveState::CreationInProgress { config, .. } => {
            ArchiveInfo {
                archive_canister: None,
                archive_config: Some(config),
            }
        }
        ArchiveState::Created { data, config } => ArchiveInfo {
            archive_canister: Some(data.archive_canister),
            archive_config: Some(config),
        },
    };

    let canister_creation_cycles_cost =
        state::persistent_state(|persistent_state| persistent_state.canister_creation_cycles_cost);

    state::storage(|storage| InternetIdentityStats {
        assigned_user_number_range: storage.assigned_anchor_number_range(),
        users_registered: storage.anchor_count() as u64,
        archive_info,
        canister_creation_cycles_cost,
        storage_layout_version: storage.version(),
    })
}

#[update]
async fn deploy_archive(wasm: ByteBuf) -> DeployArchiveResult {
    archive::deploy_archive(wasm).await
}

/// Returns a batch of entries _sorted by sequence number_ to be archived.
/// This is an update call because the archive information _must_ be certified.
/// Only callable by this IIs archive canister.
#[update]
fn fetch_entries() -> Vec<BufferedEntry> {
    archive::fetch_entries()
}

/// Removes all buffered archive entries up to sequence number.
/// Only callable by this IIs archive canister.
#[update]
fn acknowledge_entries(sequence_number: u64) {
    archive::acknowledge_entries(sequence_number)
}

#[init]
fn init(maybe_arg: Option<InternetIdentityInit>) {
    init_assets();

    apply_install_arg(maybe_arg);

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

    apply_install_arg(maybe_arg);
}

fn apply_install_arg(maybe_arg: Option<InternetIdentityInit>) {
    if let Some(arg) = maybe_arg {
        if let Some(range) = arg.assigned_user_number_range {
            state::storage_mut(|storage| {
                storage.set_anchor_number_range(range);
            });
        }
        if let Some(new_config) = arg.archive_config {
            update_archive_config(new_config);
        }
        if let Some(cost) = arg.canister_creation_cycles_cost {
            state::persistent_state_mut(|persistent_state| {
                persistent_state.canister_creation_cycles_cost = cost;
            })
        }
    }
}

fn update_archive_config(new_config: ArchiveConfig) {
    state::persistent_state_mut(|persistent_state| match persistent_state.archive_state {
        ArchiveState::NotConfigured => {
            persistent_state.archive_state = ArchiveState::Configured { config: new_config }
        }
        ArchiveState::Configured { ref mut config }
        | ArchiveState::CreationInProgress { ref mut config, .. }
        | ArchiveState::Created { ref mut config, .. } => {
            *config = new_config;
        }
    })
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
    for device in anchor.devices() {
        if caller() == Principal::self_authenticating(&device.pubkey) {
            return;
        }
    }
    trap(&format!("{} could not be authenticated.", caller()))
}

fn main() {}
