/** The functions here are derived (manually) from Internet Identity's Candid file */
use crate::framework;
use crate::framework::CallError;
use candid;
use ic_state_machine_tests::{CanisterId, PrincipalId, StateMachine};
use internet_identity_interface as types;
use sdk_ic_types::Principal;

/// A fake "health check" method that just checks the canister is alive a well.
pub fn health_check(env: &StateMachine, canister_id: CanisterId) {
    let user_number: types::UserNumber = 0;
    // XXX: we use "IDLValue" because we're just checking that the canister is sending
    // valid data, but we don't care about the actual data.
    let _: (candid::parser::value::IDLValue,) =
        framework::call_candid(env, canister_id, "lookup", (user_number,)).unwrap();
}

pub fn http_request(
    env: &StateMachine,
    canister_id: CanisterId,
    http_request: types::HttpRequest,
) -> Result<types::HttpResponse, CallError> {
    framework::query_candid(env, canister_id, "http_request", (http_request,)).map(|(x,)| x)
}

pub fn create_challenge(env: &StateMachine, canister_id: CanisterId) -> types::Challenge {
    let (c,) = framework::call_candid(env, canister_id, "create_challenge", ()).unwrap();
    c
}

pub fn register(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: PrincipalId,
    device_data: &types::DeviceData,
    challenge_attempt: types::ChallengeAttempt,
) -> Result<types::RegisterResponse, CallError> {
    framework::call_candid_as(
        env,
        canister_id,
        sender,
        "register",
        (device_data, challenge_attempt),
    )
    .map(|(x,)| x)
}

pub fn prepare_delegation(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: PrincipalId,
    user_number: types::UserNumber,
    frontend_hostname: types::FrontendHostname,
    session_key: types::SessionKey,
    max_time_to_live: Option<u64>,
) -> Result<(types::UserKey, types::Timestamp), CallError> {
    framework::call_candid_as(
        env,
        canister_id,
        sender,
        "prepare_delegation",
        (
            user_number,
            frontend_hostname,
            session_key,
            max_time_to_live,
        ),
    )
}

pub fn get_delegation(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: PrincipalId,
    user_number: types::UserNumber,
    frontend_hostname: types::FrontendHostname,
    session_key: types::SessionKey,
    timestamp: u64,
) -> Result<types::GetDelegationResponse, CallError> {
    framework::query_candid_as(
        env,
        canister_id,
        sender,
        "get_delegation",
        (user_number, frontend_hostname, session_key, timestamp),
    )
    .map(|(x,)| x)
}

pub fn get_principal(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: PrincipalId,
    user_number: types::UserNumber,
    frontend_hostname: types::FrontendHostname,
) -> Result<Principal, CallError> {
    framework::query_candid_as(
        env,
        canister_id,
        sender,
        "get_principal",
        (user_number, frontend_hostname),
    )
    .map(|(x,)| x)
}

pub fn lookup(
    env: &StateMachine,
    canister_id: CanisterId,
    user_number: types::UserNumber,
) -> Result<Vec<types::DeviceData>, CallError> {
    framework::query_candid(env, canister_id, "lookup", (user_number,)).map(|(x,)| x)
}

/// A "compatibility" module for older versions of II that don't necessarily have a
/// `protection_type`. Can be removed once the latest release includes `protection_type`.
pub mod compat {

    use crate::framework;
    use crate::framework::CallError;
    use candid;
    use ic_state_machine_tests::{CanisterId, StateMachine};
    use internet_identity_interface as types;

    use candid::{CandidType, Deserialize};

    pub fn lookup(
        env: &StateMachine,
        canister_id: CanisterId,
        user_number: types::UserNumber,
    ) -> Result<Vec<types::DeviceData>, CallError> {
        let device_data: Vec<super::compat::DeviceDataOld> =
            framework::query_candid(env, canister_id, "lookup", (user_number,)).map(|(x,)| x)?;
        Ok(device_data.iter().map(|old| old.clone().into()).collect())
    }

    /// A version of `DeviceData` that is compatible with code with or without `protection_type`.
    #[derive(Eq, PartialEq, Clone, Debug, CandidType, Deserialize)]
    pub struct DeviceDataOld {
        pub pubkey: types::DeviceKey,
        pub alias: String,
        pub credential_id: Option<types::CredentialId>,
        pub purpose: types::Purpose,
        pub key_type: types::KeyType,
        pub protection_type: Option<types::ProtectionType>,
    }

    impl From<DeviceDataOld> for types::DeviceData {
        fn from(device_data_old: DeviceDataOld) -> Self {
            Self {
                pubkey: device_data_old.pubkey,
                alias: device_data_old.alias,
                credential_id: device_data_old.credential_id,
                purpose: device_data_old.purpose,
                key_type: device_data_old.key_type,
                protection_type: device_data_old
                    .protection_type
                    .unwrap_or(types::ProtectionType::Unprotected),
            }
        }
    }
}

pub fn add(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: PrincipalId,
    user_number: types::UserNumber,
    device_data: types::DeviceData,
) -> Result<(), CallError> {
    framework::call_candid_as(env, canister_id, sender, "add", (user_number, device_data))
}

pub fn update(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: PrincipalId,
    user_number: types::UserNumber,
    device_key: types::PublicKey,
    device_data: types::DeviceData,
) -> Result<(), CallError> {
    framework::call_candid_as(
        env,
        canister_id,
        sender,
        "update",
        (user_number, device_key, device_data),
    )
}

pub fn remove(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: PrincipalId,
    user_number: types::UserNumber,
    device_key: types::PublicKey,
) -> Result<(), CallError> {
    framework::call_candid_as(
        env,
        canister_id,
        sender,
        "remove",
        (user_number, device_key),
    )
}

pub fn get_anchor_info(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: PrincipalId,
    user_number: types::UserNumber,
) -> Result<types::IdentityAnchorInfo, CallError> {
    framework::call_candid_as(env, canister_id, sender, "get_anchor_info", (user_number,))
        .map(|(x,)| x)
}

pub fn enter_device_registration_mode(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: PrincipalId,
    user_number: types::UserNumber,
) -> Result<types::Timestamp, CallError> {
    framework::call_candid_as(
        env,
        canister_id,
        sender,
        "enter_device_registration_mode",
        (user_number,),
    )
    .map(|(x,)| x)
}

pub fn exit_device_registration_mode(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: PrincipalId,
    user_number: types::UserNumber,
) -> Result<(), CallError> {
    framework::call_candid_as(
        env,
        canister_id,
        sender,
        "exit_device_registration_mode",
        (user_number,),
    )
}

pub fn add_tentative_device(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: PrincipalId,
    user_number: types::UserNumber,
    device_data: types::DeviceData,
) -> Result<types::AddTentativeDeviceResponse, CallError> {
    framework::call_candid_as(
        env,
        canister_id,
        sender,
        "add_tentative_device",
        (user_number, device_data),
    )
    .map(|(x,)| x)
}

pub fn verify_tentative_device(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: PrincipalId,
    user_number: types::UserNumber,
    verification_code: types::DeviceVerificationCode,
) -> Result<types::VerifyTentativeDeviceResponse, CallError> {
    framework::call_candid_as(
        env,
        canister_id,
        sender,
        "verify_tentative_device",
        (user_number, verification_code),
    )
    .map(|(x,)| x)
}
