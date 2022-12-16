use crate::api;
use candid::utils::{decode_args, encode_args, ArgumentDecoder, ArgumentEncoder};
use candid::Principal;
use flate2::read::GzDecoder;
use flate2::{Compression, GzBuilder};
use ic_crypto_iccsa::types::SignatureBytes;
use ic_crypto_iccsa::{public_key_bytes_from_der, verify};
use ic_state_machine_tests::{
    CanisterId, ErrorCode, PrincipalId, StateMachine, UserError, WasmResult,
};
use ic_types::crypto::Signable;
use ic_types::messages::Delegation;
use ic_types::Time;
use internet_identity_interface as types;
use lazy_static::lazy_static;
use regex::Regex;
use serde_bytes::ByteBuf;
use sha2::Digest;
use sha2::Sha256;
use std::env;
use std::fs::File;
use std::io::{Read, Write};
use std::path;
use std::time::{Duration, SystemTime};

/* The first few lines deal with actually getting the Wasm module(s) to test */

lazy_static! {
    /** The Wasm module for the current II build, i.e. the one we're testing */
    pub static ref II_WASM: Vec<u8> = {
        let def_path = path::PathBuf::from("..").join("..").join("internet_identity.wasm");
        let err = format!("
        Could not find Internet Identity Wasm module for current build.

        I will look for it at {:?}, and you can specify another path with the environment variable II_WASM (note that I run from {:?}).

        In order to build the Wasm module, please run the following command:
            II_DUMMY_CAPTCHA=1 ./scripts/build
        ", &def_path, &std::env::current_dir().map(|x| x.display().to_string()).unwrap_or("an unknown directory".to_string()));
        get_wasm_path("II_WASM".to_string(), &def_path).expect(&err)
    };

    /** The Wasm module for the current archive build, i.e. the one we're testing */
    pub static ref ARCHIVE_WASM: Vec<u8> = {
        let def_path = path::PathBuf::from("..").join("..").join("archive.wasm");
        let err = format!("
        Could not find Archive Wasm module for current build.

        I will look for it at {:?}, and you can specify another path with the environment variable ARCHIVE_WASM (note that I run from {:?}).

        In order to build the Wasm module, please run the following command:
            ./scripts/build --archive
        ", &def_path, &std::env::current_dir().map(|x| x.display().to_string()).unwrap_or("an unknown directory".to_string()));
        get_wasm_path("ARCHIVE_WASM".to_string(), &def_path).expect(&err)
    };

    /** The Wasm module for the _previous_ II build, or latest release, which is used when testing
     * upgrades and downgrades */
    pub static ref II_WASM_PREVIOUS: Vec<u8> = {
        let def_path = path::PathBuf::from("..").join("..").join("internet_identity_previous.wasm");
        let err = format!("
        Could not find Internet Identity Wasm module for previous build/latest release.

        I will look for it at {:?}, and you can specify another path with the environment variable II_WASM_PREVIOUS (note that I run from {:?}).

        In order to get the Wasm module, please run the following command:
            curl -SL https://github.com/dfinity/internet-identity/releases/latest/download/internet_identity_test.wasm -o internet_identity_previous.wasm
        ", &def_path, &std::env::current_dir().map(|x| x.display().to_string()).unwrap_or("an unknown directory".to_string()));
        get_wasm_path("II_WASM_PREVIOUS".to_string(), &def_path).expect(&err)
    };

    /** The Wasm module for the last II build that still initializes storage as V3, which is used when testing
     * the candid schema migration */
    pub static ref II_WASM_V3_LAYOUT: Vec<u8> = {
        let def_path = path::PathBuf::from("..").join("..").join("internet_identity_v3_storage.wasm");
        let err = format!("
        Could not find Internet Identity Wasm module for release with v3 storage layout.

        I will look for it at {:?}, and you can specify another path with the environment variable II_WASM_V3_LAYOUT (note that I run from {:?}).

        In order to get the Wasm module, please run the following command:
            curl -SL https://github.com/dfinity/internet-identity/releases/download/release-2022-12-07/internet_identity_test.wasm -o internet_identity_v3_storage.wasm
        ", &def_path, &std::env::current_dir().map(|x| x.display().to_string()).unwrap_or("an unknown directory".to_string()));
        get_wasm_path("II_WASM_V3_LAYOUT".to_string(), &def_path).expect(&err)
    };

        /** The Wasm module for the _previous_ archive build, or latest release, which is used when testing
            * upgrades and downgrades */
    pub static ref ARCHIVE_WASM_PREVIOUS: Vec<u8> = {
        let def_path = path::PathBuf::from("..").join("..").join("archive_previous.wasm");
        let err = format!("
        Could not find Archive Wasm module for previous build/latest release.

        I will look for it at {:?}, and you can specify another path with the environment variable ARCHIVE_WASM_PREVIOUS (note that I run from {:?}).

        In order to get the Wasm module, please run the following command:
            curl -SL https://github.com/dfinity/internet-identity/releases/latest/download/archive.wasm -o archive_previous.wasm
        ", &def_path, &std::env::current_dir().map(|x| x.display().to_string()).unwrap_or("an unknown directory".to_string()));
        get_wasm_path("ARCHIVE_WASM_PREVIOUS".to_string(), &def_path).expect(&err)
    };


    /** Empty WASM module (without any pre- and post-upgrade hooks. Useful to initialize a canister before loading a stable memory backup. */
    pub static ref EMPTY_WASM: Vec<u8> = vec![0, 0x61, 0x73, 0x6D, 1, 0, 0, 0];
}

/** Helper that returns the content of `default_path` if found, or None if the file does not exist.
 * The `env_var` environment variable is also read for custom location; if the variable is set
 * _but_ the Wasm module is not present, we simply panic (i.e. we don't return None)
 */
fn get_wasm_path(env_var: String, default_path: &path::PathBuf) -> Option<Vec<u8>> {
    match env::var_os(env_var.clone()) {
        None => {
            if !default_path.exists() {
                return None;
            }
            Some(
                std::fs::read(default_path)
                    .expect(&format!("could not read Wasm module: {:?}", default_path)),
            )
        }
        Some(path) => {
            let pathname: String = path
                .into_string()
                .expect(&format!("Invalid string path for {}", env_var.clone()));
            let path = path::PathBuf::from(pathname.clone());
            if !path.exists() {
                panic!("Could not find {}", pathname);
            }
            Some(
                std::fs::read(path.clone())
                    .expect(&format!("could not read Wasm module: {:?}", path)),
            )
        }
    }
}

/* Here are a few useful helpers for writing tests */
pub fn install_empty_canister(env: &StateMachine) -> CanisterId {
    env.install_canister(EMPTY_WASM.clone(), vec![], None)
        .expect("failed to install empty canister.")
}

pub fn install_ii_canister(env: &StateMachine, wasm: Vec<u8>) -> CanisterId {
    install_ii_canister_with_arg(&env, wasm, None)
}

pub fn install_ii_canister_with_arg(
    env: &StateMachine,
    wasm: Vec<u8>,
    arg: Option<types::InternetIdentityInit>,
) -> CanisterId {
    let byts = candid::encode_one(arg).expect("error encoding II installation arg as candid");
    env.install_canister(wasm, byts, None).unwrap()
}

pub fn arg_with_wasm_hash(wasm: Vec<u8>) -> Option<types::InternetIdentityInit> {
    Some(types::InternetIdentityInit {
        assigned_user_number_range: None,
        archive_module_hash: Some(archive_wasm_hash(&wasm)),
        canister_creation_cycles_cost: Some(0),
        layout_migration_batch_size: None,
    })
}

pub fn archive_wasm_hash(wasm: &Vec<u8>) -> [u8; 32] {
    let mut hasher = Sha256::new();
    hasher.update(&wasm);
    hasher.finalize().into()
}

pub fn upgrade_ii_canister(env: &StateMachine, canister_id: CanisterId, wasm: Vec<u8>) {
    upgrade_ii_canister_with_arg(env, canister_id, wasm, None).unwrap()
}

pub fn upgrade_ii_canister_with_arg(
    env: &StateMachine,
    canister_id: CanisterId,
    wasm: Vec<u8>,
    arg: Option<types::InternetIdentityInit>,
) -> Result<(), UserError> {
    let byts = candid::encode_one(arg).expect("error encoding II upgrade arg as candid");
    env.upgrade_canister(canister_id, wasm, byts)
}

/// Utility function to create compressed stable memory backups for use in backup tests.
pub fn save_compressed_stable_memory(
    env: &StateMachine,
    canister_id: CanisterId,
    path: &str,
    decompressed_name: &str,
) {
    let file = File::create(path).expect("Failed to create stable memory file");
    let mut encoder = GzBuilder::new()
        .filename(decompressed_name)
        .write(file, Compression::best());
    encoder
        .write(env.stable_memory(canister_id).as_slice())
        .unwrap();
    encoder.flush().unwrap();
    let mut file = encoder.finish().unwrap();
    file.flush().unwrap();
}

pub fn restore_compressed_stable_memory(env: &StateMachine, canister_id: CanisterId, path: &str) {
    let file = File::open(path).expect("Failed to open stable memory file");
    let mut decoder = GzDecoder::new(file);
    let mut buffer = vec![];
    decoder
        .read_to_end(&mut buffer)
        .expect("error while decoding stable memory file");
    env.set_stable_memory(canister_id, &buffer);
}

pub const PUBKEY_1: &str = "test";
pub const PUBKEY_2: &str = "some other key";
pub const RECOVERY_PUBKEY_1: &str = "recovery 1";
pub const RECOVERY_PUBKEY_2: &str = "recovery 2";

pub fn principal_1() -> PrincipalId {
    PrincipalId(Principal::self_authenticating(PUBKEY_1))
}

pub fn principal_2() -> PrincipalId {
    PrincipalId(Principal::self_authenticating(PUBKEY_2))
}
pub fn principal_recovery_1() -> PrincipalId {
    PrincipalId(Principal::self_authenticating(RECOVERY_PUBKEY_1))
}

pub fn principal_recovery_2() -> PrincipalId {
    PrincipalId(Principal::self_authenticating(RECOVERY_PUBKEY_2))
}

pub fn device_data_1() -> types::DeviceData {
    types::DeviceData {
        pubkey: ByteBuf::from(PUBKEY_1),
        alias: "My Device".to_string(),
        credential_id: None,
        purpose: types::Purpose::Authentication,
        key_type: types::KeyType::Unknown,
        protection: types::DeviceProtection::Unprotected,
    }
}

pub fn device_data_2() -> types::DeviceData {
    types::DeviceData {
        pubkey: ByteBuf::from(PUBKEY_2),
        alias: "My second device".to_string(),
        credential_id: None,
        purpose: types::Purpose::Authentication,
        key_type: types::KeyType::Unknown,
        protection: types::DeviceProtection::Unprotected,
    }
}

pub fn max_size_device() -> types::DeviceData {
    types::DeviceData {
        pubkey: ByteBuf::from([255u8; 300]),
        alias: "a".repeat(64).to_string(),
        credential_id: Some(ByteBuf::from([7u8; 200])),
        purpose: types::Purpose::Authentication,
        key_type: types::KeyType::Unknown,
        protection: types::DeviceProtection::Unprotected,
    }
}

pub fn recovery_device_data_1() -> types::DeviceData {
    types::DeviceData {
        pubkey: ByteBuf::from(RECOVERY_PUBKEY_1),
        alias: "Recovery Phrase 1".to_string(),
        credential_id: None,
        purpose: types::Purpose::Recovery,
        key_type: types::KeyType::SeedPhrase,
        protection: types::DeviceProtection::Unprotected,
    }
}

pub fn recovery_device_data_2() -> types::DeviceData {
    types::DeviceData {
        pubkey: ByteBuf::from(RECOVERY_PUBKEY_2),
        alias: "Recovery Phrase 2".to_string(),
        credential_id: None,
        purpose: types::Purpose::Recovery,
        key_type: types::KeyType::SeedPhrase,
        protection: types::DeviceProtection::Unprotected,
    }
}

/* Here are a few functions that are not directly related to II and could be upstreamed
 * (were actually stolen from somewhere else)
 */

/// Call a canister candid query method, anonymous.
pub fn query_candid<Input, Output>(
    env: &StateMachine,
    canister_id: CanisterId,
    method: &str,
    input: Input,
) -> Result<Output, CallError>
where
    Input: ArgumentEncoder,
    Output: for<'a> ArgumentDecoder<'a>,
{
    with_candid(input, |bytes| env.query(canister_id, method, bytes))
}

/// Call a canister candid query method, authenticated.
pub fn query_candid_as<Input, Output>(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: PrincipalId,
    method: &str,
    input: Input,
) -> Result<Output, CallError>
where
    Input: ArgumentEncoder,
    Output: for<'a> ArgumentDecoder<'a>,
{
    with_candid(input, |bytes| {
        env.query_as(sender, canister_id, method, bytes)
    })
}

/// Call a canister candid method, authenticated.
/// The state machine executes update calls synchronously, so there is no need to poll for the result.
pub fn call_candid_as<Input, Output>(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: PrincipalId,
    method: &str,
    input: Input,
) -> Result<Output, CallError>
where
    Input: ArgumentEncoder,
    Output: for<'a> ArgumentDecoder<'a>,
{
    with_candid(input, |bytes| {
        env.execute_ingress_as(sender, canister_id, method, bytes)
    })
}

/// Call a canister candid method, anonymous.
/// The state machine executes update calls synchronously, so there is no need to poll for the result.
pub fn call_candid<Input, Output>(
    env: &StateMachine,
    canister_id: CanisterId,
    method: &str,
    input: Input,
) -> Result<Output, CallError>
where
    Input: ArgumentEncoder,
    Output: for<'a> ArgumentDecoder<'a>,
{
    with_candid(input, |bytes| {
        env.execute_ingress(canister_id, method, bytes)
    })
}

#[derive(Debug)]
pub enum CallError {
    Reject(String),
    UserError(UserError),
}

/// A helper function that we use to implement both [`call_candid`] and
/// [`query_candid`].
pub fn with_candid<Input, Output>(
    input: Input,
    f: impl FnOnce(Vec<u8>) -> Result<WasmResult, UserError>,
) -> Result<Output, CallError>
where
    Input: ArgumentEncoder,
    Output: for<'a> ArgumentDecoder<'a>,
{
    let in_bytes = encode_args(input).expect("failed to encode args");
    match f(in_bytes) {
        Ok(WasmResult::Reply(out_bytes)) => Ok(decode_args(&out_bytes).unwrap_or_else(|e| {
            panic!(
                "Failed to decode response as candid type {}:\nerror: {}\nbytes: {:?}\nutf8: {}",
                std::any::type_name::<Output>(),
                e,
                out_bytes,
                String::from_utf8_lossy(&out_bytes),
            )
        })),
        Ok(WasmResult::Reject(message)) => Err(CallError::Reject(message)),
        Err(user_error) => Err(CallError::UserError(user_error)),
    }
}

pub fn expect_user_error_with_message<T: std::fmt::Debug>(
    result: Result<T, CallError>,
    error_code: ErrorCode,
    message_pattern: Regex,
) {
    match result {
        Ok(_) => panic!("expected error, got {:?}", result),
        Err(CallError::Reject(_)) => panic!("expected user error, got {:?}", result),
        Err(CallError::UserError(ref user_error)) => {
            if !(user_error.code() == error_code) {
                panic!(
                    "expected error code {:?}, got {:?}",
                    error_code,
                    user_error.code()
                );
            }
            if !message_pattern.is_match(user_error.description()) {
                panic!(
                    "expected #{:?}, got {:?}",
                    message_pattern,
                    user_error.description()
                );
            }
        }
    }
}

pub fn verify_security_headers(headers: &Vec<types::HeaderField>) {
    let expected_headers = vec![
        ("X-Frame-Options", "DENY"),
        ("X-Content-Type-Options", "nosniff"),
        ("Referrer-Policy", "same-origin"),
        (
            "Permissions-Policy",
            "accelerometer=(),\
ambient-light-sensor=(),\
autoplay=(),\
battery=(),\
camera=(),\
clipboard-read=(),\
clipboard-write=(self),\
conversion-measurement=(),\
cross-origin-isolated=(),\
display-capture=(),\
document-domain=(),\
encrypted-media=(),\
execution-while-not-rendered=(),\
execution-while-out-of-viewport=(),\
focus-without-user-activation=(),\
fullscreen=(),\
gamepad=(),\
geolocation=(),\
gyroscope=(),\
hid=(),\
idle-detection=(),\
interest-cohort=(),\
keyboard-map=(),\
magnetometer=(),\
microphone=(),\
midi=(),\
navigation-override=(),\
payment=(),\
picture-in-picture=(),\
publickey-credentials-get=(self),\
screen-wake-lock=(),\
serial=(),\
speaker-selection=(),\
sync-script=(),\
sync-xhr=(self),\
trust-token-redemption=(),\
usb=(),\
vertical-scroll=(),\
web-share=(),\
window-placement=(),\
xr-spatial-tracking=()",
        ),
    ];

    for (header_name, expected_value) in expected_headers {
        let (_, value) = headers
            .iter()
            .find(|(name, _)| name.to_lowercase() == header_name.to_lowercase())
            .expect(&format!("header \"{}\" not found", header_name));
        assert_eq!(value, expected_value);
    }

    let (_, csp) = headers
        .iter()
        .find(|(name, _)| name.to_lowercase() == "content-security-policy")
        .expect("header \"Content-Security-Policy\" not found");

    assert!(Regex::new(
        "^default-src 'none';\
connect-src 'self' https://ic0.app https://\\*\\.ic0.app;\
img-src 'self' data:;\
script-src 'sha256-[a-zA-Z0-9/=+]+' 'unsafe-inline' 'unsafe-eval' 'strict-dynamic' https:;\
base-uri 'none';\
form-action 'none';\
style-src 'self' 'unsafe-inline' https://fonts\\.googleapis\\.com;\
style-src-elem 'self' 'unsafe-inline' https://fonts\\.googleapis\\.com;\
font-src https://fonts\\.gstatic\\.com;\
upgrade-insecure-requests;\
frame-ancestors 'none';$"
    )
    .unwrap()
    .is_match(csp));
}

pub fn parse_metric(body: &str, metric: &str) -> (u64, SystemTime) {
    let metric = metric.replace("{", "\\{").replace("}", "\\}");
    let metric_capture = Regex::new(&format!("(?m)^{} (\\d+) (\\d+)$", metric))
        .unwrap()
        .captures(body)
        .expect(&format!("metric {} not found", metric));

    let metric: u64 = metric_capture.get(1).unwrap().as_str().parse().unwrap();
    let metric_timestamp = SystemTime::UNIX_EPOCH
        + Duration::from_millis(metric_capture.get(2).unwrap().as_str().parse().unwrap());
    (metric, metric_timestamp)
}

pub fn assert_metric(metrics: &str, metric_name: &str, expected: u64) {
    let (value, _) = parse_metric(&metrics, metric_name);
    assert_eq!(value, expected);
}

pub fn assert_devices_equal(
    env: &StateMachine,
    canister_id: CanisterId,
    anchor: types::UserNumber,
    mut expected_devices: Vec<types::DeviceData>,
) {
    expected_devices.sort_by(|a, b| a.pubkey.cmp(&b.pubkey));

    let mut devices = api::internet_identity::lookup(&env, canister_id, anchor).unwrap();
    devices.sort_by(|a, b| a.pubkey.cmp(&b.pubkey));
    assert_eq!(devices, expected_devices, "expected devices to match");
}

pub fn verify_delegation(
    env: &StateMachine,
    user_key: types::UserKey,
    signed_delegation: &types::SignedDelegation,
) {
    // transform delegation into ic typed delegation so that we have access to the signature domain separator
    // (via as_signed_bytes)
    let delegation = Delegation::new(
        signed_delegation.delegation.pubkey.clone().into_vec(),
        Time::from_nanos_since_unix_epoch(signed_delegation.delegation.expiration),
    );

    // this requires imports of internal crypto infrastructure
    // -> extend state-machine-tests to offer the functionality instead (see L2-739)
    verify(
        &delegation.as_signed_bytes(),
        SignatureBytes(signed_delegation.signature.clone().into_vec()),
        public_key_bytes_from_der(user_key.as_ref()).unwrap(),
        &env.root_key(),
    )
    .expect("signature invalid");
}

pub fn deploy_archive_via_ii(env: &StateMachine, ii_canister: CanisterId) -> CanisterId {
    match api::internet_identity::deploy_archive(
        &env,
        ii_canister,
        ByteBuf::from(ARCHIVE_WASM.clone()),
    ) {
        Ok(types::DeployArchiveResult::Success(archive_principal)) => {
            canister_id_from_principal(archive_principal)
        }
        err => {
            panic!("archive deployment failed: {:?}", err)
        }
    }
}

pub fn canister_id_from_principal(principal: Principal) -> CanisterId {
    CanisterId::new(PrincipalId(principal)).unwrap()
}

pub fn install_archive_canister(env: &StateMachine, wasm: Vec<u8>) -> CanisterId {
    env.install_canister(wasm, encode_config(principal_1().0), None)
        .unwrap()
}

pub fn upgrade_archive_canister(env: &StateMachine, canister_id: CanisterId, wasm: Vec<u8>) {
    env.upgrade_canister(canister_id, wasm, encode_config(principal_1().0))
        .unwrap()
}

fn encode_config(authorized_principal: Principal) -> Vec<u8> {
    let config = types::ArchiveInit {
        ii_canister: authorized_principal,
        max_entries_per_call: 10,
    };
    candid::encode_one(config).expect("error encoding II installation arg as candid")
}

pub const USER_NUMBER_1: types::UserNumber = 100001;
pub const USER_NUMBER_2: types::UserNumber = 100002;
pub const USER_NUMBER_3: types::UserNumber = 100003;

pub const TIMESTAMP_1: types::UserNumber = 999991;
pub const TIMESTAMP_2: types::UserNumber = 999992;
pub const TIMESTAMP_3: types::UserNumber = 999993;

pub fn log_entry_1() -> types::Entry {
    types::Entry {
        timestamp: TIMESTAMP_1,
        anchor: USER_NUMBER_1,
        caller: principal_1().0,
        operation: types::Operation::RegisterAnchor {
            device: types::DeviceDataWithoutAlias {
                pubkey: ByteBuf::from(PUBKEY_1),
                credential_id: None,
                purpose: types::Purpose::Authentication,
                key_type: types::KeyType::Unknown,
                protection: types::DeviceProtection::Unprotected,
            },
        },
        sequence_number: 0,
    }
}

pub fn log_entry_2() -> types::Entry {
    types::Entry {
        timestamp: TIMESTAMP_2,
        anchor: USER_NUMBER_2,
        caller: principal_1().0,
        operation: types::Operation::AddDevice {
            device: types::DeviceDataWithoutAlias {
                pubkey: ByteBuf::from(PUBKEY_1),
                credential_id: None,
                purpose: types::Purpose::Authentication,
                key_type: types::KeyType::Unknown,
                protection: types::DeviceProtection::Unprotected,
            },
        },
        sequence_number: 1,
    }
}

pub fn log_entry(idx: u64, timestamp: u64, anchor: types::Anchor) -> types::Entry {
    types::Entry {
        timestamp,
        anchor,
        caller: PrincipalId::new_user_test_id(idx).0,
        operation: types::Operation::UpdateDevice {
            device: ByteBuf::from(PUBKEY_1),
            new_values: types::DeviceDataUpdate {
                alias: None,
                credential_id: None,
                purpose: Some(types::Purpose::Authentication),
                key_type: None,
                protection: Some(types::DeviceProtection::Unprotected),
            },
        },
        sequence_number: idx,
    }
}
