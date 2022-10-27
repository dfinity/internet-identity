use candid::utils::{decode_args, encode_args, ArgumentDecoder, ArgumentEncoder};
use candid::{parser::value::IDLValue, IDLArgs, Principal};
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
use std::env;
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

        I will look for at {:?}, and you can specify another path with the environment variable II_WASM_PREVIOUS (note that I run from {:?}).

        In order to get the Wasm module, please run the following command:
            curl -SL https://github.com/dfinity/internet-identity/releases/latest/download/internet_identity_test.wasm -o internet_identity_previous.wasm
        ", &def_path, &std::env::current_dir().map(|x| x.display().to_string()).unwrap_or("an unknown directory".to_string()));
        get_wasm_path("II_WASM".to_string(), &def_path).expect(&err)
    };
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

pub fn upgrade_ii_canister(env: &StateMachine, canister_id: CanisterId, wasm: Vec<u8>) {
    let nulls = vec![IDLValue::Null; 1];
    let args = IDLArgs::new(&nulls);
    let byts = args.to_bytes().unwrap();
    env.upgrade_canister(canister_id, wasm, byts).unwrap()
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
