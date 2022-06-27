use crate::flows;
use candid::utils::{decode_args, encode_args, ArgumentDecoder, ArgumentEncoder};
use candid::{parser::value::IDLValue, IDLArgs};
use ic_crypto_internal_basic_sig_iccsa::types::SignatureBytes;
use ic_crypto_internal_basic_sig_iccsa::{public_key_bytes_from_der, verify};
use ic_error_types::ErrorCode;
use ic_interfaces::crypto::Signable;
use ic_state_machine_tests::{CanisterId, PrincipalId, StateMachine, UserError, WasmResult};
use ic_types::messages::Delegation;
use ic_types::Time;
use internet_identity_interface as types;
use internet_identity_interface::{HeaderField, SignedDelegation, UserKey};
use lazy_static::lazy_static;
use regex::Regex;
use sdk_ic_types::Principal;
use serde_bytes::ByteBuf;
use std::env;
use std::path;
use std::time::{Duration, SystemTime};

/* The first few lines deal with actually getting the Wasm module(s) to test */

lazy_static! {
    /** The Wasm module for the current build, i.e. the one we're testing */
    pub static ref II_WASM: Vec<u8> = {
        let def_path = path::PathBuf::from("..").join("..").join("internet_identity.wasm");
        let err = format!("
        Could not find Internet Identity Wasm module for current build.

        I will look for it at {:?}, and you can specify another path with the environment variable II_WASM (note that I run from {:?}).

        In order to build the Wasm module, please run the following command:
            II_DUMMY_CAPTHA=1 ./scripts/build
        ", &def_path, &std::env::current_dir().map(|x| x.display().to_string()).unwrap_or("an unknown directory".to_string()));
        get_wasm_path("II_WASM".to_string(), &def_path).expect(&err)
    };

    /** The Wasm module for the _previous_ build, or latest release, which is used when testing
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
    let nulls = vec![IDLValue::Null; 1];
    let args = IDLArgs::new(&nulls);
    let byts = args.to_bytes().unwrap();
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

pub fn principal_1() -> PrincipalId {
    PrincipalId(Principal::self_authenticating(PUBKEY_1))
}

pub fn principal_2() -> PrincipalId {
    PrincipalId(Principal::self_authenticating(PUBKEY_2))
}

pub fn device_data_1() -> types::DeviceData {
    types::DeviceData {
        pubkey: ByteBuf::from(PUBKEY_1),
        alias: "My Device".to_string(),
        credential_id: None,
        purpose: types::Purpose::Authentication,
        key_type: types::KeyType::Unknown,
    }
}

pub fn device_data_2() -> types::DeviceData {
    types::DeviceData {
        pubkey: ByteBuf::from(PUBKEY_2),
        alias: "My second device".to_string(),
        credential_id: None,
        purpose: types::Purpose::Authentication,
        key_type: types::KeyType::Unknown,
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
                "Failed to decode bytes {:?} as candid type: {}",
                std::any::type_name::<Output>(),
                e
            )
        })),
        Ok(WasmResult::Reject(message)) => Err(CallError::Reject(message)),
        Err(user_error) => Err(CallError::UserError(user_error)),
    }
}

pub fn expect_user_error_with_message<T>(
    result: Result<T, CallError>,
    error_code: ErrorCode,
    message_pattern: Regex,
) {
    assert!(matches!(result,
            Err(CallError::UserError(user_error)) if user_error.code() == error_code &&
            message_pattern.is_match(user_error.description())));
}

pub fn verify_security_headers(headers: &Vec<HeaderField>) {
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
connect-src 'self' https://ic0.app;\
img-src 'self' data:;\
script-src 'sha256-[a-zA-Z0-9/=+]+' 'unsafe-inline' 'unsafe-eval' 'strict-dynamic' https:;\
base-uri 'none';\
frame-ancestors 'none';\
form-action 'none';\
style-src 'self' 'unsafe-inline' https://fonts\\.googleapis\\.com;\
style-src-elem 'unsafe-inline' https://fonts\\.googleapis\\.com;\
font-src https://fonts\\.gstatic\\.com;\
upgrade-insecure-requests;$"
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

pub fn assert_metric(env: &StateMachine, canister_id: CanisterId, metric: &str, expected: u64) {
    let metrics = flows::get_metrics(env, canister_id);
    let (value, _) = parse_metric(&metrics, metric);
    assert_eq!(value, expected);
}

pub fn verify_delegation(
    env: &StateMachine,
    user_key: UserKey,
    signed_delegation: &SignedDelegation,
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
