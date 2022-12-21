use candid::utils::{ArgumentDecoder, ArgumentEncoder};
use candid::{decode_args, encode_args, Deserialize, Principal};
use ciborium::de::from_reader;
use ic_cdk::api::management_canister::main::{
    CanisterId, CanisterIdRecord, CanisterInstallMode, CreateCanisterArgument, InstallCodeArgument,
};
use serde::de::DeserializeOwned;
use serde::Serialize;
use serde_bytes::ByteBuf;
use std::cell::RefCell;
use std::fmt;
use std::io::{Read, Write};
use std::process::{Child, ChildStdin, ChildStdout, Command, Stdio};
use std::time::{Duration, SystemTime};

#[derive(Serialize)]
pub enum Request {
    RootKey,
    Time,
    AdvanceTime(Duration),
    CanisterUpdateCall(CanisterCall),
    CanisterQueryCall(CanisterCall),
    CanisterExists(RawCanisterId),
    CyclesBalance(RawCanisterId),
    AddCycles(AddCyclesArg),
    SetStableMemory(SetStableMemoryArg),
    ReadStableMemory(RawCanisterId),
}

#[derive(Serialize)]
pub struct AddCyclesArg {
    // raw bytes of the principal
    pub canister_id: Vec<u8>,
    pub amount: u128,
}

#[derive(Serialize)]
pub struct SetStableMemoryArg {
    // raw bytes of the principal
    pub canister_id: Vec<u8>,
    pub data: ByteBuf,
}

#[derive(Serialize)]
pub struct RawCanisterId {
    // raw bytes of the principal
    pub canister_id: Vec<u8>,
}

impl From<Principal> for RawCanisterId {
    fn from(principal: Principal) -> Self {
        Self {
            canister_id: principal.as_slice().to_vec(),
        }
    }
}

#[derive(Serialize)]
pub struct CanisterCall {
    pub sender: Vec<u8>,
    pub canister_id: Vec<u8>,
    pub method: String,
    pub arg: Vec<u8>,
}

pub struct StateMachine {
    proc: Child,
    child_in: RefCell<ChildStdin>,
    child_out: RefCell<ChildStdout>,
}

impl StateMachine {
    pub fn new(binary_path: &str, debug: bool) -> Self {
        let mut command = Command::new(binary_path);
        command
            .env("LOG_TO_STDERR", "1")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped());

        if debug {
            command.arg("--debug");
        }

        let mut child = command.spawn().unwrap_or_else(|err| {
            panic!(
                "failed to start test state machine at path {}: {:?}",
                binary_path, err
            )
        });

        let child_in = child.stdin.take().unwrap();
        let child_out = child.stdout.take().unwrap();
        Self {
            proc: child,
            child_in: RefCell::new(child_in),
            child_out: RefCell::new(child_out),
        }
    }

    pub fn update_call(
        &self,
        canister_id: Principal,
        sender: Principal,
        method: &str,
        arg: Vec<u8>,
    ) -> Result<WasmResult, UserError> {
        self.call_state_machine(Request::CanisterUpdateCall(CanisterCall {
            sender: sender.as_slice().to_vec(),
            canister_id: canister_id.as_slice().to_vec(),
            method: method.to_string(),
            arg,
        }))
    }

    pub fn query_call(
        &self,
        canister_id: Principal,
        sender: Principal,
        method: &str,
        arg: Vec<u8>,
    ) -> Result<WasmResult, UserError> {
        self.call_state_machine(Request::CanisterQueryCall(CanisterCall {
            sender: sender.as_slice().to_vec(),
            canister_id: canister_id.as_slice().to_vec(),
            method: method.to_string(),
            arg,
        }))
    }

    pub fn root_key(&self) -> Vec<u8> {
        self.call_state_machine(Request::RootKey)
    }

    pub fn create_canister(&self) -> CanisterId {
        let CanisterIdRecord { canister_id } = call_candid(
            self,
            Principal::management_canister(),
            "create_canister",
            (CreateCanisterArgument { settings: None },),
        )
        .map(|(x,)| x)
        .unwrap();
        canister_id
    }

    pub fn install_canister(&self, canister_id: CanisterId, wasm_module: Vec<u8>, arg: Vec<u8>) {
        call_candid::<(InstallCodeArgument,), ()>(
            self,
            Principal::management_canister(),
            "install_code",
            (InstallCodeArgument {
                mode: CanisterInstallMode::Install,
                canister_id,
                wasm_module,
                arg,
            },),
        )
        .unwrap();
    }

    pub fn upgrade_canister(
        &self,
        canister_id: CanisterId,
        wasm_module: Vec<u8>,
        arg: Vec<u8>,
    ) -> Result<(), CallError> {
        call_candid::<(InstallCodeArgument,), ()>(
            self,
            Principal::management_canister(),
            "install_code",
            (InstallCodeArgument {
                mode: CanisterInstallMode::Upgrade,
                canister_id,
                wasm_module,
                arg,
            },),
        )
    }

    pub fn canister_exists(&self, canister_id: Principal) -> bool {
        self.call_state_machine(Request::CanisterExists(RawCanisterId::from(canister_id)))
    }

    pub fn time(&self) -> SystemTime {
        self.call_state_machine(Request::Time)
    }

    pub fn advance_time(&self, duration: Duration) {
        self.call_state_machine(Request::AdvanceTime(duration))
    }

    pub fn stable_memory(&self, canister_id: Principal) -> Vec<u8> {
        self.call_state_machine(Request::ReadStableMemory(RawCanisterId::from(canister_id)))
    }

    pub fn set_stable_memory(&self, canister_id: Principal, data: ByteBuf) {
        self.call_state_machine(Request::SetStableMemory(SetStableMemoryArg {
            canister_id: canister_id.as_slice().to_vec(),
            data,
        }))
    }

    pub fn cycle_balance(&self, canister_id: Principal) -> u128 {
        self.call_state_machine(Request::CyclesBalance(RawCanisterId::from(canister_id)))
    }

    pub fn add_cycles(&self, canister_id: Principal, amount: u128) -> u128 {
        self.call_state_machine(Request::AddCycles(AddCyclesArg {
            canister_id: canister_id.as_slice().to_vec(),
            amount,
        }))
    }

    fn call_state_machine<T: DeserializeOwned>(&self, request: Request) -> T {
        self.send_request(request);
        self.read_response()
    }

    fn send_request(&self, request: Request) {
        let mut cbor = vec![];
        ciborium::ser::into_writer(&request, &mut cbor).expect("failed to serialize request");
        let mut child_in = self.child_in.borrow_mut();
        child_in
            .write_all(&(cbor.len() as u64).to_le_bytes())
            .expect("failed to send request length");
        child_in
            .write_all(cbor.as_slice())
            .expect("failed to send request data");
        child_in.flush().expect("failed to flush child stdin");
    }

    fn read_response<T: DeserializeOwned>(&self) -> T {
        let vec = self.read_bytes(8);
        let size = usize::from_le_bytes(TryFrom::try_from(vec).expect("failed to read data size"));
        from_reader(&self.read_bytes(size)[..]).expect("failed to deserialize response")
    }

    fn read_bytes(&self, num_bytes: usize) -> Vec<u8> {
        let mut buf = vec![0u8; num_bytes];
        self.child_out
            .borrow_mut()
            .read_exact(&mut buf)
            .expect("failed to read from child_stdout");
        buf
    }
}

impl Drop for StateMachine {
    fn drop(&mut self) {
        self.proc
            .kill()
            .expect("failed to kill state machine process")
    }
}

/// Call a canister candid query method, anonymous.
pub fn query_candid<Input, Output>(
    env: &StateMachine,
    canister_id: Principal,
    method: &str,
    input: Input,
) -> Result<Output, CallError>
where
    Input: ArgumentEncoder,
    Output: for<'a> ArgumentDecoder<'a>,
{
    query_candid_as(env, canister_id, Principal::anonymous(), method, input)
}

/// Call a canister candid query method, authenticated.
pub fn query_candid_as<Input, Output>(
    env: &StateMachine,
    canister_id: Principal,
    sender: Principal,
    method: &str,
    input: Input,
) -> Result<Output, CallError>
where
    Input: ArgumentEncoder,
    Output: for<'a> ArgumentDecoder<'a>,
{
    with_candid(input, |bytes| {
        env.query_call(canister_id, sender, method, bytes)
    })
}

/// Call a canister candid method, authenticated.
/// The state machine executes update calls synchronously, so there is no need to poll for the result.
pub fn call_candid_as<Input, Output>(
    env: &StateMachine,
    canister_id: Principal,
    sender: Principal,
    method: &str,
    input: Input,
) -> Result<Output, CallError>
where
    Input: ArgumentEncoder,
    Output: for<'a> ArgumentDecoder<'a>,
{
    with_candid(input, |bytes| {
        env.update_call(canister_id, sender, method, bytes)
    })
}

/// Call a canister candid method, anonymous.
/// The state machine executes update calls synchronously, so there is no need to poll for the result.
pub fn call_candid<Input, Output>(
    env: &StateMachine,
    canister_id: Principal,
    method: &str,
    input: Input,
) -> Result<Output, CallError>
where
    Input: ArgumentEncoder,
    Output: for<'a> ArgumentDecoder<'a>,
{
    call_candid_as(env, canister_id, Principal::anonymous(), method, input)
}

/// User-facing error codes.
///
/// The error codes are currently assigned using an HTTP-like
/// convention: the most significant digit is the corresponding reject
/// code and the rest is just a sequentially assigned two-digit
/// number.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ErrorCode {
    SubnetOversubscribed = 101,
    MaxNumberOfCanistersReached = 102,
    CanisterOutputQueueFull = 201,
    IngressMessageTimeout = 202,
    CanisterQueueNotEmpty = 203,
    CanisterNotFound = 301,
    CanisterMethodNotFound = 302,
    CanisterAlreadyInstalled = 303,
    CanisterWasmModuleNotFound = 304,
    InsufficientMemoryAllocation = 402,
    InsufficientCyclesForCreateCanister = 403,
    SubnetNotFound = 404,
    CanisterNotHostedBySubnet = 405,
    CanisterOutOfCycles = 501,
    CanisterTrapped = 502,
    CanisterCalledTrap = 503,
    CanisterContractViolation = 504,
    CanisterInvalidWasm = 505,
    CanisterDidNotReply = 506,
    CanisterOutOfMemory = 507,
    CanisterStopped = 508,
    CanisterStopping = 509,
    CanisterNotStopped = 510,
    CanisterStoppingCancelled = 511,
    CanisterInvalidController = 512,
    CanisterFunctionNotFound = 513,
    CanisterNonEmpty = 514,
    CertifiedStateUnavailable = 515,
    CanisterRejectedMessage = 516,
    QueryCallGraphLoopDetected = 517,
    UnknownManagementMessage = 518,
    InvalidManagementPayload = 519,
    InsufficientCyclesInCall = 520,
    CanisterWasmEngineError = 521,
    CanisterInstructionLimitExceeded = 522,
    CanisterInstallCodeRateLimited = 523,
    CanisterMemoryAccessLimitExceeded = 524,
    QueryCallGraphTooDeep = 525,
    QueryCallGraphTotalInstructionLimitExceeded = 526,
    CompositeQueryCalledInReplicatedMode = 527,
}

impl fmt::Display for ErrorCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // E.g. "IC0301"
        write!(f, "IC{:04}", *self as i32)
    }
}

/// The error that is sent back to users of IC if something goes
/// wrong. It's designed to be copyable and serializable so that we
/// can persist it in ingress history.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct UserError {
    pub code: ErrorCode,
    pub description: String,
}

impl fmt::Display for UserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // E.g. "IC0301: Canister 42 not found"
        write!(f, "{}: {}", self.code, self.description)
    }
}

#[derive(Debug)]
pub enum CallError {
    Reject(String),
    UserError(UserError),
}

/// This struct describes the different types that executing a Wasm function in
/// a canister can produce
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum WasmResult {
    /// Raw response, returned in a "happy" case
    Reply(#[serde(with = "serde_bytes")] Vec<u8>),
    /// Returned with an error message when the canister decides to reject the
    /// message
    Reject(String),
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
