//! End-to-end enforcement tests for read-only (queries-only) delegations.
//!
//! Every other delegation test in this suite talks to the canister through
//! PocketIC's impersonating test transport, which skips the replica's request
//! authentication entirely. These tests instead switch the instance to a live
//! HTTP endpoint (`make_live`) and submit *real signed request envelopes*, so
//! the replica's ingress validation — including the queries-only `permissions`
//! extension (dfinity/ic#10449) — actually runs.
//!
//! The envelopes are built by hand (CBOR + ed25519 + the interface spec's
//! representation-independent hash) because no released `ic-agent` /
//! `ic-transport-types` can carry the `permissions` field yet: their
//! `Delegation` wire type still ends at `targets`. Hand-rolling also lets us
//! deliberately *strip* the field to prove the signature binds it.
//!
//! What is asserted, per flow that mints read-only delegations
//! (account delegations serve both `/continue` and `/cli`; MCP has its own
//! mint path):
//!
//! 1. a queries-only delegation authenticates a real **query** call — the
//!    request passes ingress validation and executes as the delegated
//!    principal;
//! 2. a queries-only delegation is **rejected at ingress for update** calls
//!    with the replica's own enforcement error ("Update calls are not
//!    permitted…") — the request never reaches the canister;
//! 3. an unrestricted delegation authenticates **both** query and update
//!    calls end-to-end (this also pins down that dfinity/ic#10226's strict
//!    `subnet_type` certificate check does not reject pocket-ic v15's own
//!    canister-signature delegations);
//! 4. a queries-only delegation **fails closed**: resubmitting it with the
//!    `permissions` field stripped from the envelope is rejected as an
//!    invalid delegation signature — omitting the restriction cannot escalate
//!    a read-only delegation to full access.

use candid::{Encode, Principal};
use canister_tests::api::internet_identity::api_v2::{
    get_account_delegation_with_read_only, mcp_get_account_delegation,
    mcp_prepare_account_delegation, mcp_set_access_with_read_only, prepare_account_delegation,
    prepare_account_delegation_with_read_only, AccountDelegationParams,
};
use canister_tests::flows;
use canister_tests::framework::*;
use ed25519_dalek::{Signer, SigningKey};
use ic_representation_independent_hash::{representation_independent_hash, Value};
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, McpPrepareDelegation, PrepareAccountDelegation, SignedDelegation,
};
use pocket_ic::{PocketIc, Time};
use serde_bytes::ByteBuf;
use serde_cbor::Value as Cbor;
use std::collections::BTreeMap;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

/// RFC 8410 DER prefix for an ed25519 SubjectPublicKeyInfo.
const ED25519_DER_PREFIX: [u8; 12] = [
    0x30, 0x2a, 0x30, 0x05, 0x06, 0x03, 0x2b, 0x65, 0x70, 0x03, 0x21, 0x00,
];

const READ_ONLY_UPDATE_REJECTION: &str = "Update calls are not permitted";

fn session_key_pair(seed: [u8; 32]) -> (SigningKey, Vec<u8>) {
    let signing_key = SigningKey::from_bytes(&seed);
    let mut der = ED25519_DER_PREFIX.to_vec();
    der.extend_from_slice(signing_key.verifying_key().as_bytes());
    (signing_key, der)
}

/// Everything needed to authenticate live requests through an II delegation.
struct DelegatedSession {
    session_key: SigningKey,
    session_pubkey_der: Vec<u8>,
    /// The canister-signature public key (DER) the delegation chain starts at.
    user_key: Vec<u8>,
    /// `self_authenticating(user_key)` — the caller canisters observe.
    principal: Principal,
    signed_delegation: SignedDelegation,
    anchor: AnchorNumber,
}

/// Mints an account delegation (the `/continue` and `/cli` flows) onto a
/// fresh ed25519 session key.
fn mint_account_delegation(
    env: &PocketIc,
    canister_id: Principal,
    seed: [u8; 32],
    read_only: bool,
) -> DelegatedSession {
    let anchor = flows::register_anchor(env, canister_id);
    let (session_key, session_pubkey_der) = session_key_pair(seed);

    let params = AccountDelegationParams::new(
        env,
        canister_id,
        principal_1(),
        anchor,
        "https://some-dapp.com".to_string(),
        None,
        ByteBuf::from(session_pubkey_der.clone()),
    );
    let PrepareAccountDelegation {
        user_key,
        expiration,
    } = prepare_account_delegation_with_read_only(&params, None, Some(read_only))
        .expect("prepare_account_delegation call failed")
        .expect("prepare_account_delegation returned an error");
    let signed_delegation =
        get_account_delegation_with_read_only(&params, expiration, Some(read_only))
            .expect("get_account_delegation call failed")
            .expect("get_account_delegation returned an error");

    let principal = Principal::self_authenticating(user_key.as_slice());
    DelegatedSession {
        session_key,
        session_pubkey_der,
        user_key: user_key.into_vec(),
        principal,
        signed_delegation,
        anchor,
    }
}

/// Mints a per-app delegation through the `/mcp` flow: the anchor grants a
/// (read-only) access to its MCP-server principal, which then mints the
/// delegation onto our session key.
fn mint_mcp_delegation(
    env: &PocketIc,
    canister_id: Principal,
    seed: [u8; 32],
    read_only: bool,
) -> DelegatedSession {
    const MCP_ORIGIN: &str = "https://mcp.id.ai";
    let anchor = flows::register_anchor(env, canister_id);
    let (session_key, session_pubkey_der) = session_key_pair(seed);

    // The MCP server's own principal: the anchor's default-account principal
    // at the MCP origin (as derived by the canister).
    let standing_params = AccountDelegationParams::new(
        env,
        canister_id,
        principal_1(),
        anchor,
        MCP_ORIGIN.to_string(),
        None,
        ByteBuf::from("mcp standing session key"),
    );
    let PrepareAccountDelegation { user_key, .. } =
        prepare_account_delegation(&standing_params, None)
            .unwrap()
            .unwrap();
    let mcp = Principal::self_authenticating(user_key);

    mcp_set_access_with_read_only(
        env,
        canister_id,
        principal_1(),
        anchor,
        MCP_ORIGIN.to_string(),
        true,
        Some(read_only),
    )
    .unwrap()
    .unwrap();

    let McpPrepareDelegation {
        user_key,
        expiration,
        account_number,
    } = mcp_prepare_account_delegation(
        env,
        canister_id,
        mcp,
        "https://some-app.com".to_string(),
        None,
        ByteBuf::from(session_pubkey_der.clone()),
        None,
    )
    .unwrap()
    .unwrap();
    let signed_delegation = mcp_get_account_delegation(
        env,
        canister_id,
        mcp,
        "https://some-app.com".to_string(),
        account_number,
        ByteBuf::from(session_pubkey_der.clone()),
        expiration,
    )
    .unwrap()
    .unwrap();

    let principal = Principal::self_authenticating(user_key.as_slice());
    DelegatedSession {
        session_key,
        session_pubkey_der,
        user_key: user_key.into_vec(),
        principal,
        signed_delegation,
        anchor,
    }
}

fn now_nanos() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos() as u64
}

fn cbor_map(entries: Vec<(&str, Cbor)>) -> Cbor {
    Cbor::Map(
        entries
            .into_iter()
            .map(|(k, v)| (Cbor::Text(k.to_string()), v))
            .collect::<BTreeMap<_, _>>(),
    )
}

/// Builds and signs a complete request envelope for `request_type`
/// ("query" or "call"), authenticated through the session's delegation.
/// `strip_permissions` deliberately omits the delegation's `permissions`
/// field from the envelope (the signature still binds it).
fn build_envelope(
    session: &DelegatedSession,
    canister_id: Principal,
    request_type: &str,
    method_name: &str,
    arg: Vec<u8>,
    strip_permissions: bool,
) -> Vec<u8> {
    let ingress_expiry = now_nanos() + Duration::from_secs(200).as_nanos() as u64;

    // Request id: the interface spec's representation-independent hash of
    // the content map.
    let request_id = representation_independent_hash(&[
        (
            "request_type".to_string(),
            Value::String(request_type.to_string()),
        ),
        (
            "sender".to_string(),
            Value::Bytes(session.principal.as_slice().to_vec()),
        ),
        (
            "canister_id".to_string(),
            Value::Bytes(canister_id.as_slice().to_vec()),
        ),
        (
            "method_name".to_string(),
            Value::String(method_name.to_string()),
        ),
        ("arg".to_string(), Value::Bytes(arg.clone())),
        ("ingress_expiry".to_string(), Value::Number(ingress_expiry)),
    ]);

    // The session key signs the request: "\x0Aic-request" ++ request_id.
    let mut signable = vec![0x0a];
    signable.extend_from_slice(b"ic-request");
    signable.extend_from_slice(&request_id);
    let sender_sig = session.session_key.sign(&signable).to_bytes().to_vec();

    let content = cbor_map(vec![
        ("request_type", Cbor::Text(request_type.to_string())),
        ("sender", Cbor::Bytes(session.principal.as_slice().to_vec())),
        ("canister_id", Cbor::Bytes(canister_id.as_slice().to_vec())),
        ("method_name", Cbor::Text(method_name.to_string())),
        ("arg", Cbor::Bytes(arg)),
        ("ingress_expiry", Cbor::Integer(ingress_expiry as i128)),
    ]);

    let delegation = &session.signed_delegation.delegation;
    let mut delegation_fields = vec![
        ("pubkey", Cbor::Bytes(session.session_pubkey_der.clone())),
        ("expiration", Cbor::Integer(delegation.expiration as i128)),
    ];
    match &delegation.permissions {
        Some(permissions) if !strip_permissions => {
            delegation_fields.push(("permissions", Cbor::Text(permissions.clone())));
        }
        _ => {}
    }
    let sender_delegation = Cbor::Array(vec![cbor_map(vec![
        ("delegation", cbor_map(delegation_fields)),
        (
            "signature",
            Cbor::Bytes(session.signed_delegation.signature.clone().into_vec()),
        ),
    ])]);

    let envelope = cbor_map(vec![
        ("content", content),
        ("sender_pubkey", Cbor::Bytes(session.user_key.clone())),
        ("sender_sig", Cbor::Bytes(sender_sig)),
        ("sender_delegation", sender_delegation),
    ]);

    // Standard agents prepend the CBOR self-describe tag.
    let mut bytes = Vec::new();
    let mut serializer = serde_cbor::Serializer::new(&mut bytes);
    serializer
        .self_describe()
        .expect("failed to write CBOR tag");
    serde::Serialize::serialize(&envelope, &mut serializer).expect("failed to serialize envelope");
    bytes
}

/// POSTs an envelope to the live endpoint. Returns (HTTP status, body).
fn submit(
    live_url: &str,
    canister_id: Principal,
    endpoint: &str,
    envelope: Vec<u8>,
) -> (u16, Vec<u8>) {
    let url = format!(
        "{live_url}api/v2/canister/{}/{endpoint}",
        canister_id.to_text()
    );
    // Bounded timeout so a stalled endpoint fails the test deterministically
    // instead of hanging the suite.
    let client = reqwest::blocking::Client::builder()
        .timeout(Duration::from_secs(60))
        .build()
        .expect("failed to build HTTP client");
    let response = client
        .post(url)
        .header("Content-Type", "application/cbor")
        .body(envelope)
        .send()
        .expect("failed to POST envelope");
    let status = response.status().as_u16();
    let body = response.bytes().expect("failed to read body").to_vec();
    (status, body)
}

fn lossy(body: &[u8]) -> String {
    // Query responses are CBOR; ingress-validation errors are plain text.
    // A debug-rendering of the parsed CBOR (or the raw text) is enough for
    // `contains` assertions.
    match serde_cbor::from_slice::<Cbor>(body) {
        Ok(value) => format!("{value:?}"),
        Err(_) => String::from_utf8_lossy(body).to_string(),
    }
}

/// Sets up a live instance and runs the pass/fail call matrix for one
/// delegated session. `expect_read_only` asserts the delegation shape first,
/// guarding against a stale prebuilt II wasm silently ignoring `read_only`.
fn assert_ingress_enforcement(
    env: &mut PocketIc,
    canister_id: Principal,
    session: DelegatedSession,
    expect_read_only: bool,
) {
    assert_eq!(
        session.signed_delegation.delegation.permissions,
        expect_read_only.then(|| "queries".to_string()),
        "unexpected delegation shape; is internet_identity.wasm.gz up to date?"
    );

    let live_url = env.make_live(None).to_string();
    let query_arg = Encode!(&session.anchor, &"https://some-dapp.com").unwrap();
    let update_arg = Encode!(&session.anchor).unwrap();

    // Query: must pass ingress validation for BOTH delegation shapes and
    // execute as the delegated principal. (`get_principal` then traps with
    // "<caller> could not be authenticated" since the delegated principal is
    // not a device of the anchor — naming the caller is the proof that the
    // replica authenticated the delegation and handed the call to the
    // canister with the delegated identity.)
    let envelope = build_envelope(
        &session,
        canister_id,
        "query",
        "get_principal",
        query_arg,
        false,
    );
    let (status, body) = submit(&live_url, canister_id, "query", envelope);
    let body = lossy(&body);
    assert_eq!(status, 200, "query did not pass ingress validation: {body}");
    assert!(
        body.contains(&session.principal.to_text()),
        "query did not execute as the delegated principal: {body}"
    );

    // Update: accepted at ingress (HTTP 202) for an unrestricted delegation;
    // rejected at ingress with the queries-only enforcement error for a
    // read-only one — before any canister code runs.
    let envelope = build_envelope(
        &session,
        canister_id,
        "call",
        "enter_device_registration_mode",
        update_arg,
        false,
    );
    let (status, body) = submit(&live_url, canister_id, "call", envelope);
    let body = lossy(&body);
    if expect_read_only {
        assert_eq!(
            status, 400,
            "read-only update was not rejected at ingress: {body}"
        );
        assert!(
            body.contains(READ_ONLY_UPDATE_REJECTION),
            "unexpected rejection reason for read-only update: {body}"
        );
    } else {
        assert_eq!(
            status, 202,
            "full-access update was not accepted at ingress: {body}"
        );
    }

    // Fail closed: a queries-only delegation with the `permissions` field
    // stripped from the envelope must be rejected as an invalid delegation —
    // the canister signature binds the field, so omitting it cannot escalate
    // the delegation to full access.
    if expect_read_only {
        let query_arg = Encode!(&session.anchor, &"https://some-dapp.com").unwrap();
        let envelope = build_envelope(
            &session,
            canister_id,
            "query",
            "get_principal",
            query_arg,
            true,
        );
        let (status, body) = submit(&live_url, canister_id, "query", envelope);
        let body = lossy(&body);
        assert_eq!(
            status, 400,
            "stripped read-only delegation was not rejected at ingress: {body}"
        );
        assert!(
            !body.contains(&session.principal.to_text()),
            "stripped read-only delegation reached the canister: {body}"
        );
    }
}

/// Prepares a fresh instance whose clock matches real time. `make_live`
/// validates request expiry against the wall clock, while a fresh instance
/// starts at a 2021 mock time — align BEFORE minting delegations so their
/// expiry is in the future.
fn live_ready_env() -> (PocketIc, Principal) {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    env.set_time(Time::from(SystemTime::now()));
    env.tick();
    (env, canister_id)
}

#[test]
fn should_enforce_read_only_account_delegation_at_ingress() {
    let (mut env, canister_id) = live_ready_env();
    let session = mint_account_delegation(&env, canister_id, [7u8; 32], true);
    assert_ingress_enforcement(&mut env, canister_id, session, true);
}

#[test]
fn should_authenticate_unrestricted_account_delegation_at_ingress() {
    let (mut env, canister_id) = live_ready_env();
    let session = mint_account_delegation(&env, canister_id, [8u8; 32], false);
    assert_ingress_enforcement(&mut env, canister_id, session, false);
}

#[test]
fn should_enforce_read_only_mcp_delegation_at_ingress() {
    let (mut env, canister_id) = live_ready_env();
    let session = mint_mcp_delegation(&env, canister_id, [9u8; 32], true);
    assert_ingress_enforcement(&mut env, canister_id, session, true);
}

#[test]
fn should_authenticate_unrestricted_mcp_delegation_at_ingress() {
    let (mut env, canister_id) = live_ready_env();
    let session = mint_mcp_delegation(&env, canister_id, [10u8; 32], false);
    assert_ingress_enforcement(&mut env, canister_id, session, false);
}
