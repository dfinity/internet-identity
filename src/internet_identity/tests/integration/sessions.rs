//! Tests for revocable app sessions: creating one, and minting app delegations from it.

use candid::Principal;
use canister_tests::api::internet_identity::api_v2::{
    get_account_session, prepare_account_session,
};
use canister_tests::flows;
use canister_tests::framework::{
    env, install_ii_with_archive, principal_1, time, verify_delegation, BrowserKey,
};
use internet_identity_interface::internet_identity::types::{
    AccountSessionError, GetAccountSessionRequest, PrepareAccountSessionRequest,
    PrepareAccountSessionResponse,
};
use pocket_ic::{PocketIc, RejectResponse};
use pretty_assertions::assert_eq;
use serde_bytes::ByteBuf;
use std::time::Duration;

const ORIGIN: &str = "https://some-dapp.com";

fn session_request(identity_number: u64) -> PrepareAccountSessionRequest {
    session_request_from(identity_number, &BrowserKey::new(1))
}

/// The same browser presenting the same key again is what makes a sign-in a reuse rather
/// than a registration, so every test that wants a second browser passes a second key.
fn session_request_from(
    identity_number: u64,
    browser: &BrowserKey,
) -> PrepareAccountSessionRequest {
    let session_key = ByteBuf::from(vec![1; 32]);
    let next_device_key = browser.successor().public_key();
    PrepareAccountSessionRequest {
        identity_number,
        origin: ORIGIN.to_string(),
        account_number: None,
        device_name: "Chrome on MacBook".to_string(),
        current_device_key: browser.public_key(),
        current_device_key_signature: browser.sign(&session_key, &next_device_key),
        next_device_key_signature: browser
            .successor()
            .sign_as_successor(&session_key, &browser.public_key()),
        next_device_key,
        session_key,
        permissions: None,
        valid_for: None,
        max_idle: None,
    }
}

/// Creates a session and returns it together with the principal its chain roots at.
fn create_session(
    env: &PocketIc,
    canister_id: Principal,
    identity_number: u64,
) -> (PrepareAccountSessionResponse, Principal) {
    let prepared = prepare_account_session(
        env,
        canister_id,
        principal_1(),
        session_request(identity_number),
    )
    .unwrap()
    .unwrap();
    let session_principal = Principal::self_authenticating(&prepared.user_key);
    (prepared, session_principal)
}

#[test]
fn should_create_a_session_and_witness_its_delegation() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let identity_number = flows::register_anchor(&env, canister_id);

    let (prepared, _) = create_session(&env, canister_id, identity_number);
    assert!(prepared.expiration > time(&env));

    let fetched = get_account_session(
        &env,
        canister_id,
        principal_1(),
        GetAccountSessionRequest {
            identity_number,
            origin: ORIGIN.to_string(),
            account_number: None,
            session_key: ByteBuf::from(vec![1; 32]),
            expiration: prepared.expiration,
        },
    )?
    .unwrap();

    verify_delegation(
        &env,
        prepared.user_key.clone(),
        &fetched.signed_delegation,
        &env.root_key().unwrap(),
    );
    Ok(())
}

#[test]
fn should_refuse_a_session_for_another_anchor() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let identity_number = flows::register_anchor(&env, canister_id);

    let result = prepare_account_session(
        &env,
        canister_id,
        Principal::anonymous(),
        session_request(identity_number),
    )?;

    assert!(matches!(result, Err(AccountSessionError::Unauthorized(_))));

    Ok(())
}

/// Registering a browser happens once per browser per anchor, so it is rare enough to
/// archive, unlike the per-sign-in events the account design keeps out of the archive.
/// The self-reported name is redacted.
#[test]
fn should_archive_a_browser_registration_with_the_name_redacted() -> Result<(), RejectResponse> {
    use canister_tests::api::archive as archive_api;
    use canister_tests::api::internet_identity as ii_api;
    use canister_tests::framework::{
        arg_with_wasm_hash, install_ii_canister_with_arg, ARCHIVE_WASM, II_WASM,
    };
    use internet_identity_interface::archive::types::{Operation, Private};
    use internet_identity_interface::internet_identity::types::DeployArchiveResult;

    let env = env();
    let ii_canister = install_ii_canister_with_arg(
        &env,
        II_WASM.clone(),
        arg_with_wasm_hash(ARCHIVE_WASM.clone()),
    );
    let DeployArchiveResult::Success(archive_canister) =
        ii_api::deploy_archive(&env, ii_canister, &ARCHIVE_WASM)
            .expect("archive deployment failed")
    else {
        panic!("archive deployment did not succeed");
    };
    let identity_number = flows::register_anchor(&env, ii_canister);

    prepare_account_session(
        &env,
        ii_canister,
        principal_1(),
        session_request(identity_number),
    )?
    .unwrap();

    // The same browser signing in again is not a registration.
    let mut again = session_request(identity_number);
    again.origin = "https://another-dapp.com".to_string();
    prepare_account_session(&env, ii_canister, principal_1(), again)?.unwrap();

    env.advance_time(Duration::from_secs(2));
    env.tick();

    let entries = archive_api::get_entries(&env, archive_canister, None, None)?;
    let registrations = entries
        .entries
        .into_iter()
        .flatten()
        .filter(|entry| {
            matches!(
                entry.operation,
                Operation::RegisterSessionDevice {
                    name: Private::Redacted
                }
            )
        })
        .count();
    assert_eq!(registrations, 1);

    Ok(())
}

/// A request naming an account the identity does not hold is the one failure a caller can
/// provoke here, so it must be refused before anything is written. Otherwise a rejected
/// sign-in would still leave a browser in the user's list.
#[test]
fn should_refuse_an_unknown_account_without_registering_a_browser() -> Result<(), RejectResponse> {
    use canister_tests::api::internet_identity::api_v2::identity_info;

    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let identity_number = flows::register_anchor(&env, canister_id);

    let mut request = session_request(identity_number);
    request.account_number = Some(9_999);
    let result = prepare_account_session(&env, canister_id, principal_1(), request)?;

    assert_eq!(result, Err(AccountSessionError::NoSuchAccount));
    assert_eq!(
        identity_info(&env, canister_id, principal_1(), identity_number)?
            .unwrap()
            .session_devices,
        None
    );

    Ok(())
}

/// A browser is named by a key it proves possession of. Without the proof an attacker
/// holding an access method could attribute a session to a browser the user recognises.
#[test]
fn should_refuse_a_signature_from_another_key() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let identity_number = flows::register_anchor(&env, canister_id);

    let mut request = session_request(identity_number);
    request.current_device_key_signature =
        BrowserKey::new(9).sign(&request.session_key, &request.next_device_key);
    let result = prepare_account_session(&env, canister_id, principal_1(), request)?;

    assert_eq!(result, Err(AccountSessionError::InvalidDeviceKey));

    Ok(())
}

/// The proof takes its freshness from the session key, so a signature captured from one
/// request cannot be replayed to attach a second session to that browser.
#[test]
fn should_refuse_a_signature_over_another_session_key() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let identity_number = flows::register_anchor(&env, canister_id);

    let browser = BrowserKey::new(1);
    let mut request = session_request_from(identity_number, &browser);
    request.session_key = ByteBuf::from(vec![2; 32]);
    let result = prepare_account_session(&env, canister_id, principal_1(), request)?;

    assert_eq!(result, Err(AccountSessionError::InvalidDeviceKey));

    Ok(())
}

#[test]
fn should_refuse_a_key_that_is_not_a_public_key() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let identity_number = flows::register_anchor(&env, canister_id);

    let mut request = session_request(identity_number);
    request.current_device_key = ByteBuf::from(vec![0; 91]);
    let result = prepare_account_session(&env, canister_id, principal_1(), request)?;

    assert_eq!(result, Err(AccountSessionError::InvalidDeviceKey));

    Ok(())
}

/// Verification runs before anything is written, so a rejected proof leaves no browser
/// in the user's list.
#[test]
fn should_register_no_browser_when_the_proof_fails() -> Result<(), RejectResponse> {
    use canister_tests::api::internet_identity::api_v2::identity_info;

    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let identity_number = flows::register_anchor(&env, canister_id);

    let mut request = session_request(identity_number);
    request.current_device_key_signature = ByteBuf::from(vec![0; 64]);
    prepare_account_session(&env, canister_id, principal_1(), request)?.unwrap_err();

    assert_eq!(
        identity_info(&env, canister_id, principal_1(), identity_number)?
            .unwrap()
            .session_devices,
        None
    );

    Ok(())
}

/// A key the identity has not seen registers a browser of its own, which is the signal a
/// sign-in from somewhere the user does not recognise gives them.
#[test]
fn should_register_a_second_browser_for_a_key_it_has_not_seen() -> Result<(), RejectResponse> {
    use canister_tests::api::internet_identity::api_v2::identity_info;

    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let identity_number = flows::register_anchor(&env, canister_id);

    prepare_account_session(
        &env,
        canister_id,
        principal_1(),
        session_request(identity_number),
    )?
    .unwrap();

    let mut second = session_request_from(identity_number, &BrowserKey::new(2));
    second.device_name = "Firefox on Linux".to_string();
    prepare_account_session(&env, canister_id, principal_1(), second)?.unwrap();

    let devices = identity_info(&env, canister_id, principal_1(), identity_number)?
        .unwrap()
        .session_devices
        .expect("the identity should hold browsers");

    assert_eq!(devices.len(), 2);
    assert_eq!(devices[0].name, "Chrome on MacBook");
    assert_eq!(devices[1].name, "Firefox on Linux");
    assert_ne!(devices[0].id, devices[1].id);

    Ok(())
}

/// A browser that lost its key is a new browser, which is the cost of the design and
/// what the registry cap is sized for.
#[test]
fn should_register_a_fresh_browser_after_a_storage_wipe() -> Result<(), RejectResponse> {
    use canister_tests::api::internet_identity::api_v2::identity_info;

    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let identity_number = flows::register_anchor(&env, canister_id);

    prepare_account_session(
        &env,
        canister_id,
        principal_1(),
        session_request(identity_number),
    )?
    .unwrap();
    prepare_account_session(
        &env,
        canister_id,
        principal_1(),
        session_request_from(identity_number, &BrowserKey::new(3)),
    )?
    .unwrap();

    let devices = identity_info(&env, canister_id, principal_1(), identity_number)?
        .unwrap()
        .session_devices
        .expect("the identity should hold browsers");

    assert_eq!(devices.len(), 2);

    Ok(())
}

/// The browser rotates its key at every sign-in, so the successor it announced last time is
/// what it presents next.
#[test]
fn should_accept_the_successor_a_browser_announced() -> Result<(), RejectResponse> {
    use canister_tests::api::internet_identity::api_v2::identity_info;

    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let identity_number = flows::register_anchor(&env, canister_id);

    let browser = BrowserKey::new(1);
    let first = prepare_account_session(
        &env,
        canister_id,
        principal_1(),
        session_request_from(identity_number, &browser),
    )?
    .unwrap();

    let rotated = prepare_account_session(
        &env,
        canister_id,
        principal_1(),
        session_request_from(identity_number, &browser.successor()),
    )?
    .unwrap();

    assert_eq!(rotated.device_id, first.device_id);
    assert_eq!(
        identity_info(&env, canister_id, principal_1(), identity_number)?
            .unwrap()
            .session_devices
            .unwrap()
            .len(),
        1
    );

    Ok(())
}

/// Which is what stops a copied browser profile signing in alongside the original without
/// showing up: the key it copied is retired the next time the real browser signs in, so the
/// copy can only come back as a browser of its own.
#[test]
fn should_treat_a_retired_key_as_a_new_browser() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let identity_number = flows::register_anchor(&env, canister_id);

    let browser = BrowserKey::new(1);
    let first = prepare_account_session(
        &env,
        canister_id,
        principal_1(),
        session_request_from(identity_number, &browser),
    )?
    .unwrap();
    prepare_account_session(
        &env,
        canister_id,
        principal_1(),
        session_request_from(identity_number, &browser.successor()),
    )?
    .unwrap();

    // A browser generates a fresh successor for every attempt, a copy of one included, and
    // has to prove it holds it.
    let fresh = BrowserKey::new(7);
    let mut request = session_request_from(identity_number, &browser);
    request.next_device_key = fresh.public_key();
    request.current_device_key_signature =
        browser.sign(&request.session_key, &request.next_device_key);
    request.next_device_key_signature =
        fresh.sign_as_successor(&request.session_key, &browser.public_key());
    let copy = prepare_account_session(&env, canister_id, principal_1(), request)?.unwrap();

    assert_ne!(copy.device_id, first.device_id);

    Ok(())
}

/// A retired key announcing the successor that replaced it is a replay of a request the real
/// browser already made, and the successor is in use, so it is refused outright.
#[test]
fn should_refuse_a_replayed_announcement() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let identity_number = flows::register_anchor(&env, canister_id);

    let browser = BrowserKey::new(1);
    prepare_account_session(
        &env,
        canister_id,
        principal_1(),
        session_request_from(identity_number, &browser),
    )?
    .unwrap();
    prepare_account_session(
        &env,
        canister_id,
        principal_1(),
        session_request_from(identity_number, &browser.successor()),
    )?
    .unwrap();

    let replayed = prepare_account_session(
        &env,
        canister_id,
        principal_1(),
        session_request_from(identity_number, &browser),
    )?;

    assert_eq!(replayed, Err(AccountSessionError::InvalidDeviceKey));

    Ok(())
}

/// A response the browser never received leaves it proving with the key the entry still
/// holds, which must not cost it its identity.
#[test]
fn should_accept_the_current_key_when_a_response_was_lost() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let identity_number = flows::register_anchor(&env, canister_id);

    let browser = BrowserKey::new(1);
    let first = prepare_account_session(
        &env,
        canister_id,
        principal_1(),
        session_request_from(identity_number, &browser),
    )?
    .unwrap();

    let retried = prepare_account_session(
        &env,
        canister_id,
        principal_1(),
        session_request_from(identity_number, &browser),
    )?
    .unwrap();

    assert_eq!(retried.device_id, first.device_id);

    Ok(())
}

/// Presented keys are visible on the wire, so announcing a key another browser is about to
/// present would otherwise take over its entry when it does.
#[test]
fn should_refuse_a_successor_another_browser_holds() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let identity_number = flows::register_anchor(&env, canister_id);

    let victim = BrowserKey::new(1);
    prepare_account_session(
        &env,
        canister_id,
        principal_1(),
        session_request_from(identity_number, &victim),
    )?
    .unwrap();

    let attacker = BrowserKey::new(2);
    let mut request = session_request_from(identity_number, &attacker);
    request.next_device_key = victim.successor().public_key();
    request.current_device_key_signature =
        attacker.sign(&request.session_key, &request.next_device_key);
    let result = prepare_account_session(&env, canister_id, principal_1(), request)?;

    assert_eq!(result, Err(AccountSessionError::InvalidDeviceKey));

    Ok(())
}

/// A key nobody holds cannot be announced: without the successor's own signature, a key read
/// off the wire could be planted as another browser's successor and claimed later.
#[test]
fn should_refuse_a_successor_the_caller_cannot_prove() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let identity_number = flows::register_anchor(&env, canister_id);

    let browser = BrowserKey::new(1);
    let mut request = session_request_from(identity_number, &browser);
    // Everything the wire carries, but the successor's signature made by the wrong key.
    request.next_device_key_signature =
        browser.sign_as_successor(&request.session_key, &browser.public_key());
    let result = prepare_account_session(&env, canister_id, principal_1(), request)?;

    assert_eq!(result, Err(AccountSessionError::InvalidDeviceKey));

    Ok(())
}

/// Rotation is what stops a leaked browser key from being useful for longer than one
/// sign-in, so a browser cannot decline it by announcing the key it is presenting.
#[test]
fn should_refuse_a_successor_equal_to_the_key_presented() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let identity_number = flows::register_anchor(&env, canister_id);

    let browser = BrowserKey::new(1);
    let mut request = session_request_from(identity_number, &browser);
    // Both signatures are real: the caller holds the key it is naming as its own successor.
    request.next_device_key = browser.public_key();
    request.current_device_key_signature =
        browser.sign(&request.session_key, &request.next_device_key);
    request.next_device_key_signature =
        browser.sign_as_successor(&request.session_key, &browser.public_key());
    let result = prepare_account_session(&env, canister_id, principal_1(), request)?;

    assert_eq!(result, Err(AccountSessionError::InvalidDeviceKey));

    Ok(())
}

/// The idle bound is the app's to ask for. Absent it defaults, and a value below the
/// floor is clamped up rather than refused, so a short request still yields a usable
/// session.
#[test]
fn should_store_the_requested_idle_bound() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    canister_tests::api::internet_identity::init_salt(&env, canister_id)?;
    let identity_number = flows::register_anchor(&env, canister_id);

    let browser = BrowserKey::new(1);
    let mut request = session_request_from(identity_number, &browser);
    // Under the ten-minute floor, so the clamp is what makes this session usable.
    request.max_idle = Some(60_000_000_000);
    prepare_account_session(&env, canister_id, principal_1(), request)?.unwrap();

    // A session clamped up to the floor still mints, which is the point of clamping
    // rather than refusing.
    let devices = canister_tests::api::internet_identity::api_v2::identity_info(
        &env,
        canister_id,
        principal_1(),
        identity_number,
    )?
    .unwrap()
    .session_devices
    .unwrap_or_default();
    assert_eq!(devices.len(), 1);

    Ok(())
}

/// Announcing a key another browser of this identity holds keeps two entries from answering
/// to one key, which is what makes resolving a presented key unambiguous.
#[test]
fn should_refuse_a_successor_another_browser_holds_even_when_proven() -> Result<(), RejectResponse>
{
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let identity_number = flows::register_anchor(&env, canister_id);

    let victim = BrowserKey::new(1);
    prepare_account_session(
        &env,
        canister_id,
        principal_1(),
        session_request_from(identity_number, &victim),
    )?
    .unwrap();

    // The attacker proves possession of the victim's key, as a profile copy could.
    let attacker = BrowserKey::new(2);
    let mut request = session_request_from(identity_number, &attacker);
    request.next_device_key = victim.public_key();
    request.current_device_key_signature =
        attacker.sign(&request.session_key, &request.next_device_key);
    request.next_device_key_signature =
        victim.sign_as_successor(&request.session_key, &attacker.public_key());
    let result = prepare_account_session(&env, canister_id, principal_1(), request)?;

    assert_eq!(result, Err(AccountSessionError::InvalidDeviceKey));

    Ok(())
}
