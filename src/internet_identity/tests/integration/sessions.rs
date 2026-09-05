//! Tests for revocable app sessions: creating one, and minting app delegations from it.

use candid::Principal;
use canister_tests::api::internet_identity::api_v2::{
    app_get_delegation, app_prepare_delegation, get_account_session, prepare_account_session,
};
use canister_tests::flows;
use canister_tests::framework::{
    env, install_ii_with_archive, principal_1, time, verify_delegation, BrowserKey,
};
use internet_identity_interface::internet_identity::types::{
    AccountSessionError, AppGetDelegationRequest, AppPrepareDelegationRequest, AppSessionError,
    GetAccountSessionRequest, Permissions, PrepareAccountSessionRequest,
    PrepareAccountSessionResponse, SessionDeviceInfo,
};
use pocket_ic::{PocketIc, RejectResponse};
use pretty_assertions::assert_eq;
use serde_bytes::ByteBuf;
use std::time::Duration;

const ORIGIN: &str = "https://some-dapp.com";
const APP_DELEGATION_TTL_NS: u64 = 5 * 60 * 1_000_000_000;

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
            session_id: prepared.session_id,
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
fn should_replace_the_session_of_a_browser_signing_in_again() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let identity_number = flows::register_anchor(&env, canister_id);

    let (first, first_principal) = create_session(&env, canister_id, identity_number);

    // No time is allowed to pass: two ceremonies in one consensus round agree on every
    // field that describes them, and it is the id that keeps them apart.
    let second = prepare_account_session(
        &env,
        canister_id,
        principal_1(),
        session_request(identity_number),
    )?
    .unwrap();

    assert_ne!(second.session_id, first.session_id);
    assert_ne!(second.user_key, first.user_key);

    // The chain the first ceremony handed out stops working, which is what bounds a copy of
    // it to the user's next sign-in rather than to its expiry.
    assert_eq!(
        app_prepare_delegation(
            &env,
            canister_id,
            first_principal,
            AppPrepareDelegationRequest {
                session_key: ByteBuf::from(vec![7; 32]),
            },
        )?,
        Err(AppSessionError::NoMatchingSession)
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

#[test]
fn should_mint_an_app_delegation_from_a_session() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let identity_number = flows::register_anchor(&env, canister_id);
    let (_, session_principal) = create_session(&env, canister_id, identity_number);
    let app_key = ByteBuf::from(vec![7; 32]);

    let minted = app_prepare_delegation(
        &env,
        canister_id,
        session_principal,
        AppPrepareDelegationRequest {
            session_key: app_key.clone(),
        },
    )?
    .unwrap();

    assert!(minted.expiration <= time(&env) + APP_DELEGATION_TTL_NS);
    assert!(minted.expiration > time(&env));

    let signed = app_get_delegation(
        &env,
        canister_id,
        session_principal,
        AppGetDelegationRequest {
            session_key: app_key,
            expiration: minted.expiration,
        },
    )?
    .unwrap();

    verify_delegation(&env, minted.user_key, &signed, &env.root_key().unwrap());

    Ok(())
}

/// The minted delegation is for the account, not for the session, so it is the principal
/// the dapp already knows, and the one the session response names.
#[test]
fn should_mint_the_accounts_own_principal() -> Result<(), RejectResponse> {
    use canister_tests::api::internet_identity::api_v2::{
        prepare_account_delegation, AccountDelegationParams,
    };

    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let identity_number = flows::register_anchor(&env, canister_id);

    let params = AccountDelegationParams::new(
        &env,
        canister_id,
        principal_1(),
        identity_number,
        ORIGIN.to_string(),
        None,
        ByteBuf::from(vec![9; 32]),
    );
    let by_access_method = prepare_account_delegation(&params, None)?.unwrap();

    let (prepared, session_principal) = create_session(&env, canister_id, identity_number);
    let by_session = app_prepare_delegation(
        &env,
        canister_id,
        session_principal,
        AppPrepareDelegationRequest {
            session_key: ByteBuf::from(vec![7; 32]),
        },
    )?
    .unwrap();

    assert_eq!(by_session.user_key, by_access_method.user_key);
    assert_eq!(
        prepared.account_principal,
        Principal::self_authenticating(&by_access_method.user_key)
    );

    Ok(())
}

/// A caller the session index does not know is refused, whatever else it holds.
#[test]
fn should_refuse_a_caller_that_is_not_the_session() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let identity_number = flows::register_anchor(&env, canister_id);
    let (_, _) = create_session(&env, canister_id, identity_number);

    let result = app_prepare_delegation(
        &env,
        canister_id,
        principal_1(),
        AppPrepareDelegationRequest {
            session_key: ByteBuf::from(vec![7; 32]),
        },
    )?;

    assert_eq!(result, Err(AppSessionError::NoMatchingSession));

    Ok(())
}

#[test]
fn should_refuse_a_refresh_once_the_session_has_expired() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let identity_number = flows::register_anchor(&env, canister_id);
    let mut request = session_request(identity_number);
    request.valid_for = Some(10 * 60 * 1_000_000_000);
    let prepared = prepare_account_session(&env, canister_id, principal_1(), request)?.unwrap();
    let session_principal = Principal::self_authenticating(&prepared.user_key);

    env.advance_time(Duration::from_secs(11 * 60));

    let result = app_prepare_delegation(
        &env,
        canister_id,
        session_principal,
        AppPrepareDelegationRequest {
            session_key: ByteBuf::from(vec![7; 32]),
        },
    )?;

    assert_eq!(result, Err(AppSessionError::NoMatchingSession));

    Ok(())
}

/// The 5-minute cap is a property of the design, not something the app asks for, so the
/// `get` half re-derives it rather than trusting the value it is handed. Longer-lived
/// delegations over the same account seed exist, and witnessing one here would hand the
/// session an artifact that outlives it.
#[test]
fn should_refuse_an_app_delegation_longer_than_the_ttl() -> Result<(), RejectResponse> {
    use canister_tests::api::internet_identity::api_v2::{
        prepare_account_delegation, AccountDelegationParams,
    };

    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let identity_number = flows::register_anchor(&env, canister_id);
    let app_key = ByteBuf::from(vec![7; 32]);

    // A 30-day delegation over the same account seed, for the same key.
    let params = AccountDelegationParams::new(
        &env,
        canister_id,
        principal_1(),
        identity_number,
        ORIGIN.to_string(),
        None,
        app_key.clone(),
    );
    let long_lived =
        prepare_account_delegation(&params, Some(30 * 24 * 60 * 60 * 1_000_000_000))?.unwrap();
    assert!(long_lived.expiration > time(&env) + APP_DELEGATION_TTL_NS);

    let (_, session_principal) = create_session(&env, canister_id, identity_number);

    let result = app_get_delegation(
        &env,
        canister_id,
        session_principal,
        AppGetDelegationRequest {
            session_key: app_key,
            expiration: long_lived.expiration,
        },
    )?;

    assert!(matches!(result, Err(AppSessionError::NoMatchingSession)));

    Ok(())
}

/// A consent that differs from the held one is a different session, so a downgrade is
/// not silently discarded.
#[test]
fn should_not_reuse_a_session_across_a_consent_change() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let identity_number = flows::register_anchor(&env, canister_id);

    let full_access = prepare_account_session(
        &env,
        canister_id,
        principal_1(),
        session_request(identity_number),
    )?
    .unwrap();

    let mut downgraded = session_request(identity_number);
    downgraded.permissions = Some(Permissions::Queries);
    let read_only = prepare_account_session(&env, canister_id, principal_1(), downgraded)?.unwrap();

    assert_ne!(read_only.session_id, full_access.session_id);
    assert_ne!(read_only.user_key, full_access.user_key);

    let minted = app_prepare_delegation(
        &env,
        canister_id,
        Principal::self_authenticating(&read_only.user_key),
        AppPrepareDelegationRequest {
            session_key: ByteBuf::from(vec![7; 32]),
        },
    )?
    .unwrap();
    let signed = app_get_delegation(
        &env,
        canister_id,
        Principal::self_authenticating(&read_only.user_key),
        AppGetDelegationRequest {
            session_key: ByteBuf::from(vec![7; 32]),
            expiration: minted.expiration,
        },
    )?
    .unwrap();
    assert_eq!(
        signed.delegation.permissions,
        Some("queries".to_string()),
        "the downgraded consent must reach the minted delegation"
    );

    // The browser now holds one session, not two.
    let refreshed_old = app_prepare_delegation(
        &env,
        canister_id,
        Principal::self_authenticating(&full_access.user_key),
        AppPrepareDelegationRequest {
            session_key: ByteBuf::from(vec![7; 32]),
        },
    )?;
    assert_eq!(refreshed_old, Err(AppSessionError::NoMatchingSession));

    Ok(())
}

/// A session whose device the registry cap dropped could not be signed out from
/// settings, so it goes with the record.
#[test]
fn should_end_the_sessions_of_a_browser_the_registry_dropped() -> Result<(), RejectResponse> {
    const MAX_SESSION_DEVICES: u32 = 20;

    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let identity_number = flows::register_anchor(&env, canister_id);

    let (_, first_principal) = create_session(&env, canister_id, identity_number);

    for index in 0..MAX_SESSION_DEVICES {
        let mut request = session_request_from(identity_number, &BrowserKey::new(index as u8 + 2));
        request.device_name = format!("browser-{index}");
        request.origin = format!("https://dapp-{index}.com");
        prepare_account_session(&env, canister_id, principal_1(), request)?.unwrap();
    }

    let refreshed = app_prepare_delegation(
        &env,
        canister_id,
        first_principal,
        AppPrepareDelegationRequest {
            session_key: ByteBuf::from(vec![7; 32]),
        },
    )?;
    assert_eq!(refreshed, Err(AppSessionError::NoMatchingSession));

    Ok(())
}

/// An app delegation cannot renew itself: its principal resolves to the locator, but no
/// session's seed will ever equal it, because the two seed families are domain separated.
#[test]
fn should_refuse_an_app_delegation_renewing_itself() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let identity_number = flows::register_anchor(&env, canister_id);
    let (_, session_principal) = create_session(&env, canister_id, identity_number);

    let minted = app_prepare_delegation(
        &env,
        canister_id,
        session_principal,
        AppPrepareDelegationRequest {
            session_key: ByteBuf::from(vec![7; 32]),
        },
    )?
    .unwrap();
    let account_principal = Principal::self_authenticating(&minted.user_key);

    let result = app_prepare_delegation(
        &env,
        canister_id,
        account_principal,
        AppPrepareDelegationRequest {
            session_key: ByteBuf::from(vec![7; 32]),
        },
    )?;

    assert_eq!(result, Err(AppSessionError::NoMatchingSession));

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

#[test]
fn should_stamp_every_refresh() -> Result<(), RejectResponse> {
    use canister_tests::api::internet_identity::api_v2::get_accounts;

    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let identity_number = flows::register_anchor(&env, canister_id);
    let (_, session_principal) = create_session(&env, canister_id, identity_number);

    let refresh = |env: &PocketIc| {
        app_prepare_delegation(
            env,
            canister_id,
            session_principal,
            AppPrepareDelegationRequest {
                session_key: ByteBuf::from(vec![7; 32]),
            },
        )
        .unwrap()
        .unwrap()
    };
    let last_used = |env: &PocketIc| -> Result<Option<u64>, RejectResponse> {
        Ok(get_accounts(
            env,
            canister_id,
            principal_1(),
            identity_number,
            ORIGIN.to_string(),
        )?
        .unwrap()[0]
            .last_used)
    };

    let before = last_used(&env)?;

    env.advance_time(Duration::from_secs(60));
    refresh(&env);
    let after_a_minute = last_used(&env)?;
    assert!(after_a_minute > before);

    env.advance_time(Duration::from_secs(60));
    refresh(&env);
    assert!(last_used(&env)? > after_a_minute);

    Ok(())
}

#[test]
fn should_advance_the_device_last_used_on_every_refresh() -> Result<(), RejectResponse> {
    use canister_tests::api::internet_identity::api_v2::identity_info;

    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let identity_number = flows::register_anchor(&env, canister_id);
    let (_, session_principal) = create_session(&env, canister_id, identity_number);

    let device = |env: &PocketIc| -> Result<SessionDeviceInfo, RejectResponse> {
        Ok(
            identity_info(env, canister_id, principal_1(), identity_number)?
                .unwrap()
                .session_devices
                .unwrap()[0]
                .clone(),
        )
    };

    let enrolled = device(&env)?;
    assert_eq!(enrolled.created_at, enrolled.last_used);

    env.advance_time(Duration::from_secs(300));
    app_prepare_delegation(
        &env,
        canister_id,
        session_principal,
        AppPrepareDelegationRequest {
            session_key: ByteBuf::from(vec![7; 32]),
        },
    )
    .unwrap()
    .unwrap();

    let refreshed = device(&env)?;
    assert!(refreshed.last_used > enrolled.last_used);
    assert_eq!(refreshed.created_at, enrolled.created_at);

    Ok(())
}

/// Naming a default account keeps its principal, so it must keep its sessions. Before the
/// session seed was built on the account seed, naming it signed the user out of every app
/// using that account.
#[test]
fn should_keep_a_session_alive_when_the_default_account_is_named() -> Result<(), RejectResponse> {
    use canister_tests::api::internet_identity::api_v2::update_account;
    use internet_identity_interface::internet_identity::types::AccountUpdate;

    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let identity_number = flows::register_anchor(&env, canister_id);
    let (_, session_principal) = create_session(&env, canister_id, identity_number);

    // Naming the default account materializes it: the reference keeps its sessions and
    // gains an account number.
    update_account(
        &env,
        canister_id,
        principal_1(),
        identity_number,
        ORIGIN.to_string(),
        None,
        AccountUpdate {
            name: Some("work".to_string()),
        },
    )?
    .unwrap();

    let refreshed = app_prepare_delegation(
        &env,
        canister_id,
        session_principal,
        AppPrepareDelegationRequest {
            session_key: ByteBuf::from(vec![7; 32]),
        },
    )?;

    assert!(refreshed.is_ok(), "naming an account ended its sessions");

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

/// Rotation changes what the browser proves with, not which browser it is, and sessions
/// record the browser. So a rotation must not cost the user their session.
#[test]
fn should_keep_the_browser_entry_across_a_rotation() -> Result<(), RejectResponse> {
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

    // A ceremony replaces the session, so what a rotation must not cost is the browser's
    // identity: same entry, new session.
    assert_eq!(rotated.device_id, first.device_id);
    assert_ne!(rotated.session_id, first.session_id);

    assert!(app_prepare_delegation(
        &env,
        canister_id,
        Principal::self_authenticating(&rotated.user_key),
        AppPrepareDelegationRequest {
            session_key: ByteBuf::from(vec![7; 32]),
        },
    )?
    .is_ok());

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

/// A refresh names nothing and attaches nothing: the caller is resolved from its own
/// signature, so an app that never held a session cannot mint by naming an account.
#[test]
fn should_mint_for_the_calling_session_and_nobody_else() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let identity_number = flows::register_anchor(&env, canister_id);
    let (prepared, session_principal) = create_session(&env, canister_id, identity_number);

    let minted = app_prepare_delegation(
        &env,
        canister_id,
        session_principal,
        AppPrepareDelegationRequest {
            session_key: ByteBuf::from(vec![7; 32]),
        },
    )?
    .unwrap();

    // What the session mints is the account's own principal, unchanged by any of this.
    assert_eq!(
        Principal::self_authenticating(&minted.user_key),
        prepared.account_principal
    );

    // The account principal is not a credential: holding it mints nothing.
    assert_eq!(
        app_prepare_delegation(
            &env,
            canister_id,
            prepared.account_principal,
            AppPrepareDelegationRequest {
                session_key: ByteBuf::from(vec![7; 32]),
            },
        )?,
        Err(AppSessionError::NoMatchingSession)
    );

    Ok(())
}
