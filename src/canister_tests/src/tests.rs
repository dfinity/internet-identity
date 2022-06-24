use crate::framework::{device_data_1, expect_user_error_with_message, principal_2};
use crate::{api, flows, framework};
use ic_error_types::ErrorCode;
use ic_state_machine_tests::StateMachine;
use internet_identity_interface as types;
use regex::Regex;

#[test]
fn ii_canister_can_be_installed() {
    let env = StateMachine::new();
    let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());

    api::health_check(&env, canister_id);
}

#[test]
fn ii_upgrade_works() {
    let env = StateMachine::new();
    let canister_id = framework::install_ii_canister(&env, framework::II_WASM_PREVIOUS.clone());
    framework::upgrade_ii_canister(&env, canister_id, framework::II_WASM.clone());
    api::health_check(&env, canister_id);
}

#[test]
fn ii_upgrade_retains_anchors() {
    let env = StateMachine::new();
    let canister_id = framework::install_ii_canister(&env, framework::II_WASM_PREVIOUS.clone());
    let user_number = flows::register_anchor(&env, canister_id);
    framework::upgrade_ii_canister(&env, canister_id, framework::II_WASM.clone());

    let retrieved_device_data = api::lookup(&env, canister_id, user_number);

    assert_eq!(retrieved_device_data, vec![device_data_1()]);
}

#[test]
fn ii_canister_can_be_upgraded_and_rolled_back() {
    let env = StateMachine::new();
    let canister_id = framework::install_ii_canister(&env, framework::II_WASM_PREVIOUS.clone());
    framework::upgrade_ii_canister(&env, canister_id, framework::II_WASM.clone());
    api::health_check(&env, canister_id);
    framework::upgrade_ii_canister(&env, canister_id, framework::II_WASM_PREVIOUS.clone());
    api::health_check(&env, canister_id);
}

#[test]
fn registration_with_mismatched_sender_fails() {
    let env = StateMachine::new();
    let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
    let challenge = api::create_challenge(&env, canister_id);
    let result = api::register(
        &env,
        canister_id,
        principal_2(),
        device_data_1(),
        types::ChallengeAttempt {
            chars: "a".to_string(),
            key: challenge.challenge_key,
        },
    );

    expect_user_error_with_message(
        result,
        ErrorCode::CanisterCalledTrap,
        Regex::new("[a-z0-9-]+ could not be authenticated against").unwrap(),
    );
}

/// Tests related to prepare_delegation and get_delegation II canister calls.
#[cfg(test)]
mod delegation_tests {
    use crate::framework::{expect_user_error_with_message, principal_1, principal_2, CallError};
    use crate::{api, flows, framework};
    use ic_error_types::ErrorCode::CanisterCalledTrap;
    use ic_state_machine_tests::StateMachine;
    use internet_identity_interface::GetDelegationResponse;
    use regex::Regex;
    use serde_bytes::ByteBuf;
    use std::ops::Add;
    use std::time::{Duration, SystemTime, UNIX_EPOCH};

    /// Verifies that valid delegations are issued.
    #[test]
    fn should_get_valid_delegation() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);
        let frontend_hostname = "https://some-dapp.com";
        let pub_session_key = ByteBuf::from("session public key");

        let (canister_sig_key, expiration) = api::prepare_delegation(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname.to_string(),
            pub_session_key.clone(),
            None,
        )?;
        assert_eq!(
            expiration,
            env.time()
                .add(Duration::from_secs(30 * 60)) // default expiration: 30 minutes
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_nanos() as u64
        );

        let signed_delegation = match api::get_delegation(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname.to_string(),
            pub_session_key.clone(),
            expiration,
        )? {
            GetDelegationResponse::SignedDelegation(delegation) => delegation,
            GetDelegationResponse::NoSuchDelegation => panic!("failed to get delegation"),
        };

        framework::verify_delegation(&env, canister_sig_key, &signed_delegation);
        assert_eq!(signed_delegation.delegation.pubkey, pub_session_key);
        assert_eq!(signed_delegation.delegation.expiration, expiration);
        Ok(())
    }

    /// Verifies that non-default expirations are respected.
    #[test]
    fn should_get_valid_delegation_with_custom_expiration() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);
        let frontend_hostname = "https://some-dapp.com";
        let pub_session_key = ByteBuf::from("session public key");

        let (canister_sig_key, expiration) = api::prepare_delegation(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname.to_string(),
            pub_session_key.clone(),
            Some(3_600_000_000_000), // 1 hour
        )?;
        assert_eq!(
            expiration,
            env.time()
                .add(Duration::from_secs(60 * 60)) // 1 hour
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_nanos() as u64
        );

        let signed_delegation = match api::get_delegation(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname.to_string(),
            pub_session_key.clone(),
            expiration,
        )? {
            GetDelegationResponse::SignedDelegation(delegation) => delegation,
            GetDelegationResponse::NoSuchDelegation => panic!("failed to get delegation"),
        };

        framework::verify_delegation(&env, canister_sig_key, &signed_delegation);
        assert_eq!(signed_delegation.delegation.pubkey, pub_session_key);
        assert_eq!(signed_delegation.delegation.expiration, expiration);
        Ok(())
    }

    /// Verifies that the delegations are valid at most for 30 days.
    #[test]
    fn should_shorten_expiration_greater_max_ttl() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);
        let frontend_hostname = "https://some-dapp.com";
        let pub_session_key = ByteBuf::from("session public key");

        let (canister_sig_key, expiration) = api::prepare_delegation(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname.to_string(),
            pub_session_key.clone(),
            Some(Duration::from_secs(31 * 24 * 60 * 60).as_nanos() as u64), // 31 days
        )?;
        assert_eq!(
            expiration,
            env.time()
                .add(Duration::from_secs(30 * 24 * 60 * 60)) // 30 days
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_nanos() as u64
        );

        let signed_delegation = match api::get_delegation(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname.to_string(),
            pub_session_key.clone(),
            expiration,
        )? {
            GetDelegationResponse::SignedDelegation(delegation) => delegation,
            GetDelegationResponse::NoSuchDelegation => panic!("failed to get delegation"),
        };

        framework::verify_delegation(&env, canister_sig_key, &signed_delegation);
        assert_eq!(signed_delegation.delegation.pubkey, pub_session_key);
        assert_eq!(signed_delegation.delegation.expiration, expiration);
        Ok(())
    }

    /// Verifies that delegations can be requested in parallel.
    #[test]
    fn should_get_multiple_valid_delegations() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);
        let frontend_hostname_1 = "https://dapp1.com";
        let frontend_hostname_2 = "https://dapp2.com";
        let pub_session_key_1 = ByteBuf::from("session public key 1");
        let pub_session_key_2 = ByteBuf::from("session public key 2");
        let delegation_params = vec![
            (
                &pub_session_key_1,
                frontend_hostname_1,
                SystemTime::UNIX_EPOCH,
            ),
            (
                &pub_session_key_1,
                frontend_hostname_2,
                SystemTime::UNIX_EPOCH,
            ),
            (
                &pub_session_key_2,
                frontend_hostname_1,
                SystemTime::UNIX_EPOCH,
            ),
            (
                &pub_session_key_1,
                frontend_hostname_1,
                SystemTime::UNIX_EPOCH.add(Duration::from_secs(30)),
            ),
        ];

        // prepare multiple delegations in parallel before calling get_delegation
        let prepare_delegation_results =
            delegation_params
                .into_iter()
                .map(|(session_key, frontend_hostname, time)| {
                    env.set_time(time);
                    let (canister_sig_key, expiration) = api::prepare_delegation(
                        &env,
                        canister_id,
                        principal_1(),
                        user_number,
                        frontend_hostname.to_string(),
                        session_key.clone(),
                        None,
                    )
                    .expect("prepare_delegation failed");

                    assert_eq!(
                        expiration,
                        env.time()
                            .add(Duration::from_secs(30 * 60)) // default expiration: 30 minutes
                            .duration_since(UNIX_EPOCH)
                            .unwrap()
                            .as_nanos() as u64
                    );
                    (session_key, frontend_hostname, canister_sig_key, expiration)
                });

        for (session_key, frontend_hostname, canister_sig_key, expiration) in
            prepare_delegation_results
        {
            let signed_delegation = match api::get_delegation(
                &env,
                canister_id,
                principal_1(),
                user_number,
                frontend_hostname.to_string(),
                session_key.clone(),
                expiration,
            )? {
                GetDelegationResponse::SignedDelegation(delegation) => delegation,
                GetDelegationResponse::NoSuchDelegation => panic!("failed to get delegation"),
            };

            framework::verify_delegation(&env, canister_sig_key, &signed_delegation);
            assert_eq!(signed_delegation.delegation.pubkey, session_key.clone());
            assert_eq!(signed_delegation.delegation.expiration, expiration);
        }
        Ok(())
    }

    /// Verifies that an anchor that was registered using II_WASM_PREVIOUS gets valid delegations after upgrading to the current version.
    #[test]
    fn should_get_valid_delegation_for_old_anchor_after_ii_upgrade() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM_PREVIOUS.clone());
        let user_number = flows::register_anchor(&env, canister_id);
        let frontend_hostname = "https://some-dapp.com";
        let pub_session_key = ByteBuf::from("session public key");

        framework::upgrade_ii_canister(&env, canister_id, framework::II_WASM.clone());

        let (canister_sig_key, expiration) = api::prepare_delegation(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname.to_string(),
            pub_session_key.clone(),
            None,
        )?;
        assert_eq!(
            expiration,
            env.time()
                .add(Duration::from_secs(30 * 60)) // default expiration: 30 minutes
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_nanos() as u64
        );

        let signed_delegation = match api::get_delegation(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname.to_string(),
            pub_session_key.clone(),
            expiration,
        )? {
            GetDelegationResponse::SignedDelegation(delegation) => delegation,
            GetDelegationResponse::NoSuchDelegation => panic!("failed to get delegation"),
        };

        framework::verify_delegation(&env, canister_sig_key, &signed_delegation);
        assert_eq!(signed_delegation.delegation.pubkey, pub_session_key);
        assert_eq!(signed_delegation.delegation.expiration, expiration);
        Ok(())
    }

    /// Verifies that different front-ends yield different principals.
    #[test]
    fn should_issue_different_principals() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);
        let frontend_hostname_1 = "https://dapp1.com";
        let frontend_hostname_2 = "https://dapp2.com";
        let pub_session_key = ByteBuf::from("session public key");

        let (canister_sig_key_1, _) = api::prepare_delegation(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname_1.to_string(),
            pub_session_key.clone(),
            None,
        )?;
        let (canister_sig_key_2, _) = api::prepare_delegation(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname_2.to_string(),
            pub_session_key.clone(),
            None,
        )?;

        assert_ne!(canister_sig_key_1, canister_sig_key_2);
        Ok(())
    }

    /// Verifies that there is a graceful failure if II gets upgraded between prepare_delegation and get_delegation.
    #[test]
    fn should_not_get_prepared_delegation_after_ii_upgrade() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);
        let frontend_hostname = "https://some-dapp.com";
        let pub_session_key = ByteBuf::from("session public key");

        let (_, expiration) = api::prepare_delegation(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname.to_string(),
            pub_session_key.clone(),
            None,
        )?;

        // upgrade, even with the same WASM clears non-stable memory
        framework::upgrade_ii_canister(&env, canister_id, framework::II_WASM.clone());

        match api::get_delegation(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname.to_string(),
            pub_session_key.clone(),
            expiration,
        )? {
            GetDelegationResponse::SignedDelegation(_) => panic!("unexpected delegation"),
            GetDelegationResponse::NoSuchDelegation => {}
        };
        Ok(())
    }

    /// Verifies that there is a graceful failure if get_delegation is called after the expiration of the delegation.
    #[test]
    fn should_not_get_delegation_after_expiration() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);
        let frontend_hostname = "https://some-dapp.com";
        let pub_session_key = ByteBuf::from("session public key");

        let (_, expiration) = api::prepare_delegation(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname.to_string(),
            pub_session_key.clone(),
            None,
        )?;

        env.advance_time(Duration::from_secs(30 * 60 + 1)); // one second more than delegation validity of 30 min

        // we have to call prepare again, because expired signatures can only be pruned in update calls
        api::prepare_delegation(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname.to_string(),
            pub_session_key.clone(),
            None,
        )?;

        match api::get_delegation(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname.to_string(),
            pub_session_key.clone(),
            expiration,
        )? {
            GetDelegationResponse::SignedDelegation(_) => panic!("unexpected delegation"),
            GetDelegationResponse::NoSuchDelegation => {}
        };
        Ok(())
    }

    /// Verifies that delegations can only be prepared by the matching user.
    #[test]
    fn can_not_prepare_delegation_for_different_user() {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        let result = api::prepare_delegation(
            &env,
            canister_id,
            principal_2(),
            user_number, // belongs to principal_1
            "https://some-dapp.com".to_string(),
            ByteBuf::from("session key"),
            None,
        );

        expect_user_error_with_message(
            result,
            CanisterCalledTrap,
            Regex::new("[a-z\\d-]+ could not be authenticated.").unwrap(),
        );
    }

    /// Verifies that get_delegation can only be called by the matching user.
    #[test]
    fn can_not_get_delegation_for_different_user() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);
        let frontend_hostname = "https://some-dapp.com";
        let pub_session_key = ByteBuf::from("session public key");

        let (_, expiration) = api::prepare_delegation(
            &env,
            canister_id,
            principal_1(),
            user_number,
            frontend_hostname.to_string(),
            pub_session_key.clone(),
            None,
        )?;
        let result = api::get_delegation(
            &env,
            canister_id,
            principal_2(),
            user_number,
            frontend_hostname.to_string(),
            pub_session_key.clone(),
            expiration,
        );

        expect_user_error_with_message(
            result,
            CanisterCalledTrap,
            Regex::new("[a-z\\d-]+ could not be authenticated.").unwrap(),
        );
        Ok(())
    }
}

/// Tests for the HTTP interactions according to the HTTP gateway spec: https://internetcomputer.org/docs/current/references/ic-interface-spec/#http-gateway
#[cfg(test)]
mod http_tests {
    use crate::certificate_validation::validate_certification;
    use crate::framework::CallError;
    use crate::{api, framework};
    use ic_state_machine_tests::StateMachine;
    use internet_identity_interface::HttpRequest;
    use serde_bytes::ByteBuf;

    /// Verifies that expected assets are delivered, certified and have security headers.
    #[test]
    fn ii_canister_serves_http_assets() -> Result<(), CallError> {
        let assets: Vec<(&str, Option<&str>)> = vec![
            ("/", None),
            ("/index.html", None),
            ("/index.js", Some("gzip")),
            ("/loader.webp", None),
            ("/favicon.ico", None),
            ("/ic-badge.svg", None),
        ];
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());

        // for each asset, fetch the asset, check the HTTP status code, headers and certificate.
        for (asset, encoding) in assets {
            let http_response = api::http_request(
                &env,
                canister_id,
                HttpRequest {
                    method: "GET".to_string(),
                    url: asset.to_string(),
                    headers: vec![],
                    body: ByteBuf::new(),
                },
            )?;

            assert_eq!(http_response.status_code, 200);

            // check the appropriate Content-Encoding header is set
            if let Some(enc) = encoding {
                let (_, content_encoding) = http_response
                    .headers
                    .iter()
                    .find(|(name, _)| name.to_lowercase() == "content-encoding")
                    .expect("Content-Encoding header not found");
                assert_eq!(
                    content_encoding, enc,
                    "unexpected Content-Encoding header value"
                );
            }

            // 1. It searches for a response header called Ic-Certificate (case-insensitive).
            let (_, ic_certificate) = http_response
                .headers
                .iter()
                .find(|(name, _)| name.to_lowercase() == "ic-certificate")
                .expect("IC-Certificate header not found");

            validate_certification(
                ic_certificate,
                canister_id,
                asset,
                &http_response.body,
                None, // should really be `encoding`, but cannot use it because II certifies encoded response bodies, see L2-722 for details
                env.root_key(),
                env.time(),
            )
            .expect(&format!("validation for asset \"{}\" failed", asset));
            framework::verify_security_headers(&http_response.headers);
        }
        Ok(())
    }
}

/// Tests concerning the device registration flow for remote devices (i.e. authenticators on another computer).
/// The flow has the following steps:
/// 1. on device 1: enter registration mode
/// 2. on device 2: register the new device tentatively -> this returns a verification code
/// 3. on device 1: enter the verification code
///
/// Additionally, there are the following bounds on the registration flow:
/// 1. registration mode expires after 15 minutes
/// 2. there is a limit of 3 attempts for step 3 in the above process
#[cfg(test)]
mod remote_device_registration_tests {
    use crate::framework::{
        device_data_2, expect_user_error_with_message, principal_1, principal_2, CallError,
    };
    use crate::{api, flows, framework};
    use ic_error_types::ErrorCode;
    use ic_state_machine_tests::StateMachine;
    use internet_identity_interface as types;
    use regex::Regex;
    use std::time::{Duration, SystemTime};

    /// Test entering registration mode including returned expiration time.
    #[test]
    fn can_enter_device_registration_mode() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        let result =
            api::enter_device_registration_mode(&env, canister_id, principal_1(), user_number)?;

        assert!(matches!(result, 900_000_000_000)); // 900 seconds -> 15 min
        Ok(())
    }

    /// Tests that only an authenticated user can enter device registration mode for themselves.
    #[test]
    fn can_not_enter_device_registration_mode_for_other_user() {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        let result =
            api::enter_device_registration_mode(&env, canister_id, principal_2(), user_number);

        expect_user_error_with_message(
            result,
            ErrorCode::CanisterCalledTrap,
            Regex::new("[a-z0-9-]+ could not be authenticated.").unwrap(),
        );
    }

    /// Tests that the device registration flow can be completed successfully.
    #[test]
    fn can_register_remote_device() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        api::enter_device_registration_mode(&env, canister_id, principal_1(), user_number)?;
        let add_response = api::add_tentative_device(
            &env,
            canister_id,
            principal_2(),
            user_number,
            device_data_2(),
        )?;
        let verification_code = match add_response {
            types::AddTentativeDeviceResponse::AddedTentatively {
                verification_code, ..
            } => verification_code,
            err => panic!("failed to add tentative device: {:?}", err),
        };
        let verification_response = api::verify_tentative_device(
            &env,
            canister_id,
            principal_1(),
            user_number,
            verification_code,
        )?;

        assert!(matches!(
            verification_response,
            types::VerifyTentativeDeviceResponse::Verified
        ));
        Ok(())
    }

    /// Tests that the device registration flow can be completed successfully after submitting an invalid code.
    #[test]
    fn can_verify_remote_device_after_failed_attempt() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        api::enter_device_registration_mode(&env, canister_id, principal_1(), user_number)?;
        let add_response = api::add_tentative_device(
            &env,
            canister_id,
            principal_2(),
            user_number,
            device_data_2(),
        )?;
        let verification_code = match add_response {
            types::AddTentativeDeviceResponse::AddedTentatively {
                verification_code, ..
            } => verification_code,
            err => panic!("failed to add tentative device: {:?}", err),
        };

        assert!(matches!(
            api::verify_tentative_device(
                &env,
                canister_id,
                principal_1(),
                user_number,
                "invalid code".to_string()
            )?,
            types::VerifyTentativeDeviceResponse::WrongCode { retries_left: 2 }
        ));

        let verification_response = api::verify_tentative_device(
            &env,
            canister_id,
            principal_1(),
            user_number,
            verification_code,
        )?;

        assert!(matches!(
            verification_response,
            types::VerifyTentativeDeviceResponse::Verified
        ));
        Ok(())
    }

    /// Tests that the anchor info call returns information about tentative devices.
    /// This enables the front-end to continue an in progress flow (e.g. after a refresh of the page).
    #[test]
    fn anchor_info_should_return_tentative_device() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        api::enter_device_registration_mode(&env, canister_id, principal_1(), user_number)?;
        let new_device = device_data_2();
        api::add_tentative_device(
            &env,
            canister_id,
            principal_2(),
            user_number,
            new_device.clone(),
        )?;
        let anchor_info = api::get_anchor_info(&env, canister_id, principal_1(), user_number)?;

        assert!(matches!(
            anchor_info.device_registration,
            Some(types::DeviceRegistrationInfo {
                tentative_device: Some(tenative_device),
                ..
            }) if tenative_device == new_device
        ));
        Ok(())
    }

    /// Tests that devices cannot be registered tentatively if the registration mode is not enabled.
    #[test]
    fn reject_tentative_device_if_not_in_registration_mode() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        api::enter_device_registration_mode(&env, canister_id, principal_1(), user_number)?;
        api::exit_device_registration_mode(&env, canister_id, principal_1(), user_number)?;
        let result = api::add_tentative_device(
            &env,
            canister_id,
            principal_2(),
            user_number,
            device_data_2(),
        )?;

        assert!(matches!(
            result,
            types::AddTentativeDeviceResponse::DeviceRegistrationModeOff
        ));
        Ok(())
    }

    /// Tests device registration mode expiration.
    #[test]
    fn reject_tentative_device_if_registration_mode_is_expired() -> Result<(), CallError> {
        const REGISTRATION_MODE_EXPIRATION: Duration = Duration::from_secs(900);
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        api::enter_device_registration_mode(&env, canister_id, principal_1(), user_number)?;
        env.set_time(
            SystemTime::UNIX_EPOCH + REGISTRATION_MODE_EXPIRATION + Duration::from_secs(1),
        );
        let result = api::add_tentative_device(
            &env,
            canister_id,
            principal_2(),
            user_number,
            device_data_2(),
        )?;

        assert!(matches!(
            result,
            types::AddTentativeDeviceResponse::DeviceRegistrationModeOff
        ));
        Ok(())
    }

    /// Tests that an appropriate result is returned when a verification code is submitted without a
    /// corresponding tentative device.
    #[test]
    fn reject_verification_without_tentative_device() -> Result<(), CallError> {
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        api::enter_device_registration_mode(&env, canister_id, principal_1(), user_number)?;
        let verification_response = api::verify_tentative_device(
            &env,
            canister_id,
            principal_1(),
            user_number,
            "some code".to_string(),
        )?;

        assert!(matches!(
            verification_response,
            types::VerifyTentativeDeviceResponse::NoDeviceToVerify
        ));
        Ok(())
    }

    /// Tests that the flow is aborted after the expected number of failed verification attempts.
    #[test]
    fn reject_verification_with_wrong_code() -> Result<(), CallError> {
        const MAX_RETRIES: u8 = 3;
        let env = StateMachine::new();
        let canister_id = framework::install_ii_canister(&env, framework::II_WASM.clone());
        let user_number = flows::register_anchor(&env, canister_id);

        api::enter_device_registration_mode(&env, canister_id, principal_1(), user_number)?;
        api::add_tentative_device(
            &env,
            canister_id,
            principal_2(),
            user_number,
            device_data_2(),
        )?;
        for expected_retries in (0..MAX_RETRIES).rev() {
            assert!(matches!(
                api::verify_tentative_device(
                    &env,
                    canister_id,
                    principal_1(),
                    user_number,
                    "invalid code".to_string()
                )?,
                types::VerifyTentativeDeviceResponse::WrongCode {
                    retries_left
                } if retries_left == expected_retries
            ));
        }

        assert!(matches!(
            api::verify_tentative_device(
                &env,
                canister_id,
                principal_1(),
                user_number,
                "invalid code".to_string()
            )?,
            types::VerifyTentativeDeviceResponse::DeviceRegistrationModeOff
        ));
        Ok(())
    }
}
