mod dynamic_captcha;

use crate::v2_api::authn_method_test_helpers::{
    create_identity_with_authn_method, create_identity_with_authn_method_and_name,
    test_authn_method,
};
use candid::Principal;
use canister_tests::api::internet_identity::{api_v2, get_anchor_info};
use canister_tests::framework::{
    arg_with_anchor_range, arg_with_rate_limit, assert_metric, env, get_metrics,
    install_ii_canister_with_arg, install_ii_with_archive, test_principal, time, II_WASM,
};
use internet_identity_interface::internet_identity::types::IdentityInfoError::Unauthorized;
use internet_identity_interface::internet_identity::types::{
    CaptchaConfig, CaptchaTrigger, CheckCaptchaError, IdRegFinishError, IdRegStartError,
    InternetIdentityInit, MetadataEntryV2, RateLimitConfig, RegistrationFlowNextStep,
    StaticCaptchaTrigger,
};
use pocket_ic::CallError;
use serde_bytes::ByteBuf;
use std::time::Duration;

#[test]
fn should_register_new_identity() {
    let env = env();
    let canister_id =
        install_ii_canister_with_arg(&env, II_WASM.clone(), arg_with_anchor_range((42, 44)));
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);

    assert_eq!(identity_number, 42);
}

#[test]
fn should_register_multiple_identities() {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_method = test_authn_method();
    let identity_number_1 = create_identity_with_authn_method(&env, canister_id, &authn_method);
    let identity_number_2 = create_identity_with_authn_method(&env, canister_id, &authn_method);

    assert_ne!(identity_number_1, identity_number_2);
}

#[test]
fn should_transition_flow_principal_to_temp_key() {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_method = test_authn_method();
    let flow_principal = test_principal(time(&env));

    api_v2::identity_registration_start(&env, canister_id, flow_principal)
        .expect("API call failed")
        .expect("registration start failed");

    api_v2::check_captcha(&env, canister_id, flow_principal, "a".to_string())
        .expect("API call failed")
        .expect("check_captcha failed");

    let identity_nr = api_v2::identity_registration_finish(
        &env,
        canister_id,
        flow_principal,
        &authn_method,
        None,
    )
    .expect("API call failed")
    .expect("registration finish failed")
    .identity_number;

    // authenticated call
    let result = api_v2::identity_info(&env, canister_id, flow_principal, identity_nr)
        .expect("API call failed");
    assert!(result.is_ok());

    env.advance_time(Duration::from_secs(601));

    // temp_key is expired now
    let result = api_v2::identity_info(&env, canister_id, flow_principal, identity_nr)
        .expect("API call failed");
    assert!(matches!(result, Err(Unauthorized(_))));
}

#[test]
fn should_not_exceed_configured_identity_range() {
    let env = env();
    let canister_id =
        install_ii_canister_with_arg(&env, II_WASM.clone(), arg_with_anchor_range((42, 44)));

    let authn_method = test_authn_method();
    create_identity_with_authn_method(&env, canister_id, &authn_method);
    create_identity_with_authn_method(&env, canister_id, &authn_method);

    let flow_principal = test_principal(0);
    api_v2::identity_registration_start(&env, canister_id, flow_principal)
        .expect("API call failed")
        .expect("registration start failed");

    api_v2::check_captcha(&env, canister_id, flow_principal, "a".to_string())
        .expect("API call failed")
        .expect("check_captcha failed");

    let result = api_v2::identity_registration_finish(
        &env,
        canister_id,
        flow_principal,
        &authn_method,
        None,
    )
    .expect("API call failed");
    assert!(matches!(
        result,
        Err(IdRegFinishError::IdentityLimitReached)
    ));
}

#[test]
fn should_not_allow_wrong_captcha() {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);

    let flow_principal = test_principal(0);
    api_v2::identity_registration_start(&env, canister_id, flow_principal)
        .expect("API call failed")
        .expect("registration start failed");

    let result = api_v2::check_captcha(
        &env,
        canister_id,
        flow_principal,
        "wrong solution".to_string(),
    )
    .expect("API call failed");

    assert!(matches!(
        result,
        Err(CheckCaptchaError::WrongSolution { .. })
    ));
}

#[test]
fn should_not_allow_anonymous_principal_to_start_registration() {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);

    let result = api_v2::identity_registration_start(&env, canister_id, Principal::anonymous())
        .expect("API call failed");

    assert!(matches!(result, Err(IdRegStartError::InvalidCaller)));
}

#[test]
fn should_not_allow_different_principal() {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);

    let flow_principal1 = test_principal(1);
    let flow_principal2 = test_principal(2);
    api_v2::identity_registration_start(&env, canister_id, flow_principal1)
        .expect("API call failed")
        .expect("registration start failed");

    let result = api_v2::check_captcha(&env, canister_id, flow_principal2, "a".to_string())
        .expect("API call failed");

    assert!(matches!(result, Err(CheckCaptchaError::NoRegistrationFlow)));
}

#[test]
fn should_allow_captcha_retries() {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);

    let flow_principal = test_principal(0);
    api_v2::identity_registration_start(&env, canister_id, flow_principal)
        .expect("API call failed")
        .expect("registration start failed");

    let result = api_v2::check_captcha(
        &env,
        canister_id,
        flow_principal,
        "wrong solution".to_string(),
    )
    .expect("API call failed");

    assert!(matches!(
        result,
        Err(CheckCaptchaError::WrongSolution { .. })
    ));

    let result = api_v2::check_captcha(&env, canister_id, flow_principal, "a".to_string())
        .expect("API call failed")
        .expect("check_captcha failed");
    assert!(matches!(result.next_step, RegistrationFlowNextStep::Finish))
}

#[test]
fn should_not_allow_to_continue_expired_flow() {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);

    let flow_principal = test_principal(0);
    api_v2::identity_registration_start(&env, canister_id, flow_principal)
        .expect("API call failed")
        .expect("registration start failed");

    env.advance_time(Duration::from_secs(301)); // one second longer than the flow validity

    let result = api_v2::check_captcha(&env, canister_id, flow_principal, "a".to_string())
        .expect("API call failed");

    // flow is not known anymore, as it has been pruned
    assert!(matches!(result, Err(CheckCaptchaError::NoRegistrationFlow)));
}

#[test]
fn should_fail_on_invalid_metadata() {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let mut authn_method = test_authn_method();
    authn_method.metadata.insert(
        "usage".to_string(),
        MetadataEntryV2::Bytes(ByteBuf::from("invalid")),
    );

    let flow_principal = test_principal(0);
    api_v2::identity_registration_start(&env, canister_id, flow_principal)
        .expect("API call failed")
        .expect("registration start failed");

    api_v2::check_captcha(&env, canister_id, flow_principal, "a".to_string())
        .expect("API call failed")
        .expect("check_captcha failed");

    let result = api_v2::identity_registration_finish(
        &env,
        canister_id,
        flow_principal,
        &authn_method,
        None,
    )
    .expect("API call failed");
    assert!(matches!(
        result,
        Err(IdRegFinishError::InvalidAuthnMethod(_))
    ));
}

#[test]
fn should_trigger_rate_limit_on_too_many_flows() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister_with_arg(
        &env,
        II_WASM.clone(),
        arg_with_rate_limit(RateLimitConfig {
            time_per_token_ns: Duration::from_secs(1).as_nanos() as u64,
            max_tokens: 3,
        }),
    );

    // the canister starts out with max_tokens, so use some to make the replenish actually do something
    for i in 0..3 {
        api_v2::identity_registration_start(&env, canister_id, test_principal(i))
            .expect("API call failed")
            .expect("registration start failed");
    }

    assert_metric(
        &get_metrics(&env, canister_id),
        "internet_identity_register_rate_limit_current_tokens",
        0f64,
    );

    let flow_principal = test_principal(3);
    let result = api_v2::identity_registration_start(&env, canister_id, flow_principal)
        .expect("API call failed");
    assert!(matches!(result, Err(IdRegStartError::RateLimitExceeded)));

    env.advance_time(Duration::from_secs(100));

    let result = api_v2::identity_registration_start(&env, canister_id, flow_principal)
        .expect("API call failed");
    assert!(result.is_ok());

    assert_metric(
        &get_metrics(&env, canister_id),
        "internet_identity_register_rate_limit_current_tokens",
        2f64,
    );
    Ok(())
}

#[test]
fn should_not_require_captcha_when_disabled() {
    let env = env();
    let canister_id = install_ii_canister_with_arg(
        &env,
        II_WASM.clone(),
        Some(InternetIdentityInit {
            captcha_config: Some(CaptchaConfig {
                max_unsolved_captchas: 50,
                captcha_trigger: CaptchaTrigger::Static(StaticCaptchaTrigger::CaptchaDisabled),
            }),
            ..InternetIdentityInit::default()
        }),
    );

    let result = api_v2::identity_registration_start(&env, canister_id, test_principal(0))
        .expect("API call failed")
        .expect("registration start failed");

    assert!(matches!(result.next_step, RegistrationFlowNextStep::Finish));
}

#[test]
fn should_register_new_identity_with_name() {
    let env = env();
    let canister_id =
        install_ii_canister_with_arg(&env, II_WASM.clone(), arg_with_anchor_range((42, 44)));
    let authn_method = test_authn_method();
    let name = Some("John Doe".to_string());
    let identity_number =
        create_identity_with_authn_method_and_name(&env, canister_id, &authn_method, name.clone());
    let anchor_info =
        get_anchor_info(&env, canister_id, authn_method.principal(), identity_number).unwrap();
    assert_eq!(anchor_info.name, name);
}
