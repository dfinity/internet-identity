use crate::v2_api::authn_method_test_helpers::{
    create_identity_with_authn_method, test_authn_method,
};
use canister_tests::api::internet_identity::api_v2;
use canister_tests::framework::{
    arg_with_dynamic_captcha, env, install_ii_canister_with_arg, test_principal, II_WASM,
};
use internet_identity_interface::internet_identity::types::RegistrationFlowNextStep;
use std::time::Duration;

#[test]
fn should_not_require_captcha_below_threshold_rate() {
    let env = env();
    let canister_id =
        install_ii_canister_with_arg(&env, II_WASM.clone(), arg_with_dynamic_captcha());
    let authn_method = test_authn_method();

    let flow_principal = test_principal(0);
    let result = api_v2::identity_registration_start(&env, canister_id, flow_principal)
        .expect("API call failed")
        .expect("registration start failed");

    assert!(matches!(result.next_step, RegistrationFlowNextStep::Finish));

    api_v2::identity_registration_finish(
        &env,
        canister_id,
        flow_principal,
        &authn_method.clone(),
        None,
    )
    .expect("API call failed")
    .expect("registration finish failed");
}

#[test]
fn should_require_captcha_above_threshold_rate() {
    let env = env();
    let canister_id =
        install_ii_canister_with_arg(&env, II_WASM.clone(), arg_with_dynamic_captcha());
    let authn_method = test_authn_method();

    // initialize a base rate of one registration every 4 seconds for 100 seconds (reference rate)
    for _ in 0..25 {
        create_identity_with_authn_method(&env, canister_id, &authn_method);
        env.advance_time(Duration::from_secs(4))
    }

    // Double the rate of registrations to one per second
    // The 20% threshold rate should allow 2 registrations before the captcha kicks in
    for i in 0..2 {
        let flow_principal = test_principal(i);
        let result = api_v2::identity_registration_start(&env, canister_id, flow_principal)
            .expect("API call failed")
            .expect("registration start failed");

        assert!(matches!(result.next_step, RegistrationFlowNextStep::Finish));

        api_v2::identity_registration_finish(
            &env,
            canister_id,
            flow_principal,
            &authn_method,
            None,
        )
        .expect("API call failed")
        .expect("registration finish failed");
        env.advance_time(Duration::from_secs(1));
    }

    let result = api_v2::identity_registration_start(&env, canister_id, test_principal(99))
        .expect("API call failed")
        .expect("registration start failed");

    // captcha kicks in
    assert!(matches!(
        result.next_step,
        RegistrationFlowNextStep::CheckCaptcha { .. }
    ));
}
