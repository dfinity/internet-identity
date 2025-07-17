use crate::v2_api::authn_method_test_helpers::{
    create_identity_with_authn_method, test_authn_method,
};
use canister_tests::api::internet_identity as api;
use canister_tests::framework::{
    env, install_ii_canister_with_arg, upgrade_ii_canister_with_arg, II_WASM,
};
use internet_identity_interface::internet_identity::types::CaptchaTrigger::Static;
use internet_identity_interface::internet_identity::types::StaticCaptchaTrigger::CaptchaDisabled;
use internet_identity_interface::internet_identity::types::{
    CaptchaConfig, InternetIdentityInit, OpenIdConfig,
};

#[test]
fn should_init_default() {
    let env = env();

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), None);
    assert_eq!(
        api::config(&env, canister_id)
            .unwrap()
            .assigned_user_number_range,
        Some((10000, 67_116_816))
    );
}

#[test]
fn should_init_config() {
    let env = env();
    let configs = vec![
        InternetIdentityInit {
            assigned_user_number_range: Some((10000, 7_569_744)),
            ..Default::default()
        },
        InternetIdentityInit {
            assigned_user_number_range: Some((3456, 798_977)),
            ..Default::default()
        },
    ];

    for config in configs {
        let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config.clone()));
        assert_eq!(
            api::config(&env, canister_id)
                .unwrap()
                .assigned_user_number_range,
            config.assigned_user_number_range
        );
    }
}

#[test]
fn should_update_config() {
    let env = env();
    let mut config = InternetIdentityInit {
        assigned_user_number_range: Some((3456, 798_977)),
        ..Default::default()
    };
    let updated_value = Some((10000, 67_116_816));

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config.clone()));
    config.assigned_user_number_range = updated_value;
    upgrade_ii_canister_with_arg(&env, canister_id, II_WASM.clone(), Some(config.clone())).unwrap();
    assert_eq!(
        api::config(&env, canister_id)
            .unwrap()
            .assigned_user_number_range,
        updated_value
    );
}

#[test]
fn should_retain_config() {
    let env = env();
    let configs = vec![
        InternetIdentityInit {
            assigned_user_number_range: Some((10000, 7_569_744)),
            ..Default::default()
        },
        InternetIdentityInit {
            assigned_user_number_range: Some((3456, 798_977)),
            ..Default::default()
        },
    ];

    for config in configs {
        let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config.clone()));
        // No config argument
        upgrade_ii_canister_with_arg(&env, canister_id, II_WASM.clone(), None).unwrap();
        // Unrelated config change
        upgrade_ii_canister_with_arg(
            &env,
            canister_id,
            II_WASM.clone(),
            Some(InternetIdentityInit {
                openid_google: Some(Some(OpenIdConfig {
                    client_id: "https://example.com".into(),
                })),
                ..Default::default()
            }),
        )
        .unwrap();
        assert_eq!(
            api::config(&env, canister_id)
                .unwrap()
                .assigned_user_number_range,
            config.assigned_user_number_range
        );
    }
}

#[test]
fn should_retain_anchor_on_user_range_start_increase() {
    let env = env();
    let config = InternetIdentityInit {
        assigned_user_number_range: Some((1000, 7_569_744)),
        captcha_config: Some(CaptchaConfig {
            max_unsolved_captchas: 100,
            captcha_trigger: Static(CaptchaDisabled),
        }),
        ..Default::default()
    };

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config));
    let authn_method = test_authn_method();
    assert_eq!(
        create_identity_with_authn_method(&env, canister_id, &authn_method),
        1000
    );
    assert!(upgrade_ii_canister_with_arg(
        &env,
        canister_id,
        II_WASM.clone(),
        Some(InternetIdentityInit {
            assigned_user_number_range: Some((1001, 7_569_744)),
            ..Default::default()
        }),
    )
    .is_err());
}

#[test]
fn should_retain_anchor_on_user_range_end_decrease() {
    let env = env();
    let config = InternetIdentityInit {
        assigned_user_number_range: Some((1000, 7_569_744)),
        captcha_config: Some(CaptchaConfig {
            max_unsolved_captchas: 100,
            captcha_trigger: Static(CaptchaDisabled),
        }),
        ..Default::default()
    };

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config));
    let authn_method = test_authn_method();
    assert_eq!(
        create_identity_with_authn_method(&env, canister_id, &authn_method),
        1000
    );
    assert_eq!(
        create_identity_with_authn_method(&env, canister_id, &authn_method),
        1001
    );
    assert!(upgrade_ii_canister_with_arg(
        &env,
        canister_id,
        II_WASM.clone(),
        Some(InternetIdentityInit {
            assigned_user_number_range: Some((1000, 1001)),
            ..Default::default()
        }),
    )
    .is_err());
}
