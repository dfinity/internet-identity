use canister_tests::api::internet_identity as api;
use canister_tests::framework::{
    env, install_ii_canister_with_arg, upgrade_ii_canister_with_arg, II_WASM,
};
use internet_identity_interface::internet_identity::types::InternetIdentityInit;

#[test]
fn should_init_default() {
    let env = env();

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), None);
    assert_eq!(api::config(&env, canister_id).unwrap().backend_origin, None);
}

#[test]
fn should_init_config() {
    let env = env();
    let backend_origin = Some("https://backend.id.ai".to_string());

    let canister_id = install_ii_canister_with_arg(
        &env,
        II_WASM.clone(),
        Some(InternetIdentityInit {
            backend_origin: backend_origin.clone(),
            ..Default::default()
        }),
    );
    assert_eq!(
        api::config(&env, canister_id).unwrap().backend_origin,
        backend_origin
    );
}

#[test]
fn should_update_config() {
    let env = env();
    let mut config = InternetIdentityInit {
        backend_origin: Some("https://backend.id.ai".into()),
        ..Default::default()
    };
    let updated_value = Some("https://backend.beta.id.ai".to_string());

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config.clone()));
    config.backend_origin = updated_value.clone();
    upgrade_ii_canister_with_arg(&env, canister_id, II_WASM.clone(), Some(config)).unwrap();
    assert_eq!(
        api::config(&env, canister_id).unwrap().backend_origin,
        updated_value
    );
}

#[test]
fn should_retain_config() {
    let env = env();
    let backend_origin = Some("https://backend.id.ai".to_string());

    let canister_id = install_ii_canister_with_arg(
        &env,
        II_WASM.clone(),
        Some(InternetIdentityInit {
            backend_origin: backend_origin.clone(),
            ..Default::default()
        }),
    );
    // No config argument: the previously stored value must be retained.
    upgrade_ii_canister_with_arg(&env, canister_id, II_WASM.clone(), None).unwrap();
    // Unrelated config change: the backend origin must still be retained.
    upgrade_ii_canister_with_arg(
        &env,
        canister_id,
        II_WASM.clone(),
        Some(InternetIdentityInit {
            enable_dapps_explorer: Some(true),
            ..Default::default()
        }),
    )
    .unwrap();
    assert_eq!(
        api::config(&env, canister_id).unwrap().backend_origin,
        backend_origin
    );
}
