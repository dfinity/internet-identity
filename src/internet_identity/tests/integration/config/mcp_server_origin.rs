use canister_tests::api::internet_identity as api;
use canister_tests::framework::{
    env, install_ii_canister_with_arg, upgrade_ii_canister_with_arg, II_WASM,
};
use internet_identity_interface::internet_identity::types::InternetIdentityInit;

#[test]
fn should_init_default() {
    let env = env();

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), None);
    assert_eq!(
        api::config(&env, canister_id).unwrap().mcp_server_origin,
        None
    );
}

#[test]
fn should_init_config() {
    let env = env();
    let mcp_server_origin = Some("https://mcp.id.ai".to_string());

    let canister_id = install_ii_canister_with_arg(
        &env,
        II_WASM.clone(),
        Some(InternetIdentityInit {
            mcp_server_origin: mcp_server_origin.clone(),
            ..Default::default()
        }),
    );
    assert_eq!(
        api::config(&env, canister_id).unwrap().mcp_server_origin,
        mcp_server_origin
    );
}

#[test]
fn should_update_config() {
    let env = env();
    let mut config = InternetIdentityInit {
        mcp_server_origin: Some("https://mcp.id.ai".into()),
        ..Default::default()
    };
    let updated_value = Some("https://mcp.beta.id.ai".to_string());

    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(config.clone()));
    config.mcp_server_origin = updated_value.clone();
    upgrade_ii_canister_with_arg(&env, canister_id, II_WASM.clone(), Some(config)).unwrap();
    assert_eq!(
        api::config(&env, canister_id).unwrap().mcp_server_origin,
        updated_value
    );
}

#[test]
fn should_retain_config() {
    let env = env();
    let mcp_server_origin = Some("https://mcp.id.ai".to_string());

    let canister_id = install_ii_canister_with_arg(
        &env,
        II_WASM.clone(),
        Some(InternetIdentityInit {
            mcp_server_origin: mcp_server_origin.clone(),
            ..Default::default()
        }),
    );
    // Upgrading with no arg must leave the stored value untouched.
    upgrade_ii_canister_with_arg(&env, canister_id, II_WASM.clone(), None).unwrap();
    assert_eq!(
        api::config(&env, canister_id).unwrap().mcp_server_origin,
        mcp_server_origin
    );
}
