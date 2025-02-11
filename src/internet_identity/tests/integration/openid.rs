//! Tests related to prepare_delegation, get_delegation and get_principal II canister calls.

use candid::Principal;
use canister_tests::api::internet_identity as api;
use canister_tests::flows;
use canister_tests::framework::*;
use internet_identity_interface::internet_identity::types::GetDelegationResponse;
use internet_identity_interface::internet_identity::types::OpenIdPrepareDelegationResponse;
use pocket_ic::CallError;
use pocket_ic::ErrorCode::CanisterCalledTrap;
use regex::Regex;
use serde_bytes::ByteBuf;
use std::time::Duration;

/// Verifies that valid JWT delegations are issued.
#[test]
fn should_get_valid_jwt_delegation() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let pub_session_key = ByteBuf::from("session public key");
    let jwt = jwt();
    let salt = &[0u8; 32];

    let prepare_response = match api::openid_prepare_delegation(
        &env,
        canister_id,
        principal_1(),
        &jwt,
        &salt,
        &pub_session_key,
    )? {
        Ok(response) => response,
        Err(err) => panic!("Failing at openid_prepare_delegation: {:?}", err),
    };

    assert_eq!(
        prepare_response.expiration,
        time(&env) + Duration::from_secs(30 * 60).as_nanos() as u64 // default expiration: 30 minutes
    );

    let signed_delegation = match api::openid_get_delegation(
        &env,
        canister_id,
        principal_1(),
        &jwt,
        &salt,
        &pub_session_key,
        &prepare_response.expiration,
    )? {
        Ok(signed_delegation) => signed_delegation,
        Err(err) => {
            panic!("Failing at openid_get_delegation: {:?}", err)
        }
    };

    verify_delegation(
        &env,
        prepare_response.user_key,
        &signed_delegation,
        &env.root_key().unwrap(),
    );
    assert_eq!(signed_delegation.delegation.pubkey, pub_session_key);
    assert_eq!(
        signed_delegation.delegation.expiration,
        prepare_response.expiration
    );
    Ok(())
}

fn jwt() -> String {
    "eyJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJodHRwczovL2V4YW1wbGUuY29tIn0.SBeD7pV65F98wStsBuC_VRn-yjLoyf6iojJl9Y__wN0".to_string()
}

fn salt() -> String {
    //TODO:
    // export const createAnonymousNonce = async (
    //     principal: Principal
    //   ): Promise<{ nonce: string; salt: Uint8Array }> => {
    //     const salt = window.crypto.getRandomValues(new Uint8Array(32));
    //     const bytes = new Uint8Array(32 + principal.toUint8Array().byteLength);
    //     bytes.set(salt);
    //     bytes.set(principal.toUint8Array(), 32);
    //     const nonce = toBase64URL(
    //       await window.crypto.subtle.digest("SHA-256", bytes)
    //     );
    //     return { nonce, salt };
    //   };
    todo!();
}
