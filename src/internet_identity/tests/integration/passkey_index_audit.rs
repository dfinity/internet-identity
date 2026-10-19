//! Tests for the temporary, read-only passkey index audit.

use canister_tests::api::internet_identity as api;
use canister_tests::flows;
use canister_tests::framework::*;
use internet_identity_interface::internet_identity::types::{
    PasskeyIndexAuditCounts, PasskeyIndexAuditTotals,
};
use pocket_ic::RejectResponse;
use std::time::Duration;

/// Verifies that after an upgrade the audit examines the anchors written by the previous
/// release and finds every passkey indexed to its anchor.
#[test]
fn should_report_every_passkey_indexed_after_an_upgrade() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM_PREVIOUS.clone());
    let anchor_with_two_passkeys =
        flows::register_anchor_with(&env, canister_id, principal_1(), &device_data_1());
    api::add(
        &env,
        canister_id,
        principal_1(),
        anchor_with_two_passkeys,
        &device_data_2(),
    )?;
    // A recovery phrase is not a passkey, so the audit leaves it out.
    api::add(
        &env,
        canister_id,
        principal_1(),
        anchor_with_two_passkeys,
        &recovery_device_data_1(),
    )?;
    flows::register_anchor_with_device(&env, canister_id, &distinct_device(&device_data_1(), 0));

    upgrade_ii_canister(&env, canister_id, II_WASM.clone());

    env.advance_time(Duration::from_secs(5));
    for _ in 0..5 {
        env.tick();
    }

    let status = api::passkey_index_audit_status(&env, canister_id, principal_1())?;
    assert!(status.is_done, "the audit should report completion");
    let all_indexed_to_anchor = PasskeyIndexAuditCounts {
        indexed_to_anchor: 3,
        ..PasskeyIndexAuditCounts::default()
    };
    assert_eq!(
        status.totals,
        PasskeyIndexAuditTotals {
            anchors: 2,
            passkeys: 3,
            pubkey_index: all_indexed_to_anchor,
            credential_id_index: all_indexed_to_anchor,
            dummy_credential_ids: 0,
        }
    );
    Ok(())
}
