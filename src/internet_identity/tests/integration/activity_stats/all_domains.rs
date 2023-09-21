//! Tests related to the daily and monthly active anchor statistics (domain independent).

use canister_tests::api::internet_identity as api;
use canister_tests::flows;
use canister_tests::framework::*;
use pocket_ic::CallError;
use std::time::Duration;

const DAY_SECONDS: u64 = 24 * 60 * 60;
const MONTH_SECONDS: u64 = 30 * DAY_SECONDS;

/// Tests that daily active anchors are counted correctly.
#[test]
fn should_report_daily_active_anchors() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());

    // ensure stats are initially absent
    assert!(!get_metrics(&env, canister_id).contains("internet_identity_daily_active_anchors"));

    let anchor_number = flows::register_anchor(&env, canister_id);
    flows::register_anchor(&env, canister_id);

    // repeated activity within the 24h collection period should not increase the counter
    api::get_anchor_info(&env, canister_id, principal_1(), anchor_number)?;

    env.advance_time(Duration::from_secs(DAY_SECONDS));

    // some activity is required to update the stats
    api::get_anchor_info(&env, canister_id, principal_1(), anchor_number)?;

    assert_metric(
        &get_metrics(&env, canister_id),
        "internet_identity_daily_active_anchors",
        2f64,
    );
    Ok(())
}

/// Tests that monthly active anchors are counted correctly.
#[test]
fn should_report_monthly_active_anchors() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());

    // ensure stats are initially absent
    assert!(!get_metrics(&env, canister_id).contains("internet_identity_monthly_active_anchors"));

    let anchor_number = flows::register_anchor(&env, canister_id);
    flows::register_anchor(&env, canister_id);

    // repeated activity within the 30-day collection period should not increase the counter
    api::get_anchor_info(&env, canister_id, principal_1(), anchor_number)?;

    env.advance_time(Duration::from_secs(MONTH_SECONDS));

    // some activity is required to update the stats
    api::get_anchor_info(&env, canister_id, principal_1(), anchor_number)?;

    assert_metric(
        &get_metrics(&env, canister_id),
        "internet_identity_monthly_active_anchors",
        2f64,
    );
    Ok(())
}

/// Tests that monthly active anchors are updated every 24h.
#[test]
fn should_update_monthly_active_anchors_every_day() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());

    // ensure stats are initially absent
    assert!(!get_metrics(&env, canister_id).contains("internet_identity_monthly_active_anchors"));

    let anchor_number = flows::register_anchor(&env, canister_id);
    flows::register_anchor(&env, canister_id);

    // advance by 24h to record in the shifted 30-day period
    env.advance_time(Duration::from_secs(DAY_SECONDS));

    api::get_anchor_info(&env, canister_id, principal_1(), anchor_number)?;

    // advance time to complete the first 30-day collection period
    env.advance_time(Duration::from_secs(29 * DAY_SECONDS));

    // some activity is required to update the stats
    api::get_anchor_info(&env, canister_id, principal_1(), anchor_number)?;

    assert_metric(
        &get_metrics(&env, canister_id),
        "internet_identity_monthly_active_anchors",
        2f64,
    );

    // advance by 24h to complete the next 30-day period
    env.advance_time(Duration::from_secs(DAY_SECONDS));

    // some activity is required to update the stats
    api::get_anchor_info(&env, canister_id, principal_1(), anchor_number)?;

    assert_metric(
        &get_metrics(&env, canister_id),
        "internet_identity_monthly_active_anchors",
        1f64,
    );

    Ok(())
}

/// Tests that active anchor stats are kept across upgrades from the previous and the same release.
#[test]
fn should_keep_stats_across_upgrades() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM_PREVIOUS.clone());

    // ensure stats are initially absent
    assert!(!get_metrics(&env, canister_id).contains("internet_identity_daily_active_anchors"));

    let anchor_number = flows::register_anchor(&env, canister_id);

    // upgrade previous -> current
    upgrade_ii_canister(&env, canister_id, II_WASM.clone());
    // upgrade current -> current
    upgrade_ii_canister(&env, canister_id, II_WASM.clone());

    env.advance_time(Duration::from_secs(DAY_SECONDS));

    // some activity is required to update the stats
    api::get_anchor_info(&env, canister_id, principal_1(), anchor_number)?;

    assert_metric(
        &get_metrics(&env, canister_id),
        "internet_identity_daily_active_anchors",
        1f64,
    );

    Ok(())
}

/// Tests that the stats are updated correctly even with long periods of no activity at all.
#[test]
fn should_have_correct_stats_after_long_inactivity() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());

    // ensure stats are initially absent
    assert!(!get_metrics(&env, canister_id).contains("internet_identity_daily_active_anchors"));

    // some activity
    let anchor_number = flows::register_anchor(&env, canister_id);

    // more than 24h without activity
    env.advance_time(Duration::from_secs(2 * DAY_SECONDS + 1));

    // some activity to update stats
    api::get_anchor_info(&env, canister_id, principal_1(), anchor_number)?;

    assert_metric(
        &get_metrics(&env, canister_id),
        "internet_identity_daily_active_anchors",
        0f64,
    );

    // more than 30 days without activity
    env.advance_time(Duration::from_secs(31 * DAY_SECONDS));

    // some activity to update stats
    api::get_anchor_info(&env, canister_id, principal_1(), anchor_number)?;

    assert_metric(
        &get_metrics(&env, canister_id),
        "internet_identity_monthly_active_anchors",
        0f64,
    );

    Ok(())
}
