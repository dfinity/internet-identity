/// Tests for the II domain specific active anchor statistics.
use canister_tests::api::internet_identity as api;
use canister_tests::flows;
use canister_tests::framework::*;
use ic_test_state_machine_client::CallError;
use std::ops::Add;
use std::time::{Duration, UNIX_EPOCH};

const DAY_SECONDS: u64 = 24 * 60 * 60;
const MONTH_SECONDS: u64 = 30 * DAY_SECONDS;

const ICP0_APP_ORIGIN: &str = "https://identity.ic0.app";
const INTERNETCOMPUTER_ORG_ORIGIN: &str = "https://identity.internetcomputer.org";
const OTHER_ORIGIN: &str = "https://example.com";

/// Tests that daily active anchors are counted correctly.
#[test]
fn should_report_daily_active_anchors() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let ic0_app_device = device_with_origin(Some(ICP0_APP_ORIGIN.to_string()));
    let internetcomputer_org_device =
        device_with_origin(Some(INTERNETCOMPUTER_ORG_ORIGIN.to_string()));
    let other_origin_device = device_with_origin(Some(OTHER_ORIGIN.to_string()));
    let no_origin_device = device_with_origin(None);

    // ensure stats are initially absent
    assert_eq!(
        api::stats(&env, canister_id)?.domain_active_anchor_stats,
        None
    );

    let ic0_app_anchor = flows::register_anchor_with_device(&env, canister_id, &ic0_app_device);

    let both_domains_anchor =
        flows::register_anchor_with_device(&env, canister_id, &ic0_app_device);
    api::add(
        &env,
        canister_id,
        principal(&ic0_app_device),
        both_domains_anchor,
        internetcomputer_org_device.clone(),
    )?;
    // some activity on the other domain for the both_domains_anchor
    api::get_anchor_info(
        &env,
        canister_id,
        principal(&internetcomputer_org_device),
        both_domains_anchor,
    )?;

    // more activity to get #active users different from 1
    for _ in 0..5 {
        flows::register_anchor_with_device(&env, canister_id, &ic0_app_device);
    }
    flows::register_anchor_with_device(&env, canister_id, &internetcomputer_org_device);
    flows::register_anchor_with_device(&env, canister_id, &internetcomputer_org_device);
    flows::register_anchor_with_device(&env, canister_id, &other_origin_device);
    flows::register_anchor_with_device(&env, canister_id, &no_origin_device);

    let stats = api::stats(&env, canister_id)?
        .domain_active_anchor_stats
        .unwrap()
        .ongoing
        .daily_active_anchors;
    assert_eq!(stats.ic0_app_counter, 6);
    assert_eq!(stats.internetcomputer_org_counter, 2);
    assert_eq!(stats.both_ii_domains_counter, 1);

    // repeated activity within the 24h collection period should not increase the counter
    api::get_anchor_info(
        &env,
        canister_id,
        principal(&ic0_app_device),
        ic0_app_anchor,
    )?;

    let stats = api::stats(&env, canister_id)?
        .domain_active_anchor_stats
        .unwrap()
        .ongoing
        .daily_active_anchors;
    assert_eq!(stats.ic0_app_counter, 6);

    env.advance_time(Duration::from_secs(DAY_SECONDS));

    // some activity is required to update the stats
    api::get_anchor_info(
        &env,
        canister_id,
        principal(&ic0_app_device),
        ic0_app_anchor,
    )?;

    let metrics = get_metrics(&env, canister_id);
    assert_metric(
        &metrics,
        "internet_identity_daily_active_anchors_by_domain{domain=\"identity.ic0.app\"}",
        6f64,
    );
    assert_metric(
            &metrics,
            "internet_identity_daily_active_anchors_by_domain{domain=\"identity.internetcomputer.org\"}",
            2f64,
        );
    assert_metric(
        &metrics,
        "internet_identity_daily_active_anchors_by_domain{domain=\"both_ii_domains\"}",
        1f64,
    );

    let stats = api::stats(&env, canister_id)?
        .domain_active_anchor_stats
        .unwrap()
        .completed
        .daily_active_anchors
        .unwrap();
    assert_eq!(stats.ic0_app_counter, 6);
    assert_eq!(stats.internetcomputer_org_counter, 2);
    assert_eq!(stats.both_ii_domains_counter, 1);
    Ok(())
}

/// Tests that monthly active anchors are counted correctly.
#[test]
fn should_report_monthly_active_anchors() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let ic0_app_device = device_with_origin(Some(ICP0_APP_ORIGIN.to_string()));
    let internetcomputer_org_device =
        device_with_origin(Some(INTERNETCOMPUTER_ORG_ORIGIN.to_string()));
    let other_origin_device = device_with_origin(Some(OTHER_ORIGIN.to_string()));
    let no_origin_device = device_with_origin(None);

    // ensure stats are initially absent
    assert_eq!(
        api::stats(&env, canister_id)?.domain_active_anchor_stats,
        None
    );

    let ic0_app_anchor = flows::register_anchor_with_device(&env, canister_id, &ic0_app_device);

    let both_domains_anchor =
        flows::register_anchor_with_device(&env, canister_id, &ic0_app_device);
    api::add(
        &env,
        canister_id,
        principal(&ic0_app_device),
        both_domains_anchor,
        internetcomputer_org_device.clone(),
    )?;
    // some activity on the other domain for the both_domains_anchor
    api::get_anchor_info(
        &env,
        canister_id,
        principal(&internetcomputer_org_device),
        both_domains_anchor,
    )?;

    // more activity to get #active users different from 1
    flows::register_anchor_with_device(&env, canister_id, &ic0_app_device);
    flows::register_anchor_with_device(&env, canister_id, &internetcomputer_org_device);
    flows::register_anchor_with_device(&env, canister_id, &internetcomputer_org_device);
    flows::register_anchor_with_device(&env, canister_id, &internetcomputer_org_device);
    flows::register_anchor_with_device(&env, canister_id, &other_origin_device);
    flows::register_anchor_with_device(&env, canister_id, &no_origin_device);

    let stats = api::stats(&env, canister_id)?
        .domain_active_anchor_stats
        .unwrap()
        .ongoing
        .monthly_active_anchors
        .first()
        .unwrap()
        .clone();
    assert_eq!(stats.ic0_app_counter, 2);
    assert_eq!(stats.internetcomputer_org_counter, 3);
    assert_eq!(stats.both_ii_domains_counter, 1);

    // repeated activity within the 24h collection period should not increase the counter
    api::get_anchor_info(
        &env,
        canister_id,
        principal(&ic0_app_device),
        ic0_app_anchor,
    )?;

    let stats = api::stats(&env, canister_id)?
        .domain_active_anchor_stats
        .unwrap()
        .ongoing
        .monthly_active_anchors
        .first()
        .unwrap()
        .clone();
    assert_eq!(stats.ic0_app_counter, 2);

    env.advance_time(Duration::from_secs(MONTH_SECONDS));

    // some activity is required to update the stats
    api::get_anchor_info(
        &env,
        canister_id,
        principal(&ic0_app_device),
        ic0_app_anchor,
    )?;

    let metrics = get_metrics(&env, canister_id);
    assert_metric(
        &metrics,
        "internet_identity_monthly_active_anchors_by_domain{domain=\"identity.ic0.app\"}",
        2f64,
    );
    assert_metric(
            &metrics,
            "internet_identity_monthly_active_anchors_by_domain{domain=\"identity.internetcomputer.org\"}",
            3f64,
        );
    assert_metric(
        &metrics,
        "internet_identity_monthly_active_anchors_by_domain{domain=\"both_ii_domains\"}",
        1f64,
    );

    let stats = api::stats(&env, canister_id)?
        .domain_active_anchor_stats
        .unwrap()
        .completed
        .monthly_active_anchors
        .unwrap();
    assert_eq!(stats.ic0_app_counter, 2);
    assert_eq!(stats.internetcomputer_org_counter, 3);
    assert_eq!(stats.both_ii_domains_counter, 1);
    Ok(())
}

/// Tests that monthly active anchors are updated every 24h.
#[test]
fn should_update_monthly_active_anchors_every_day() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let ic0_app_device = device_with_origin(Some(ICP0_APP_ORIGIN.to_string()));
    let anchor_number = flows::register_anchor_with_device(&env, canister_id, &ic0_app_device);
    flows::register_anchor_with_device(&env, canister_id, &ic0_app_device);

    let stats = api::stats(&env, canister_id)?;
    assert_eq!(
        stats
            .domain_active_anchor_stats
            .unwrap()
            .ongoing
            .monthly_active_anchors
            .first()
            .unwrap()
            .ic0_app_counter,
        2
    );

    // advance by 24h to record in the shifted 30-day period
    env.advance_time(Duration::from_secs(DAY_SECONDS));

    api::get_anchor_info(&env, canister_id, principal(&ic0_app_device), anchor_number)?;

    let stats = api::stats(&env, canister_id)?
        .domain_active_anchor_stats
        .unwrap()
        .ongoing
        .monthly_active_anchors;
    assert_eq!(stats.first().unwrap().ic0_app_counter, 2);
    assert_eq!(stats.get(1).unwrap().ic0_app_counter, 1);

    // advance time to complete the first 30-day collection period
    env.advance_time(Duration::from_secs(29 * DAY_SECONDS));

    // some activity is required to update the stats
    api::get_anchor_info(&env, canister_id, principal(&ic0_app_device), anchor_number)?;

    assert_metric(
        &get_metrics(&env, canister_id),
        "internet_identity_monthly_active_anchors_by_domain{domain=\"identity.ic0.app\"}",
        2f64,
    );

    let stats = api::stats(&env, canister_id)?
        .domain_active_anchor_stats
        .unwrap();
    assert_eq!(
        stats
            .ongoing
            .monthly_active_anchors
            .first()
            .unwrap()
            .ic0_app_counter,
        1
    );
    assert_eq!(
        stats
            .completed
            .monthly_active_anchors
            .unwrap()
            .ic0_app_counter,
        2
    );

    // advance by 24h to complete the next 30-day period
    env.advance_time(Duration::from_secs(DAY_SECONDS));

    // some activity is required to update the stats
    api::get_anchor_info(&env, canister_id, principal(&ic0_app_device), anchor_number)?;

    assert_metric(
        &get_metrics(&env, canister_id),
        "internet_identity_monthly_active_anchors_by_domain{domain=\"identity.ic0.app\"}",
        1f64,
    );

    Ok(())
}

/// Tests that active anchor stats are kept across upgrades.
#[test]
fn should_keep_stats_across_upgrades() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    flows::register_anchor(&env, canister_id);

    let stats = api::stats(&env, canister_id)?;
    assert_eq!(
        stats
            .domain_active_anchor_stats
            .unwrap()
            .ongoing
            .daily_active_anchors
            .internetcomputer_org_counter,
        1
    );

    upgrade_ii_canister(&env, canister_id, II_WASM.clone());

    let stats = api::stats(&env, canister_id)?;
    assert_eq!(
        stats
            .domain_active_anchor_stats
            .unwrap()
            .ongoing
            .daily_active_anchors
            .internetcomputer_org_counter,
        1
    );

    Ok(())
}

/// Tests that the ongoing monthly collection periods are rolled over correctly.
#[test]
fn should_have_30_parallel_monthly_collection_windows() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let anchor_number = flows::register_anchor(&env, canister_id);

    let stats = api::stats(&env, canister_id)?;
    assert_eq!(
        stats
            .domain_active_anchor_stats
            .unwrap()
            .ongoing
            .monthly_active_anchors
            .len(),
        1
    );

    for _ in 0..30 {
        // advance time to trigger the next ongoing 30-day collection period
        env.advance_time(Duration::from_secs(DAY_SECONDS));
        // some activity is required to update the stats
        api::get_anchor_info(&env, canister_id, principal_1(), anchor_number)?;
    }
    let stats = api::stats(&env, canister_id)?;
    assert_eq!(
        stats
            .domain_active_anchor_stats
            .as_ref()
            .unwrap()
            .ongoing
            .monthly_active_anchors
            .len(),
        30
    );

    // after 30 days the first monthly collection period should be completed
    assert_eq!(
        stats
            .domain_active_anchor_stats
            .as_ref()
            .unwrap()
            .completed
            .monthly_active_anchors
            .as_ref()
            .unwrap()
            .internetcomputer_org_counter,
        1
    );

    Ok(())
}

/// Tests that the stats are updated correctly even with long periods of no activity at all.
#[test]
fn should_have_correct_stats_after_long_inactivity() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());

    // some activity
    let anchor_number = flows::register_anchor(&env, canister_id);

    let t0 = env.time();

    // more than 24h without activity
    env.advance_time(Duration::from_secs(2 * DAY_SECONDS + 1));

    // some activity to update stats
    api::get_anchor_info(&env, canister_id, principal_1(), anchor_number)?;

    let stats = api::stats(&env, canister_id)?;
    let daily_stats = stats
        .domain_active_anchor_stats
        .unwrap()
        .completed
        .daily_active_anchors
        .unwrap();
    assert_eq!(daily_stats.internetcomputer_org_counter, 0);
    assert_eq!(
        daily_stats.start_timestamp,
        t0.duration_since(UNIX_EPOCH)
            .unwrap()
            .add(Duration::from_secs(DAY_SECONDS))
            .as_nanos() as u64
    );

    // more than 30 days without activity
    env.advance_time(Duration::from_secs(31 * DAY_SECONDS));

    // some activity to update stats
    api::get_anchor_info(&env, canister_id, principal_1(), anchor_number)?;

    let stats = api::stats(&env, canister_id)?;
    let monthly_stats = stats
        .domain_active_anchor_stats
        .as_ref()
        .unwrap()
        .completed
        .monthly_active_anchors
        .as_ref()
        .unwrap();
    assert_eq!(monthly_stats.internetcomputer_org_counter, 0);
    assert_eq!(
        monthly_stats.start_timestamp,
        t0.duration_since(UNIX_EPOCH)
            .unwrap()
            .add(Duration::from_secs(3 * DAY_SECONDS))
            .as_nanos() as u64
    );

    // there is one ongoing collection period setup
    assert_eq!(
        stats
            .domain_active_anchor_stats
            .unwrap()
            .ongoing
            .monthly_active_anchors
            .len(),
        1
    );

    Ok(())
}

/// Tests that activity on other domain and an II domain is attributed to the II domain.
#[test]
fn should_count_activity_on_other_and_ii_domain() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let internetcomputer_org_device =
        device_with_origin(Some(INTERNETCOMPUTER_ORG_ORIGIN.to_string()));
    let other_origin_device = device_with_origin(Some(OTHER_ORIGIN.to_string()));

    // ensure stats are initially absent
    assert_eq!(
        api::stats(&env, canister_id)?.domain_active_anchor_stats,
        None
    );

    let anchor_number = flows::register_anchor_with_device(&env, canister_id, &other_origin_device);

    // activity not attributed to any II domain --> no stats
    assert_eq!(
        api::stats(&env, canister_id)?.domain_active_anchor_stats,
        None
    );

    api::add(
        &env,
        canister_id,
        principal(&other_origin_device),
        anchor_number,
        internetcomputer_org_device.clone(),
    )?;

    // some activity on the II domain device
    api::get_anchor_info(
        &env,
        canister_id,
        principal(&internetcomputer_org_device),
        anchor_number,
    )?;

    let stats = api::stats(&env, canister_id)?
        .domain_active_anchor_stats
        .unwrap()
        .ongoing
        .daily_active_anchors;

    // activity is now attributed to II domain
    assert_eq!(stats.ic0_app_counter, 0);
    assert_eq!(stats.internetcomputer_org_counter, 1);
    assert_eq!(stats.both_ii_domains_counter, 0);
    Ok(())
}
