//! Tests for making sure that the current version can be upgraded to from the last release.
//! This tests stable memory compatibility and pre / post install hooks.

use crate::email_recovery::fresh_identity;
use candid::{CandidType, Deserialize, Principal};
use canister_tests::api::internet_identity as api;
use canister_tests::flows;
use canister_tests::framework::*;
use internet_identity_interface::internet_identity::types::email_challenge::EmailChallengeDnsInput;
use internet_identity_interface::internet_identity::types::*;
use pocket_ic::common::rest::RawEffectivePrincipal;
use pocket_ic::ErrorCode::CanisterCalledTrap;
use pocket_ic::{call_candid, PocketIc, RejectResponse};
use regex::Regex;

/// Basic upgrade test.
#[test]
fn ii_upgrade_works() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    env.upgrade_canister(
        canister_id,
        II_WASM.clone(),
        candid::encode_one(None::<InternetIdentityInit>).unwrap(),
        None,
    )?;
    api::health_check(&env, canister_id);
    Ok(())
}

/// Test to verify that anchors are kept across upgrades.
#[test]
fn ii_upgrade_retains_anchors() {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM_PREVIOUS.clone());
    let user_number = flows::register_anchor(&env, canister_id);
    upgrade_ii_canister(&env, canister_id, II_WASM.clone());

    let anchor_info = api::get_anchor_info(&env, canister_id, principal_1(), user_number)
        .expect("get_anchor_info failed");
    assert_eq!(anchor_info.into_device_data(), vec![device_data_1()]);
}

/// Test to verify that anchor numbers are unchanged by changing the user range.
#[test]
fn should_retain_anchor_on_user_range_change() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM_PREVIOUS.clone());
    let user_number = flows::register_anchor(&env, canister_id);

    upgrade_ii_canister_with_arg(
        &env,
        canister_id,
        II_WASM.clone(),
        arg_with_anchor_range((10_000, 11_000)),
    )?;

    let anchor_info = api::get_anchor_info(&env, canister_id, principal_1(), user_number)?;

    assert_eq!(anchor_info.into_device_data(), vec![device_data_1()]);
    Ok(())
}

/// Test to verify that anchors number range can be changed on upgrade.
#[test]
fn should_allow_change_of_user_range_on_upgrade() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());

    upgrade_ii_canister_with_arg(
        &env,
        canister_id,
        II_WASM.clone(),
        arg_with_anchor_range((2000, 4000)),
    )?;

    let stats = api::stats(&env, canister_id)?;
    assert_eq!(stats.assigned_user_number_range, (2000, 4000));
    Ok(())
}

/// Test to verify that the user range cannot be changed to modify anchor numbers of existing anchors.
#[test]
fn should_not_affect_existing_anchors_on_user_range_change() {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());

    flows::register_anchor(&env, canister_id);

    let result = upgrade_ii_canister_with_arg(
        &env,
        canister_id,
        II_WASM.clone(),
        arg_with_anchor_range((2000, 4000)),
    );

    expect_user_error_with_message(
        result,
        CanisterCalledTrap,
        Regex::new("specified range \\[\\d+, \\d+\\) does not start from the same number \\(\\d+\\) as the existing range thus would make existing anchors invalid")
            .unwrap(),
    );
}

/// Test to verify that the user range cannot be changed to allow fewer anchors than are already registered.
#[test]
fn should_not_allow_fewer_users_than_existing_on_user_range_change() {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());

    flows::register_anchor(&env, canister_id);

    let result = upgrade_ii_canister_with_arg(
        &env,
        canister_id,
        II_WASM.clone(),
        arg_with_anchor_range((10_000, 10_000)),
    );

    expect_user_error_with_message(
        result,
        CanisterCalledTrap,
        Regex::new("specified range \\[\\d+, \\d+\\) does not accommodate all \\d+ anchors thus would make existing anchors invalid")
            .unwrap(),
    );
}

/// Test to verify that the user range cannot be changed to exceed the max capacity of the II canister.
#[test]
fn should_not_allow_user_range_exceeding_capacity() {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());

    flows::register_anchor(&env, canister_id);

    let result = upgrade_ii_canister_with_arg(
        &env,
        canister_id,
        II_WASM.clone(),
        arg_with_anchor_range((10_000, 10_000_000_000_000)),
    );

    expect_user_error_with_message(
        result,
        CanisterCalledTrap,
        Regex::new("specified range \\[\\d+, \\d+\\) is too large for this canister \\(max \\d+ entries\\)")
            .unwrap(),
    );
}

/// Verifies that an anchor stored before the session-device registry existed decodes
/// after an upgrade, and reports no devices rather than failing.
#[test]
fn should_report_no_browsers_for_an_anchor_from_the_previous_release() -> Result<(), RejectResponse>
{
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM_PREVIOUS.clone());
    let identity_number = flows::register_anchor(&env, canister_id);

    upgrade_ii_canister(&env, canister_id, II_WASM.clone());

    let info =
        api::api_v2::identity_info(&env, canister_id, principal_1(), identity_number)?.unwrap();
    assert_eq!(info.browsers, None);
    assert_eq!(info.authn_methods.len(), 1);

    Ok(())
}

/// `DohConfig` as `II_WASM_DOH_ALLOWLIST` declares it in its `.did`: an
/// allowlist of registered domains instead of `reserved_domains`.
#[derive(CandidType, Deserialize, Clone, Debug, PartialEq, Eq)]
pub(crate) struct LegacyDohConfig {
    pub(crate) allowed_domains: Vec<String>,
    pub(crate) max_cache_age_secs: Option<u64>,
}

/// The `doh_config` field of `II_WASM_DOH_ALLOWLIST`'s `InternetIdentityInit`,
/// encoded as its upgrade arg and decoded from its `config` query; every other
/// field is optional there, so it is left out.
#[derive(CandidType, Deserialize, Clone, Debug, PartialEq, Eq)]
pub(crate) struct LegacyDohInit {
    pub(crate) doh_config: Option<Option<LegacyDohConfig>>,
}

/// Upgrades `canister_id` to `II_WASM_DOH_ALLOWLIST` with `doh_config` as its arg.
pub(crate) fn configure_doh_allowlist_release(
    env: &PocketIc,
    canister_id: Principal,
    doh_config: LegacyDohConfig,
) {
    let arg = Some(LegacyDohInit {
        doh_config: Some(Some(doh_config)),
    });
    env.upgrade_canister(
        canister_id,
        II_WASM_DOH_ALLOWLIST.clone(),
        candid::encode_one(arg).expect("error encoding legacy DoH upgrade arg"),
        None,
    )
    .expect("upgrade to the DoH allowlist release with a legacy DoH config failed");
}

/// Reads `doh_config` from the `config` query in `II_WASM_DOH_ALLOWLIST`'s shape.
pub(crate) fn doh_allowlist_release_config(
    env: &PocketIc,
    canister_id: Principal,
) -> Option<Option<LegacyDohConfig>> {
    let (config,): (LegacyDohInit,) =
        call_candid(env, canister_id, RawEffectivePrincipal::None, "config", ())
            .expect("config query failed");
    config.doh_config
}

/// Verifies that a DoH allowlist stored by `II_WASM_DOH_ALLOWLIST` keeps DoH configured
/// after an upgrade without an arg: no domain is reserved, the cache age survives, and
/// an address outside the old allowlist is accepted.
#[test]
fn should_keep_doh_configured_with_no_reserved_domains_after_upgrading_a_legacy_allowlist() {
    for allowed_domains in [vec!["example.com".to_string()], vec![]] {
        let env = env();
        let canister_id = install_ii_canister(&env, II_WASM_DOH_ALLOWLIST.clone());
        let legacy_config = LegacyDohConfig {
            allowed_domains,
            max_cache_age_secs: Some(1234),
        };
        configure_doh_allowlist_release(&env, canister_id, legacy_config.clone());
        assert_eq!(
            doh_allowlist_release_config(&env, canister_id),
            Some(Some(legacy_config))
        );

        upgrade_ii_canister(&env, canister_id, II_WASM.clone());

        assert_eq!(
            api::config(&env, canister_id).unwrap().doh_config,
            Some(Some(DohConfig {
                reserved_domains: None,
                max_cache_age_secs: Some(1234),
            }))
        );
        let (identity_number, principal) = fresh_identity(&env, canister_id);
        let challenge = api::email_recovery_credential_prepare_add(
            &env,
            canister_id,
            principal,
            identity_number,
            EmailChallengeDnsInput {
                address: "bob@other.example".into(),
                dns_proof: None,
            },
        )
        .expect("prepare_add call failed")
        .expect("prepare_add should accept a domain outside the legacy allowlist");
        assert!(
            challenge.nonce.starts_with("II-Recovery-"),
            "unexpected nonce: {}",
            challenge.nonce
        );
    }
}

/// Verifies that once a legacy DoH allowlist has been upgraded, an upgrade arg sets
/// `reserved_domains`.
#[test]
fn should_reserve_doh_domains_by_upgrade_arg_after_upgrading_a_legacy_allowlist() {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM_DOH_ALLOWLIST.clone());
    configure_doh_allowlist_release(
        &env,
        canister_id,
        LegacyDohConfig {
            allowed_domains: vec!["example.com".into()],
            max_cache_age_secs: Some(1234),
        },
    );
    upgrade_ii_canister(&env, canister_id, II_WASM.clone());
    let reserved_config = DohConfig {
        reserved_domains: Some(vec!["gmail.com".into(), "outlook.com".into()]),
        max_cache_age_secs: Some(60),
    };

    upgrade_ii_canister_with_arg(
        &env,
        canister_id,
        II_WASM.clone(),
        Some(InternetIdentityInit {
            doh_config: Some(Some(reserved_config.clone())),
            ..Default::default()
        }),
    )
    .unwrap();

    assert_eq!(
        api::config(&env, canister_id).unwrap().doh_config,
        Some(Some(reserved_config))
    );
}
