use crate::archive::ArchiveState;
use crate::ii_domain::{maybe_domain_to_label, IIDomain};
use crate::state::PersistentState;
use crate::stats::activity_stats::activity_counter::ActivityCounter;
use crate::stats::activity_stats::ActivityStats;
use crate::stats::event_stats::AggregationWindow::{Day, Month};
use crate::stats::event_stats::{retrieve_aggregation, Aggregation, PD_COUNT, PD_SESS_SEC};
use crate::{state, IC0_APP_DOMAIN, INTERNETCOMPUTER_ORG_DOMAIN};
use ic_cdk::api::stable::{stable_size, WASM_PAGE_SIZE_IN_BYTES};
use ic_cdk::api::time;
use ic_metrics_encoder::{LabeledMetricsBuilder, MetricsEncoder};
use std::time::Duration;
use IIDomain::Ic0App;

/// Collects the various metrics exposed by the Internet Identity canister.
/// Returns a ascii-encoded string of the metrics in the Prometheus exposition format.
pub fn metrics() -> Result<Vec<u8>, std::io::Error> {
    let mut writer = MetricsEncoder::new(vec![], time() as i64 / 1_000_000);
    encode_metrics(&mut writer)?;
    Ok(writer.into_inner())
}

fn encode_metrics(w: &mut MetricsEncoder<Vec<u8>>) -> std::io::Result<()> {
    state::storage_borrow(|storage| {
        w.encode_gauge(
            "internet_identity_user_count",
            storage.anchor_count() as f64,
            "Number of users registered in this canister.",
        )?;
        let (lo, hi) = storage.assigned_anchor_number_range();
        w.encode_gauge(
            "internet_identity_min_user_number",
            lo as f64,
            "The lowest Identity Anchor served by this canister.",
        )?;
        w.encode_gauge(
            "internet_identity_max_user_number",
            (hi - 1) as f64,
            "The highest Identity Anchor that can be served by this canister.",
        )?;
        w.encode_gauge(
            "internet_identity_event_data_count",
            storage.event_data.len() as f64,
            "Number of events stored in event_data map.",
        )?;
        w.encode_gauge(
            "internet_identity_event_aggregations_count",
            storage.event_aggregations.len() as f64,
            "Number of entries in the event_aggregations map.",
        )?;
        w.encode_gauge(
            "internet_identity_total_accounts_count",
            storage.get_total_accounts_counter().stored_accounts as f64,
            "Number of total accounts registered in this canister.",
        )?;
        w.encode_gauge(
            "internet_identity_total_account_references_count",
            storage
                .get_total_accounts_counter()
                .stored_account_references as f64,
            "Number of total account references registered in this canister.",
        )?;
        w.encode_gauge(
            "internet_identity_account_counter_discrepancy_count",
            storage.get_discrepancy_counter().account_counter_rebuilds as f64,
            "Number of times the account counter was found to be inconsistent with the actual number of accounts.",
        )?;
        w.encode_gauge(
            "internet_identity_total_application_count",
            storage.get_total_application_count() as f64,
            "Number of total applications registered in this canister.",
        )?;
        if let Some(registration_rates) = storage.registration_rates.registration_rates() {
            w.gauge_vec(
                "internet_identity_registrations_per_second",
                "Rate of new identity registrations on Internet Identity",
            )?
            .value(
                &[("type", "reference_rate")],
                registration_rates.reference_rate_per_second,
            )?
            .value(
                &[("type", "current_rate")],
                registration_rates.current_rate_per_second,
            )?
            .value(
                &[("type", "captcha_threshold_rate")],
                registration_rates.captcha_threshold_rate,
            )?;
        }

        let mut virtual_memory_stats_builder = w.gauge_vec(
            "internet_identity_virtual_memory_size_pages",
            "Size of the managed memory in pages.",
        )?;
        for (memory, memory_size) in storage.memory_sizes() {
            virtual_memory_stats_builder =
                virtual_memory_stats_builder.value(&[("memory", &memory)], memory_size as f64)?;
        }
        Ok::<(), std::io::Error>(())
    })?;
    state::signature_map(|sigs| {
        w.encode_gauge(
            "internet_identity_signature_count",
            sigs.len() as f64,
            "Number of active signatures issued by this canister.",
        )
    })?;
    w.encode_gauge(
        "internet_identity_stable_memory_pages",
        stable_size() as f64,
        "Number of stable memory pages used by this canister.",
    )?;
    // Don't use the prefix internet_identity so that it gets picked up by a default alarm.
    w.encode_gauge(
        "stable_memory_bytes",
        (stable_size() * WASM_PAGE_SIZE_IN_BYTES) as f64,
        "Size of the stable memory allocated by this canister.",
    )?;
    #[cfg(target_arch = "wasm32")]
    w.encode_gauge(
        "internet_identity_heap_pages",
        core::arch::wasm32::memory_size::<0>() as f64,
        "Number of heap memory pages used by this canister.",
    )?;
    // Don't use the prefix internet_identity so that it gets picked up by a default alarm.
    #[cfg(target_arch = "wasm32")]
    w.encode_gauge(
        "heap_memory_bytes",
        (core::arch::wasm32::memory_size::<0>() as u64 * WASM_PAGE_SIZE_IN_BYTES) as f64,
        "Size of the heap memory allocated by this canister.",
    )?;
    w.encode_gauge(
        "internet_identity_temp_keys_count",
        state::with_temp_keys(|temp_keys| temp_keys.num_temp_keys()) as f64,
        "Number of temporary keys.",
    )?;
    w.encode_gauge(
        "internet_identity_last_upgrade_timestamp",
        state::last_upgrade_timestamp() as f64,
        "The most recent IC time (in nanos) when this canister was successfully upgraded.",
    )?;
    state::inflight_challenges(|inflight_challenges| {
        w.encode_gauge(
            "internet_identity_inflight_challenges",
            inflight_challenges.len() as f64,
            "The number of inflight CAPTCHA challenges",
        )
    })?;
    state::tentative_device_registrations(|tentative_device_registrations| {
        w.encode_gauge(
            "internet_identity_users_in_registration_mode",
            tentative_device_registrations.len() as f64,
            "The number of users in registration mode",
        )
    })?;
    state::lookup_tentative_device_registration(|lookup_tentative_device_registration_v2| {
        w.encode_gauge(
            "internet_identity_users_in_registration_mode_v2",
            lookup_tentative_device_registration_v2.len() as f64,
            "The number of users in registration mode 2.0",
        )
    })?;
    state::usage_metrics(|usage_metrics| {
        w.encode_gauge(
            "internet_identity_delegation_counter",
            usage_metrics.delegation_counter as f64,
            "The number of delegations created since last upgrade",
        )?;
        w.encode_gauge(
            "internet_identity_anchor_operations_counter",
            usage_metrics.anchor_operation_counter as f64,
            "The number of anchor operations since last upgrade",
        )?;
        w.encode_gauge(
            "internet_identity_prepare_id_alias_counter",
            usage_metrics.prepare_id_alias_counter as f64,
            "The number of successful prepare_id_alias calls handled since last upgrade. For each VC presentation flow, exactly one prepare_id_alias is made.",
        )
    })?;
    if let ArchiveState::Created { ref data, config } = state::archive_state() {
        w.encode_gauge(
            "internet_identity_archive_sequence_number",
            data.sequence_number as f64,
            "The number of entries written to the archive.",
        )?;
        w.encode_gauge(
            "internet_identity_buffered_archive_entries",
            data.buffered_entries_count() as f64,
            "The number of buffered archive entries.",
        )?;
        w.encode_gauge(
            "internet_identity_archive_config_entries_buffer_limit",
            config.entries_buffer_limit as f64,
            "Max number of buffered archive entries.",
        )?;
        w.encode_gauge(
            "internet_identity_archive_config_fetch_limit",
            config.entries_fetch_limit as f64,
            "Max number of entries fetched by the archive per call.",
        )?;
        w.encode_gauge(
            "internet_identity_archive_config_polling_interval_seconds",
            Duration::from_nanos(config.polling_interval_ns).as_secs() as f64,
            "Polling interval of the archive to fetch new entries from II.",
        )?;
    }
    state::persistent_state(|persistent_state| persistent_state_metrics(w, persistent_state))?;
    state::registration_rate_limit(|rate_limit_opt| {
        if let Some(ref rate_limit_state) = rate_limit_opt {
            w.encode_gauge(
                "internet_identity_register_rate_limit_current_tokens",
                rate_limit_state.tokens as f64,
                "The number of `register` calls that are still allowed in the current time window.",
            )?;
        }
        Ok::<(), std::io::Error>(())
    })?;

    event_metrics(w)?;
    Ok(())
}

fn event_metrics(w: &mut MetricsEncoder<Vec<u8>>) -> Result<(), std::io::Error> {
    let sess_sec_builder = w.gauge_vec(
        "internet_identity_prepare_delegation_session_seconds",
        "Cumulative authenticated session duration in seconds for the given front-end origin.",
    )?;
    pd_aggregation_metrics(PD_SESS_SEC, sess_sec_builder)?;
    let count_builder = w.gauge_vec(
        "internet_identity_prepare_delegation_count",
        "Number of authenticated sessions for the given front-end origin.",
    )?;
    pd_aggregation_metrics(PD_COUNT, count_builder)?;
    Ok(())
}

fn pd_aggregation_metrics(
    aggregation: &dyn Aggregation,
    mut metrics_builder: LabeledMetricsBuilder<Vec<u8>>,
) -> Result<(), std::io::Error> {
    for window in &[Day, Month] {
        let data = retrieve_aggregation(aggregation, window.clone(), Some(Ic0App));

        for (frontend_origin, sess_sec) in data.iter().take(10) {
            metrics_builder = metrics_builder.value(
                &[
                    ("dapp", frontend_origin),
                    ("window", window.label()),
                    ("ii_origin", maybe_domain_to_label(&Some(Ic0App))),
                ],
                *sess_sec as f64,
            )?;
        }
    }
    Ok(())
}

fn persistent_state_metrics(
    w: &mut MetricsEncoder<Vec<u8>>,
    persistent_state: &PersistentState,
) -> Result<(), std::io::Error> {
    let register_rate_limit_config = &persistent_state.registration_rate_limit;
    w.encode_gauge(
        "internet_identity_register_rate_limit_max_tokens",
        register_rate_limit_config.max_tokens as f64,
        "The maximum number of `register` calls that are allowed in any time window.",
    )?;
    w.encode_gauge(
        "internet_identity_register_rate_limit_time_per_tokens_seconds",
        Duration::from_nanos(register_rate_limit_config.time_per_token_ns).as_secs() as f64,
        "Min number of seconds between two register calls to not exceed the rate limit (sustained).",
    )?;

    let stats = &persistent_state.active_anchor_stats;
    if let Some(ref daily_active_anchor_stats) = stats.completed.daily_events {
        w.encode_gauge(
            "internet_identity_daily_active_anchors",
            daily_active_anchor_stats.counter as f64,
            "The number of unique active anchors in the last completed 24h collection window.",
        )?;
        w.encode_gauge(
            "internet_identity_daily_active_anchors_start_timestamp_seconds",
            Duration::from_nanos(daily_active_anchor_stats.start_timestamp).as_secs() as f64,
            "Timestamp of the last completed 24h collection window for unique active anchors.",
        )?;
    }
    if let Some(ref monthly_active_anchor_stats) = stats.completed.monthly_events {
        w.encode_gauge(
            "internet_identity_monthly_active_anchors",
            monthly_active_anchor_stats.counter as f64,
            "The number of unique active anchors in the last completed 30-day collection window.",
        )?;
        w.encode_gauge(
            "internet_identity_monthly_active_anchors_start_timestamp_seconds",
            Duration::from_nanos(monthly_active_anchor_stats.start_timestamp).as_secs() as f64,
            "Timestamp of the last completed 30-day collection window for unique active anchors.",
        )?;
    }
    let stats = &persistent_state.domain_active_anchor_stats;
    const BOTH_DOMAINS: &str = "both_ii_domains";

    let labels = ActivityMetricsLabels {
        daily_stats_label: "internet_identity_daily_active_anchors_by_domain",
        daily_stats_doc: "The number of unique active anchors in the last completed 24h collection window aggregated by II domains used.",
        monthly_stats_label: "internet_identity_monthly_active_anchors_by_domain",
        monthly_stats_doc: "The number of unique active anchors in the last completed 30-day collection window aggregated by II domains used.",
    };
    labelled_activity_metrics(w, stats, labels, |counter, encoder| {
        encoder
            .value(
                &[("domain", IC0_APP_DOMAIN)],
                counter.ic0_app_counter as f64,
            )?
            .value(
                &[("domain", INTERNETCOMPUTER_ORG_DOMAIN)],
                counter.internetcomputer_org_counter as f64,
            )?
            .value(
                &[("domain", BOTH_DOMAINS)],
                counter.both_ii_domains_counter as f64,
            )?;
        Ok(())
    })?;

    let stats = &persistent_state.active_authn_method_stats;
    let labels = ActivityMetricsLabels {
        daily_stats_label: "internet_identity_daily_active_authn_methods",
        daily_stats_doc: "The number of unique authentication methods used in the last completed 24h collection window on II domains.",
        monthly_stats_label: "internet_identity_monthly_active_authn_methods",
        monthly_stats_doc: "The number of unique authentication methods used in the last completed 30-day collection window on II domains.",
    };
    labelled_activity_metrics(w, stats, labels, |counter, encoder| {
        let mut labeled = encoder
            .value(
                &[("type", "webauthn_auth")],
                counter.webauthn_auth_counter as f64,
            )?
            .value(
                &[("type", "webauthn_recovery")],
                counter.webauthn_recovery_counter as f64,
            )?
            .value(
                &[("type", "recovery_phrase")],
                counter.recovery_phrase_counter as f64,
            )?
            .value(
                &[("type", "browser_storage_key")],
                counter.browser_storage_key_counter as f64,
            )?
            .value(&[("type", "other")], counter.other_counter as f64)?;
        if let Some(openid_counter) = &counter.openid_credential_auth_counter {
            for (issuer, count) in openid_counter {
                labeled =
                    labeled.value(&[("type", "openid"), ("issuer", issuer)], *count as f64)?;
            }
        }
        Ok(())
    })?;
    Ok(())
}

struct ActivityMetricsLabels<'a> {
    daily_stats_label: &'a str,
    daily_stats_doc: &'a str,
    monthly_stats_label: &'a str,
    monthly_stats_doc: &'a str,
}

fn labelled_activity_metrics<T: ActivityCounter>(
    w: &mut MetricsEncoder<Vec<u8>>,
    stats: &ActivityStats<T>,
    labels: ActivityMetricsLabels<'_>,
    encoding: impl Fn(&T, LabeledMetricsBuilder<Vec<u8>>) -> Result<(), std::io::Error>,
) -> Result<(), std::io::Error> {
    if let Some(ref daily_stats) = stats.completed.daily_events {
        let builder = w.gauge_vec(labels.daily_stats_label, labels.daily_stats_doc)?;
        encoding(daily_stats, builder)?;
    }
    if let Some(ref monthly_stats) = stats.completed.monthly_events {
        let builder = w.gauge_vec(labels.monthly_stats_label, labels.monthly_stats_doc)?;
        encoding(monthly_stats, builder)?;
    }
    Ok(())
}
