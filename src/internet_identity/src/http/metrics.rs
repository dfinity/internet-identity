use crate::activity_stats::activity_counter::ActivityCounter;
use crate::activity_stats::ActivityStats;
use crate::archive::ArchiveState;
use crate::state::PersistentState;
use crate::{state, IC0_APP_DOMAIN, INTERNETCOMPUTER_ORG_DOMAIN};
use ic_cdk::api::stable::stable64_size;
use ic_cdk::api::time;
use ic_metrics_encoder::{LabeledMetricsBuilder, MetricsEncoder};
use std::time::Duration;

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
        )
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
        stable64_size() as f64,
        "Number of stable memory pages used by this canister.",
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
            data.entries_buffer.len() as f64,
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
    Ok(())
}

fn persistent_state_metrics(
    w: &mut MetricsEncoder<Vec<u8>>,
    persistent_state: &PersistentState,
) -> Result<(), std::io::Error> {
    if let Some(ref register_rate_limit_config) = persistent_state.registration_rate_limit {
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
    }
    if let Some(ref stats) = persistent_state.active_anchor_stats {
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
    };
    if let Some(ref stats) = persistent_state.domain_active_anchor_stats {
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
    };
    if let Some(delegation_origins_limit) = persistent_state.max_num_latest_delegation_origins {
        w.encode_gauge(
            "internet_identity_max_num_latest_delegation_origins",
            delegation_origins_limit as f64,
            "The maximum number of latest delegation origins that were used with II bound devices.",
        )?;
    }
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
