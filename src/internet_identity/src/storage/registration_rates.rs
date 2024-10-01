//! Module to track the rate at which new registrations are started over both a longer reference
//! period and a short period (to determine the current rate).
//! These rates are then used to determine whether a captcha needs to be solved or not.

use crate::state;
use ic_stable_structures::{Memory, MinHeap};
use internet_identity_interface::internet_identity::types::{CaptchaTrigger, Timestamp};
use std::time::Duration;

pub struct RegistrationRates<M: Memory> {
    /// Min heap of registration timestamps to calculate the reference registration rate.
    /// [prune_data] removes values that  are in the past whenever new data is added.
    reference_rate_data: MinHeap<Timestamp, M>,
    /// Min heap of registration timestamps to calculate the current registration rate.
    /// [prune_data] removes values that  are in the past whenever new data is added.
    current_rate_data: MinHeap<Timestamp, M>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NormalizedRegistrationRates {
    pub reference_rate_per_second: f64,
    pub current_rate_per_second: f64,
    pub captcha_threshold_rate: f64,
}

struct DynamicCaptchaConfig {
    reference_rate_retention_ns: u64,
    current_rate_retention_ns: u64,
    threshold_multiplier: f64,
}

impl<M: Memory> RegistrationRates<M> {
    pub fn new(
        reference_rate_data: MinHeap<Timestamp, M>,
        current_rate_data: MinHeap<Timestamp, M>,
    ) -> Self {
        Self {
            reference_rate_data,
            current_rate_data,
        }
    }
    pub fn new_registration(&mut self) {
        self.prune_expired();
        let Some(data_retention) = dynamic_captcha_config() else {
            return;
        };

        let now = time();
        self.reference_rate_data
            .push(&(now + data_retention.reference_rate_retention_ns))
            .expect("out of memory");
        self.current_rate_data
            .push(&(now + data_retention.current_rate_retention_ns))
            .expect("out of memory");
    }

    pub fn registration_rates(&self) -> Option<NormalizedRegistrationRates> {
        let config = dynamic_captcha_config()?;
        let now = time();

        let reference_rate_per_second = calculate_registration_rate(
            now,
            &self.reference_rate_data,
            config.reference_rate_retention_ns,
        );
        let current_rate_per_second = calculate_registration_rate(
            now,
            &self.current_rate_data,
            config.current_rate_retention_ns,
        );
        let captcha_threshold_rate = reference_rate_per_second * config.threshold_multiplier;
        let rates = NormalizedRegistrationRates {
            reference_rate_per_second,
            current_rate_per_second,
            captcha_threshold_rate,
        };
        Some(rates)
    }

    fn prune_expired(&mut self) {
        prune_data(&mut self.reference_rate_data);
        prune_data(&mut self.current_rate_data);
    }
}

/// Calculates the rate per second of registrations taking into account for how long data has
/// already been collected. Adjusting the window to the actual data collected is important because
/// * rates are underestimated by fixed window calculations
/// * the reference registration rate window is generally longer than the current rate window
/// 
/// => this means that the captcha would be triggered prematurely during the period where data has
/// not been collected for the full reference registration rate data retention window.
///
/// Example:
/// * `data_retention_ns` is 3 weeks
/// * there are currently 3 data points: `[1727768623000000000, 1727855023000000000, 1727941423000000000]`
///   (these are 24h apart each)
///
/// If the rate was calculated over a 3-week time window, this would be
/// 3 registrations / 1814400 seconds = 0.000001653439153 registrations / second
///
/// However, because the data is not actually spanning 3 weeks, this underestimates the actual rate.
/// Taking into account that the data is only spanning 3 days we get the following:
/// 3 registrations / 259200 seconds = 0.00001157407407 registrations / second
fn calculate_registration_rate<M: Memory>(
    now: u64,
    data: &MinHeap<Timestamp, M>,
    data_retention_ns: u64,
) -> f64 {
    data
        // get the oldest expiration timestamp
        .peek()
        // calculate the registration timestamp from expiration
        .map(|ts| ts - data_retention_ns)
        // calculate the time window length with respect to the current time
        .map(|ts| now - ts)
        // the value _could_ be 0 if the oldest timestamp was added in the same execution round
        // -> filter to avoid division by 0
        .filter(|val| *val != 0)
        // use the value to calculate the rate per second
        .map(|val| rate_per_second(data.len(), val))
        // if we don't have data, the rate is 0
        .unwrap_or(0.0)
}

fn rate_per_second(count: u64, duration_ns: u64) -> f64 {
    count as f64 / Duration::from_nanos(duration_ns).as_secs_f64()
}

fn dynamic_captcha_config() -> Option<DynamicCaptchaConfig> {
    let trigger = state::persistent_state(|ps| ps.captcha_config.captcha_trigger.clone());
    match trigger {
        CaptchaTrigger::Static(_) => None,
        CaptchaTrigger::Dynamic {
            current_rate_sampling_interval_s,
            reference_rate_sampling_interval_s,
            threshold_pct,
        } => Some(DynamicCaptchaConfig {
            reference_rate_retention_ns: Duration::from_secs(reference_rate_sampling_interval_s)
                .as_nanos() as u64,
            current_rate_retention_ns: Duration::from_secs(current_rate_sampling_interval_s)
                .as_nanos() as u64,
            threshold_multiplier: 1.0 + (threshold_pct as f64 / 100.0),
        }),
    }
}

fn prune_data<M: Memory>(data: &mut MinHeap<Timestamp, M>) {
    const MAX_TO_PRUNE: usize = 100;

    let now = time();
    for _ in 0..MAX_TO_PRUNE {
        let Some(timestamp) = data.peek() else {
            break;
        };

        // The timestamps are sorted because the expiration is constant and time() is monotonic
        // -> we can stop pruning once we reach a not expired timestamp
        if timestamp > now {
            break;
        }
        data.pop();
    }
}

#[cfg(not(test))]
fn time() -> u64 {
    ic_cdk::api::time()
}

#[cfg(test)]
fn time() -> u64 {
    test::TIME.with_borrow(|t| *t)
}

#[cfg(test)]
mod test {
    use crate::state;
    use crate::storage::registration_rates::{NormalizedRegistrationRates, RegistrationRates};
    use ic_stable_structures::{StableMinHeap, VectorMemory};
    use internet_identity_interface::internet_identity::types::{
        CaptchaConfig, CaptchaTrigger, StaticCaptchaTrigger, Timestamp,
    };
    use std::cell::RefCell;
    use std::time::Duration;

    const TEST_DEFAULT_TIME: u64 = 1_727_682_423_957_507_272;

    thread_local! {
        pub static TIME: RefCell<u64> = const { RefCell::new(0) };
    }

    #[test]
    fn should_calculate_rates() {
        let mut registration_rates = setup();

        // no data -> 0 rates
        assert_eq!(
            registration_rates.registration_rates().unwrap(),
            NormalizedRegistrationRates {
                reference_rate_per_second: 0.0,
                current_rate_per_second: 0.0,
                captcha_threshold_rate: 0.0,
            }
        );

        registration_rates.new_registration();

        // 1 data point -> still 0 rates
        assert_eq!(
            registration_rates.registration_rates().unwrap(),
            NormalizedRegistrationRates {
                reference_rate_per_second: 0.0,
                current_rate_per_second: 0.0,
                captcha_threshold_rate: 0.0,
            }
        );

        TIME.with_borrow_mut(|t| *t += Duration::from_secs(1).as_nanos() as u64);
        registration_rates.new_registration();

        // 2 registrations with 1 sec difference -> rate is 2 per second
        assert_eq!(
            registration_rates.registration_rates().unwrap(),
            NormalizedRegistrationRates {
                reference_rate_per_second: 2.0,
                current_rate_per_second: 2.0,
                captcha_threshold_rate: 2.4, // 20% more than the reference rate, as per config
            }
        );
    }

    #[test]
    fn should_calculate_rates_many_data_points() {
        let mut registration_rates = setup();
        for _ in 0..1000 {
            registration_rates.new_registration();
            TIME.with_borrow_mut(|t| *t += Duration::from_secs(1).as_nanos() as u64);
        }
        // 1 registration per second over 1000 seconds difference -> rate is 1 per second
        assert_eq!(
            registration_rates.registration_rates().unwrap(),
            NormalizedRegistrationRates {
                reference_rate_per_second: 1.0,
                current_rate_per_second: 1.0,
                captcha_threshold_rate: 1.2, // 20% more than the reference rate, as per config
            }
        );
    }

    #[test]
    fn should_only_use_recent_data_for_current_rate() {
        let mut registration_rates = setup();
        // initialize reference rate with 1 registration / second
        for _ in 0..1000 {
            registration_rates.new_registration();
            TIME.with_borrow_mut(|t| *t += Duration::from_secs(1).as_nanos() as u64);
        }

        // raise current rate to 3 registrations / second
        for _ in 0..100 {
            registration_rates.new_registration();
            registration_rates.new_registration();
            registration_rates.new_registration();
            TIME.with_borrow_mut(|t| *t += Duration::from_secs(1).as_nanos() as u64);
        }

        assert_eq!(
            registration_rates.registration_rates().unwrap(),
            NormalizedRegistrationRates {
                current_rate_per_second: 3.0,
                reference_rate_per_second: 1.2, // increases too, but more slowly
                captcha_threshold_rate: 1.44,   // reference rate * 1.2 (as per config)
            }
        );
    }

    #[test]
    fn should_prune_old_data() {
        let mut registration_rates = setup();

        // add 100 registrations at t0
        for _ in 0..100 {
            registration_rates.new_registration();
        }

        // shift time a bit, so we can calculate the rate
        TIME.with_borrow_mut(|t| *t += Duration::from_secs(1).as_nanos() as u64);

        assert_eq!(
            registration_rates.registration_rates().unwrap(),
            NormalizedRegistrationRates {
                current_rate_per_second: 100.0,
                reference_rate_per_second: 100.0,
                captcha_threshold_rate: 120.0,
            }
        );

        // move time forward by reference rate time interval
        TIME.with_borrow_mut(|t| *t += Duration::from_secs(1000).as_nanos() as u64);

        // Adding a new data point prunes everything except the one data point added now
        // -> there are at least 2 data points required to calculate a rate
        // -> rates are 0.0
        registration_rates.new_registration();

        assert_eq!(
            registration_rates.registration_rates().unwrap(),
            NormalizedRegistrationRates {
                current_rate_per_second: 0.0,
                reference_rate_per_second: 0.0,
                captcha_threshold_rate: 0.0,
            }
        );
    }

    #[test]
    fn should_be_disabled_on_static_captcha() {
        reset_time();

        state::persistent_state_mut(|ps| {
            ps.captcha_config = CaptchaConfig {
                max_unsolved_captchas: 500,
                captcha_trigger: CaptchaTrigger::Static(StaticCaptchaTrigger::CaptchaDisabled),
            }
        });

        let reference_data_memory = VectorMemory::default();
        let current_data_memory = VectorMemory::default();
        let reference_rate_data = StableMinHeap::init(reference_data_memory.clone()).unwrap();
        let current_rate_data = StableMinHeap::init(current_data_memory.clone()).unwrap();
        let mut registration_rates = RegistrationRates::new(reference_rate_data, current_rate_data);

        registration_rates.new_registration();

        assert_eq!(
            StableMinHeap::<Timestamp, VectorMemory>::init(reference_data_memory)
                .unwrap()
                .len(),
            0
        );
        assert_eq!(
            StableMinHeap::<Timestamp, VectorMemory>::init(current_data_memory)
                .unwrap()
                .len(),
            0
        );
        assert!(registration_rates.registration_rates().is_none());
    }

    fn setup() -> RegistrationRates<VectorMemory> {
        reset_time();

        // setup config
        state::persistent_state_mut(|ps| {
            ps.captcha_config = CaptchaConfig {
                max_unsolved_captchas: 500,
                captcha_trigger: CaptchaTrigger::Dynamic {
                    threshold_pct: 20,
                    current_rate_sampling_interval_s: 100,
                    reference_rate_sampling_interval_s: 1000,
                },
            }
        });

        // setup fake storage
        let reference_rate_data = StableMinHeap::init(VectorMemory::default()).unwrap();
        let current_rate_data = StableMinHeap::init(VectorMemory::default()).unwrap();
        RegistrationRates::new(reference_rate_data, current_rate_data)
    }

    fn reset_time() {
        TIME.with_borrow_mut(|t| *t = TEST_DEFAULT_TIME);
    }
}
