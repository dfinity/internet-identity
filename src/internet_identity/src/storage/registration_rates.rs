//! Module to track the rate at which new registrations are started over both a longer reference
//! period and a short period (to determine the current rate).
//! These rates are then used to determine whether a captcha needs to be solved or not.

use crate::state;
use ic_stable_structures::{Memory, MinHeap};
use internet_identity_interface::internet_identity::types::{CaptchaTrigger, Timestamp};
use std::time::Duration;

pub struct RegistrationRates<M: Memory> {
    /// Min heap of registration timestamps to calculate the reference registration rate.
    /// [prune_data] removes values that are older than now minus the reference rate interval whenever new data is added.
    reference_rate_data: MinHeap<Timestamp, M>,
    /// The interval between the moment of prunning and the oldest data point pruned for the `reference_rate_data`
    reference_last_pruned_interval_ns: Option<u64>,
    /// Min heap of registration timestamps to calculate the current registration rate.
    /// [prune_data] removes values that are older than now minus the current rate interval whenever new data is added.
    current_rate_data: MinHeap<Timestamp, M>,
    /// The interval between the moment of prunning and the oldest data point pruned for the `current_rate_data`
    current_last_pruned_interval_ns: Option<u64>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NormalizedRegistrationRates {
    pub reference_rate_per_second: f64,
    pub current_rate_per_second: f64,
    pub captcha_threshold_rate: f64,
    pub is_reference_data_insufficient: bool,
    pub is_current_data_insufficient: bool,
}

impl NormalizedRegistrationRates {
    pub fn captcha_required(&self) -> bool {
        // The data might be insuficcient when the config changes to a longer window.
        // In that case, we trigger the captcha to be on the safe side.
        self.is_current_data_insufficient
            || self.is_reference_data_insufficient
            || self.current_rate_per_second > self.captcha_threshold_rate
    }
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
            reference_last_pruned_interval_ns: None,
            current_rate_data,
            current_last_pruned_interval_ns: None,
        }
    }
    pub fn new_registration(&mut self) {
        self.prune_expired();
        // Return if no dynamic captcha
        if dynamic_captcha_config().is_none() {
            return;
        };

        let now = time();
        self.reference_rate_data.push(&now).expect("out of memory");
        self.current_rate_data.push(&now).expect("out of memory");
    }

    /// Calculates the registration rates for the current and reference intervals along with the threschold.
    ///
    /// The calculation assumes that the data has been pruned of old timestamps before the rates are calculated.
    ///
    /// Initially the window for the rate calculation was the difference between now and the oldest point.
    /// However, we have accumulated data the last 3 weeks.
    /// Therefore, we can assume that we have data for all the interval and use the interval as the window.
    /// Otherwise, the rate will be underestimated.
    pub fn registration_rates(&self) -> Option<NormalizedRegistrationRates> {
        let config = dynamic_captcha_config()?;

        let reference_rate_per_second = rate_per_second(
            self.reference_rate_data.len(),
            config.reference_rate_retention_ns,
        );
        let current_rate_per_second = rate_per_second(
            self.current_rate_data.len(),
            config.current_rate_retention_ns,
        );

        let captcha_threshold_rate = reference_rate_per_second * config.threshold_multiplier;
        let rates = NormalizedRegistrationRates {
            reference_rate_per_second,
            current_rate_per_second,
            captcha_threshold_rate,
            // The only moment we won't have last pruned interval is when we upgrade to the code with this data.
            // In that case, assume we have sufficient data because we won't change the config at the same time.
            is_reference_data_insufficient: self
                .reference_last_pruned_interval_ns
                .map(|interval| interval < config.reference_rate_retention_ns)
                .unwrap_or(false),
            is_current_data_insufficient: self
                .current_last_pruned_interval_ns
                .map(|interval| interval < config.current_rate_retention_ns)
                .unwrap_or(false),
        };

        Some(rates)
    }

    fn prune_expired(&mut self) {
        let Some(data_retention) = dynamic_captcha_config() else {
            return;
        };
        // If prunning doesn't return a a new interval, do not change the interval.
        self.reference_last_pruned_interval_ns = prune_data(
            &mut self.reference_rate_data,
            data_retention.reference_rate_retention_ns,
        )
        .or(self.reference_last_pruned_interval_ns);
        // If prunning doesn't return a a new interval, do not change the interval.
        self.current_last_pruned_interval_ns = prune_data(
            &mut self.current_rate_data,
            data_retention.current_rate_retention_ns,
        )
        .or(self.current_last_pruned_interval_ns);
    }
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

/// Prune data older than the `now - interval` and return the longest interval pruned
fn prune_data<M: Memory>(data: &mut MinHeap<Timestamp, M>, interval_ns: u64) -> Option<u64> {
    const MAX_TO_PRUNE: usize = 100;

    let now = time();
    let mut latest_pruned_interval = None;
    for _ in 0..MAX_TO_PRUNE {
        let Some(timestamp) = data.peek() else {
            break;
        };

        // The timestamps are sorted because the expiration is constant and time() is monotonic
        // -> we can stop pruning once we reach a not expired timestamp
        if timestamp > (now - interval_ns) {
            break;
        }

        latest_pruned_interval = Some(now - timestamp);
        data.pop();
    }

    latest_pruned_interval
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
        let mut registration_rates = setup(100, 1000);

        // no data -> 0 rates
        assert_eq!(
            registration_rates.registration_rates().unwrap(),
            NormalizedRegistrationRates {
                reference_rate_per_second: 0.0,
                current_rate_per_second: 0.0,
                captcha_threshold_rate: 0.0,
                is_current_data_insufficient: false,
                is_reference_data_insufficient: false,
            }
        );

        registration_rates.new_registration();

        assert_eq!(
            registration_rates.registration_rates().unwrap(),
            NormalizedRegistrationRates {
                reference_rate_per_second: 0.001, // 1 / 1000, as per config
                current_rate_per_second: 0.01,    // 1 / 100, as per config
                captcha_threshold_rate: 0.0012,   // 20% more than the reference rate, as per config
                is_current_data_insufficient: false,
                is_reference_data_insufficient: false,
            }
        );

        TIME.with_borrow_mut(|t| *t += Duration::from_secs(1).as_nanos() as u64);
        registration_rates.new_registration();

        // 2 registrations with 1 sec difference -> rate is 2 per second
        assert_eq!(
            registration_rates.registration_rates().unwrap(),
            NormalizedRegistrationRates {
                reference_rate_per_second: 0.002, // 2 / 1000, as per config
                current_rate_per_second: 0.02,    // 2 / 100, as per config
                captcha_threshold_rate: 0.0024,   // 20% more than the reference rate, as per config
                is_current_data_insufficient: false,
                is_reference_data_insufficient: false,
            }
        );
    }

    #[test]
    fn should_calculate_rates_many_data_points() {
        let mut registration_rates = setup(100, 1000);
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
                is_current_data_insufficient: false,
                is_reference_data_insufficient: false,
            }
        );
    }

    #[test]
    fn should_enable_captcha_if_current_rate_could_have_missing_data() {
        let mut registration_rates = setup(100, 1000);

        // Add data so that reference rate has enough data.
        for _ in 0..1000 {
            registration_rates.new_registration();
            TIME.with_borrow_mut(|t| *t += Duration::from_secs(1).as_nanos() as u64);
        }

        TIME.with_borrow_mut(|t| *t += Duration::from_secs(10).as_nanos() as u64);
        registration_rates.new_registration();

        // State of the current registrations heap: [10]

        TIME.with_borrow_mut(|t| *t += Duration::from_secs(60).as_nanos() as u64);
        registration_rates.new_registration();

        // State of the current registrations heap: [10, 70]

        TIME.with_borrow_mut(|t| *t += Duration::from_secs(10).as_nanos() as u64);
        registration_rates.new_registration();

        // State of the current registrations heap: [10, 70, 80]

        TIME.with_borrow_mut(|t| *t += Duration::from_secs(20).as_nanos() as u64);
        registration_rates.new_registration();

        // State of the current registrations heap: [10, 70, 80, 100]

        TIME.with_borrow_mut(|t| *t += Duration::from_secs(90).as_nanos() as u64);
        registration_rates.new_registration();

        // State of the current registrations heap: [100, 190]. 10, 70 and 80 got pruned.

        assert!(!registration_rates
            .registration_rates()
            .expect("reference rates is not defined")
            .captcha_required());

        // Change the config for current rate interval from 100 to 150
        // Timestamps [100, 190] are there. However, 70 and 80 should have been counted with a config of 150.
        // They were pruned because the config at that time was 100. Therefore, there is missing data when we go to 150.
        let new_current_interval_s = 150;
        state::persistent_state_mut(|ps| {
            ps.captcha_config = CaptchaConfig {
                max_unsolved_captchas: 500,
                captcha_trigger: CaptchaTrigger::Dynamic {
                    threshold_pct: 10,
                    current_rate_sampling_interval_s: new_current_interval_s,
                    reference_rate_sampling_interval_s: 100,
                },
            }
        });

        // Captcha should be disable because we could have insufficient data
        assert!(registration_rates
            .registration_rates()
            .expect("reference rates is not defined")
            .captcha_required());
    }

    #[test]
    fn should_enable_captcha_if_reference_rate_could_have_missing_data() {
        let mut registration_rates = setup(100, 100);

        // Add data so that reference rate has enough data.
        for _ in 0..100 {
            registration_rates.new_registration();
            TIME.with_borrow_mut(|t| *t += Duration::from_secs(1).as_nanos() as u64);
        }

        TIME.with_borrow_mut(|t| *t += Duration::from_secs(10).as_nanos() as u64);
        registration_rates.new_registration();

        // State of the current registrations heap: [10]

        TIME.with_borrow_mut(|t| *t += Duration::from_secs(60).as_nanos() as u64);
        registration_rates.new_registration();

        // State of the current registrations heap: [10, 70]

        TIME.with_borrow_mut(|t| *t += Duration::from_secs(10).as_nanos() as u64);
        registration_rates.new_registration();

        // State of the current registrations heap: [10, 70, 80]

        TIME.with_borrow_mut(|t| *t += Duration::from_secs(20).as_nanos() as u64);
        registration_rates.new_registration();

        // State of the current registrations heap: [10, 70, 80, 100]

        TIME.with_borrow_mut(|t| *t += Duration::from_secs(90).as_nanos() as u64);
        registration_rates.new_registration();

        // State of the current registrations heap: [100, 190]. 10, 70 and 80 got pruned.

        assert!(!registration_rates
            .registration_rates()
            .expect("reference rates is not defined")
            .captcha_required());

        // Change the config for current rate interval from 100 to 150
        // Timestamps [100, 190] are there. However, 70 and 80 should have been counted with a config of 150.
        // They were pruned because the config at that time was 100. Therefore, there is missing data when we go to 150.
        let new_reference_interval_s = 150;
        state::persistent_state_mut(|ps| {
            ps.captcha_config = CaptchaConfig {
                max_unsolved_captchas: 500,
                captcha_trigger: CaptchaTrigger::Dynamic {
                    threshold_pct: 10,
                    current_rate_sampling_interval_s: 100,
                    reference_rate_sampling_interval_s: new_reference_interval_s,
                },
            }
        });

        // Captcha should be disable because we could have insufficient data
        assert!(registration_rates
            .registration_rates()
            .expect("reference rates is not defined")
            .captcha_required());
    }

    #[test]
    fn should_only_use_recent_data_for_current_rate() {
        let mut registration_rates = setup(100, 1000);
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
                is_current_data_insufficient: false,
                is_reference_data_insufficient: false,
            }
        );
    }

    #[test]
    fn previous_config_should_not_affect_new_config_shorter_window() {
        let mut registration_rates = setup(100, 1000);
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
                is_current_data_insufficient: false,
                is_reference_data_insufficient: false,
            }
        );

        // Change the config where both reference and base rate have the same interval.
        let interval_s = 100;
        state::persistent_state_mut(|ps| {
            ps.captcha_config = CaptchaConfig {
                max_unsolved_captchas: 500,
                captcha_trigger: CaptchaTrigger::Dynamic {
                    threshold_pct: 10,
                    current_rate_sampling_interval_s: interval_s,
                    reference_rate_sampling_interval_s: interval_s,
                },
            }
        });

        // Set current rate to 2 registrations per seconds for 100 seconds
        for _ in 0..100 {
            registration_rates.new_registration();
            registration_rates.new_registration();
            TIME.with_borrow_mut(|t| *t += Duration::from_secs(1).as_nanos() as u64);
        }
        let current_rates = registration_rates.registration_rates().unwrap();
        // If the intervals are the same, the rates are also the same.
        assert_eq!(
            current_rates.reference_rate_per_second,
            current_rates.current_rate_per_second
        );
    }

    #[test]
    fn changing_longer_reference_window_enables_captcha() {
        let mut registration_rates = setup(100, 1000);
        // initialize reference rate with 1 registration / second
        for _ in 0..1000 {
            registration_rates.new_registration();
            TIME.with_borrow_mut(|t| *t += Duration::from_secs(1).as_nanos() as u64);
        }

        assert!(!registration_rates
            .registration_rates()
            .expect("reference rates is not defined")
            .captcha_required());

        let longer_reference_window = 2000;
        state::persistent_state_mut(|ps| {
            ps.captcha_config = CaptchaConfig {
                max_unsolved_captchas: 500,
                captcha_trigger: CaptchaTrigger::Dynamic {
                    threshold_pct: 10,
                    current_rate_sampling_interval_s: 100,
                    reference_rate_sampling_interval_s: longer_reference_window,
                },
            }
        });

        assert!(registration_rates
            .registration_rates()
            .expect("reference rates is not defined")
            .captcha_required());
    }

    #[test]
    fn changing_longer_current_window_enables_captcha() {
        let mut registration_rates = setup(100, 1000);
        // initialize reference rate with 1 registration / second
        for _ in 0..1000 {
            registration_rates.new_registration();
            TIME.with_borrow_mut(|t| *t += Duration::from_secs(1).as_nanos() as u64);
        }

        assert!(!registration_rates
            .registration_rates()
            .expect("reference rates is not defined")
            .captcha_required());

        let longer_current_window = 200;
        state::persistent_state_mut(|ps| {
            ps.captcha_config = CaptchaConfig {
                max_unsolved_captchas: 500,
                captcha_trigger: CaptchaTrigger::Dynamic {
                    threshold_pct: 10,
                    current_rate_sampling_interval_s: longer_current_window,
                    reference_rate_sampling_interval_s: 1000,
                },
            }
        });

        assert!(registration_rates
            .registration_rates()
            .expect("reference rates is not defined")
            .captcha_required());
    }

    #[test]
    fn should_prune_old_data() {
        let mut registration_rates = setup(100, 1000);

        // add 100 registrations at t0
        for _ in 0..100 {
            registration_rates.new_registration();
        }

        // shift time a bit, so we can calculate the rate
        TIME.with_borrow_mut(|t| *t += Duration::from_secs(1).as_nanos() as u64);

        assert_eq!(
            registration_rates.registration_rates().unwrap(),
            NormalizedRegistrationRates {
                current_rate_per_second: 1.0,
                reference_rate_per_second: 0.1,
                captcha_threshold_rate: 0.12,
                is_current_data_insufficient: false,
                is_reference_data_insufficient: false,
            }
        );

        // move time forward by reference rate time interval
        TIME.with_borrow_mut(|t| *t += Duration::from_secs(1000).as_nanos() as u64);

        // Adding a new data point prunes everything except the one data point added now
        registration_rates.new_registration();

        assert_eq!(
            registration_rates.registration_rates().unwrap(),
            NormalizedRegistrationRates {
                current_rate_per_second: 0.01,
                reference_rate_per_second: 0.001,
                captcha_threshold_rate: 0.0012,
                is_current_data_insufficient: false,
                is_reference_data_insufficient: false,
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

    fn setup(
        current_rate_sampling_interval_s: u64,
        reference_rate_sampling_interval_s: u64,
    ) -> RegistrationRates<VectorMemory> {
        reset_time();

        // setup config
        state::persistent_state_mut(|ps| {
            ps.captcha_config = CaptchaConfig {
                max_unsolved_captchas: 500,
                captcha_trigger: CaptchaTrigger::Dynamic {
                    threshold_pct: 20,
                    current_rate_sampling_interval_s,
                    reference_rate_sampling_interval_s,
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
