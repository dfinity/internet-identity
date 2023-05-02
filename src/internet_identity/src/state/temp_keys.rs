use crate::{state, MINUTE_NS};
use candid::Principal;
use ic_cdk::api::time;
use internet_identity_interface::internet_identity::types::{AnchorNumber, DeviceKey, Timestamp};
use std::collections::{HashMap, VecDeque};

// Expiration for temp keys, the same as the front-end delegation expiry
const TEMP_KEY_EXPIRATION_NS: u64 = 10 * MINUTE_NS;

/// Temporary keys are used as a session mechanism for the onboarding of new anchors.
/// Temporary keys are created like regular session keys (with a delegation), but work without
/// a signed delegation (since the anchor is new, there is no device to sign the delegation).
///
/// There are two kinds of temporary keys:
/// - onboarding keys:   these keys are used to track a session before the corresponding anchor has been created
/// - device bound keys: these keys are attached to a newly created anchor and device. They are used
///                      to avoid having users interact with a newly created WebAuthn device immediately.
///                      See below for more details.
#[derive(Default, Debug)]
pub struct TempKeys {
    /// A map of temporary keys that solved a captcha and can be used for registration (without
    /// submitting another captcha solution). The principals are mapped to their expiration time\
    /// (which is the only metadata we keep about onboarding keys).
    onboarding_keys: HashMap<Principal, Timestamp>,

    /// A map of "temporary keys" attached to devices (and a specific anchor). A temporary key can be used in lieu
    /// of the device but has a short expiration time. These keys are used as a workaround for WebAuthn
    /// needing two users interactions: one for "create" and one for "sign". So instead we only "create"
    /// and instead authenticate the user with a temporary key for their first visit.
    ///
    /// Note: we link the temporary keys to a device so that we can make sure the temporary key is dropped
    /// if the device itself is removed. This ensures the temporary key is no more powerful than the device,
    /// i.e. if the user decides to remove the device right after registration, then the the temporary key
    /// cannot be used to authenticate (similarly to how the browser session key pair cannot be used to
    /// authenticate if the actual delegated WebAuthn device is removed).
    ///
    /// In addition, the temporary key is also linked to an anchor so that even if the same device is
    /// added to multiple anchors, it can only be used in the context of the anchor it was created for.
    ///
    /// Since temp keys can only be added during registration, the the max number of temp keys is
    /// bounded by the registration rate limit.
    device_bound_keys: HashMap<DeviceKey, DeviceBoundTempKey>,

    /// Deque to efficiently prune expired temp keys
    expirations: VecDeque<TempKeyInfo>,
}

impl TempKeys {
    pub fn add_onboarding_temp_key(&mut self, temp_key: Principal) -> Result<(), ()> {
        self.prune_expired_keys();

        // tie the max number of onboarding keys to the registration rate limit
        if self.onboarding_keys.len() as u64
            > state::persistent_state(|ps| {
                ps.registration_rate_limit
                    .as_ref()
                    .map(|rate_limit| rate_limit.max_tokens)
                    .unwrap_or(20_000)
            })
        {
            return Err(());
        }

        let expiration = time() + TEMP_KEY_EXPIRATION_NS;
        self.expirations.push_back(TempKeyInfo {
            principal: temp_key,
            expiration,
        });
        self.onboarding_keys.insert(temp_key, expiration);
        Ok(())
    }

    pub fn add_device_bound_temp_key(
        &mut self,
        device_key: &DeviceKey,
        anchor: AnchorNumber,
        temp_key: Principal,
    ) {
        // Remove the temp key from onboarding keys if it exists.
        // It is ok for the temp key to not exist as long as we still support
        // providing the captcha solution on `register`. Afterwards it will
        // become an error.
        self.onboarding_keys.remove(&temp_key);

        let tmp_key = DeviceBoundTempKey {
            anchor,
            temp_key: TempKeyInfo {
                principal: temp_key,
                expiration: time() + TEMP_KEY_EXPIRATION_NS,
            },
        };

        self.expirations.push_back(tmp_key.temp_key.clone());
        self.device_bound_keys.insert(device_key.clone(), tmp_key);
    }

    /// Removes the temporary key for the given device if it exists and is linked to the provided anchor.
    pub fn remove_temp_key(&mut self, anchor: AnchorNumber, device_key: &DeviceKey) {
        // we can skip the removal from expirations because there it will be removed
        // during amortized clean-up operations
        if let Some(temp_key) = self.device_bound_keys.get(device_key) {
            if temp_key.anchor == anchor {
                // Only remove temp key if the anchor matches
                self.device_bound_keys.remove(device_key);
            }
        }
    }

    /// Checks that the temporary key is a valid onboarding key (i.e. has solved the captcha).
    ///
    /// Requires a mutable reference because it does amortized clean-up of expired temp keys.
    pub fn check_onboarding_temp_key(&mut self, caller: &Principal) -> Result<(), ()> {
        self.prune_expired_keys();

        let Some(expiration) = self.onboarding_keys.get(caller) else {
            return Err(());
        };
        if expiration < &time() {
            return Err(());
        }
        Ok(())
    }

    /// Checks that the temporary key is valid for the given device and anchor.
    ///
    /// Requires a mutable reference because it does amortized clean-up of expired temp keys.
    pub fn check_device_bound_temp_key(
        &mut self,
        caller: &Principal,
        device_key: &DeviceKey,
        anchor: AnchorNumber,
    ) -> Result<(), ()> {
        self.prune_expired_keys();

        let Some(device_bound_key) = self.device_bound_keys.get(device_key) else {
            return Err(());
        };
        if device_bound_key.anchor != anchor {
            return Err(());
        }
        if device_bound_key.temp_key.expiration < time() {
            return Err(());
        }
        if &device_bound_key.temp_key.principal != caller {
            return Err(());
        }
        Ok(())
    }

    fn prune_expired_keys(&mut self) {
        const MAX_TO_PRUNE: usize = 100;

        let now = time();
        for _ in 0..MAX_TO_PRUNE {
            let Some(temp_key) = self.expirations.front() else {
                break;
            };

            // The expirations are sorted by expiration time because the expiration is constant
            // (10 minutes) and time() is monotonous
            // -> we can stop pruning once we reach a temp key that is not expired
            if temp_key.expiration > now {
                break;
            }

            self.expirations.pop_front();
        }
    }

    pub fn num_temp_keys(&self) -> usize {
        self.device_bound_keys.len()
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DeviceBoundTempKey {
    /// The anchor the temporary key is linked to
    anchor: AnchorNumber,
    /// The temp key principal & expiration
    temp_key: TempKeyInfo,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TempKeyInfo {
    /// The principal of the temp key
    principal: Principal,
    /// The expiration timestamp of the temp key
    expiration: Timestamp,
}
