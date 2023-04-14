use crate::MINUTE_NS;
use candid::Principal;
use ic_cdk::api::time;
use internet_identity_interface::internet_identity::types::{AnchorNumber, DeviceKey, Timestamp};
use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap};

// Expiration for temp keys, the same as the front-end delegation expiry
const TEMP_KEY_EXPIRATION_NS: u64 = 10 * MINUTE_NS;

#[derive(Default, Debug)]
pub struct TempKeys {
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
    temp_keys: HashMap<DeviceKey, TempKey>,

    /// Heap to efficiently prune expired temp keys
    expirations: BinaryHeap<TempKeyExpiration>,
}

impl TempKeys {
    pub fn add_temp_key(
        &mut self,
        device_key: &DeviceKey,
        anchor: AnchorNumber,
        temp_key: Principal,
    ) {
        let tmp_key = TempKey {
            principal: temp_key,
            anchor,
            expiration: time() + TEMP_KEY_EXPIRATION_NS,
        };

        self.expirations.push(TempKeyExpiration {
            device_key: device_key.clone(),
            expiration: tmp_key.expiration,
        });
        self.temp_keys.insert(device_key.clone(), tmp_key);
    }

    /// Removes the temporary key for the given device if it exists and is linked to the provided anchor.
    pub fn remove_temp_key(&mut self, anchor: AnchorNumber, device_key: &DeviceKey) {
        // we can skip the removal from expirations because there it will be removed
        // during amortized clean-up operations
        if let Some(temp_key) = self.temp_keys.get(device_key) {
            if temp_key.anchor == anchor {
                // Only remove temp key if the anchor matches
                self.temp_keys.remove(device_key);
            }
        }
    }

    pub fn prune_expired_keys(&mut self) {
        const MAX_TO_PRUNE: usize = 100;

        let now = time();
        for _ in 0..MAX_TO_PRUNE {
            let Some(expiration) = self.expirations.peek() else {
                break;
            };

            if expiration.expiration > now {
                break;
            }
            self.temp_keys.remove(&expiration.device_key);
            self.expirations.pop();
        }
    }

    pub fn check_temp_key(
        &self,
        caller: &Principal,
        device_key: &DeviceKey,
        anchor: AnchorNumber,
    ) -> Result<(), TempKeyError> {
        let Some(temp_key) = self.temp_keys.get(device_key) else {
            return Err(TempKeyError::NotFound);
        };
        if temp_key.expiration < time() {
            return Err(TempKeyError::Expired);
        }
        if temp_key.anchor != anchor {
            return Err(TempKeyError::AnchorMismatch);
        }
        if &temp_key.principal != caller {
            return Err(TempKeyError::Invalid);
        }
        Ok(())
    }

    pub fn num_temp_keys(&self) -> usize {
        self.temp_keys.len()
    }
}

pub enum TempKeyError {
    /// There is no temp key for the current device
    NotFound,
    /// The temporary key does not match the caller
    Invalid,
    /// The temporary key is not linked to the provided anchor
    AnchorMismatch,
    /// The temporary key is expired
    Expired,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TempKey {
    /// The temp key principal
    principal: Principal,
    /// The anchor the temporary key is linked to
    anchor: AnchorNumber,
    /// The expiration timestamp of the temp key
    expiration: Timestamp,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TempKeyExpiration {
    /// The device key the temp key is linked to
    pub device_key: DeviceKey,
    /// The expiration timestamp of the temp key
    pub expiration: Timestamp,
}

impl Ord for TempKeyExpiration {
    fn cmp(&self, other: &Self) -> Ordering {
        // reverse ordering because we want to have the smallest timestamp first
        other.expiration.cmp(&self.expiration)
    }
}

impl PartialOrd<Self> for TempKeyExpiration {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
