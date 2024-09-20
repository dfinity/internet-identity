use crate::MINUTE_NS;
use candid::Principal;
use ic_cdk::api::time;
use internet_identity_interface::internet_identity::types::{IdentityNumber, PublicKey, Timestamp};
use std::collections::{HashMap, VecDeque};

// Expiration for temp keys, the same as the front-end delegation expiry
const TEMP_KEY_EXPIRATION_NS: u64 = 10 * MINUTE_NS;

/// A temp key identifier
#[derive(Debug, Clone, Eq, PartialOrd, PartialEq, Hash)]
pub enum TempKeyId {
    /// A temp key that is tied to an existing identity and authn_method.
    /// We need to track the authn_method because the associated temp_key
    /// must become invalid if the authn_method is removed from the identity.
    IdentityAuthnMethod {
        identity_number: IdentityNumber,
        authn_method_pubkey: PublicKey,
    },
}

impl TempKeyId {
    pub fn from_identity_authn_method(
        identity_number: IdentityNumber,
        authn_method_pubkey: PublicKey,
    ) -> Self {
        TempKeyId::IdentityAuthnMethod {
            identity_number,
            authn_method_pubkey,
        }
    }
}

#[derive(Default, Debug)]
pub struct TempKeys {
    /// A map of "temporary keys" attached to specific anchor and device combination.
    /// A temporary key can be used in lieu of the device but has a short expiration time.
    /// These keys are used as a workaround for WebAuthn needing two users interactions:
    /// one for "create" and one for "sign". So instead we only "create"
    /// and instead authenticate the user with a temporary key for their first visit.
    ///
    /// Note: we link the temporary keys to a device so that we can make sure the temporary key is dropped
    /// if the device itself is removed. This ensures the temporary key is no more powerful than the device,
    /// i.e. if the user decides to remove the device right after registration, then the temporary key
    /// cannot be used to authenticate (similarly to how the browser session key pair cannot be used to
    /// authenticate if the actual delegated WebAuthn device is removed).
    ///
    /// In addition, the temporary key is also linked to an anchor so that even if the same device is
    /// added to multiple anchors, it can only be used in the context of the anchor it was created for.
    ///
    /// Since temp keys can only be added during registration, the max number of temp keys is
    /// bounded by the registration rate limit.
    temp_keys: HashMap<TempKeyId, TempKey>,

    /// Deque to efficiently prune expired temp keys
    expirations: VecDeque<TempKeyExpiration>,
}

impl TempKeys {
    pub fn add_temp_key(&mut self, key_id: TempKeyId, temp_key: Principal) {
        let tmp_key = TempKey {
            principal: temp_key,
            expiration: time() + TEMP_KEY_EXPIRATION_NS,
        };

        self.expirations.push_back(TempKeyExpiration {
            key_id: key_id.clone(),
            expiration: tmp_key.expiration,
        });
        self.temp_keys.insert(key_id, tmp_key);
    }

    /// Removes the temporary key for the given device if it exists and is linked to the provided anchor.
    pub fn remove_temp_key(&mut self, key_id: &TempKeyId) {
        // we can skip the removal from expirations because there it will be removed
        // during amortized clean-up operations
        self.temp_keys.remove(key_id);
    }

    /// Checks that the temporary key is valid for the given device and anchor.
    ///
    /// Requires a mutable reference because it does amortized clean-up of expired temp keys.
    pub fn check_temp_key(&mut self, caller: &Principal, key_id: &TempKeyId) -> Result<(), ()> {
        self.prune_expired_keys();

        let Some(temp_key) = self.temp_keys.get(key_id) else {
            return Err(());
        };
        if temp_key.expiration < time() {
            return Err(());
        }
        if &temp_key.principal != caller {
            return Err(());
        }
        Ok(())
    }

    fn prune_expired_keys(&mut self) {
        const MAX_TO_PRUNE: usize = 100;

        let now = time();
        for _ in 0..MAX_TO_PRUNE {
            let Some(expiration) = self.expirations.front() else {
                break;
            };

            // The expirations are sorted by expiration time because the expiration is constant
            // (10 minutes) and time() is monotonous
            // -> we can stop pruning once we reach a temp key that is not expired
            if expiration.expiration > now {
                break;
            }
            self.temp_keys.remove(&expiration.key_id);
            self.expirations.pop_front();
        }
    }

    pub fn num_temp_keys(&self) -> usize {
        self.temp_keys.len()
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct TempKey {
    /// The temp key principal
    principal: Principal,
    /// The expiration timestamp of the temp key
    expiration: Timestamp,
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct TempKeyExpiration {
    /// Key with which to find the temp key in the `temp_keys` map
    pub key_id: TempKeyId,
    /// The expiration timestamp of the temp key
    pub expiration: Timestamp,
}
