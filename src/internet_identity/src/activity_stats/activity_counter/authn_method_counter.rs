use crate::activity_stats::activity_counter::ActivityCounter;
use crate::storage::anchor::Device;
use candid::{CandidType, Deserialize};
use internet_identity_interface::internet_identity::types::{KeyType, Purpose, Timestamp};

/// Counter for authentications with different methods.
///
/// Only authentications with keys that are bound to an Internet Identity domain are counted.
#[derive(Clone, CandidType, Deserialize, Eq, PartialEq, Debug)]
pub struct AuthnMethodCounter {
    pub start_timestamp: Timestamp,
    /// Number of authentications with a WebAuthn key with purpose `authentication`.
    pub webauthn_auth_counter: u64,
    /// Number of authentications with a WebAuthn key with purpose `recovery`.
    pub webauthn_recovery_counter: u64,
    /// Number of authentications with a recovery phrase.
    pub recovery_phrase_counter: u64,
    /// Number of authentications with a browser storage key.
    pub browser_storage_key_counter: u64,
    /// Number of authentications with a key not fitting any of the above criteria.
    pub other_counter: u64,
}

impl ActivityCounter for AuthnMethodCounter {
    type CountingContext<'a> = &'a Device;

    fn new(start_timestamp: Timestamp) -> Self {
        Self {
            start_timestamp,
            webauthn_auth_counter: 0,
            webauthn_recovery_counter: 0,
            recovery_phrase_counter: 0,
            browser_storage_key_counter: 0,
            other_counter: 0,
        }
    }

    fn start_timestamp(&self) -> Timestamp {
        self.start_timestamp
    }

    /// Increases the counter for the corresponding authn_method of the provided devce, if there was
    /// no activity before.
    ///
    /// Only called if `current_domain` corresponds to an II domain.
    fn count_event(&mut self, device: &Self::CountingContext<'_>) {
        // only count authentications on devices that have not already been counted
        // i.e. the last usage timestamp is before the start of the window
        if let Some(timestamp) = device.last_usage_timestamp {
            if timestamp >= self.start_timestamp {
                return;
            }
        }

        match device.key_type {
            KeyType::SeedPhrase => self.recovery_phrase_counter += 1,
            KeyType::BrowserStorageKey => self.browser_storage_key_counter += 1,
            KeyType::Platform | KeyType::CrossPlatform | KeyType::Unknown => {
                if device.credential_id.is_none() {
                    // if the credential id is not set, it is not WebAuthn
                    self.other_counter += 1;
                    return;
                }

                match device.purpose {
                    Purpose::Authentication => self.webauthn_auth_counter += 1,
                    Purpose::Recovery => self.webauthn_recovery_counter += 1,
                }
            }
        }
    }
}
