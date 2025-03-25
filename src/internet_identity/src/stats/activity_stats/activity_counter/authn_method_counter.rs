use crate::stats::activity_stats::activity_counter::ActivityCounter;
use crate::storage::anchor::Anchor;
use candid::{CandidType, Deserialize};
use internet_identity_interface::internet_identity::types::{
    AuthorizationKey, KeyType, Purpose, Timestamp,
};
use std::collections::HashMap;

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
    /// Number of authentications with an OpenID credential per issuer
    pub openid_credential_auth_counter: Option<HashMap<String, u64>>,
    /// Number of authentications with a key not fitting any of the above criteria.
    pub other_counter: u64,
}

impl ActivityCounter for AuthnMethodCounter {
    type CountingContext<'a> = (&'a Anchor, &'a AuthorizationKey);

    fn new(start_timestamp: Timestamp) -> Self {
        Self {
            start_timestamp,
            webauthn_auth_counter: 0,
            webauthn_recovery_counter: 0,
            recovery_phrase_counter: 0,
            browser_storage_key_counter: 0,
            openid_credential_auth_counter: Some(HashMap::new()),
            other_counter: 0,
        }
    }

    fn start_timestamp(&self) -> Timestamp {
        self.start_timestamp
    }

    /// Increases the counter for the corresponding authn_method of the provided authorization,
    /// if there was no activity before.
    ///
    /// Counter only increases for devices where the domain is an II domain.
    fn count_event(&mut self, (anchor, authorization_key): &Self::CountingContext<'_>) {
        let last_usage_timestamp = match authorization_key {
            AuthorizationKey::DeviceKey(device_key) => anchor
                .device(device_key)
                .and_then(|d| d.last_usage_timestamp),
            AuthorizationKey::OpenIdCredentialKey(openid_credential_key) => anchor
                .openid_credential(openid_credential_key)
                .and_then(|c| c.last_usage_timestamp),
        };

        // only count authentications on devices that have not already been counted
        // i.e. the last usage timestamp is before the start of the window
        if let Some(timestamp) = last_usage_timestamp {
            if timestamp >= self.start_timestamp {
                return;
            }
        }

        match authorization_key {
            AuthorizationKey::DeviceKey(device_key) => {
                if let Some(device) = anchor.device(device_key) {
                    // Only increase counter if device has an II domain
                    if device.ii_domain().is_some() {
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
            }
            AuthorizationKey::OpenIdCredentialKey((iss, _)) => {
                if let Some(map) = &mut self.openid_credential_auth_counter {
                    map.entry(iss.clone())
                        .and_modify(|count| *count += 1)
                        .or_insert(1);
                }
            }
        }
    }
}
