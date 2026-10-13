use crate::storage::storable::account_number::StorableAccountNumber;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use internet_identity_interface::internet_identity::types::Timestamp;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

/// What the identity holds for one app. Key 1 is retired and never reused: rows that
/// carry it decode with it skipped.
#[derive(Encode, Decode, Default, Clone, Ord, Eq, PartialEq, PartialOrd, Debug)]
#[cbor(map)]
pub struct AnchorApplicationConfig {
    #[n(0)]
    pub default_account_number: Option<StorableAccountNumber>, // None is the unreserved synthetic account
    #[n(2)]
    pub last_notified_at_ns: Option<Timestamp>, // None if the app never reached a browser
    #[n(3)]
    pub notifications: Option<PermissionDecision>, // None if the identity never decided
}

/// The identity's answer to an app asking for a permission. Switching the permission keeps
/// the decision, so a permission that was allowed and then denied is remembered as denied.
/// Only forgetting the app clears it.
#[derive(Encode, Decode, Clone, Copy, Ord, Eq, PartialEq, PartialOrd, Debug)]
#[cbor(map)]
pub struct PermissionDecision {
    #[n(0)]
    pub allowed: bool,
    #[n(1)]
    pub changed_at_ns: Timestamp,
}

impl AnchorApplicationConfig {
    pub fn notifications_allowed(&self) -> bool {
        self.notifications.is_some_and(|decision| decision.allowed)
    }

    /// Whether the identity decided any permission for the app, which is what lists it.
    pub fn has_permission_decision(&self) -> bool {
        let Self {
            default_account_number: _,
            last_notified_at_ns: _,
            notifications,
        } = self;
        notifications.is_some()
    }

    /// The config with every permission decision undone, and what followed from them.
    pub fn forgotten(self) -> Self {
        let Self {
            default_account_number,
            last_notified_at_ns: _,
            notifications: _,
        } = self;
        Self {
            default_account_number,
            last_notified_at_ns: None,
            notifications: None,
        }
    }
}

impl Storable for AnchorApplicationConfig {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode AnchorApplicationConfig");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode AnchorApplicationConfig")
    }

    const BOUND: Bound = Bound::Unbounded;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_roundtrip_through_storable() {
        let config = AnchorApplicationConfig {
            default_account_number: None,
            last_notified_at_ns: Some(2),
            notifications: Some(PermissionDecision {
                allowed: false,
                changed_at_ns: 3,
            }),
        };
        assert_eq!(
            AnchorApplicationConfig::from_bytes(config.to_bytes()),
            config
        );
    }

    /// Rows carrying the retired key 1 decode as an app the identity decided nothing for.
    #[test]
    fn should_skip_the_retired_consent_key() {
        let mut buffer = Vec::new();
        let mut encoder = minicbor::Encoder::new(&mut buffer);
        encoder.map(2).unwrap();
        encoder.u8(1).unwrap().u64(7).unwrap();
        encoder.u8(2).unwrap().u64(9).unwrap();

        let decoded = AnchorApplicationConfig::from_bytes(Cow::Borrowed(&buffer));
        assert_eq!(decoded.notifications, None);
        assert_eq!(decoded.last_notified_at_ns, Some(9));
        assert!(!decoded.has_permission_decision());
    }

    #[test]
    fn should_keep_the_default_account_when_forgotten() {
        let config = AnchorApplicationConfig {
            default_account_number: Some(5),
            last_notified_at_ns: Some(2),
            notifications: Some(PermissionDecision {
                allowed: true,
                changed_at_ns: 3,
            }),
        };
        assert_eq!(
            config.forgotten(),
            AnchorApplicationConfig {
                default_account_number: Some(5),
                last_notified_at_ns: None,
                notifications: None,
            }
        );
    }
}
