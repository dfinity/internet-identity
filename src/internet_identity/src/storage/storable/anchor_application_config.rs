use crate::storage::anchor::WebPushSubscription;
use crate::storage::storable::account_number::StorableAccountNumber;
use crate::storage::storable::browser_id::StorableBrowserId;
use crate::storage::storable::notifications::webpush::subscription::StorableWebPushSubscription;
use candid::Principal;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use internet_identity_interface::internet_identity::types::{BrowserId, Timestamp};
use minicbor::{Decode, Encode};
use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet};

/// An installed web app's subscription on one browser entry, and the account it delivers
/// for: the one whose session subscribed last, by the principal the account keeps when it
/// is given a number.
#[derive(Encode, Decode, Clone, Ord, Eq, PartialEq, PartialOrd, Debug)]
#[cbor(map)]
pub struct StorablePwaSubscription {
    #[n(0)]
    pub subscription: StorableWebPushSubscription,
    #[cbor(n(1), with = "minicbor::bytes")]
    pub account_principal: Vec<u8>,
}

/// [`StorablePwaSubscription`] read back.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PwaSubscription {
    pub subscription: WebPushSubscription,
    pub account_principal: Principal,
}

impl TryFrom<StorablePwaSubscription> for PwaSubscription {
    type Error = String;

    /// A row this canister wrote, read back: refusing it means the row is not what it
    /// claims to be.
    fn try_from(value: StorablePwaSubscription) -> Result<Self, Self::Error> {
        let StorablePwaSubscription {
            subscription,
            account_principal,
        } = value;
        let subscription = WebPushSubscription::try_from(subscription)?;
        let account_principal = Principal::try_from_slice(&account_principal)
            .map_err(|err| format!("the account principal does not read back: {err}"))?;
        Ok(Self {
            subscription,
            account_principal,
        })
    }
}

impl From<PwaSubscription> for StorablePwaSubscription {
    fn from(value: PwaSubscription) -> Self {
        let PwaSubscription {
            subscription,
            account_principal,
        } = value;
        let subscription = StorableWebPushSubscription::from(subscription);
        let account_principal = account_principal.as_slice().to_vec();
        Self {
            subscription,
            account_principal,
        }
    }
}

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
    /// The app's own installed web app subscriptions, one per browser entry. None if the
    /// app never subscribed one, or every one it did was removed.
    #[n(4)]
    pub pwa_webpush_subscriptions: Option<BTreeMap<StorableBrowserId, StorablePwaSubscription>>,
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
            pwa_webpush_subscriptions: _,
        } = self;
        notifications.is_some()
    }

    /// The config with every permission decision undone, and what followed from them.
    pub fn forgotten(self) -> Self {
        let Self {
            default_account_number,
            last_notified_at_ns: _,
            notifications: _,
            pwa_webpush_subscriptions: _,
        } = self;
        Self {
            default_account_number,
            last_notified_at_ns: None,
            notifications: None,
            pwa_webpush_subscriptions: None,
        }
    }

    /// The installed app subscription on `browser_id`. One that does not read back reads
    /// as none, which the app recovers from by subscribing again.
    pub fn pwa_webpush_subscription(&self, browser_id: BrowserId) -> Option<PwaSubscription> {
        let stored_subscriptions = self.pwa_webpush_subscriptions.as_ref()?;
        let stored_subscription = stored_subscriptions.get(&browser_id)?;
        PwaSubscription::try_from(stored_subscription.clone())
            .inspect_err(|reason| {
                ic_cdk::println!(
                    "ERROR: dropped the installed app subscription of browser {browser_id} as unreadable: {reason}"
                );
            })
            .ok()
    }

    /// Puts `subscription` on `browser_id`, or with `None` empties it.
    pub fn set_pwa_webpush_subscription(
        &mut self,
        browser_id: BrowserId,
        subscription: Option<PwaSubscription>,
    ) {
        put_browser_entry(
            &mut self.pwa_webpush_subscriptions,
            browser_id,
            subscription.map(StorablePwaSubscription::from),
        );
    }

    /// Empties every browser entry in `browser_ids`, answering whether any held anything.
    pub fn remove_pwa_webpush_subscriptions_of_browsers(
        &mut self,
        browser_ids: &BTreeSet<StorableBrowserId>,
    ) -> bool {
        remove_browser_entries(&mut self.pwa_webpush_subscriptions, browser_ids)
    }

    /// Empties every browser entry.
    pub fn clear_pwa_webpush_subscriptions(&mut self) {
        self.pwa_webpush_subscriptions = None;
    }
}

/// Puts `value` on `browser_id` in `entries_by_browser`, or with `None` empties it. A
/// map holding nothing is stored as `None`.
fn put_browser_entry<T>(
    entries_by_browser: &mut Option<BTreeMap<StorableBrowserId, T>>,
    browser_id: BrowserId,
    value: Option<T>,
) {
    let mut entries = entries_by_browser.take().unwrap_or_default();
    match value {
        Some(value) => {
            entries.insert(browser_id, value);
        }
        None => {
            entries.remove(&browser_id);
        }
    }
    *entries_by_browser = (!entries.is_empty()).then_some(entries);
}

/// Empties every browser entry in `browser_ids` in `entries_by_browser`, answering
/// whether any was held.
fn remove_browser_entries<T>(
    entries_by_browser: &mut Option<BTreeMap<StorableBrowserId, T>>,
    browser_ids: &BTreeSet<StorableBrowserId>,
) -> bool {
    let Some(mut entries) = entries_by_browser.take() else {
        return false;
    };
    let held_count = entries.len();
    entries.retain(|browser_id, _| !browser_ids.contains(browser_id));
    let has_removed = entries.len() != held_count;
    *entries_by_browser = (!entries.is_empty()).then_some(entries);
    has_removed
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
            pwa_webpush_subscriptions: Some(BTreeMap::from([(
                4,
                StorablePwaSubscription {
                    subscription: StorableWebPushSubscription::Pending {
                        secret_key: vec![1; 32],
                        sender_seed: vec![2; 32],
                        minted_at_ns: 6,
                    },
                    account_principal: vec![5; 29],
                },
            )])),
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
            pwa_webpush_subscriptions: Some(BTreeMap::from([(
                4,
                StorablePwaSubscription {
                    subscription: StorableWebPushSubscription::Pending {
                        secret_key: vec![1; 32],
                        sender_seed: vec![2; 32],
                        minted_at_ns: 6,
                    },
                    account_principal: vec![5; 29],
                },
            )])),
        };
        assert_eq!(
            config.forgotten(),
            AnchorApplicationConfig {
                default_account_number: Some(5),
                last_notified_at_ns: None,
                notifications: None,
                pwa_webpush_subscriptions: None,
            }
        );
    }
}
