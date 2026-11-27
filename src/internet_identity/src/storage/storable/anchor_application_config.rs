use crate::origin::OriginSha256;
use crate::storage::anchor::WebPushSubscription;
use crate::storage::storable::account_number::StorableAccountNumber;
use crate::storage::storable::application::StorableOriginSha256;
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
    #[n(2)]
    pub disabled_at_ns: Option<Timestamp>, // None while the app has not disabled it
}

/// [`StorablePwaSubscription`] read back.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PwaSubscription {
    pub subscription: WebPushSubscription,
    pub account_principal: Principal,
    /// When the app disabled the subscription on its device: nothing is sent to it from
    /// then on, and the device is not notified another way, until the slot is prepared or
    /// set again or the user allows notifications there again. A disabled slot outlives
    /// its sessions, its endpoint and a removal without a subscription. `None` while the
    /// app has not.
    pub disabled_at_ns: Option<Timestamp>,
}

impl TryFrom<StorablePwaSubscription> for PwaSubscription {
    type Error = String;

    /// A row this canister wrote, read back: refusing it means the row is not what it
    /// claims to be.
    fn try_from(value: StorablePwaSubscription) -> Result<Self, Self::Error> {
        let StorablePwaSubscription {
            subscription,
            account_principal,
            disabled_at_ns,
        } = value;
        let subscription = WebPushSubscription::try_from(subscription)?;
        let account_principal = Principal::try_from_slice(&account_principal)
            .map_err(|err| format!("the account principal does not read back: {err}"))?;
        Ok(Self {
            subscription,
            account_principal,
            disabled_at_ns,
        })
    }
}

impl From<PwaSubscription> for StorablePwaSubscription {
    fn from(value: PwaSubscription) -> Self {
        let PwaSubscription {
            subscription,
            account_principal,
            disabled_at_ns,
        } = value;
        let subscription = StorableWebPushSubscription::from(subscription);
        let account_principal = account_principal.as_slice().to_vec();
        Self {
            subscription,
            account_principal,
            disabled_at_ns,
        }
    }
}

/// Which installed web app a subscription is for: the browser entry it was subscribed on,
/// and the frontend of the app it was subscribed from, by the SHA-256 of that frontend's
/// origin. An app served from several origins has one installed web app per origin.
#[derive(Encode, Decode, Clone, Copy, Ord, Eq, PartialEq, PartialOrd, Debug)]
#[cbor(array)]
pub struct StorablePwaSlot {
    #[n(0)]
    pub browser_id: StorableBrowserId,
    #[n(1)]
    pub frontend_origin_hash: StorableOriginSha256,
}

/// What the identity holds for one app. Keys 1 and 4 are retired and never reused: rows
/// that carry them decode with them skipped.
#[derive(Encode, Decode, Default, Clone, Ord, Eq, PartialEq, PartialOrd, Debug)]
#[cbor(map)]
pub struct AnchorApplicationConfig {
    #[n(0)]
    pub default_account_number: Option<StorableAccountNumber>, // None is the unreserved synthetic account
    #[n(2)]
    pub last_notified_at_ns: Option<Timestamp>, // None if the app never reached a browser
    #[n(3)]
    pub notifications: Option<PermissionDecision>, // None if the identity never decided
    // #[n(4)] RETIRED, never reuse
    /// The app's own installed web app subscriptions, one per browser entry and frontend.
    /// None if the app never subscribed one, or every one it did was removed.
    #[n(5)]
    pub pwa_webpush_subscriptions: Option<BTreeMap<StorablePwaSlot, StorablePwaSubscription>>,
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

    /// The installed app subscription on `browser_id` for the frontend whose origin hashes
    /// to `frontend_origin_hash`. One that does not read back reads as none, which the app
    /// recovers from by subscribing again.
    pub fn pwa_webpush_subscription(
        &self,
        browser_id: BrowserId,
        frontend_origin_hash: OriginSha256,
    ) -> Option<PwaSubscription> {
        let stored_subscriptions = self.pwa_webpush_subscriptions.as_ref()?;
        let frontend_origin_hash = StorableOriginSha256::from(frontend_origin_hash);
        let stored_subscription = stored_subscriptions.get(&StorablePwaSlot {
            browser_id,
            frontend_origin_hash,
        })?;
        read_back_pwa_subscription(browser_id, stored_subscription)
    }

    /// Every installed app subscription on `browser_id`, whichever frontend it is for, with
    /// the hash of that frontend's origin. Ones that do not read back are left out.
    pub fn pwa_webpush_subscriptions_of_browser(
        &self,
        browser_id: BrowserId,
    ) -> Vec<(OriginSha256, PwaSubscription)> {
        let mut subscriptions = Vec::new();
        for (slot, stored_subscription) in self.pwa_webpush_subscriptions.iter().flatten() {
            if slot.browser_id != browser_id {
                continue;
            }
            if let Some(subscription) = read_back_pwa_subscription(browser_id, stored_subscription)
            {
                subscriptions.push((OriginSha256::from(slot.frontend_origin_hash), subscription));
            }
        }
        subscriptions
    }

    /// Every installed app subscription, on every browser entry and for every frontend,
    /// with the entry and the hash of the frontend's origin it is held under. Ones that do
    /// not read back are left out.
    pub fn pwa_webpush_subscription_slots(
        &self,
    ) -> Vec<(BrowserId, OriginSha256, PwaSubscription)> {
        let mut subscriptions = Vec::new();
        for (slot, stored_subscription) in self.pwa_webpush_subscriptions.iter().flatten() {
            if let Some(subscription) =
                read_back_pwa_subscription(slot.browser_id, stored_subscription)
            {
                let frontend_origin_hash = OriginSha256::from(slot.frontend_origin_hash);
                subscriptions.push((slot.browser_id, frontend_origin_hash, subscription));
            }
        }
        subscriptions
    }

    /// Puts `subscription` on `browser_id` for the frontend whose origin hashes to
    /// `frontend_origin_hash`, or with `None` empties that slot. A map holding nothing is
    /// stored as `None`.
    pub fn set_pwa_webpush_subscription(
        &mut self,
        browser_id: BrowserId,
        frontend_origin_hash: OriginSha256,
        subscription: Option<PwaSubscription>,
    ) {
        let frontend_origin_hash = StorableOriginSha256::from(frontend_origin_hash);
        let slot = StorablePwaSlot {
            browser_id,
            frontend_origin_hash,
        };
        let mut subscriptions = self.pwa_webpush_subscriptions.take().unwrap_or_default();
        match subscription {
            Some(subscription) => {
                subscriptions.insert(slot, StorablePwaSubscription::from(subscription));
            }
            None => {
                subscriptions.remove(&slot);
            }
        }
        self.pwa_webpush_subscriptions = (!subscriptions.is_empty()).then_some(subscriptions);
    }

    /// Empties every slot of every browser entry in `browser_ids`, answering whether any
    /// held anything.
    pub fn remove_pwa_webpush_subscriptions_of_browsers(
        &mut self,
        browser_ids: &BTreeSet<StorableBrowserId>,
    ) -> bool {
        let Some(mut subscriptions) = self.pwa_webpush_subscriptions.take() else {
            return false;
        };
        let held_count = subscriptions.len();
        subscriptions.retain(|slot, _| !browser_ids.contains(&slot.browser_id));
        let has_removed = subscriptions.len() != held_count;
        self.pwa_webpush_subscriptions = (!subscriptions.is_empty()).then_some(subscriptions);
        has_removed
    }

    /// Empties every slot.
    pub fn clear_pwa_webpush_subscriptions(&mut self) {
        self.pwa_webpush_subscriptions = None;
    }
}

/// `stored_subscription`, held on `browser_id`, read back. `None`, logged, where it does
/// not read back.
fn read_back_pwa_subscription(
    browser_id: BrowserId,
    stored_subscription: &StorablePwaSubscription,
) -> Option<PwaSubscription> {
    PwaSubscription::try_from(stored_subscription.clone())
        .inspect_err(|reason| {
            ic_cdk::println!(
                "ERROR: dropped an installed app subscription of browser {browser_id} as unreadable: {reason}"
            );
        })
        .ok()
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
                StorablePwaSlot {
                    browser_id: 4,
                    frontend_origin_hash: StorableOriginSha256::from_origin(
                        &"https://app.example".to_string(),
                    ),
                },
                StorablePwaSubscription {
                    subscription: StorableWebPushSubscription::Pending {
                        secret_key: vec![1; 32],
                        sender_seed: vec![2; 32],
                        minted_at_ns: 6,
                    },
                    account_principal: vec![5; 29],
                    disabled_at_ns: None,
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

    fn pending_subscription(seed: u8) -> StorablePwaSubscription {
        StorablePwaSubscription {
            subscription: StorableWebPushSubscription::Pending {
                secret_key: vec![seed; 32],
                sender_seed: vec![seed; 32],
                minted_at_ns: 6,
            },
            account_principal: vec![5; 29],
            disabled_at_ns: None,
        }
    }

    fn frontend(origin: &str) -> OriginSha256 {
        OriginSha256::from_origin(origin)
    }

    /// Rows carrying the retired key 4, subscriptions keyed by browser entry alone, decode
    /// as holding none.
    #[test]
    fn should_skip_the_retired_per_browser_subscriptions_key() {
        let mut buffer = Vec::new();
        let mut encoder = minicbor::Encoder::new(&mut buffer);
        encoder.map(2).unwrap();
        encoder.u8(2).unwrap().u64(9).unwrap();
        encoder.u8(4).unwrap().map(1).unwrap().u32(4).unwrap();
        encoder.encode(pending_subscription(1)).unwrap();

        let decoded = AnchorApplicationConfig::from_bytes(Cow::Borrowed(&buffer));
        assert_eq!(decoded.pwa_webpush_subscriptions, None);
        assert_eq!(decoded.last_notified_at_ns, Some(9));
    }

    /// A subscription written before it could be disabled, without key 2, decodes as
    /// enabled, and a disabled one keeps when it was disabled.
    #[test]
    fn should_read_a_subscription_without_a_disabled_time_as_enabled() {
        #[derive(Encode)]
        #[cbor(map)]
        struct StorablePwaSubscriptionWithoutDisabledTime {
            #[n(0)]
            subscription: StorableWebPushSubscription,
            #[cbor(n(1), with = "minicbor::bytes")]
            account_principal: Vec<u8>,
        }
        let StorablePwaSubscription {
            subscription,
            account_principal,
            disabled_at_ns: _,
        } = pending_subscription(1);
        let mut buffer = Vec::new();
        minicbor::encode(
            StorablePwaSubscriptionWithoutDisabledTime {
                subscription,
                account_principal,
            },
            &mut buffer,
        )
        .unwrap();

        let decoded: StorablePwaSubscription = minicbor::decode(&buffer).unwrap();
        assert_eq!(decoded, pending_subscription(1));

        let disabled_subscription = StorablePwaSubscription {
            disabled_at_ns: Some(8),
            ..pending_subscription(1)
        };
        let mut buffer = Vec::new();
        minicbor::encode(&disabled_subscription, &mut buffer).unwrap();
        let decoded: StorablePwaSubscription = minicbor::decode(&buffer).unwrap();
        assert_eq!(decoded, disabled_subscription);
    }

    #[test]
    fn should_keep_one_slot_per_frontend_on_a_browser_entry() {
        let mut config = AnchorApplicationConfig::default();
        let subscription_of =
            |seed| PwaSubscription::try_from(pending_subscription(seed)).expect("a readable row");
        config.set_pwa_webpush_subscription(
            4,
            frontend("https://a.example"),
            Some(subscription_of(1)),
        );
        config.set_pwa_webpush_subscription(
            4,
            frontend("https://b.example"),
            Some(subscription_of(2)),
        );
        config.set_pwa_webpush_subscription(
            5,
            frontend("https://a.example"),
            Some(subscription_of(3)),
        );

        assert_eq!(
            config.pwa_webpush_subscription(4, frontend("https://b.example")),
            Some(subscription_of(2))
        );
        assert_eq!(
            config.pwa_webpush_subscription(4, frontend("https://c.example")),
            None
        );
        let mut frontends_of_browser: Vec<OriginSha256> = config
            .pwa_webpush_subscriptions_of_browser(4)
            .into_iter()
            .map(|(frontend_origin_hash, _)| frontend_origin_hash)
            .collect();
        frontends_of_browser.sort();
        let mut expected_frontends =
            vec![frontend("https://a.example"), frontend("https://b.example")];
        expected_frontends.sort();
        assert_eq!(frontends_of_browser, expected_frontends);

        config.set_pwa_webpush_subscription(4, frontend("https://a.example"), None);
        assert_eq!(
            config.pwa_webpush_subscription(4, frontend("https://b.example")),
            Some(subscription_of(2))
        );
        assert!(config.remove_pwa_webpush_subscriptions_of_browsers(&BTreeSet::from([4])));
        assert!(config.pwa_webpush_subscriptions_of_browser(4).is_empty());
        assert_eq!(
            config.pwa_webpush_subscription(5, frontend("https://a.example")),
            Some(subscription_of(3))
        );
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
                StorablePwaSlot {
                    browser_id: 4,
                    frontend_origin_hash: StorableOriginSha256::from_origin(
                        &"https://app.example".to_string(),
                    ),
                },
                StorablePwaSubscription {
                    subscription: StorableWebPushSubscription::Pending {
                        secret_key: vec![1; 32],
                        sender_seed: vec![2; 32],
                        minted_at_ns: 6,
                    },
                    account_principal: vec![5; 29],
                    disabled_at_ns: None,
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
