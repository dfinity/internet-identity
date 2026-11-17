use crate::storage::anchor::{PendingTransport, WebPushSubscription};
use crate::storage::storable::account_number::StorableAccountNumber;
use crate::storage::storable::browser_id::StorableBrowserId;
use crate::storage::storable::notifications::webpush::subscription::StorableWebPushSubscription;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use internet_identity_interface::internet_identity::types::Timestamp;
use minicbor::{Decode, Encode};
use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet};

/// Which of an app's installed web app subscriptions on one identity: the browser entry
/// the subscribing session signed in from, and the account that session is at.
#[derive(Encode, Decode, Clone, Copy, Ord, Eq, Hash, PartialEq, PartialOrd, Debug)]
#[cbor(array)]
pub struct PwaSubscriptionSlot {
    #[n(0)]
    pub browser_id: StorableBrowserId,
    #[n(1)]
    pub account_number: Option<StorableAccountNumber>, // None is the unreserved synthetic account
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
    /// The app's own installed web app subscriptions, one per browser entry and account.
    /// None if the app never subscribed one, or every one it did was removed.
    #[n(4)]
    pub pwa_webpush_subscriptions: Option<PwaSlots>,
    /// The transport keys installed web apps fetched for a subscribe that has yet to land,
    /// one per slot, held apart so the subscription a slot holds keeps delivering and
    /// keeps its endpoint until the subscribe replaces it. Only the pending half is read.
    /// None if no subscribe is waiting.
    #[n(5)]
    pub pwa_pending_transports: Option<PwaSlots>,
}

/// One registration per installed web app slot.
pub type PwaSlots = BTreeMap<PwaSubscriptionSlot, StorableWebPushSubscription>;

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
            pwa_pending_transports: _,
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
            pwa_pending_transports: _,
        } = self;
        Self {
            default_account_number,
            last_notified_at_ns: None,
            notifications: None,
            pwa_webpush_subscriptions: None,
            pwa_pending_transports: None,
        }
    }

    /// The installed app subscription in `slot`. An entry that does not read back reads as
    /// none, which the app recovers from by subscribing again.
    pub fn pwa_webpush_subscription(
        &self,
        slot: PwaSubscriptionSlot,
    ) -> Option<WebPushSubscription> {
        let stored = self.pwa_webpush_subscriptions.as_ref()?.get(&slot)?.clone();
        WebPushSubscription::try_from(stored)
            .inspect_err(|reason| {
                ic_cdk::println!(
                    "ERROR: dropped the installed app subscription of browser {} as unreadable: {reason}",
                    slot.browser_id
                );
            })
            .ok()
    }

    /// The transport key fetched for a subscribe of `slot` that has yet to land.
    pub fn pwa_pending_transport(&self, slot: PwaSubscriptionSlot) -> Option<PendingTransport> {
        let stored = self.pwa_pending_transports.as_ref()?.get(&slot)?.clone();
        WebPushSubscription::try_from(stored)
            .inspect_err(|reason| {
                ic_cdk::println!(
                    "ERROR: dropped the installed app transport key of browser {} as unreadable: {reason}",
                    slot.browser_id
                );
            })
            .ok()?
            .pending()
            .cloned()
    }

    /// Puts `subscription` in `slot`, or with `None` empties the slot. Either way the
    /// transport key of a subscribe waiting for `slot` is spent.
    pub fn set_pwa_webpush_subscription(
        &mut self,
        slot: PwaSubscriptionSlot,
        subscription: Option<WebPushSubscription>,
    ) {
        self.refresh_pwa_webpush_subscription(slot, subscription);
        put_slot(&mut self.pwa_pending_transports, slot, None);
    }

    /// Puts `subscription` in `slot`, or with `None` empties the slot, leaving the
    /// transport key of a subscribe waiting for `slot` to that subscribe.
    pub fn refresh_pwa_webpush_subscription(
        &mut self,
        slot: PwaSubscriptionSlot,
        subscription: Option<WebPushSubscription>,
    ) {
        put_slot(
            &mut self.pwa_webpush_subscriptions,
            slot,
            subscription.map(StorableWebPushSubscription::from),
        );
    }

    /// Records the transport key fetched for a subscribe of `slot`, leaving the
    /// subscription `slot` holds as it is.
    pub fn set_pwa_pending_transport(
        &mut self,
        slot: PwaSubscriptionSlot,
        pending: PendingTransport,
    ) {
        put_slot(
            &mut self.pwa_pending_transports,
            slot,
            Some(StorableWebPushSubscription::from(
                WebPushSubscription::Pending(pending),
            )),
        );
    }

    /// Moves every slot of account `from` to account `to`, answering whether any moved.
    pub fn move_pwa_webpush_subscriptions(
        &mut self,
        from: Option<StorableAccountNumber>,
        to: Option<StorableAccountNumber>,
    ) -> bool {
        let has_moved_subscription = move_slots(&mut self.pwa_webpush_subscriptions, from, to);
        let has_moved_transport = move_slots(&mut self.pwa_pending_transports, from, to);
        has_moved_subscription || has_moved_transport
    }

    /// Empties every slot of a browser entry in `browser_ids`, answering whether any was held.
    pub fn remove_pwa_webpush_subscriptions_of_browsers(
        &mut self,
        browser_ids: &BTreeSet<StorableBrowserId>,
    ) -> bool {
        let has_removed_subscription =
            remove_browser_slots(&mut self.pwa_webpush_subscriptions, browser_ids);
        let has_removed_transport =
            remove_browser_slots(&mut self.pwa_pending_transports, browser_ids);
        has_removed_subscription || has_removed_transport
    }

    /// Empties every slot.
    pub fn clear_pwa_webpush_subscriptions(&mut self) {
        self.pwa_webpush_subscriptions = None;
        self.pwa_pending_transports = None;
    }
}

/// Puts `value` in `slot` of `slots`, or with `None` empties it. Slots holding nothing
/// are stored as `None`.
fn put_slot(
    slots: &mut Option<PwaSlots>,
    slot: PwaSubscriptionSlot,
    value: Option<StorableWebPushSubscription>,
) {
    let mut held = slots.take().unwrap_or_default();
    match value {
        Some(value) => {
            held.insert(slot, value);
        }
        None => {
            held.remove(&slot);
        }
    }
    *slots = (!held.is_empty()).then_some(held);
}

/// Moves every slot of account `from` in `slots` to account `to`, answering whether any
/// moved.
fn move_slots(
    slots: &mut Option<PwaSlots>,
    from: Option<StorableAccountNumber>,
    to: Option<StorableAccountNumber>,
) -> bool {
    let Some(held) = slots.take() else {
        return false;
    };
    let mut has_moved = false;
    let mut moved = BTreeMap::new();
    for (slot, value) in held {
        let slot = match slot.account_number == from {
            true => {
                has_moved = true;
                PwaSubscriptionSlot {
                    browser_id: slot.browser_id,
                    account_number: to,
                }
            }
            false => slot,
        };
        moved.insert(slot, value);
    }
    *slots = Some(moved);
    has_moved
}

/// Empties every slot of a browser entry in `browser_ids` in `slots`, answering whether
/// any was held.
fn remove_browser_slots(
    slots: &mut Option<PwaSlots>,
    browser_ids: &BTreeSet<StorableBrowserId>,
) -> bool {
    let Some(mut held) = slots.take() else {
        return false;
    };
    let held_count = held.len();
    held.retain(|slot, _| !browser_ids.contains(&slot.browser_id));
    let has_removed = held.len() != held_count;
    *slots = (!held.is_empty()).then_some(held);
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
                PwaSubscriptionSlot {
                    browser_id: 4,
                    account_number: Some(5),
                },
                StorableWebPushSubscription::Pending {
                    secret_key: vec![1; 32],
                    sender_seed: vec![2; 32],
                    minted_at_ns: 6,
                },
            )])),
            pwa_pending_transports: None,
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
                PwaSubscriptionSlot {
                    browser_id: 4,
                    account_number: None,
                },
                StorableWebPushSubscription::Pending {
                    secret_key: vec![1; 32],
                    sender_seed: vec![2; 32],
                    minted_at_ns: 6,
                },
            )])),
            pwa_pending_transports: None,
        };
        assert_eq!(
            config.forgotten(),
            AnchorApplicationConfig {
                default_account_number: Some(5),
                last_notified_at_ns: None,
                notifications: None,
                pwa_webpush_subscriptions: None,
                pwa_pending_transports: None,
            }
        );
    }
}
