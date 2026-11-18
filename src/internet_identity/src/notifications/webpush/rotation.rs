//! Refreshing what a subscription's payloads are sealed with, a bounded number per pass.
//!
//! RFC 8291 mints a sender keypair per push, and at 11.9M instructions for the scalar
//! multiplication that is far too much to spend on every destination: it would cap a pass
//! at a couple of hundred where reusing the keying material allows tens of thousands. So
//! the keying material is kept on the subscription and refreshed on a budget.
//!
//! The browser notices nothing either way. Every push already carries the sender's public
//! key as its `keyid`, because the specification expects that key to change every time.

use sha2::{Digest, Sha256};
use std::collections::{BTreeMap, BTreeSet, HashSet};

use super::payload;
use super::pwa_subscription;
use super::validation::{PayloadIkm, PayloadKeyId, SubscriberKey};
use crate::authz_utils::NotificationKey;
use crate::notifications::dispatch::DeliveryTarget;
use crate::notifications::installed_app::NotificationAppId;
use crate::state::{self, storage_borrow, storage_borrow_mut};
use crate::storage::anchor::{Anchor, ReadySubscription, WebPushSubscription};
use crate::storage::PwaSubscriptionHolder;
use internet_identity_interface::internet_identity::types::{AnchorNumber, BrowserId, Timestamp};

/// How many subscriptions one pass refreshes. A pass that refreshes anything spends
/// `X + 1` scalar multiplications: one to turn its random bytes into a sender key, then
/// one per subscription. At eight that is ~107M instructions, around 5% of a round, and
/// each of `A` active subscriptions comes round every `A / X` passes.
///
/// A ceiling rather than a quota: a pass carrying three wake-ups refreshes at most three,
/// so at low traffic every push gets a fresh key, exactly as the specification intends.
pub(crate) const ROTATIONS_PER_PASS: usize = 8;

/// Separates the pass's sender key from the salts drawn off the same bytes, so one secret
/// is not doing two jobs.
const PASS_SENDER_INFO: &[u8] = b"II webpush pass sender\0";

/// Which registration a wake-up is sealed against, and so which one a refresh writes to.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub(crate) struct Destination {
    pub anchor_number: AnchorNumber,
    pub browser_id: BrowserId,
    pub target: DeliveryTarget,
}

/// What a refresh writes to, which is not the destination itself: a Home Screen app's
/// record is shared by every browser pointing at it, so two of them in one pass are one
/// thing to refresh and one scalar multiplication, not two.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub(crate) enum RegistrationId {
    App(NotificationAppId),
    Browser {
        anchor_number: AnchorNumber,
        browser_id: BrowserId,
    },
    Pwa(PwaSubscriptionHolder),
}

impl RegistrationId {
    /// The subscription a call signed with `key` for `anchor_number` was woken through: the
    /// calling browser's own, or the record of the app that signed.
    pub(crate) fn of_key(anchor_number: AnchorNumber, key: NotificationKey) -> Self {
        match key {
            NotificationKey::Browser(browser_id) => RegistrationId::Browser {
                anchor_number,
                browser_id,
            },
            NotificationKey::App(app_id) => RegistrationId::App(app_id),
        }
    }
}

impl Destination {
    pub(crate) fn registration(self) -> RegistrationId {
        match self.target {
            DeliveryTarget::App(app_id) => RegistrationId::App(app_id),
            DeliveryTarget::Browser => RegistrationId::Browser {
                anchor_number: self.anchor_number,
                browser_id: self.browser_id,
            },
            DeliveryTarget::Pwa { application_number } => {
                RegistrationId::Pwa(PwaSubscriptionHolder {
                    anchor_number: self.anchor_number,
                    application_number,
                    browser_id: self.browser_id,
                })
            }
        }
    }
}

/// This pass's sender key, which every refresh in the pass shares.
///
/// One keypair per pass rather than per push is the whole of the deviation from RFC 8291,
/// and it is bounded by the pass: the private half is a local here and is gone when the
/// pass ends, so nothing that could re-derive an earlier key outlives the pass that used
/// it.
pub(crate) fn pass_sender(pass_bytes: &[u8; 32]) -> p256::SecretKey {
    let scalar: [u8; 32] = Sha256::new()
        .chain_update(PASS_SENDER_INFO)
        .chain_update(pass_bytes)
        .finalize()
        .into();
    super::seed_pool::secret_key_from(&scalar)
}

/// What one destination seals with, carried on the wake-up from the moment it is planned.
///
/// Carried rather than read back at sealing time: a pass holds up to
/// [`MAX_POSTS_PER_PASS`](crate::notifications::dispatch::MAX_POSTS_PER_PASS) wake-ups,
/// and reading each one's anchor again to find what has not changed would cost more than
/// the refreshes the pass is budgeted for.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct SealingKeys {
    pub payload_ikm: PayloadIkm,
    pub payload_keyid: PayloadKeyId,
    pub last_rotated_ns: Timestamp,
}

impl From<&ReadySubscription> for SealingKeys {
    fn from(registered: &ReadySubscription) -> Self {
        Self {
            payload_ikm: registered.payload_ikm.clone(),
            payload_keyid: registered.payload_keyid.clone(),
            last_rotated_ns: registered.last_rotated_ns,
        }
    }
}

/// What a refresh found at a destination.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum Refresh {
    /// Still the registration the pass planned for, now sealing with these.
    Refreshed(SealingKeys),
    /// Nothing ready to refresh any more: the registration went, or waits on a subscribe.
    Absent,
    /// Another registration, with another endpoint or `p256dh`, replaced the one the pass
    /// planned for. Nothing posted to the planned endpoint can be sealed for it.
    Replaced,
}

/// Derives fresh keying material against `sender` and writes it back, where
/// `registered_subscription`, the registration `destination` names as the pass read it,
/// is still the one at `endpoint` with `subscriber_key` the pass planned for.
pub(crate) fn refresh(
    destination: Destination,
    registered_subscription: Option<&ReadySubscription>,
    endpoint: &str,
    subscriber_key: &SubscriberKey,
    sender: &p256::SecretKey,
    now_ns: Timestamp,
) -> Result<Refresh, String> {
    let Some(registered_subscription) = registered_subscription else {
        return Ok(Refresh::Absent);
    };
    if !is_planned_registration(registered_subscription, endpoint, subscriber_key) {
        return Ok(Refresh::Replaced);
    }
    let (payload_ikm, payload_keyid) = payload::derive_ikm(
        sender,
        &registered_subscription.subscriber_key,
        &registered_subscription.auth,
    )?;
    let mut registered_subscription = registered_subscription.clone();
    registered_subscription.payload_ikm = payload_ikm;
    registered_subscription.payload_keyid = payload_keyid;
    registered_subscription.last_rotated_ns = now_ns;
    let refreshed_sealing = SealingKeys::from(&registered_subscription);
    write_registration(destination.registration(), registered_subscription)?;
    Ok(Refresh::Refreshed(refreshed_sealing))
}

/// Whether `registered_subscription` still posts to `endpoint` with `subscriber_key`, as
/// it did when a pass planned a wake-up through it. One replaced since is not: what was
/// planned would be sealed for keys its device no longer holds.
pub(crate) fn is_planned_registration(
    registered_subscription: &ReadySubscription,
    endpoint: &str,
    subscriber_key: &SubscriberKey,
) -> bool {
    registered_subscription.endpoint.as_str() == endpoint
        && registered_subscription.subscriber_key == *subscriber_key
}

/// The ready subscription each registration of `destinations` names, as
/// [`read_registration`] reads it, keyed by the registration; one that is gone or waits
/// on a subscribe has no entry. Each anchor is decoded once however many of its browsers
/// are among `destinations`, since decoding one reads every browser's inbox with it.
pub(crate) fn read_registrations(
    destinations: &[Destination],
) -> BTreeMap<RegistrationId, ReadySubscription> {
    let mut anchors: BTreeMap<AnchorNumber, Anchor> = BTreeMap::new();
    let mut registrations = BTreeMap::new();
    let mut read_registration_ids = BTreeSet::new();
    for destination in destinations {
        let registration_id = destination.registration();
        if !read_registration_ids.insert(registration_id) {
            continue;
        }
        let registered_subscription = match registration_id {
            RegistrationId::Browser {
                anchor_number,
                browser_id,
            } => anchors
                .entry(anchor_number)
                .or_insert_with(|| state::anchor(anchor_number))
                .webpush_subscription(browser_id)
                .and_then(WebPushSubscription::ready)
                .cloned(),
            RegistrationId::App(_) => read_registration(registration_id),
            RegistrationId::Pwa(_) => read_registration(registration_id),
        };
        if let Some(registered_subscription) = registered_subscription {
            registrations.insert(registration_id, registered_subscription);
        }
    }
    registrations
}

/// The ready subscription `registration_id` names, read through the registration rather
/// than the route its browser delivers through now. A browser that holds its own and also
/// points at an app would otherwise be read from one and written to the other.
pub(super) fn read_registration(registration_id: RegistrationId) -> Option<ReadySubscription> {
    match registration_id {
        RegistrationId::App(app_id) => {
            let app = storage_borrow(|storage| storage.notification_app(app_id))?;
            app.webpush_subscription
                .and_then(WebPushSubscription::into_ready)
        }
        RegistrationId::Browser {
            anchor_number,
            browser_id,
        } => state::anchor(anchor_number)
            .webpush_subscription(browser_id)
            .and_then(WebPushSubscription::ready)
            .cloned(),
        RegistrationId::Pwa(holder) => pwa_subscription::read_pwa_subscription(holder)
            .and_then(|held_subscription| held_subscription.subscription.into_ready()),
    }
}

/// Writes `registered_subscription` to the subscription `registration_id` names.
pub(super) fn write_registration(
    registration_id: RegistrationId,
    registered_subscription: ReadySubscription,
) -> Result<(), String> {
    match registration_id {
        // Without stamping the record as used: a refresh is this canister's work, and an
        // app that has not called is how a deleted one is told apart from a live one.
        RegistrationId::App(app_id) => storage_borrow_mut(|storage| {
            storage.refresh_notification_app_subscription(
                app_id,
                Some(WebPushSubscription::Ready(registered_subscription)),
            )
        })
        .map_err(|err| format!("{err}")),
        RegistrationId::Browser {
            anchor_number,
            browser_id,
        } => {
            let mut anchor = storage_borrow(|storage| storage.read(anchor_number))
                .map_err(|err| format!("{err}"))?;
            anchor.set_webpush_subscription(
                browser_id,
                Some(WebPushSubscription::Ready(registered_subscription)),
            );
            storage_borrow_mut(|storage| storage.write(anchor)).map_err(|err| format!("{err}"))
        }
        RegistrationId::Pwa(holder) => storage_borrow_mut(|storage| {
            storage.refresh_pwa_webpush_subscription(holder, registered_subscription)
        })
        .map_err(|err| format!("{err}")),
    }
}

/// The `limit` stalest of `candidates`, oldest first.
///
/// Candidates are the pass's own destinations, already named by the wake-ups it is about
/// to post, so this is a partial sort over a list that is at most one pass long rather
/// than a scan of every subscription. No wake-ups means no candidates, and a quiet pass
/// costs nothing at all.
///
/// Oldest first means a subscription dormant for a year is refreshed on the pass that
/// wakes it, before it seals anything.
pub(crate) fn stalest(
    mut candidates: Vec<(Destination, Timestamp)>,
    limit: usize,
) -> Vec<Destination> {
    candidates.sort_by_key(|(_, last_rotated_ns)| *last_rotated_ns);
    let mut seen = HashSet::new();
    candidates
        .into_iter()
        .filter(|(destination, _)| seen.insert(destination.registration()))
        .take(limit)
        .map(|(destination, _)| destination)
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    fn at(browser_id: BrowserId) -> Destination {
        Destination {
            anchor_number: 10_000,
            browser_id,
            target: DeliveryTarget::Browser,
        }
    }

    #[test]
    fn the_oldest_stamps_go_first() {
        let picked = stalest(vec![(at(1), 30), (at(2), 10), (at(3), 20)], 2);
        assert_eq!(picked, vec![at(2), at(3)]);
    }

    /// Two wake-ups for one browser in one pass are one subscription, and refreshing it
    /// twice would spend a second scalar multiplication to no effect.
    #[test]
    fn one_destination_is_refreshed_once() {
        let picked = stalest(vec![(at(1), 10), (at(1), 10), (at(2), 20)], 8);
        assert_eq!(picked, vec![at(1), at(2)]);
    }

    /// Two browsers pointing at one Home Screen app share its registration, so the pass
    /// has one thing to refresh however many of them it is waking.
    #[test]
    fn two_browsers_through_one_app_are_one_refresh() {
        let through_app = |browser_id| Destination {
            anchor_number: 10_000,
            browser_id,
            target: DeliveryTarget::App(7),
        };
        let picked = stalest(vec![(through_app(1), 10), (through_app(2), 10)], 8);
        assert_eq!(picked, vec![through_app(1)]);
    }

    /// Two identities' browsers both numbered 1 are two registrations, not one.
    #[test]
    fn the_same_browser_number_under_two_identities_is_two_refreshes() {
        let elsewhere = Destination {
            anchor_number: 10_001,
            browser_id: 1,
            target: DeliveryTarget::Browser,
        };
        assert_eq!(
            stalest(vec![(at(1), 10), (elsewhere, 20)], 8),
            vec![at(1), elsewhere]
        );
    }

    #[test]
    fn the_budget_is_a_ceiling_not_a_quota() {
        assert_eq!(stalest(vec![(at(1), 10)], 8), vec![at(1)]);
        assert!(stalest(Vec::new(), 8).is_empty());
    }

    /// The salt of every push in a pass comes off the same bytes, so a sender key that
    /// was those bytes would be recoverable from a header if the hash ever were.
    #[test]
    fn the_pass_sender_key_is_not_the_pass_bytes() {
        let pass_bytes = [5u8; 32];
        let sender = pass_sender(&pass_bytes);
        assert_ne!(&sender.to_bytes()[..], &pass_bytes[..]);
    }

    #[test]
    fn two_passes_do_not_share_a_sender_key() {
        assert_ne!(
            pass_sender(&[5u8; 32]).to_bytes(),
            pass_sender(&[6u8; 32]).to_bytes()
        );
    }
}
