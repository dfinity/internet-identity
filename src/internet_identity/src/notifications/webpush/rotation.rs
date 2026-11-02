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
use std::collections::HashSet;

use super::payload;
use super::validation::{PayloadIkm, PayloadKeyId};
use crate::notifications::dispatch::DeliveryTarget;
use crate::notifications::installed_app::NotificationAppId;
use crate::state::{self, storage_borrow, storage_borrow_mut};
use crate::storage::anchor::{ReadySubscription, WebPushSubscription};
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

impl Destination {
    /// The registration this destination refreshes, which is not the destination itself:
    /// a Home Screen app's record is shared by every browser pointing at it, so two of
    /// them in one pass are one thing to refresh and one scalar multiplication, not two.
    pub(crate) fn registration(self) -> (Option<NotificationAppId>, AnchorNumber, BrowserId) {
        match self.target {
            DeliveryTarget::App(app_id) => (Some(app_id), 0, 0),
            DeliveryTarget::Browser => (None, self.anchor_number, self.browser_id),
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
    super::randomness::secret_key_from(&scalar)
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

/// Derives fresh keying material against `sender` and writes it back.
///
/// Silent on a destination that has gone between planning and here: the pass has nothing
/// to refresh and the wake-up it was for will not seal either.
pub(crate) fn refresh(
    destination: Destination,
    sender: &p256::SecretKey,
    now_ns: Timestamp,
) -> Result<Option<SealingKeys>, String> {
    let Some(mut registered) = read(destination) else {
        return Ok(None);
    };
    let (payload_ikm, payload_keyid) =
        payload::derive_ikm(sender, &registered.subscriber_key, &registered.auth)?;
    registered.payload_ikm = payload_ikm;
    registered.payload_keyid = payload_keyid;
    registered.last_rotated_ns = now_ns;
    let refreshed = SealingKeys::from(&registered);
    write(destination, registered)?;
    Ok(Some(refreshed))
}

/// Reads the registration `target` names, rather than the one this browser happens to
/// deliver through now. A browser that holds its own and also points at an app would
/// otherwise be read from one and written to the other.
fn read(destination: Destination) -> Option<ReadySubscription> {
    match destination.target {
        DeliveryTarget::App(app_id) => storage_borrow(|storage| storage.notification_app(app_id))?
            .webpush_subscription
            .and_then(WebPushSubscription::into_ready),
        DeliveryTarget::Browser => state::anchor(destination.anchor_number)
            .webpush_subscription(destination.browser_id)
            .and_then(WebPushSubscription::ready)
            .cloned(),
    }
}

fn write(destination: Destination, registered: ReadySubscription) -> Result<(), String> {
    match destination.target {
        // Without stamping the record as used: a refresh is this canister's work, and an
        // app that has not called is how a deleted one is told apart from a live one.
        DeliveryTarget::App(app_id) => storage_borrow_mut(|storage| {
            storage.refresh_notification_app_subscription(
                app_id,
                Some(WebPushSubscription::Ready(registered)),
            )
        })
        .map_err(|err| format!("{err}")),
        DeliveryTarget::Browser => {
            let mut anchor = storage_borrow(|storage| storage.read(destination.anchor_number))
                .map_err(|err| format!("{err}"))?;
            anchor.set_webpush_subscription(
                destination.browser_id,
                Some(WebPushSubscription::Ready(registered)),
            );
            storage_borrow_mut(|storage| storage.write(anchor)).map_err(|err| format!("{err}"))
        }
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
