//! Linking an installed web app's browser entry to another entry of the same identity on
//! the same device.
//!
//! On iOS a Home Screen app and Safari keep separate storage, so they sign in as separate
//! browser entries, and an app the user has installed there is notified twice: once
//! through its own subscription and once through ICP Alerts or Safari's. Nothing tells the
//! canister the two entries are one phone, but the user can show it by opening Safari from
//! the installed app. The app registers a one-shot key on its entry, the page it opens in
//! Safari signs a claim with that key, and the app's entry then points at Safari's. An
//! app's notification skips an entry while an installed web app on an entry pointing at
//! it can deliver it, or its app disabled it. A device keeps one link per frontend of an
//! app: linking a new installed web app unlinks the entry of one removed before it.

use crate::browser_key;
use crate::notifications::dispatch;
use crate::state::{storage_borrow, storage_borrow_mut};
use crate::storage::anchor::Anchor;
use candid::Principal;
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, ApplicationNumber, BrowserId, PublicKey, SameDeviceLinkStatus, Timestamp,
};
use std::collections::BTreeSet;

/// How long a one-shot key stays claimable: long enough to switch to Safari and sign in
/// there, short enough that a link left in a browser history is of little use.
const PENDING_SAME_DEVICE_LINK_LIFETIME_NS: u64 = 30 * 60 * 1_000_000_000;

/// Why a claim was refused.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum ClaimRefusal {
    /// No other entry of the identity awaits the key, or its time is up.
    NoPendingLink,
    /// The claiming entry already points at another entry.
    ClaimerAlreadyLinked,
    /// The entry that registered the key is already pointed at by another entry.
    BrowserAlreadyLinked,
    /// The claiming entry holds an installed web app subscription of its own.
    ClaimerHasInstalledApp,
    /// The signature is not the key's over this claim.
    InvalidSignature,
    StorageError(String),
}

/// Whether `browser_id`'s entry of `anchor` is linked to an entry the identity still has,
/// or whether another entry of the identity may deliver notifications at `now_ns`.
/// `NoOtherPath` whatever the entries hold where the identity has not allowed the app
/// numbered `application_number` to notify it, so an app is told nothing about the
/// identity's other devices without that consent.
pub(crate) fn status(
    anchor: &Anchor,
    browser_id: BrowserId,
    application_number: ApplicationNumber,
    now_ns: Timestamp,
) -> SameDeviceLinkStatus {
    let is_allowed = storage_borrow(|storage| {
        storage
            .anchor_application_config(anchor.anchor_number(), application_number)
            .is_some_and(|config| config.notifications_allowed())
    });
    if !is_allowed {
        return SameDeviceLinkStatus::NoOtherPath;
    }
    if anchor.same_device_browser_of(browser_id).is_some() {
        return SameDeviceLinkStatus::Linked;
    }
    let is_other_path = storage_borrow(|storage| {
        anchor.browsers().iter().any(|other_browser| {
            other_browser.id != browser_id && dispatch::can_wake(storage, other_browser, now_ns)
        })
    });
    if is_other_path {
        return SameDeviceLinkStatus::OtherPathMayDeliver;
    }
    SameDeviceLinkStatus::NoOtherPath
}

/// Registers `link_public_key` on `browser_id`'s entry of `anchor_number` until
/// [`PENDING_SAME_DEVICE_LINK_LIFETIME_NS`] after `now_ns`, replacing any earlier one.
pub(crate) fn prepare(
    anchor_number: AnchorNumber,
    browser_id: BrowserId,
    link_public_key: PublicKey,
    now_ns: Timestamp,
) -> Result<(), String> {
    if !browser_key::is_p256_public_key(&link_public_key) {
        return Err("link_public_key is not a P-256 public key".to_string());
    }
    let mut anchor =
        storage_borrow(|storage| storage.read(anchor_number)).map_err(|err| format!("{err}"))?;
    anchor.start_same_device_link(
        browser_id,
        link_public_key,
        now_ns.saturating_add(PENDING_SAME_DEVICE_LINK_LIFETIME_NS),
    )?;
    storage_borrow_mut(|storage| storage.write(anchor)).map_err(|err| format!("{err}"))
}

/// Links the entry of `anchor` whose pending link holds `link_public_key` to
/// `claimer`'s, the entry of `anchor` that `claimer_principal` signs as, where
/// `signature` is that key's over the claim.
///
/// Links never chain: the claimer must point at nothing and the entry that registered
/// the key must be pointed at by nothing. And the claimer must hold no installed web
/// app of its own, for any frontend of any app, which an entry shares with its installed
/// apps only where one browser runs both, so there is no second entry to link. An entry
/// that is skipped therefore never has an installed web app of its own that would compete
/// with the one skipping it.
///
/// A device needs one link per frontend of an app: an installed web app removed from the
/// device keeps its entry and its link. So an entry linked to `claimer`'s that holds an
/// installed web app of a frontend the linking entry holds one of too is unlinked in the
/// same write.
pub(crate) fn claim(
    mut anchor: Anchor,
    claimer: BrowserId,
    claimer_principal: Principal,
    link_public_key: &PublicKey,
    signature: &[u8],
    now_ns: Timestamp,
) -> Result<(), ClaimRefusal> {
    let linking_browser_id = anchor
        .browser_awaiting_same_device_link(link_public_key, claimer, now_ns)
        .ok_or(ClaimRefusal::NoPendingLink)?;
    let is_claimer_linked = anchor
        .browser(claimer)
        .is_some_and(|browser| browser.same_device_browser_id.is_some());
    if is_claimer_linked {
        return Err(ClaimRefusal::ClaimerAlreadyLinked);
    }
    let is_linking_pointed_at = anchor
        .browsers()
        .iter()
        .any(|browser| browser.same_device_browser_id == Some(linking_browser_id));
    if is_linking_pointed_at {
        return Err(ClaimRefusal::BrowserAlreadyLinked);
    }
    let is_claimer_installed_app = storage_borrow(|storage| {
        storage.holds_pwa_webpush_subscription(anchor.anchor_number(), claimer)
    });
    if is_claimer_installed_app {
        return Err(ClaimRefusal::ClaimerHasInstalledApp);
    }
    let is_signed = browser_key::verify_same_device_link_claim(
        link_public_key,
        signature,
        anchor.anchor_number(),
        claimer_principal.as_slice(),
    );
    if !is_signed {
        return Err(ClaimRefusal::InvalidSignature);
    }
    let superseded_browser_ids = find_superseded_links(&anchor, linking_browser_id, claimer);
    anchor.link_same_device(linking_browser_id, claimer, &superseded_browser_ids);
    storage_borrow_mut(|storage| storage.write(anchor))
        .map_err(|err| ClaimRefusal::StorageError(format!("{err}")))
}

/// The entries of `anchor` other than `linking_browser_id`'s, linked to
/// `same_device_browser_id`'s, that hold an installed web app subscription of an app's
/// frontend that `linking_browser_id`'s entry holds one of too.
fn find_superseded_links(
    anchor: &Anchor,
    linking_browser_id: BrowserId,
    same_device_browser_id: BrowserId,
) -> BTreeSet<BrowserId> {
    storage_borrow(|storage| {
        let linking_frontends =
            storage.pwa_webpush_frontends_of_browser(anchor.anchor_number(), linking_browser_id);
        let mut superseded_browser_ids = BTreeSet::new();
        for browser in anchor.browsers() {
            if browser.id == linking_browser_id
                || browser.same_device_browser_id != Some(same_device_browser_id)
            {
                continue;
            }
            let browser_frontends =
                storage.pwa_webpush_frontends_of_browser(anchor.anchor_number(), browser.id);
            if !browser_frontends.is_disjoint(&linking_frontends) {
                superseded_browser_ids.insert(browser.id);
            }
        }
        superseded_browser_ids
    })
}
