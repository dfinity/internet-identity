//! Checking, after a sign-in, that the ways an app's notifications reach a device still
//! work.
//!
//! Nothing tells Internet Identity that a Home Screen app was deleted or a subscription
//! broke, so each subscription counts the pushes nobody fetched (see
//! [`crate::notifications::webpush::unanswered`]). A browser reads those counts for one
//! path, probes the path with Internet Identity's own notification where they are past
//! the limit, and removes an installed web app that does not answer, so its app's
//! notifications go back to where the browser itself delivers them.

use crate::notifications::backlog::identity::IdentityWakeUpPath;
use crate::notifications::dispatch;
use crate::notifications::webpush;
use crate::notifications::webpush::pwa_subscription;
use crate::notifications::webpush::rotation::{Destination, RegistrationId};
use crate::notifications::webpush::unanswered;
use crate::origin::ApplicationOrigin;
use crate::state::storage_borrow;
use crate::storage::anchor::{Anchor, Browser, ReadySubscription};
use crate::storage::PwaSubscriptionHolder;
use candid::Principal;
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, ApplicationNumber, BrowserId, IdentityForNotification, InstalledAppsPath,
    NotificationPath, NotificationPathStatus, SubscriptionDeliveryStatus, Timestamp,
};

/// A way an identity's notifications reach the calling browser's device, with the app
/// already resolved.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum DevicePath {
    /// Where the calling browser's wake-ups go.
    ThisBrowser,
    /// The installed web apps of the app numbered `application_number` on the entries
    /// linked to the calling browser's as sharing its device, that deliver for the account
    /// `account_principal` names. `None` for an app nobody has signed in to, which has
    /// none.
    ///
    /// Not the calling browser's own entry: an installed app sharing it holds the session
    /// a sign-in from the browser replaces, so it could not answer a probe sent now. It
    /// checks its own path when it next starts.
    InstalledApps {
        application_number: Option<ApplicationNumber>,
        account_principal: Principal,
    },
}

impl DevicePath {
    /// Resolves `path`'s app to the number it is stored under.
    pub(crate) fn resolve(path: NotificationPath) -> Result<Self, String> {
        match path {
            NotificationPath::ThisBrowser => Ok(DevicePath::ThisBrowser),
            NotificationPath::InstalledApps(InstalledAppsPath {
                origin,
                account_principal,
            }) => Ok(DevicePath::InstalledApps {
                application_number: application_number_of(&origin)?,
                account_principal,
            }),
        }
    }
}

/// The number the app at `origin` is stored under. `None` for an app nobody has signed in
/// to; an error for a string that is not an origin.
pub(crate) fn application_number_of(origin: &str) -> Result<Option<ApplicationNumber>, String> {
    let origin = ApplicationOrigin::try_from(origin)?;
    Ok(storage_borrow(|storage| {
        storage.lookup_application_number_with_origin(&origin.as_str().to_string())
    }))
}

/// Why a probe was not sent.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum ProbeRefusal {
    /// Nothing on the path can be woken now.
    NotReachable,
    /// The identity's share of the backlog for its own notifications is full.
    BacklogFull,
}

/// One subscription on a path that can deliver now.
struct PathSubscription {
    /// The entry it is woken through.
    browser_id: BrowserId,
    /// Which registration it is, which is what a probe is recorded on.
    registration: RegistrationId,
    subscription: ReadySubscription,
    /// How a probe reaches it.
    wake_up_path: IdentityWakeUpPath,
}

/// The subscriptions on `path`, for `browser_id` of `anchor`, and their unanswered
/// pushes as at `now_ns`.
pub(crate) fn path_status(
    anchor: &Anchor,
    browser_id: BrowserId,
    path: DevicePath,
    now_ns: Timestamp,
) -> NotificationPathStatus {
    NotificationPathStatus {
        subscriptions: path_subscriptions(anchor, browser_id, path, now_ns)
            .iter()
            .map(|on_path| SubscriptionDeliveryStatus {
                unanswered_pushes: webpush::unanswered_pushes_of(&on_path.subscription, now_ns),
                probe_sent_at_ns: on_path.subscription.probe_sent_at_ns,
            })
            .collect(),
    }
}

/// Sends Internet Identity's own notification about `identity` through every
/// subscription on `path` for `browser_id` of `anchor` that has no probe outstanding.
///
/// Every subscription is tried: one the backlog refused leaves the others probed, so the
/// refusal is answered only where none of them was.
pub(crate) fn probe(
    anchor: &Anchor,
    browser_id: BrowserId,
    path: DevicePath,
    identity: &IdentityForNotification,
    now_ns: Timestamp,
) -> Result<(), ProbeRefusal> {
    let on_path = path_subscriptions(anchor, browser_id, path, now_ns);
    if on_path.is_empty() {
        return Err(ProbeRefusal::NotReachable);
    }
    let mut num_refused = 0usize;
    let num_on_path = on_path.len();
    for one in on_path {
        if let Err(refusal) = probe_one(anchor.anchor_number(), one, identity, now_ns) {
            ic_cdk::println!("ERROR: A probe was not sent: {refusal:?}");
            num_refused = num_refused.saturating_add(1);
        }
    }
    if num_refused == num_on_path {
        return Err(ProbeRefusal::BacklogFull);
    }
    Ok(())
}

/// Sends Internet Identity's own notification about `identity` to the installed web app
/// `holder` names, where it can be woken now for the account `account_principal` names
/// and has no probe outstanding.
pub(crate) fn probe_installed_app(
    anchor: &Anchor,
    holder: PwaSubscriptionHolder,
    account_principal: Principal,
    identity: &IdentityForNotification,
    now_ns: Timestamp,
) -> Result<(), ProbeRefusal> {
    let browser = anchor
        .browser(holder.browser_id)
        .ok_or(ProbeRefusal::NotReachable)?;
    let Some(one) = installed_app_on_path(
        browser,
        holder.anchor_number,
        holder.application_number,
        now_ns,
    )
    .filter(|one| {
        one.wake_up_path
            == IdentityWakeUpPath::InstalledApp {
                application_number: holder.application_number,
                account_principal,
            }
    }) else {
        return Err(ProbeRefusal::NotReachable);
    };
    probe_one(holder.anchor_number, one, identity, now_ns)
}

/// Submits a probe through `one`, unless one is outstanding. It is recorded there only
/// once the push service accepts it: one still waiting for its pass, or refused, reached
/// no device and is no evidence that nothing answers.
fn probe_one(
    anchor_number: AnchorNumber,
    one: PathSubscription,
    identity: &IdentityForNotification,
    now_ns: Timestamp,
) -> Result<(), ProbeRefusal> {
    if unanswered::is_probe_outstanding(&one.subscription, now_ns) {
        return Ok(());
    }
    let is_admitted = dispatch::probe_path(
        anchor_number,
        one.browser_id,
        identity.clone(),
        one.wake_up_path,
        now_ns,
    );
    if !is_admitted {
        return Err(ProbeRefusal::BacklogFull);
    }
    Ok(())
}

/// Removes the installed web app subscriptions of the app numbered `application_number`
/// on the entries of `anchor` linked to `browser_id`'s that deliver for the account
/// `account_principal` names, where they can deliver now and a probe through them was
/// never fetched in [`unanswered::MIN_PROBE_ANSWER_TIME_NS`]. One that answered its probe,
/// or whose probe is younger than that, is kept. `None` for an app nobody has signed in
/// to, which has none.
///
/// Every one is tried, so a failure to remove one leaves the others removed; failures
/// are logged.
pub(crate) fn remove_unanswered_installed_apps(
    anchor: &Anchor,
    browser_id: BrowserId,
    application_number: Option<ApplicationNumber>,
    account_principal: Principal,
    now_ns: Timestamp,
) {
    let path = DevicePath::InstalledApps {
        application_number,
        account_principal,
    };
    for on_path in path_subscriptions(anchor, browser_id, path, now_ns) {
        // Only a probe nobody fetched, in the time it takes to answer one, is evidence: a
        // count raised again by an ordinary push after the installed app answered its
        // probe is not, and neither is a probe the push service accepted a moment ago.
        if !unanswered::has_probe_had_its_answer_time(&on_path.subscription, now_ns) {
            continue;
        }
        let RegistrationId::Pwa(holder) = on_path.registration else {
            continue;
        };
        if let Err(err) = pwa_subscription::write(holder, None, None) {
            ic_cdk::println!("ERROR: Failed to remove an unanswered installed app: {err}");
        }
    }
}

/// The subscriptions on `path` for `browser_id` of `anchor` that can deliver now.
fn path_subscriptions(
    anchor: &Anchor,
    browser_id: BrowserId,
    path: DevicePath,
    now_ns: Timestamp,
) -> Vec<PathSubscription> {
    let Some(browser) = anchor.browser(browser_id) else {
        return Vec::new();
    };
    match path {
        DevicePath::ThisBrowser => routed_on_path(browser, anchor.anchor_number(), now_ns)
            .into_iter()
            .collect(),
        DevicePath::InstalledApps {
            application_number: None,
            ..
        } => Vec::new(),
        DevicePath::InstalledApps {
            application_number: Some(application_number),
            account_principal,
        } => {
            let mut on_path = Vec::new();
            for entry in same_device_entries(anchor, browser) {
                on_path.extend(
                    installed_app_on_path(
                        entry,
                        anchor.anchor_number(),
                        application_number,
                        now_ns,
                    )
                    .filter(|one| {
                        one.wake_up_path
                            == IdentityWakeUpPath::InstalledApp {
                                application_number,
                                account_principal,
                            }
                    }),
                );
            }
            on_path
        }
    }
}

/// Where `browser`'s wake-ups go now, as a subscription on a path.
fn routed_on_path(
    browser: &Browser,
    anchor_number: AnchorNumber,
    now_ns: Timestamp,
) -> Option<PathSubscription> {
    let (subscription, target) =
        storage_borrow(|storage| dispatch::routed_subscription(storage, browser, now_ns))?;
    Some(PathSubscription {
        browser_id: browser.id,
        registration: Destination {
            anchor_number,
            browser_id: browser.id,
            target,
        }
        .registration(),
        subscription,
        wake_up_path: IdentityWakeUpPath::Routed,
    })
}

/// `browser`'s installed web app of the app numbered `application_number`, as a
/// subscription on a path, where it can deliver now.
fn installed_app_on_path(
    browser: &Browser,
    anchor_number: AnchorNumber,
    application_number: ApplicationNumber,
    now_ns: Timestamp,
) -> Option<PathSubscription> {
    let (subscription, account_principal) = storage_borrow(|storage| {
        dispatch::installed_app_subscription(
            storage,
            anchor_number,
            application_number,
            browser,
            now_ns,
        )
    })?;
    Some(PathSubscription {
        browser_id: browser.id,
        registration: RegistrationId::Pwa(PwaSubscriptionHolder {
            anchor_number,
            application_number,
            browser_id: browser.id,
        }),
        subscription,
        wake_up_path: IdentityWakeUpPath::InstalledApp {
            application_number,
            account_principal,
        },
    })
}

/// Every entry of `anchor` linked to `browser` as sharing its device.
fn same_device_entries<'a>(anchor: &'a Anchor, browser: &'a Browser) -> Vec<&'a Browser> {
    anchor
        .browsers()
        .iter()
        .filter(|other| other.id != browser.id && other.same_device_browser_id == Some(browser.id))
        .collect()
}
