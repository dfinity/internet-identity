//! Notify Internet Identity users from a Rust canister.
//!
//! One call to send; the library owns the ids, the batching, the retries, the
//! content Internet Identity pulls back, and the delivery metrics.
//!
//! ```ignore
//! use candid::Principal;
//! use ic_cdk::api::msg_caller;
//! use ic_cdk::update;
//! use identity_notifications as notifications;
//! use notifications::Notification;
//!
//! notifications::endpoints!();
//!
//! #[update]
//! fn send_message(to: Principal, text: String) {
//!     let from = msg_caller();
//!     // ... store the message ...
//!     notifications::send(
//!         to,
//!         Notification {
//!             title: "New message".into(),
//!             body: text,
//!             url: Some(format!("https://chat.example.com/chats/{from}")),
//!             key: Some(format!("chat-{from}")),
//!             ..Default::default()
//!         },
//!     );
//! }
//! ```
//!
//! Configured by environment variables: `notification_sender`,
//! `notification_origin`, and optionally `notification_capacity_mb`.

pub mod internal;
pub mod types;

use candid::Principal;
use ic_cdk::api::{msg_caller_info_data, msg_caller_info_signer, time};
use ic_cdk::call::Call;
use ic_cdk_timers::{clear_timer, set_timer, TimerId};
use ic_stable_structures::memory_manager::MemoryManager;
use ic_stable_structures::DefaultMemoryImpl;
use internal::config::{self, Config};
use internal::flush::{self, Outcome};
use internal::ii;
pub use internal::store::Memories;
use internal::store::{Entry, Event, Store};
use std::cell::RefCell;
use std::time::Duration;
pub use types::{
    Bucket, Content, Metrics, Misconfigured, MisconfiguredSince, Notification, NotificationId,
    SenderInfo, Urgency,
};

/// How long sends accumulate before one call carries them. Urgency decides
/// lane order, not call timing, so everything waits for it.
const COALESCE: Duration = Duration::from_secs(2);

/// What Internet Identity accepts in one call, until `TooManyNotifications`
/// says otherwise.
const BATCH_GUESS: usize = 1_000;

/// Internet Identity's own default, mirrored so the outbox orders and expires
/// notifications by the window they actually have.
const II_DEFAULT_RETENTION_NS: u64 = 5 * 60 * 1_000_000_000;

thread_local! {
    /// Everything durable is in stable memory; what is here is the handle to
    /// it, plus two flags that only matter within one execution round.
    static STORE: RefCell<Option<Store>> = const { RefCell::new(None) };
    static SENDER: RefCell<Option<Principal>> = const { RefCell::new(None) };
    /// The wake-up that is set, and when it fires, so a nearer one can take
    /// its place.
    static ARMED: RefCell<Option<(TimerId, u64)>> = const { RefCell::new(None) };
    static FLUSHING: RefCell<bool> = const { RefCell::new(false) };
}

/// Hands the library the stable memories it keeps everything in.
///
/// Call it from `#[init]` **and** `#[post_upgrade]`. Nothing is serialised or
/// restored on upgrade: the maps are already where they were.
///
/// ```ignore
/// use ic_cdk::{init, post_upgrade};
/// use ic_stable_structures::memory_manager::MemoryManager;
///
/// thread_local! {
///     static MEMORIES: RefCell<MemoryManager<DefaultMemoryImpl>> =
///         RefCell::new(MemoryManager::init(DefaultMemoryImpl::default()));
/// }
///
/// #[init]
/// #[post_upgrade]
/// fn init() {
///     MEMORIES.with_borrow(|manager| {
///         notifications::init(
///             manager,
///             Memories {
///                 entries: MemoryId::new(10),
///                 content: MemoryId::new(11),
///                 metrics: MemoryId::new(12),
///             },
///         )
///     });
/// }
/// ```
pub fn init(manager: &MemoryManager<DefaultMemoryImpl>, memories: Memories) {
    let mut store = Store::new(manager, memories);
    if store.batch_limit() <= 1 {
        store.set_batch_limit(BATCH_GUESS);
    }

    STORE.set(Some(store));

    // A timer does not survive an upgrade, so an outbox that did needs one.
    // A fresh install has nothing queued, which arming answers with no timer.
    arm();
}

fn with_store<T>(f: impl FnOnce(&mut Store) -> T) -> Option<T> {
    STORE.with_borrow_mut(|store| store.as_mut().map(f))
}

/// Sends a notification, or updates the one this recipient's `key` already
/// names.
///
/// Never traps and never awaits, so it cannot fail the call it is made from.
/// What goes wrong afterwards shows up in [`metrics`].
pub fn send(recipient: Principal, notification: Notification) {
    let now = time();

    // The app asked, so the hour and the funnel it asked in count the ask,
    // whether or not there is anything to send it through.
    let funnel = with_store(|store| {
        let funnel = store.funnel_of(&notification);
        store.record(now, Event::Queued, 1);
        store.record_in_funnel(funnel, Event::Queued, 1);
        funnel
    })
    .unwrap_or_default();
    let dropped = |store: &mut Store| {
        store.record(now, Event::Dropped, 1);
        store.record_in_funnel(funnel, Event::Dropped, 1);
    };

    let config = match configured() {
        Ok(config) => config,
        Err(why) => {
            note(why, now);
            with_store(dropped);
            return;
        }
    };

    if sender(&config).is_none() {
        note(Misconfigured::Sender, now);
        with_store(dropped);
        return;
    }

    let added = with_store(|store| {
        let added = store.add(
            recipient,
            notification,
            now + II_DEFAULT_RETENTION_NS,
            config.capacity_bytes as u64,
            now,
        );
        if added.is_none() {
            dropped(store);
        }
        added
    });

    // Refused by the ceiling, or `init` was never called: nothing to wake for.
    if let Some(Some(_)) = added {
        arm();
    }
}

/// Sends through this canister, and trusts it on a pull, in place of the
/// `notification_sender` variable; `None` goes back to the variable. For a
/// test canister retargeting Internet Identity at runtime, and not remembered
/// across an upgrade.
pub fn set_sender(sender: Option<Principal>) {
    SENDER.set(sender);
}

/// Records that somebody acted on the notification a channel showed them.
pub fn notification_opened(id: NotificationId) {
    if sender_info().is_none() {
        return;
    }
    with_store(|store| store.mark_opened(id));
}

/// Drops a funnel's row. What it counted goes with it, and notifications still
/// carrying it are counted in their hour alone.
pub fn forget_funnel(name: &str) {
    with_store(|store| store.forget_funnel(name));
}

/// This notification no longer needs anyone's attention.
pub fn dismiss(recipient: Principal, key: &str) {
    with_store(|store| store.dismiss(recipient, key));
}

/// The delivery pipeline, hour by hour.
pub fn metrics() -> Metrics {
    with_store(|store| Metrics {
        hours: store.hours(),
        funnels: store.funnels(),
        backlog: store.backlog(),
        misconfigured: store.misconfigured(),
    })
    .unwrap_or(Metrics {
        hours: Vec::new(),
        funnels: Vec::new(),
        backlog: 0,
        misconfigured: None,
    })
}

/// Answers Internet Identity's content pull, for an app that would rather
/// declare the endpoint itself than use [`endpoints!`].
pub fn notification_content(id: NotificationId) -> Option<Content> {
    sender_info()?;
    with_store(|store| store.content_of(id)).flatten()
}

/// Records Internet Identity's receipt.
pub fn notification_received(id: NotificationId) {
    if sender_info().is_none() {
        return;
    }
    let now = time();
    with_store(|store| {
        if store.holds(id) && store.mark_received(id) {
            store.record(now, Event::Received, 1);
        }
    });
}

/// The two endpoints Internet Identity calls: a `query` for the content and an
/// `update` for the receipt.
#[macro_export]
macro_rules! endpoints {
    () => {
        // The type is imported rather than named in the signature, and
        // `ic_cdk` is left unqualified, because `ic_cdk::export_candid!`
        // re-parses these signatures and chokes on a leading `::` — which is
        // exactly what `$crate` expands to.
        use $crate::types::Content as IiNotificationContent;

        #[ic_cdk::query]
        fn _internet_identity_notification_content(id: u64) -> Option<IiNotificationContent> {
            $crate::notification_content(id)
        }

        #[ic_cdk::update]
        fn _internet_identity_notification_received(id: u64) {
            $crate::notification_received(id)
        }

        #[ic_cdk::update]
        fn _internet_identity_notification_opened(id: u64) {
            $crate::notification_opened(id)
        }
    };
}

fn configured() -> Result<Config, Misconfigured> {
    config::read()
}

/// Who this canister sends through, and whose caller info it trusts: what
/// [`set_sender`] was given, or the configured variable.
fn sender(config: &Config) -> Option<Principal> {
    SENDER.with_borrow(|sender| *sender).or(config.sender)
}

fn note(why: Misconfigured, now: u64) {
    with_store(|store| store.note(why, now));
}

/// Internet Identity signed this call's caller info, and it names our own
/// origin. Anything else is not ours to answer.
fn sender_info() -> Option<SenderInfo> {
    let config = configured().ok()?;
    if Some(msg_caller_info_signer()?) != sender(&config) {
        return None;
    }

    let info = internal::caller_info::decode(&msg_caller_info_data())?;
    (internal::origin::fold(&info.origin) == internal::origin::fold(&config.origin)).then_some(info)
}

/// Sets the wake-up for when the outbox next has something to do: the coalesce
/// window where anything is waiting, the due time where an entry is parked for
/// later, and no timer at all for an empty outbox. A call in flight carries its
/// own wake-up, and one already set for later gives way to a nearer one.
fn arm() {
    if FLUSHING.with_borrow(|flushing| *flushing) {
        return;
    }
    let Some(due) = with_store(|store| store.next_due()).flatten() else {
        return;
    };

    let now = time();
    let at = due.max(now + COALESCE.as_nanos() as u64);
    ARMED.with_borrow_mut(|armed| {
        if armed.is_some_and(|(_, when)| when <= at) {
            return;
        }
        if let Some((timer, _)) = armed.take() {
            clear_timer(timer);
        }
        *armed = Some((
            set_timer(Duration::from_nanos(at.saturating_sub(now)), flush_once()),
            at,
        ));
    });
}

async fn flush_once() {
    ARMED.set(None);

    let config = match configured() {
        Ok(config) => config,
        Err(why) => {
            note(why, time());
            return;
        }
    };

    let Some(sender) = sender(&config) else {
        note(Misconfigured::Sender, time());
        return;
    };

    if with_store(|store| store.is_seeded()) == Some(false) {
        seed_ids().await;
    }

    let now = time();
    let batch = with_store(|store| {
        store.sweep(now);
        store.promote(now);
        let batch = store.take(store.batch_limit());
        let (live, _expired) = store.sendable(batch, now);
        live
    })
    .unwrap_or_default();

    if batch.is_empty() {
        // Nothing is sendable yet: a parked entry still has its turn coming.
        arm();
        return;
    }

    FLUSHING.set(true);
    let outcome = send_batch(sender, &config.origin, &batch).await;
    FLUSHING.set(false);

    match &outcome {
        Outcome::TooMany(limit) => {
            with_store(|store| store.set_batch_limit((*limit).max(1)));
        }
        Outcome::NotAuthorized => note(Misconfigured::NotAuthorizedSender, now),
        _ => {}
    }

    with_store(|store| flush::apply(store, batch, outcome, now));

    arm();
}

async fn send_batch(sender: Principal, origin: &str, batch: &[Entry]) -> Outcome {
    let arg = flush::arg(origin, batch);

    let response = Call::unbounded_wait(sender, "app_send_notification")
        .with_arg(&arg)
        .await;

    match response.map(|reply| reply.candid::<ii::SendNotificationResult>()) {
        Ok(Ok(ii::SendNotificationResult::Ok(response))) => {
            Outcome::Answered(response.not_accepted)
        }
        Ok(Ok(ii::SendNotificationResult::Err(error))) => match error {
            ii::SendNotificationError::TooManyNotifications { limit } => {
                Outcome::TooMany(limit as usize)
            }
            ii::SendNotificationError::NoSuchSender => Outcome::NotAuthorized,
            ii::SendNotificationError::InternalCanisterError(_) => Outcome::Unreachable,
        },
        Ok(Err(_)) | Err(_) => Outcome::Unreachable,
    }
}

/// A random offset for counter ids, taken once.
async fn seed_ids() {
    let reply = Call::unbounded_wait(Principal::management_canister(), "raw_rand").await;

    if let Ok(Ok(bytes)) = reply.map(|reply| reply.candid::<Vec<u8>>()) {
        with_store(|store| store.seed(&bytes));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn principal(n: u64) -> Principal {
        Principal::from_slice(&n.to_be_bytes())
    }

    fn configured_as(sender: Option<Principal>) -> Config {
        Config {
            sender,
            origin: "https://app.example.com".to_string(),
            capacity_bytes: 1_024,
        }
    }

    #[test]
    fn an_override_answers_for_the_variable_and_gives_it_back() {
        let from_variable = principal(1);
        let from_override = principal(2);
        let config = configured_as(Some(from_variable));

        assert_eq!(sender(&config), Some(from_variable));

        set_sender(Some(from_override));
        assert_eq!(sender(&config), Some(from_override));

        set_sender(None);
        assert_eq!(sender(&config), Some(from_variable));
    }

    #[test]
    fn an_override_is_enough_on_its_own() {
        let config = configured_as(None);
        assert_eq!(sender(&config), None, "nothing to send through");

        set_sender(Some(principal(3)));
        assert_eq!(sender(&config), Some(principal(3)));
        set_sender(None);
    }
}
