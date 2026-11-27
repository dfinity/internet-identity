//! Turning a caller's notification batch into one the notification logic can act on:
//! bounded and collapsed to one entry per recipient and id.

use internet_identity_interface::internet_identity::types::Notification;
use std::collections::HashMap;

/// The message a refusal carries while this deployment does not notify.
pub(crate) const NOT_ENABLED_MESSAGE: &str = "notifications are not enabled";

/// Bounds the work one message asks for, not what it could enqueue: a batch
/// may name one pair many times. Fixed, never a capacity signal.
pub const MAX_NOTIFICATIONS_PER_APP_CALL: usize = 1_000;

/// More than one message's worth of work was asked for. Carries the limit so the
/// endpoint can say what it is without reaching for the constant itself.
pub struct TooManyNotifications {
    pub limit: usize,
}

/// What one `app_send_notification` call may enqueue: at most
/// `MAX_NOTIFICATIONS_PER_APP_CALL` entries, each `(recipient, id)` named once.
///
/// A plain `Vec` out rather than a type of its own: the bound is an admission decision
/// and the collapse is what the sender asked for, and neither is something a later
/// caller has to be held to.
pub fn admit_batch(
    notifications: Vec<Notification>,
) -> Result<Vec<Notification>, TooManyNotifications> {
    if notifications.len() > MAX_NOTIFICATIONS_PER_APP_CALL {
        return Err(TooManyNotifications {
            limit: MAX_NOTIFICATIONS_PER_APP_CALL,
        });
    }
    Ok(dedup_last_wins(notifications))
}

/// Collapses each `(recipient, id)` to its last entry, in the place the first
/// held: a re-send replaces a pending notification rather than queueing behind
/// it.
fn dedup_last_wins(notifications: Vec<Notification>) -> Vec<Notification> {
    let mut placed = HashMap::new();
    let mut kept: Vec<Notification> = Vec::with_capacity(notifications.len());
    for notification in notifications {
        let pair = (notification.recipient, notification.id);
        match placed.get(&pair) {
            Some(&index) => kept[index] = notification,
            None => {
                placed.insert(pair, kept.len());
                kept.push(notification);
            }
        }
    }
    kept
}

/// Whether this deployment notifies, for every app at once.
pub fn notifications_enabled() -> bool {
    crate::state::persistent_state(|s| s.notifications_enabled).unwrap_or(false)
}

/// Checks that this deployment notifies. While it does not, returns the error
/// `refusal` builds from the not-enabled message, for an endpoint to answer with.
pub fn check_notifications_enabled<E>(refusal: impl FnOnce(String) -> E) -> Result<(), E> {
    if notifications_enabled() {
        return Ok(());
    }
    Err(refusal(NOT_ENABLED_MESSAGE.to_string()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::origin::ApplicationOrigin;
    use internet_identity_interface::internet_identity::types::Urgency;
    fn set_enabled(enabled: Option<bool>) {
        crate::state::persistent_state_mut(|s| s.notifications_enabled = enabled);
    }

    fn notification(id: u64, recipient: &str, urgency: Urgency) -> Notification {
        Notification {
            id,
            recipient: candid::Principal::from_text(recipient).expect("a principal"),
            expires_at: None,
            urgency: Some(urgency),
        }
    }

    fn admitted(notifications: Vec<Notification>) -> Vec<Notification> {
        set_enabled(Some(true));
        admit_batch(notifications).unwrap_or_else(|_| panic!("within the cap"))
    }

    /// A batch applies in order, so a repeated (recipient, id) keeps the last
    /// entry — the one that supersedes — and the pair appears once.
    #[test]
    fn a_repeated_recipient_and_id_keeps_the_last_entry() {
        let notifications = admitted(vec![
            notification(1, "ryjl3-tyaaa-aaaaa-aaaba-cai", Urgency::Low),
            notification(1, "ryjl3-tyaaa-aaaaa-aaaba-cai", Urgency::High),
        ]);

        assert_eq!(notifications.len(), 1);
        assert_eq!(
            notifications.first().and_then(|kept| kept.urgency.clone()),
            Some(Urgency::High)
        );
    }

    /// One id sent to two recipients is two notifications, not a repeat.
    #[test]
    fn one_id_for_two_recipients_survives_as_two() {
        let notifications = admitted(vec![
            notification(1, "ryjl3-tyaaa-aaaaa-aaaba-cai", Urgency::Normal),
            notification(1, "rrkah-fqaaa-aaaaa-aaaaq-cai", Urgency::Normal),
        ]);

        assert_eq!(notifications.len(), 2);
    }

    /// Surviving entries keep the order they were submitted in.
    #[test]
    fn deduplication_keeps_the_submitted_order() {
        let notifications = admitted(vec![
            notification(1, "ryjl3-tyaaa-aaaaa-aaaba-cai", Urgency::Normal),
            notification(2, "ryjl3-tyaaa-aaaaa-aaaba-cai", Urgency::Normal),
            notification(1, "ryjl3-tyaaa-aaaaa-aaaba-cai", Urgency::High),
        ]);

        let ids: Vec<u64> = notifications.iter().map(|kept| kept.id).collect();
        assert_eq!(ids, vec![1, 2]);
        assert_eq!(
            notifications.first().and_then(|kept| kept.urgency.clone()),
            Some(Urgency::High)
        );
    }

    /// The cap counts what was submitted, not what survives deduplication: a
    /// batch naming one pair many times still costs a message that much work.
    #[test]
    fn the_cap_counts_submitted_entries() {
        set_enabled(Some(true));
        let repeated = notification(1, "ryjl3-tyaaa-aaaaa-aaaba-cai", Urgency::Normal);

        let refused = admit_batch(vec![repeated; MAX_NOTIFICATIONS_PER_APP_CALL + 1]);

        assert_eq!(
            refused.err().map(|TooManyNotifications { limit }| limit),
            Some(MAX_NOTIFICATIONS_PER_APP_CALL)
        );
    }

    #[test]
    fn canonicalizes_every_gateway_to_the_legacy_one() {
        set_enabled(Some(true));
        for origin in [
            "https://abc-cai.icp0.io",
            "https://abc-cai.icp.net",
            "https://abc-cai.ic0.app",
        ] {
            assert_eq!(
                ApplicationOrigin::try_from(origin).unwrap().as_str(),
                "https://abc-cai.ic0.app"
            );
        }
    }

    #[test]
    fn leaves_a_custom_domain_alone() {
        set_enabled(Some(true));
        assert_eq!(
            ApplicationOrigin::try_from("https://oisy.com")
                .unwrap()
                .as_str(),
            "https://oisy.com"
        );
    }

    /// The frontend only remaps a single `[\w-]+(.raw)?` label, so a deeper name is a
    /// different origin to it and must stay a different consent row here.
    #[test]
    fn leaves_a_deeper_subdomain_alone() {
        set_enabled(Some(true));
        assert_eq!(
            ApplicationOrigin::try_from("https://foo.bar.icp0.io")
                .unwrap()
                .as_str(),
            "https://foo.bar.icp0.io"
        );
    }

    #[test]
    fn keeps_the_raw_label_the_frontend_keeps() {
        set_enabled(Some(true));
        assert_eq!(
            ApplicationOrigin::try_from("https://abc-cai.raw.icp0.io")
                .unwrap()
                .as_str(),
            "https://abc-cai.raw.ic0.app"
        );
    }

    /// The gate is asked of the deployment, not of the origin: a well-formed origin
    /// reads as well-formed whether or not anything is notified.
    #[test]
    fn an_origin_is_admitted_while_enabled() {
        set_enabled(Some(true));
        assert!(check_notifications_enabled(|message| message).is_ok());
        assert!(ApplicationOrigin::try_from("https://any.example").is_ok());
    }

    #[test]
    fn every_endpoint_is_refused_while_disabled() {
        set_enabled(Some(false));
        assert_eq!(
            check_notifications_enabled(|message| message),
            Err(NOT_ENABLED_MESSAGE.to_string())
        );
        assert!(ApplicationOrigin::try_from("https://any.example").is_ok());
    }

    #[test]
    fn every_endpoint_is_refused_when_nothing_is_configured() {
        set_enabled(None);
        assert_eq!(
            check_notifications_enabled(|message| message),
            Err(NOT_ENABLED_MESSAGE.to_string())
        );
    }
}
