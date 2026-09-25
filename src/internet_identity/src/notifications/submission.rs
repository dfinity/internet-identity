//! Put an app's batch into the backlog, answering for what II cannot take.

use crate::notifications::admission_queue::Admission;
use crate::notifications::backlog::PendingNotification;
use crate::notifications::{dispatch, ValidatedSendNotificationArg, BROWSER_GONE_AFTER_NS};
use crate::state::{self, storage_borrow};
use crate::storage::storable::application::StorableOriginSha256;
use candid::Principal;
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, FrontendHostname, NotAccepted, NotAcceptedReason, Notification,
    SendNotificationResponse, Timestamp, Urgency,
};
use std::collections::BTreeMap;

/// Queue what can reach its recipient, and schedule a pass for it.
pub fn submit(
    ValidatedSendNotificationArg {
        origin,
        notifications,
        ..
    }: ValidatedSendNotificationArg,
    now_ns: Timestamp,
) -> SendNotificationResponse {
    let mut not_accepted = Vec::new();
    let mut resolved: BTreeMap<Principal, Result<AnchorNumber, NotAcceptedReason>> =
        BTreeMap::new();
    let mut reachable = Vec::new();

    for Notification {
        id,
        recipient,
        expires_at,
        urgency,
    } in notifications
    {
        let resolution = resolved
            .entry(recipient)
            .or_insert_with(|| resolve_recipient(recipient, &origin, now_ns))
            .clone();
        match resolution {
            Ok(anchor_number) => reachable.push(PendingNotification {
                recipient,
                anchor_number,
                notification_id: id,
                urgency: urgency.unwrap_or(Urgency::Normal),
                expires_at_ns: expires_at,
            }),
            Err(reason) => not_accepted.push(NotAccepted {
                id,
                recipient,
                reason,
            }),
        }
    }
    if reachable.is_empty() {
        return SendNotificationResponse { not_accepted };
    }

    let admissions = state::notification_backlog_mut(now_ns, |backlog| {
        backlog.admit(
            StorableOriginSha256::from_origin(&origin),
            reachable,
            now_ns,
        )
    });
    for admitted in admissions {
        match admitted.admission {
            Admission::Accepted | Admission::Folded | Admission::Dropped => {}
            Admission::Full { retry_after_ns } => {
                let (recipient, id) = admitted.key;
                not_accepted.push(NotAccepted {
                    id,
                    recipient,
                    reason: NotAcceptedReason::Deferred {
                        retry_after: now_ns.saturating_add(retry_after_ns),
                    },
                });
            }
        }
    }
    dispatch::schedule_pass(now_ns);
    SendNotificationResponse { not_accepted }
}

/// The identity a principal names at `origin`. No such recipient when it names none
/// there, or when none of its browsers was used within [`BROWSER_GONE_AFTER_NS`]. No
/// channel when it has not allowed the app, or no browser can be woken now.
fn resolve_recipient(
    recipient: Principal,
    origin: &FrontendHostname,
    now_ns: Timestamp,
) -> Result<AnchorNumber, NotAcceptedReason> {
    storage_borrow(|storage| {
        let account = storage
            .lookup_account_with_principal(recipient)
            .filter(|account| account.origin == *origin)
            .ok_or(NotAcceptedReason::NoSuchRecipient)?;
        let anchor = storage
            .read(account.anchor_number)
            .map_err(|_| NotAcceptedReason::NoSuchRecipient)?;
        let live = anchor
            .browsers()
            .iter()
            .any(|browser| now_ns.saturating_sub(browser.last_used) < BROWSER_GONE_AFTER_NS);
        if !live {
            return Err(NotAcceptedReason::NoSuchRecipient);
        }
        let consented = storage
            .read_anchor_application_config(account.anchor_number, origin)
            .and_then(|config| config.notifications_consented_at_ns)
            .is_some();
        if !consented
            || !anchor
                .browsers()
                .iter()
                .any(|browser| dispatch::wake_up_jwt(browser, now_ns).is_some())
        {
            return Err(NotAcceptedReason::NoChannel);
        }
        Ok(account.anchor_number)
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::notifications::webpush::fixtures::{anchor, setup, subscribe};
    use crate::notifications::{write_consent, ValidatedSendNotificationArg};
    use crate::state::storage_borrow_mut;
    use internet_identity_interface::internet_identity::types::SendNotificationArg;
    use pretty_assertions::assert_eq;

    const APP: &str = "https://app.example";
    const RELAY: &str = "https://relay.example/wpush/abc";
    const NOW_NS: Timestamp = 1_000_000_000_000;

    struct Recipient {
        anchor_number: AnchorNumber,
        principal: Principal,
    }

    /// An identity signed in at `APP`, as the principal the app knows it by. Signing in
    /// leaves one browser on the identity.
    fn signed_in() -> Recipient {
        storage_borrow_mut(|storage| {
            let anchor = storage.allocate_anchor(0).expect("allocating an anchor");
            let anchor_number = anchor.anchor_number();
            storage.write(anchor).expect("writing the anchor");
            storage.sign_in_for_testing(anchor_number, &APP.to_string());
            Recipient {
                anchor_number,
                principal: storage
                    .default_account_principal_for_testing(anchor_number, &APP.to_string()),
            }
        })
    }

    /// Signed in, registered for Web Push, and allowing `APP` to notify it.
    fn reachable() -> Recipient {
        let recipient = signed_in();
        let browser_id = anchor(recipient.anchor_number).browsers()[0].id;
        subscribe(recipient.anchor_number, browser_id, RELAY, 0);
        allow(&recipient);
        recipient
    }

    fn allow(recipient: &Recipient) {
        write_consent(
            recipient.anchor_number,
            &APP.to_string(),
            Some(NOW_NS),
            NOW_NS,
        )
        .expect("writing consent");
    }

    fn notification(recipient: &Recipient, id: u64) -> Notification {
        Notification {
            id,
            recipient: recipient.principal,
            expires_at: None,
            urgency: None,
        }
    }

    fn send(notifications: Vec<Notification>, now_ns: Timestamp) -> Vec<NotAccepted> {
        let request = ValidatedSendNotificationArg::try_from(SendNotificationArg {
            origin: APP.to_string(),
            notifications,
        })
        .expect("a valid batch");
        submit(request, now_ns).not_accepted
    }

    fn queued(now_ns: Timestamp) -> Vec<PendingNotification> {
        state::notification_backlog_mut(now_ns, |backlog| {
            backlog.take_batch(usize::MAX, now_ns, |batch| {
                Ok::<_, std::convert::Infallible>(
                    batch.iter().map(|taken| taken.entry.item.clone()).collect(),
                )
            })
        })
        .unwrap_or_default()
    }

    fn rejected(recipient: &Recipient, id: u64, reason: NotAcceptedReason) -> NotAccepted {
        NotAccepted {
            id,
            recipient: recipient.principal,
            reason,
        }
    }

    #[test]
    fn a_notification_for_a_reachable_recipient_is_queued_at_normal_urgency() {
        setup();
        let recipient = reachable();

        assert_eq!(send(vec![notification(&recipient, 7)], NOW_NS), vec![]);

        assert_eq!(
            queued(NOW_NS),
            vec![PendingNotification {
                recipient: recipient.principal,
                anchor_number: recipient.anchor_number,
                notification_id: 7,
                urgency: Urgency::Normal,
                expires_at_ns: None,
            }]
        );
    }

    #[test]
    fn a_principal_that_names_no_identity_is_no_such_recipient() {
        setup();
        let stranger = Recipient {
            anchor_number: 0,
            principal: Principal::from_slice(&[9; 29]),
        };

        assert_eq!(
            send(vec![notification(&stranger, 7)], NOW_NS),
            vec![rejected(&stranger, 7, NotAcceptedReason::NoSuchRecipient)]
        );
        assert!(queued(NOW_NS).is_empty());
    }

    #[test]
    fn an_identity_whose_every_browser_went_quiet_is_no_such_recipient() {
        setup();
        let recipient = reachable();
        let long_after = BROWSER_GONE_AFTER_NS + NOW_NS;

        assert_eq!(
            send(vec![notification(&recipient, 7)], long_after),
            vec![rejected(&recipient, 7, NotAcceptedReason::NoSuchRecipient)]
        );
    }

    #[test]
    fn an_identity_that_has_not_allowed_the_app_has_no_channel() {
        setup();
        let recipient = signed_in();
        let browser_id = anchor(recipient.anchor_number).browsers()[0].id;
        subscribe(recipient.anchor_number, browser_id, RELAY, 0);

        assert_eq!(
            send(vec![notification(&recipient, 7)], NOW_NS),
            vec![rejected(&recipient, 7, NotAcceptedReason::NoChannel)]
        );
    }

    #[test]
    fn an_identity_with_no_registered_browser_has_no_channel() {
        setup();
        let recipient = signed_in();
        allow(&recipient);

        assert_eq!(
            send(vec![notification(&recipient, 7)], NOW_NS),
            vec![rejected(&recipient, 7, NotAcceptedReason::NoChannel)]
        );
    }

    /// The pass would skip it, so the app would hear nothing of a notification never sent.
    #[test]
    fn an_identity_whose_browser_spent_its_signed_pool_has_no_channel() {
        setup();
        let recipient = reachable();
        let spent = 3 * 24 * 60 * 60 * 1_000_000_000 + NOW_NS;

        assert_eq!(
            send(vec![notification(&recipient, 7)], spent),
            vec![rejected(&recipient, 7, NotAcceptedReason::NoChannel)]
        );
    }

    #[test]
    fn a_full_backlog_defers_to_a_time_rather_than_a_delay() {
        setup();
        let recipient = reachable();
        let batch = (0..21).map(|id| notification(&recipient, id)).collect();

        let answers = send(batch, NOW_NS);

        assert_eq!(answers.len(), 1);
        let NotAcceptedReason::Deferred { retry_after } = answers[0].reason else {
            panic!("the 21st was not deferred: {answers:?}");
        };
        assert!(retry_after > NOW_NS);
        assert_eq!(queued(NOW_NS).len(), 20);
    }

    #[test]
    fn a_batch_the_backlog_turned_away_still_schedules_a_pass() {
        setup();
        let recipient = reachable();
        send(
            (0..20).map(|id| notification(&recipient, id)).collect(),
            NOW_NS,
        );
        dispatch::reset_pass_schedule_for_testing();

        let answers = send(vec![notification(&recipient, 20)], NOW_NS);

        assert!(matches!(
            answers[..],
            [NotAccepted {
                reason: NotAcceptedReason::Deferred { .. },
                ..
            }]
        ));
        assert_eq!(dispatch::pass_due_at_for_testing(), Some(NOW_NS));
    }

    #[test]
    fn a_notification_that_expired_before_it_arrived_is_neither_queued_nor_refused() {
        setup();
        let recipient = reachable();
        let mut late = notification(&recipient, 7);
        late.expires_at = Some(NOW_NS - 1);

        assert_eq!(send(vec![late], NOW_NS), vec![]);

        assert!(queued(NOW_NS).is_empty());
    }

    #[test]
    fn one_batch_answers_each_recipient_on_its_own() {
        setup();
        let recipient = reachable();
        let unallowed = signed_in();

        assert_eq!(
            send(
                vec![notification(&recipient, 1), notification(&unallowed, 2)],
                NOW_NS
            ),
            vec![rejected(&unallowed, 2, NotAcceptedReason::NoChannel)]
        );
        assert_eq!(queued(NOW_NS).len(), 1);
    }
}
