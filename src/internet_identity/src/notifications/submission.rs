//! Put an app's batch into the backlog, answering for what II cannot take.

use crate::admission_queue::{Admission, FullCause};
use crate::notifications::backlog::app::PendingNotification;
use crate::notifications::delivery_metrics::{self, DeferCause, Labels, Reason};
use crate::notifications::{dispatch, BROWSER_GONE_AFTER_NS};
use crate::origin::ApplicationOrigin;
use crate::state::{self, storage_borrow};
use crate::storage::storable::application::StorableOriginSha256;
use candid::Principal;
use internet_identity_interface::internet_identity::types::{
    AccountNumber, AnchorNumber, ApplicationNumber, NotAccepted, NotAcceptedReason, Notification,
    SendNotificationResponse, Timestamp, Urgency,
};
use std::collections::BTreeMap;

/// Queue what can reach its recipient, and schedule a pass for it. `sender` is the app
/// canister the service worker fetches the content from.
pub fn submit(
    origin: &ApplicationOrigin,
    notifications: Vec<Notification>,
    sender: Principal,
    now_ns: Timestamp,
) -> SendNotificationResponse {
    let mut not_accepted = Vec::new();
    let mut resolutions: BTreeMap<Principal, Result<Resolved, Refusal>> = BTreeMap::new();
    let mut reachable = Vec::new();

    for Notification {
        id,
        recipient,
        expires_at,
        urgency,
    } in notifications
    {
        let resolution = resolutions
            .entry(recipient)
            .or_insert_with(|| resolve_recipient(recipient, origin, now_ns))
            .clone();
        match resolution {
            Ok(Resolved {
                anchor_number,
                application_number,
                account_number,
            }) => reachable.push(PendingNotification {
                recipient,
                anchor_number,
                application_number,
                account_number,
                sender,
                notification_id: id,
                urgency: urgency.unwrap_or(Urgency::Normal),
                expires_at_ns: expires_at,
            }),
            Err(refusal) => {
                let labels = Labels::app(origin.as_str(), urgency.as_ref());
                delivery_metrics::count_received(&labels);
                delivery_metrics::count_lost(refusal.metric_reason(), &labels);
                not_accepted.push(NotAccepted {
                    id,
                    recipient,
                    reason: refusal.answer(),
                })
            }
        }
    }
    if reachable.is_empty() {
        return SendNotificationResponse { not_accepted };
    }

    for pending in &reachable {
        delivery_metrics::number_application(pending.application_number, origin.as_str());
    }
    let counted_notifications: Vec<(Labels, Option<Timestamp>)> = reachable
        .iter()
        .map(|pending| {
            (
                Labels::app(origin.as_str(), Some(&pending.urgency)),
                pending.expires_at_ns,
            )
        })
        .collect();
    let admissions = state::notification_backlog_mut(now_ns, |backlog| {
        backlog.admit(
            StorableOriginSha256::from_origin(&origin.as_str().to_string()),
            reachable,
            now_ns,
        )
    });
    for (admitted, (labels, expires_at_ns)) in admissions.into_iter().zip(counted_notifications) {
        match admitted.admission {
            Admission::Accepted => {
                delivery_metrics::count_received(&labels);
                delivery_metrics::count_accepted(&labels);
            }
            Admission::Folded => delivery_metrics::count_folded(&labels),
            Admission::Dropped => {
                delivery_metrics::count_received(&labels);
                delivery_metrics::count_accepted(&labels);
                delivery_metrics::count_lost(Reason::ExpiredOnArrival, &labels);
            }
            Admission::Full {
                retry_after_ns,
                cause,
            } => {
                let retry_after = now_ns.saturating_add(retry_after_ns);
                count_deferral(&labels, cause, retry_after, expires_at_ns);
                let (recipient, id) = admitted.key;
                not_accepted.push(NotAccepted {
                    id,
                    recipient,
                    reason: NotAcceptedReason::Deferred { retry_after },
                });
            }
        }
    }
    dispatch::schedule_pass(now_ns);
    SendNotificationResponse { not_accepted }
}

/// A deferral is counted beside the funnel, as the app is expected to send it again. One
/// told to come back no sooner than it expires cannot arrive, so it is a loss instead.
fn count_deferral(
    labels: &Labels,
    cause: FullCause,
    retry_after: Timestamp,
    expires_at_ns: Option<Timestamp>,
) {
    let is_capacity_cause = cause == FullCause::Queue;
    if expires_at_ns.is_some_and(|expires_at_ns| retry_after >= expires_at_ns) {
        delivery_metrics::count_received(labels);
        delivery_metrics::count_lost(
            if is_capacity_cause {
                Reason::CapacityPastExpiry
            } else {
                Reason::AppLimitPastExpiry
            },
            labels,
        );
        return;
    }
    delivery_metrics::count_deferred(
        if is_capacity_cause {
            DeferCause::Capacity
        } else {
            DeferCause::AppLimit
        },
        labels,
    );
}

/// Why a recipient cannot be reached, finer than the answer the app is given.
#[derive(Clone, Copy, Debug)]
enum Refusal {
    /// The principal names no account at the origin.
    UnknownPrincipal,
    /// None of the identity's browsers was used within [`BROWSER_GONE_AFTER_NS`].
    Dormant,
    NoChannel,
}

impl Refusal {
    fn answer(self) -> NotAcceptedReason {
        match self {
            Refusal::UnknownPrincipal | Refusal::Dormant => NotAcceptedReason::NoSuchRecipient,
            Refusal::NoChannel => NotAcceptedReason::NoChannel,
        }
    }

    fn metric_reason(self) -> Reason {
        match self {
            Refusal::UnknownPrincipal => Reason::UnknownPrincipal,
            Refusal::Dormant => Reason::DormantIdentity,
            Refusal::NoChannel => Reason::NoChannel,
        }
    }
}

/// Where a recipient principal points: its identity, the app, and its account there.
#[derive(Clone)]
struct Resolved {
    anchor_number: AnchorNumber,
    application_number: ApplicationNumber,
    account_number: Option<AccountNumber>,
}

/// What a principal names at `origin`. No such recipient when it names none
/// there, or when none of its browsers was used within [`BROWSER_GONE_AFTER_NS`]. No
/// channel when it has not allowed the app, or no browser can be woken now.
fn resolve_recipient(
    recipient: Principal,
    origin: &ApplicationOrigin,
    now_ns: Timestamp,
) -> Result<Resolved, Refusal> {
    let origin = &origin.as_str().to_string();
    storage_borrow(|storage| {
        let account = storage
            .lookup_account_with_principal(recipient)
            .filter(|account| account.origin == *origin)
            .ok_or(Refusal::UnknownPrincipal)?;
        let application_number = storage
            .lookup_application_number_with_origin(origin)
            .ok_or(Refusal::UnknownPrincipal)?;
        let anchor = storage
            .read(account.anchor_number)
            .map_err(|_| Refusal::UnknownPrincipal)?;
        let live = anchor
            .browsers()
            .iter()
            .any(|browser| now_ns.saturating_sub(browser.last_used) < BROWSER_GONE_AFTER_NS);
        if !live {
            return Err(Refusal::Dormant);
        }
        let consented = storage
            .read_anchor_application_config(account.anchor_number, origin)
            .is_some_and(|config| config.notifications_allowed());
        if !consented
            || !anchor
                .browsers()
                .iter()
                .any(|browser| dispatch::can_wake(storage, browser, now_ns))
        {
            return Err(Refusal::NoChannel);
        }
        Ok(Resolved {
            anchor_number: account.anchor_number,
            application_number,
            account_number: account.account_number,
        })
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::notifications::delivery_metrics;
    use crate::notifications::webpush::fixtures::{anchor, setup, subscribe};
    use crate::notifications::write_notifications_decision;
    use crate::state::storage_borrow_mut;
    use pretty_assertions::assert_eq;

    const APP: &str = "https://app.example";
    const RELAY: &str = "https://relay.example/wpush/abc";
    const NOW_NS: Timestamp = 1_000_000_000_000;

    fn sender() -> Principal {
        Principal::from_slice(&[7; 10])
    }

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
        write_notifications_decision(recipient.anchor_number, &app_origin(), true, NOW_NS)
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

    fn app_origin() -> ApplicationOrigin {
        ApplicationOrigin::try_from(APP).expect("a bare origin")
    }

    fn send(notifications: Vec<Notification>, now_ns: Timestamp) -> Vec<NotAccepted> {
        submit(&app_origin(), notifications, sender(), now_ns).not_accepted
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
                application_number: storage_borrow(|storage| {
                    storage.lookup_application_number_with_origin(&APP.to_string())
                })
                .expect("signing in stores the application"),
                account_number: None,
                sender: sender(),
                notification_id: 7,
                urgency: Urgency::Normal,
                expires_at_ns: None,
            }]
        );
    }

    #[test]
    fn a_notification_for_a_named_account_is_queued_for_that_account() {
        setup();
        let recipient = reachable();
        let (account_number, principal) = storage_borrow_mut(|storage| {
            let account = storage
                .create_account(
                    recipient.anchor_number,
                    APP.to_string(),
                    "Work".to_string(),
                    NOW_NS,
                )
                .expect("creating an account");
            (
                account.account_number,
                storage.account_principal_for_testing(&account),
            )
        });
        let named = Recipient {
            anchor_number: recipient.anchor_number,
            principal,
        };

        assert_eq!(send(vec![notification(&named, 7)], NOW_NS), vec![]);

        assert!(account_number.is_some());
        assert_eq!(
            queued(NOW_NS)
                .iter()
                .map(|pending| pending.account_number)
                .collect::<Vec<_>>(),
            vec![account_number]
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
    fn a_queued_notification_is_counted_received_and_accepted() {
        setup();
        let recipient = reachable();

        send(vec![notification(&recipient, 7)], NOW_NS);

        assert_eq!(
            delivery_metrics::notification_count_for_testing("received"),
            1
        );
        assert_eq!(
            delivery_metrics::notification_count_for_testing("accepted"),
            1
        );
    }

    #[test]
    fn a_refusal_is_counted_received_and_lost_for_its_reason() {
        setup();
        let stranger = Recipient {
            anchor_number: 0,
            principal: Principal::from_slice(&[9; 29]),
        };
        let long_after = BROWSER_GONE_AFTER_NS + NOW_NS;
        let dormant_recipient = reachable();

        send(vec![notification(&stranger, 1)], NOW_NS);
        send(vec![notification(&dormant_recipient, 2)], long_after);

        assert_eq!(
            delivery_metrics::notification_count_for_testing("received"),
            2
        );
        assert_eq!(
            delivery_metrics::loss_count_for_testing("unknown_principal"),
            1
        );
        assert_eq!(
            delivery_metrics::loss_count_for_testing("dormant_identity"),
            1
        );
    }

    #[test]
    fn a_resend_of_a_waiting_notification_is_folded_rather_than_received() {
        setup();
        let recipient = reachable();

        send(vec![notification(&recipient, 7)], NOW_NS);
        send(vec![notification(&recipient, 7)], NOW_NS);

        assert_eq!(
            delivery_metrics::notification_count_for_testing("received"),
            1
        );
    }

    #[test]
    fn a_deferral_is_not_received_unless_it_cannot_come_back_in_time() {
        setup();
        let recipient = reachable();
        let mut batch: Vec<_> = (0..21).map(|id| notification(&recipient, id)).collect();
        let mut late_notification = notification(&recipient, 21);
        late_notification.expires_at = Some(NOW_NS + 1);
        batch.push(late_notification);

        send(batch, NOW_NS);

        assert_eq!(
            delivery_metrics::notification_count_for_testing("received"),
            21
        );
        assert_eq!(
            delivery_metrics::loss_count_for_testing("app_limit_past_expiry"),
            1
        );
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
