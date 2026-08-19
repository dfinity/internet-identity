//! `notification_send`: accept content-free notifications from a dApp, authorize
//! the sender, resolve recipients, and enqueue into a bounded transient buffer.
//! Delivery — the dispatcher that POSTs sealed blobs to the relays — is a
//! separate PR; here the buffer just fills.

use super::check_enabled;
use super::consent::has_consent;
use super::sender::origin_for;
use super::webpush::subscription::has_any_subscription;
use crate::delegation::get_principal;
use crate::state::{persistent_state, persistent_state_mut, storage_borrow};
use candid::{CandidType, Principal};
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, FrontendHostname, Timestamp,
};
use serde::Deserialize;
use serde_bytes::ByteBuf;
use std::cell::RefCell;
use std::collections::VecDeque;

#[derive(CandidType, Deserialize, Clone, Copy, Debug, PartialEq, Eq)]
pub enum NotificationUrgency {
    #[serde(rename = "low")]
    Low,
    #[serde(rename = "normal")]
    Normal,
    #[serde(rename = "high")]
    High,
}

#[derive(CandidType, Deserialize, Clone, Debug)]
pub struct Notification {
    pub id: ByteBuf,
    pub recipient: Principal,
    pub urgency: Option<NotificationUrgency>,
    pub expires_at: Option<Timestamp>,
}

#[derive(CandidType, Deserialize, Clone, Copy, Debug, PartialEq, Eq)]
pub enum NotificationRejection {
    #[serde(rename = "no_consent")]
    NoConsent,
    #[serde(rename = "not_subscribed")]
    NotSubscribed,
    #[serde(rename = "invalid")]
    Invalid,
}

#[derive(CandidType, Deserialize, Clone, Debug)]
pub struct NotificationSendRequest {
    pub notifications: Option<Vec<Notification>>,
}

#[derive(CandidType, Deserialize, Clone, Debug)]
pub struct RejectedNotification {
    pub id: ByteBuf,
    pub reason: NotificationRejection,
}

#[derive(CandidType, Deserialize, Clone, Debug, Default)]
pub struct NotificationSendResponse {
    pub accepted: Option<u32>,
    pub rejected: Option<Vec<RejectedNotification>>,
    pub retry_after_ms: Option<u32>,
    pub buffer_epoch: Option<u64>,
}

/// Transient send buffer: heap-resident, dropped on upgrade. Bounded so a
/// flood can't exhaust the heap; a full buffer answers with `retry_after_ms`.
const MAX_BUFFER: usize = 10_000;
const RETRY_AFTER_MS: u32 = 5_000;

/// A content-free routing pointer: enough for the dispatcher to seal a ping to
/// the anchor's devices for this origin. The sender's notification `id` isn't
/// kept because the ping carries no content to correlate it to.
#[derive(Clone)]
pub struct BufferedNotification {
    pub anchor_number: AnchorNumber,
    pub origin: FrontendHostname,
    pub expires_at_ns: Option<Timestamp>,
    pub urgency: Option<NotificationUrgency>,
}

thread_local! {
    static BUFFER: RefCell<VecDeque<BufferedNotification>> = const { RefCell::new(VecDeque::new()) };
}

/// Bumped on upgrade (the heap buffer is gone), so a changed epoch tells senders
/// to resend unacked notifications.
pub fn bump_buffer_epoch() {
    persistent_state_mut(|s| {
        s.notifications_buffer_epoch =
            Some(s.notifications_buffer_epoch.unwrap_or(0).wrapping_add(1));
    });
}

fn buffer_epoch() -> u64 {
    persistent_state(|s| s.notifications_buffer_epoch.unwrap_or(0))
}

pub fn notification_send(request: NotificationSendRequest) -> NotificationSendResponse {
    let mut response = NotificationSendResponse {
        accepted: Some(0),
        rejected: Some(Vec::new()),
        retry_after_ms: None,
        buffer_epoch: Some(buffer_epoch()),
    };
    if check_enabled().is_err() {
        return response;
    }

    let notifications = request.notifications.unwrap_or_default();

    // Origin is derived from the caller against the sender cache, never passed
    // in. An unknown sender can authorize nothing.
    let Some(origin) = origin_for(ic_cdk::caller()) else {
        response.rejected = Some(
            notifications
                .into_iter()
                .map(|n| RejectedNotification {
                    id: n.id,
                    reason: NotificationRejection::Invalid,
                })
                .collect(),
        );
        return response;
    };

    let now = ic_cdk::api::time();
    let mut accepted = 0u32;
    let mut rejected = Vec::new();
    for notification in notifications {
        if buffer_len() >= MAX_BUFFER {
            // At capacity: stop accepting and tell the sender to retry the rest.
            response.retry_after_ms = Some(RETRY_AFTER_MS);
            break;
        }
        match accept_one(&origin, &notification, now) {
            Ok(buffered) => {
                enqueue(buffered);
                accepted += 1;
            }
            Err(reason) => rejected.push(RejectedNotification {
                id: notification.id.clone(),
                reason,
            }),
        }
    }
    response.accepted = Some(accepted);
    response.rejected = Some(rejected);
    response
}

fn accept_one(
    origin: &FrontendHostname,
    notification: &Notification,
    now_ns: Timestamp,
) -> Result<BufferedNotification, NotificationRejection> {
    if notification
        .expires_at
        .is_some_and(|expiry| expiry <= now_ns)
    {
        return Err(NotificationRejection::Invalid);
    }

    // recipient -> anchor. Unknown and not-consented are merged into one reason
    // so a sender can't probe which identities exist.
    let anchor_number = storage_borrow(|storage| {
        storage
            .notifications_recipient_index_memory
            .get(&notification.recipient)
    })
    .ok_or(NotificationRejection::NoConsent)?;

    // The recipient must be this origin's per-user principal — stops a sender
    // for origin A from notifying a principal that consented to origin B.
    if get_principal(anchor_number, origin.clone()) != notification.recipient {
        return Err(NotificationRejection::NoConsent);
    }
    if !has_consent(anchor_number, origin.clone()) {
        return Err(NotificationRejection::NoConsent);
    }
    if !has_any_subscription(anchor_number) {
        return Err(NotificationRejection::NotSubscribed);
    }

    Ok(BufferedNotification {
        anchor_number,
        origin: origin.clone(),
        expires_at_ns: notification.expires_at,
        urgency: notification.urgency,
    })
}

fn buffer_len() -> usize {
    BUFFER.with(|buffer| buffer.borrow().len())
}

pub(super) fn enqueue(notification: BufferedNotification) {
    BUFFER.with(|buffer| buffer.borrow_mut().push_back(notification));
}

/// Pops the oldest buffered notification for the delivery drain (FIFO).
pub(super) fn take_next() -> Option<BufferedNotification> {
    BUFFER.with(|buffer| buffer.borrow_mut().pop_front())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::notifications::test_setup as setup;

    fn clear_buffer() {
        BUFFER.with(|buffer| buffer.borrow_mut().clear());
    }

    fn notification(id: u8, expires_at: Option<Timestamp>) -> Notification {
        Notification {
            id: ByteBuf::from(vec![id]),
            recipient: Principal::from_slice(&[id; 10]),
            urgency: None,
            expires_at,
        }
    }

    #[test]
    fn already_expired_is_invalid() {
        setup();
        let result = accept_one(
            &"https://app.example".to_string(),
            &notification(1, Some(100)),
            200,
        );
        assert!(matches!(result, Err(NotificationRejection::Invalid)));
    }

    #[test]
    fn unknown_recipient_is_no_consent() {
        setup();
        // recipient is not in the index -> resolved before the get_principal check
        let result = accept_one(
            &"https://app.example".to_string(),
            &notification(2, None),
            0,
        );
        assert!(matches!(result, Err(NotificationRejection::NoConsent)));
    }

    #[test]
    fn buffer_epoch_bumps_on_upgrade() {
        setup();
        let before = buffer_epoch();
        bump_buffer_epoch();
        assert_eq!(buffer_epoch(), before + 1);
    }

    #[test]
    fn buffer_drains_fifo() {
        setup();
        clear_buffer();
        for anchor in 1..=5u64 {
            enqueue(BufferedNotification {
                anchor_number: anchor,
                origin: "https://app.example".to_string(),
                expires_at_ns: None,
                urgency: None,
            });
        }
        assert_eq!(buffer_len(), 5);
        let drained: Vec<_> = std::iter::from_fn(take_next)
            .map(|n| n.anchor_number)
            .collect();
        assert_eq!(drained, vec![1, 2, 3, 4, 5]);
    }
}
