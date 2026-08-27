//! `notification_send`: accept content-free notifications from a dApp, authorize
//! the sender, resolve recipients, and enqueue into a bounded transient buffer.
//! Delivery — the dispatcher that POSTs sealed blobs to the relays — is a
//! separate PR; here the buffer just fills.

use super::check_enabled;
use super::consent::{is_deliverable, record_sent};
use super::sender::is_authorized_sender;
use super::webpush::subscription::has_subscribed_device;
use crate::delegation::get_principal;
use crate::state::{last_upgrade_timestamp, storage_borrow};
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
    /// The origin the sender claims to send for. Accepted only if that origin's
    /// well-known list also named this canister, so an origin can't authorize a
    /// canister it doesn't own.
    pub origin: Option<FrontendHostname>,
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
    /// The canister's last-upgrade time (ns). It changes only when II is
    /// upgraded, which is the only thing that drops the heap send buffer, so a
    /// sender that sees a new value resends anything unacked.
    pub resend_epoch: Option<u64>,
}

/// Transient send buffer: heap-resident, dropped on upgrade. Bounded so a
/// flood can't exhaust the heap; a full buffer answers with `retry_after_ms`.
const MAX_BUFFER: usize = 10_000;
const RETRY_AFTER_MS: u32 = 5_000;

// Fields are read by the dispatcher (next PR); here they are only written.
#[allow(dead_code)]
#[derive(Clone)]
pub struct BufferedNotification {
    pub anchor_number: AnchorNumber,
    pub origin: FrontendHostname,
    pub id: Vec<u8>,
    pub expires_at_ns: Option<Timestamp>,
    pub enqueued_at_ns: Timestamp,
}

thread_local! {
    static BUFFER: RefCell<VecDeque<BufferedNotification>> = const { RefCell::new(VecDeque::new()) };
}

pub fn notification_send(request: NotificationSendRequest) -> NotificationSendResponse {
    let mut response = NotificationSendResponse {
        accepted: Some(0),
        rejected: Some(Vec::new()),
        retry_after_ms: None,
        resend_epoch: Some(last_upgrade_timestamp()),
    };
    if check_enabled().is_err() {
        return response;
    }

    let notifications = request.notifications.unwrap_or_default();

    // The sender declares the origin it sends for; accept only if that origin's
    // well-known list also named this canister (bound at consent). This two-way
    // check — caller declares O, O vouches for caller — is what stops an origin
    // from claiming a canister it doesn't own. Verifying here, before any
    // `get_principal`, also keeps an oversized/garbage origin from reaching
    // `check_frontend_length` (which traps): an unbound origin is just rejected.
    let Some(origin) = request.origin else {
        response.rejected = Some(reject_all(notifications));
        return response;
    };
    if !is_authorized_sender(ic_cdk::caller(), &origin) {
        response.rejected = Some(reject_all(notifications));
        return response;
    }

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

/// Rejects a whole batch as `invalid` — used when the sender is not an
/// authorized sender for the origin it declared (or declared none).
fn reject_all(notifications: Vec<Notification>) -> Vec<RejectedNotification> {
    notifications
        .into_iter()
        .map(|n| RejectedNotification {
            id: n.id,
            reason: NotificationRejection::Invalid,
        })
        .collect()
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
    if !is_deliverable(anchor_number, origin.clone()) {
        return Err(NotificationRejection::NoConsent);
    }
    // No subscribed device means the ping has nowhere to land, so reject rather
    // than buffer it. After the consent check, so a missing device is only
    // revealed to an already-consented sender (unknown ones stay `no_consent`).
    if !has_subscribed_device(anchor_number) {
        return Err(NotificationRejection::NotSubscribed);
    }

    record_sent(anchor_number, origin, now_ns);
    Ok(BufferedNotification {
        anchor_number,
        origin: origin.clone(),
        id: notification.id.clone().into_vec(),
        expires_at_ns: notification.expires_at,
        enqueued_at_ns: now_ns,
    })
}

fn buffer_len() -> usize {
    BUFFER.with(|buffer| buffer.borrow().len())
}

fn enqueue(notification: BufferedNotification) {
    BUFFER.with(|buffer| buffer.borrow_mut().push_back(notification));
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
    fn buffer_enqueues_in_order() {
        setup();
        clear_buffer();
        for id in 0..5u8 {
            enqueue(BufferedNotification {
                anchor_number: 1,
                origin: "https://app.example".to_string(),
                id: vec![id],
                expires_at_ns: None,
                enqueued_at_ns: 0,
            });
        }
        assert_eq!(buffer_len(), 5);
    }
}
