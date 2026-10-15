//! Internet Identity's app-facing send interface, as the candid declares it.

use candid::{CandidType, Principal};
use serde::Deserialize;

#[derive(CandidType, Clone, Copy, Debug)]
pub enum Urgency {
    VeryLow,
    Low,
    Normal,
    High,
}

#[derive(CandidType, Clone, Debug)]
pub struct Notification {
    pub id: u64,
    pub recipient: Principal,
    pub expires_at: Option<u64>,
    pub urgency: Option<Urgency>,
}

#[derive(CandidType, Clone, Debug)]
pub struct SendNotificationArg {
    pub origin: String,
    pub notifications: Vec<Notification>,
}

#[derive(CandidType, Deserialize, Clone, Copy, Debug)]
pub enum NotAcceptedReason {
    NoSuchRecipient,
    NoChannel,
    Deferred { retry_after: u64 },
}

#[derive(CandidType, Deserialize, Clone, Copy, Debug)]
pub struct NotAccepted {
    pub id: u64,
    pub recipient: Principal,
    pub reason: NotAcceptedReason,
}

#[derive(CandidType, Deserialize, Clone, Debug)]
pub struct SendNotificationResponse {
    pub not_accepted: Vec<NotAccepted>,
}

#[derive(CandidType, Deserialize, Clone, Debug)]
pub enum SendNotificationError {
    NoSuchSender,
    TooManyNotifications { limit: u32 },
    InternalCanisterError(String),
}

#[derive(CandidType, Deserialize, Clone, Debug)]
pub enum SendNotificationResult {
    Ok(SendNotificationResponse),
    Err(SendNotificationError),
}
