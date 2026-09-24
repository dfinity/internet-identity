//! The Web Push channel: what a browser registered, and the pool of VAPID JWTs that
//! browser signed for it.
//!
//! Callers reach this through `main.rs`, which validates and authorizes first, so
//! everything here acts on a browser the caller has already been shown to be.

mod subscription;
mod validation;

#[cfg(test)]
mod fixtures;

pub use subscription::{remove_subscription, set_subscription, subscription_status};
pub use validation::{
    ValidatedGetWebPushSubscriptionStatusRequest, ValidatedRemoveWebPushSubscriptionRequest,
    ValidatedSetWebPushSubscriptionRequest,
};
