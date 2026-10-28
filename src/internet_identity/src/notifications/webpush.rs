//! The Web Push channel: what a browser registered, and the VAPID identity Internet
//! Identity signs its wake-ups with.
//!
//! Callers reach this through `main.rs`, which validates and authorizes first, so
//! everything here acts on a browser the caller has already been shown to be.

pub(crate) mod payload;
mod subscription;
mod validation;
pub(crate) mod vapid_key;

#[cfg(test)]
pub(crate) mod fixtures;

pub(crate) use subscription::{clear_gone_subscription, delivering_subscription};
pub use subscription::{remove_subscription, set_subscription, subscription_status};
pub use validation::{deliverable_endpoint, PayloadIkm, PayloadKeyId, PushEndpoint};
