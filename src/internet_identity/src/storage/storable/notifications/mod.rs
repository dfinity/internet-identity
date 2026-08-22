//! Storable types for notifications.
//!
//! [`consent`] is channel-agnostic: a user consents to an app notifying them,
//! not to a particular delivery channel. Everything channel-specific lives
//! under a submodule, so a second channel adds a sibling rather than touching
//! the generic parts.

pub mod consent;
pub mod sender;
pub mod webpush;
