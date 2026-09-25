//! Storable types for notifications.
//!
//! Everything channel-specific lives under a submodule, so a second delivery channel
//! adds a sibling. Consent is not here at all: it rides on the per-app config.

pub mod browser_queue;
pub mod webpush;
