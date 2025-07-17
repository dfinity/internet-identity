//! Here you'll find modules related to testing the canisters:
//!  * `api`: Rust-bindings for the II and archive canisters
//!  * `flows`: Reusable flows consisting of multiple canister interactions
//!  * `framework`: Helpers of various kinds for writing tests.
//!
//! Most changes should happen in the canister specific `tests` folders. The split was done this way so that `tests` is as simple
//! as possible to make tests easy to read and write.
//!
pub mod api;
pub mod flows;
pub mod framework;
