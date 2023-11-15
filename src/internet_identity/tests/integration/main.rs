//! Having a single integration test binary has advantages:
//! - It's faster to compile.
//! - All tests are run in parallel by default.
//!
//! See https://matklad.github.io/2021/02/27/delete-cargo-integration-tests.html#Implications for more details.

mod activity_stats;
mod anchor_management;
mod archive_integration;
mod delegation;
mod http;
mod latest_delegation_origins;
mod rollback;
mod stable_memory;
mod upgrade;
mod v2_api;
mod vc_mvp;