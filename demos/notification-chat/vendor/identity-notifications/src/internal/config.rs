//! Three environment variables, one of them optional. Read on use: a
//! canister's environment does not change under it.

use crate::types::Misconfigured;
use candid::Principal;

pub const DEFAULT_CAPACITY_MB: usize = 1024;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Config {
    /// `None` where the variable is unset or not a principal, which
    /// [`crate::set_sender`] can still answer for.
    pub sender: Option<Principal>,
    pub origin: String,
    pub capacity_bytes: usize,
}

/// Missing configuration is reported rather than guessed at.
#[cfg(target_arch = "wasm32")]
pub fn read() -> Result<Config, Misconfigured> {
    use ic_cdk::api::{env_var_name_exists, env_var_value};

    let origin = if env_var_name_exists("notification_origin") {
        env_var_value("notification_origin")
    } else {
        return Err(Misconfigured::Origin);
    };
    if origin.is_empty() {
        return Err(Misconfigured::Origin);
    }

    let sender = if env_var_name_exists("notification_sender") {
        Principal::from_text(env_var_value("notification_sender")).ok()
    } else {
        None
    };

    let capacity_mb = if env_var_name_exists("notification_capacity_mb") {
        env_var_value("notification_capacity_mb")
            .parse()
            .unwrap_or(DEFAULT_CAPACITY_MB)
    } else {
        DEFAULT_CAPACITY_MB
    };

    Ok(Config {
        sender,
        origin,
        capacity_bytes: capacity_mb * 1_048_576,
    })
}

/// Off-canister there is no environment to read.
#[cfg(not(target_arch = "wasm32"))]
pub fn read() -> Result<Config, Misconfigured> {
    Err(Misconfigured::Origin)
}
