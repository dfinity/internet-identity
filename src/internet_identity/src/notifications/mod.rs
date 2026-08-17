//! Notifications: who may notify a user, and how they're reached.
//!
//! Consent is channel-agnostic and lives here; channel-specific state lives
//! under [`push`]. Each `pub fn` checks the feature flag and authorizes the
//! anchor, then delegates to a storage-only helper taking the resolved
//! `anchor_number` — keeping the storage logic testable off-canister.

pub mod consent;
pub mod push;

use crate::authz_utils::{check_authorization, check_authz_and_record_activity};
use internet_identity_interface::internet_identity::types::AnchorNumber;

/// Bounds the origin so `delegation::get_principal` can't trap on it.
pub const MAX_ORIGIN_LEN: usize = 255;

fn validate_origin(origin: &str) -> Result<(), String> {
    if origin.is_empty() || origin.len() > MAX_ORIGIN_LEN {
        return Err(format!(
            "origin length {} out of range (1..={MAX_ORIGIN_LEN})",
            origin.len()
        ));
    }
    Ok(())
}

fn feature_enabled() -> bool {
    crate::state::persistent_state(|s| s.notifications_enabled.unwrap_or(false))
}

/// Server-side kill switch; every entry point checks it first.
fn ensure_enabled() -> Result<(), String> {
    if feature_enabled() {
        Ok(())
    } else {
        Err("notifications are disabled".to_string())
    }
}

/// Authorize an update via the standard activity-recording gate. Takes the
/// anchor as an argument (not a caller reverse-lookup) so it works for
/// OpenID-only identities too.
fn authorize_update(anchor_number: AnchorNumber) -> Result<(), String> {
    check_authz_and_record_activity(anchor_number).map_err(|err| format!("Unauthorized: {err}"))?;
    Ok(())
}

/// Read-only authorization; a query must not record activity.
fn authorize_query(anchor_number: AnchorNumber) -> bool {
    check_authorization(anchor_number).is_ok()
}

// ---- storage-only helpers ----

#[cfg(test)]
pub(crate) fn test_setup() {
    use crate::state::{storage_borrow_mut, storage_replace};
    use crate::storage::Storage;
    use ic_stable_structures::VectorMemory;
    storage_replace(Storage::new((0, 100), VectorMemory::default()));
    storage_borrow_mut(|s| s.update_salt([7u8; 32]));
}
