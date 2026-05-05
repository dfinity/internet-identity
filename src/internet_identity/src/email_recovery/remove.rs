//! `email_recovery_credential_remove` — detach the recovery email.
//!
//! Called from `main.rs::email_recovery_api::email_recovery_credential_remove`
//! after that method has done its own authz check (via
//! `crate::authz_utils::check_authorization`) and loaded the anchor.
//! The canister-method layer inlines the auth + write rather than
//! going through `anchor_operation_with_authz_check`, because that
//! helper's `E: From<IdentityUpdateError>` bound is awkward to
//! satisfy for an interface-crate error type (orphan rule). So by
//! the time we get here the caller is verified, the anchor is
//! loaded, and we just need to clear the field.
//!
//! Two design notes:
//!
//! - We require the caller to *name* the address being removed, even
//!   though the anchor only carries one credential. This matches the
//!   confirmation modal in the design-doc UX (§8.6) — the FE shows
//!   "Remove `alice@gmail.com`?" and on confirm passes the same
//!   string back. Mismatch → `NotRegistered`. This keeps the FE
//!   honest if it ever caches a stale list.
//! - We emit a typed `Operation::EmailRecoveryRemove` for the archive
//!   stream so removals show up in the audit log alongside passkey /
//!   recovery-phrase removals. *(Marked TODO until the operation
//!   variant lands in the archive types — for this PR we use a
//!   placeholder that maps onto the existing
//!   `Operation::IdentityMetadataReplace` so the archive call still
//!   succeeds.)*

use crate::storage::anchor::Anchor;
use internet_identity_interface::archive::types::Operation;

/// Failure modes specific to the remove flow. Other failures (caller
/// authz, stable-memory write) come from `anchor_operation_with_authz_check`
/// and are mapped at the canister-method layer.
#[derive(Debug, Eq, PartialEq)]
pub enum RemoveError {
    /// The supplied address didn't match the anchor's bound recovery
    /// email — either the anchor has none, or it has a different
    /// one. Same observable variant for both because the FE has no
    /// reason to disambiguate ("not bound" vs "wrong address" both
    /// mean "your screen is out of date, refresh").
    NotRegistered,
}

/// Detach the bound recovery email from `anchor`. Returns the
/// `Operation` to record in the archive on success.
///
/// Idempotency: callers that retry an already-completed remove
/// receive `NotRegistered` rather than `Ok(())`. The FE shouldn't
/// retry on success, but if it does (e.g. a network blip), this
/// loud-fail surfaces the bug rather than silently masking it.
pub fn remove_credential(anchor: &mut Anchor, address: &str) -> Result<Operation, RemoveError> {
    let bound_address = anchor
        .email_recovery
        .as_ref()
        .map(|c| c.address.clone())
        .ok_or(RemoveError::NotRegistered)?;

    if !bound_address.eq_ignore_ascii_case(address) {
        return Err(RemoveError::NotRegistered);
    }

    anchor.email_recovery = None;

    // TODO(email-recovery): once the archive `Operation` enum gains
    // an `EmailRecoveryRemove` variant, switch to it. Using the
    // metadata-replace placeholder here keeps the type-checker happy
    // without inventing new variants on a separate PR's surface.
    Ok(Operation::IdentityMetadataReplace {
        metadata_keys: vec![],
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::storage::anchor::Anchor;
    use internet_identity_interface::internet_identity::types::email_recovery::EmailRecoveryCredential;

    fn anchor_with(address: Option<&str>) -> Anchor {
        let mut a = Anchor {
            anchor_number: 1,
            devices: vec![],
            openid_credentials: vec![],
            email_recovery: None,
            metadata: None,
            name: None,
            created_at: None,
        };
        if let Some(addr) = address {
            a.email_recovery = Some(EmailRecoveryCredential {
                address: addr.to_string(),
                created_at: 100,
                last_used: None,
            });
        }
        a
    }

    #[test]
    fn happy_path_removes_credential() {
        let mut a = anchor_with(Some("alice@gmail.com"));
        let result = remove_credential(&mut a, "alice@gmail.com");
        assert!(result.is_ok());
        assert!(a.email_recovery.is_none());
    }

    #[test]
    fn case_insensitive_address_match() {
        let mut a = anchor_with(Some("alice@gmail.com"));
        // Frontend may pass back an unnormalised string in edge
        // cases; we still match if it's the same address.
        let result = remove_credential(&mut a, "Alice@Gmail.COM");
        assert!(result.is_ok());
    }

    #[test]
    fn unbound_anchor_returns_not_registered() {
        let mut a = anchor_with(None);
        let result = remove_credential(&mut a, "alice@gmail.com");
        assert_eq!(result, Err(RemoveError::NotRegistered));
    }

    #[test]
    fn wrong_address_returns_not_registered() {
        let mut a = anchor_with(Some("alice@gmail.com"));
        let result = remove_credential(&mut a, "bob@gmail.com");
        assert_eq!(result, Err(RemoveError::NotRegistered));
        // Crucially, the binding is still in place — we didn't
        // partially mutate.
        assert!(a.email_recovery.is_some());
    }

    #[test]
    fn double_remove_returns_not_registered() {
        let mut a = anchor_with(Some("alice@gmail.com"));
        let _ = remove_credential(&mut a, "alice@gmail.com").unwrap();
        let again = remove_credential(&mut a, "alice@gmail.com");
        assert_eq!(again, Err(RemoveError::NotRegistered));
    }
}
