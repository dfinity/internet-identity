//! `verified_email_remove` — drop a verified address from an anchor.
//!
//! Parallel to `email_recovery::remove::remove_credential`. Same
//! design notes apply:
//!
//! - The caller must *name* the address being removed, even though
//!   the FE could in principle pass the index. Matching by string
//!   keeps the FE honest if it ever caches a stale list, and lets
//!   the confirmation modal show the address verbatim.
//! - Emits `Operation::RemoveVerifiedEmail` for the archive stream.
//!   The variant carries no payload — anchor + timestamp on the
//!   surrounding entry are enough to answer "who dropped a verified
//!   email when?" without leaking the address itself.
//!
//! Idempotency: a second remove of the same address returns
//! `NotRegistered` rather than `Ok(())`. The FE shouldn't retry on
//! success, but if it does (network blip), this loud-fail surfaces
//! the bug instead of silently masking it.

use crate::storage::anchor::Anchor;
use internet_identity_interface::archive::types::Operation;

/// Why a `verified_email_remove` call failed. The verified-email flow
/// defines its own error rather than sharing `email_recovery::RemoveError`
/// so the two anchor primitives stay structurally independent — see
/// the module rustdoc on [`crate::verified_emails`].
#[derive(Debug, Eq, PartialEq)]
pub enum RemoveError {
    /// No entry on the anchor's `verified_emails` list matched the
    /// supplied address.
    NotRegistered,
}

/// Detach the verified email at `address` from `anchor`. Returns
/// the archive `Operation` on success.
pub fn remove(anchor: &mut Anchor, address: &str) -> Result<Operation, RemoveError> {
    let position = anchor
        .verified_emails
        .iter()
        .position(|c| c.address.eq_ignore_ascii_case(address))
        .ok_or(RemoveError::NotRegistered)?;

    anchor.verified_emails.remove(position);

    Ok(Operation::RemoveVerifiedEmail)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::storage::anchor::Anchor;
    use internet_identity_interface::internet_identity::types::email_recovery::VerifiedEmail;

    fn anchor_with(addresses: &[&str]) -> Anchor {
        let mut a = Anchor {
            anchor_number: 1,
            devices: vec![],
            openid_credentials: vec![],
            email_recovery: vec![],
            verified_emails: vec![],
            metadata: None,
            name: None,
            created_at: None,
        };
        for addr in addresses {
            a.verified_emails.push(VerifiedEmail {
                address: (*addr).to_string(),
                verified_at: 100,
            });
        }
        a
    }

    #[test]
    fn happy_path_removes_one() {
        let mut a = anchor_with(&["alice@gmail.com", "bob@example.com"]);
        let result = remove(&mut a, "alice@gmail.com");
        assert!(result.is_ok());
        assert_eq!(a.verified_emails.len(), 1);
        assert_eq!(a.verified_emails[0].address, "bob@example.com");
    }

    #[test]
    fn case_insensitive_match() {
        let mut a = anchor_with(&["alice@gmail.com"]);
        let result = remove(&mut a, "Alice@Gmail.COM");
        assert!(result.is_ok());
        assert!(a.verified_emails.is_empty());
    }

    #[test]
    fn empty_anchor_returns_not_registered() {
        let mut a = anchor_with(&[]);
        let result = remove(&mut a, "alice@gmail.com");
        assert_eq!(result, Err(RemoveError::NotRegistered));
    }

    #[test]
    fn wrong_address_returns_not_registered() {
        let mut a = anchor_with(&["alice@gmail.com"]);
        let result = remove(&mut a, "bob@gmail.com");
        assert_eq!(result, Err(RemoveError::NotRegistered));
        // No partial mutation.
        assert_eq!(a.verified_emails.len(), 1);
    }

    #[test]
    fn double_remove_returns_not_registered() {
        let mut a = anchor_with(&["alice@gmail.com"]);
        remove(&mut a, "alice@gmail.com").unwrap();
        let again = remove(&mut a, "alice@gmail.com");
        assert_eq!(again, Err(RemoveError::NotRegistered));
    }
}
