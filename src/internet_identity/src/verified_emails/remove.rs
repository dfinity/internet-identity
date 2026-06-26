use crate::storage::anchor::Anchor;
use internet_identity_interface::archive::types::Operation;

#[derive(Debug, Eq, PartialEq)]
pub enum RemoveError {
    NotRegistered,
}

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
    use internet_identity_interface::internet_identity::types::verified_email::VerifiedEmail;

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
