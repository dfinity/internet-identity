//! Who Internet Identity says this call is for.
//!
//! The IC verifies the signature over the caller info before the callee runs,
//! so nothing here is cryptography. The caller principal is ignored: it
//! carries no authority, and the account in the info is who the call is for.

use crate::types::SenderInfo;
use candid::Principal;

const DOMAIN: &str = "notification-sender-info";

/// `<domain><origin><account>`, each length-prefixed, as Internet Identity
/// signs it.
pub fn decode(info: &[u8]) -> Option<SenderInfo> {
    let mut rest = info;

    let (domain, after_domain) = field(rest)?;
    if domain != DOMAIN.as_bytes() {
        return None;
    }
    rest = after_domain;

    let (origin, after_origin) = field(rest)?;
    let origin = std::str::from_utf8(origin).ok()?.to_string();
    rest = after_origin;

    let (account, after_account) = field(rest)?;
    if !after_account.is_empty() {
        return None;
    }

    Some(SenderInfo {
        origin,
        account: Principal::try_from_slice(account).ok()?,
    })
}

fn field(bytes: &[u8]) -> Option<(&[u8], &[u8])> {
    let (length, rest) = bytes.split_first()?;
    let length = usize::from(*length);
    if rest.len() < length {
        return None;
    }
    Some(rest.split_at(length))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn encode(domain: &str, origin: &str, account: Principal) -> Vec<u8> {
        let mut blob = Vec::new();
        for part in [domain.as_bytes(), origin.as_bytes(), account.as_slice()] {
            blob.push(part.len() as u8);
            blob.extend_from_slice(part);
        }
        blob
    }

    fn account() -> Principal {
        Principal::from_text("un4fu-tqaaa-aaaab-qadjq-cai").unwrap()
    }

    #[test]
    fn decodes_what_internet_identity_signs() {
        let info = encode(DOMAIN, "https://app.example.com", account());
        let decoded = decode(&info).expect("well-formed");

        assert_eq!(decoded.origin, "https://app.example.com");
        assert_eq!(decoded.account, account());
    }

    #[test]
    fn refuses_another_flows_domain() {
        let info = encode("ic-sender-info", "https://app.example.com", account());
        assert!(decode(&info).is_none());
    }

    #[test]
    fn refuses_truncated_or_padded_blobs() {
        let info = encode(DOMAIN, "https://app.example.com", account());

        assert!(decode(&info[..info.len() - 1]).is_none(), "truncated");
        assert!(decode(&[]).is_none(), "empty");

        let mut padded = info.clone();
        padded.push(0);
        assert!(decode(&padded).is_none(), "trailing bytes");
    }

    #[test]
    fn refuses_a_length_that_runs_past_the_end() {
        assert!(decode(&[200, 1, 2, 3]).is_none());
    }
}
