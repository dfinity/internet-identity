use candid::Principal;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

/// Value stored in `push_sender_memory`: which canister may send push
/// notifications *as* a given origin.
///
/// This is what makes `notify_user` usable by the party that actually has
/// something to say. The recipient's `in_app_principal` is a canister-signature
/// principal derived from II's own seed, so only the user's browser can ever
/// present it as `caller()`; an inter-canister call always arrives as the
/// calling canister's own principal. Authorizing on "prove you are the
/// recipient" therefore rejects every real sender, which is why senders are
/// registered against the origin instead.
#[derive(Encode, Decode, Clone, Debug, PartialEq)]
#[cbor(map)]
pub struct StorablePushSenderRegistration {
    /// The canister allowed to send as this origin, as raw principal bytes.
    #[cbor(n(0), with = "minicbor::bytes")]
    pub sender: Vec<u8>,
    #[n(1)]
    pub registered_at_ns: u64,
}

impl StorablePushSenderRegistration {
    pub fn new(sender: Principal, registered_at_ns: u64) -> Self {
        Self {
            sender: sender.as_slice().to_vec(),
            registered_at_ns,
        }
    }

    /// The registered sender, or `None` when the stored bytes are not a valid
    /// principal. Malformed rows fail closed at the call site rather than
    /// trapping — this is decoded on the `notify_user` path, which must not
    /// take the canister down over one bad row.
    pub fn sender_principal(&self) -> Option<Principal> {
        // Empty is rejected explicitly: `try_from_slice(&[])` succeeds and
        // yields the management canister principal, so a malformed or cleared
        // row would otherwise decode to `aaaaa-aa` rather than "no sender".
        if self.sender.is_empty() {
            return None;
        }
        Principal::try_from_slice(&self.sender).ok()
    }
}

impl Storable for StorablePushSenderRegistration {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        match minicbor::encode(self, &mut buffer) {
            Ok(()) => Cow::Owned(buffer),
            // Defensive: the shape is fixed and cannot fail to encode. An
            // empty value decodes back to `None` above rather than trapping.
            Err(_) => Cow::Owned(Vec::new()),
        }
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        minicbor::decode(&bytes).unwrap_or(Self {
            sender: Vec::new(),
            registered_at_ns: 0,
        })
    }

    const BOUND: Bound = Bound::Unbounded;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn round_trips() {
        let principal = Principal::from_text("rdmx6-jaaaa-aaaaa-aaadq-cai").unwrap();
        let entry = StorablePushSenderRegistration::new(principal, 1234);

        let decoded = StorablePushSenderRegistration::from_bytes(entry.to_bytes());

        assert_eq!(decoded, entry);
        assert_eq!(decoded.sender_principal(), Some(principal));
    }

    #[test]
    fn malformed_bytes_decode_to_no_sender_instead_of_trapping() {
        let decoded = StorablePushSenderRegistration::from_bytes(Cow::Owned(vec![0xff, 0xff]));

        assert_eq!(decoded.sender_principal(), None);
    }
}
