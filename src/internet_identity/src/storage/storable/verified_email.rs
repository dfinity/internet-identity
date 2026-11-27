use internet_identity_interface::internet_identity::types::verified_email::VerifiedEmail;
use internet_identity_interface::internet_identity::types::Timestamp;
use minicbor::{Decode, Encode};

#[derive(Encode, Decode, Clone, Debug, Eq, PartialEq)]
#[cbor(map)]
pub struct StorableVerifiedEmail {
    /// Lowercased canonical address.
    #[n(0)]
    pub address: String,
    /// Nanoseconds since the Unix epoch.
    #[n(1)]
    pub verified_at: Timestamp,
    /// `Some(true)` for an Apple private relay address; absent counts as not one.
    #[n(2)]
    pub is_private_relay: Option<bool>,
}

impl From<VerifiedEmail> for StorableVerifiedEmail {
    fn from(value: VerifiedEmail) -> Self {
        let VerifiedEmail {
            address,
            verified_at,
            is_private_relay,
        } = value;
        Self {
            address,
            verified_at,
            is_private_relay,
        }
    }
}

impl From<StorableVerifiedEmail> for VerifiedEmail {
    fn from(value: StorableVerifiedEmail) -> Self {
        let StorableVerifiedEmail {
            address,
            verified_at,
            is_private_relay,
        } = value;
        Self {
            address,
            verified_at,
            is_private_relay,
        }
    }
}
