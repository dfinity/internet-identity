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
}

impl From<VerifiedEmail> for StorableVerifiedEmail {
    fn from(value: VerifiedEmail) -> Self {
        Self {
            address: value.address,
            verified_at: value.verified_at,
        }
    }
}

impl From<StorableVerifiedEmail> for VerifiedEmail {
    fn from(value: StorableVerifiedEmail) -> Self {
        Self {
            address: value.address,
            verified_at: value.verified_at,
        }
    }
}
