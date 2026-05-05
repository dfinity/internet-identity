//! On-disk representation of an `EmailRecoveryCredential`.
//!
//! Stored inline on the anchor (see `StorableAnchor`) — *not* in a
//! separate stable map. `minicbor-derive`'s `#[cbor(map)]` shape is
//! forward-compatible across optional-field additions, so an anchor
//! that doesn't yet carry an email-recovery credential just decodes
//! with `email_recovery: None`.

use internet_identity_interface::internet_identity::types::email_recovery::EmailRecoveryCredential;
use internet_identity_interface::internet_identity::types::Timestamp;
use minicbor::{Decode, Encode};

#[derive(Encode, Decode, Clone, Debug, Eq, PartialEq)]
#[cbor(map)]
pub struct StorableEmailRecoveryCredential {
    /// Lowercased canonical address — see
    /// [`EmailRecoveryCredential::address`].
    #[n(0)]
    pub address: String,
    /// Unix-seconds.
    #[n(1)]
    pub created_at: Timestamp,
    /// Unix-seconds — `None` until the first `smtp_request` actually
    /// uses this credential to authorise a recovery.
    #[n(2)]
    pub last_used: Option<Timestamp>,
}

impl From<EmailRecoveryCredential> for StorableEmailRecoveryCredential {
    fn from(value: EmailRecoveryCredential) -> Self {
        Self {
            address: value.address,
            created_at: value.created_at,
            last_used: value.last_used,
        }
    }
}

impl From<StorableEmailRecoveryCredential> for EmailRecoveryCredential {
    fn from(value: StorableEmailRecoveryCredential) -> Self {
        Self {
            address: value.address,
            created_at: value.created_at,
            last_used: value.last_used,
        }
    }
}
