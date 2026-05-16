use crate::storage::storable::email_recovery_credential::StorableEmailRecoveryCredential;
use crate::storage::storable::openid_credential::StorableOpenIdCredential;
use crate::storage::storable::passkey_credential::StorablePasskeyCredential;
use crate::storage::storable::recovery_key::StorableRecoveryKey;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

#[derive(Encode, Decode, Clone)]
#[cbor(map)]
pub struct StorableAnchor {
    #[n(0)]
    pub name: Option<String>,
    #[n(1)]
    pub openid_credentials: Vec<StorableOpenIdCredential>,
    #[n(2)]
    pub created_at_ns: Option<u64>,
    #[n(3)]
    pub passkey_credentials: Option<Vec<StorablePasskeyCredential>>,
    #[n(4)]
    pub recovery_keys: Option<Vec<StorableRecoveryKey>>,
    /// Bound recovery emails (see `docs/ongoing/email-recovery.md`
    /// PR #3836). The current canister API enforces at most one
    /// entry; the data model is a `Vec` so multi-credential support
    /// can land without another schema bump. `Option` so anchors
    /// written under the previous schema decode cleanly — same
    /// pattern as `passkey_credentials` / `recovery_keys` above.
    #[n(5)]
    pub email_recovery: Option<Vec<StorableEmailRecoveryCredential>>,
}

impl Storable for StorableAnchor {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableAnchor");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorableAnchor")
    }

    const BOUND: Bound = Bound::Unbounded;
}
