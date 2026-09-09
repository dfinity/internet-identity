use crate::storage::storable::browser_description::StorableBrowserDescription;
use crate::storage::storable::browser_id::StorableBrowserId;
use internet_identity_interface::internet_identity::types::Timestamp;
use minicbor::{Decode, Encode};

#[derive(Encode, Decode, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
#[cbor(map)]
pub struct StorableBrowser {
    #[n(0)]
    pub id: StorableBrowserId,
    /// What this browser reported about itself when it registered. Immutable: a
    /// sign-in that reports something else is a browser this anchor has not seen, and
    /// registers under its own entry.
    #[n(1)]
    pub description: StorableBrowserDescription,
    #[n(2)]
    pub created_at: Timestamp,
    #[n(3)]
    pub last_used: Timestamp,
    #[cbor(n(4), with = "minicbor::bytes")]
    pub current_browser_key: Vec<u8>,
    #[cbor(n(5), with = "minicbor::bytes")]
    pub next_browser_key: Vec<u8>,
}
