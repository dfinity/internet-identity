use crate::storage::storable::special_device_migration::SpecialDeviceMigration;
use minicbor::{Decode, Encode};

#[derive(Encode, Decode, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[cbor(map)]
pub struct StorablePasskeyCredential {
    #[cbor(n(0), with = "minicbor::bytes")]
    pub pubkey: Vec<u8>,
    #[cbor(n(1), with = "minicbor::bytes")]
    pub credential_id: Vec<u8>,
    #[n(2)]
    pub origin: String,
    #[n(3)]
    pub created_at_ns: Option<u64>,
    #[n(4)]
    pub last_usage_timestamp_ns: Option<u64>,
    #[n(5)]
    pub alias: Option<String>,
    #[cbor(n(6), with = "minicbor::bytes")]
    pub aaguid: Option<Vec<u8>>,

    #[n(7)]
    pub special_device_migration: Option<SpecialDeviceMigration>,
}
