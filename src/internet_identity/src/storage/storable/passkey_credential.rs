use minicbor::{Decode, Encode};

#[derive(Encode, Decode, Clone)]
#[cbor(map)]
pub struct StorablePasskeyCredential {
    #[n(0)]
    pub pubkey: Vec<u8>,
    #[n(1)]
    pub credential_id: Vec<u8>,
    #[n(2)]
    pub origin: String,
    #[n(3)]
    pub created_at_ns: Option<u64>,
    #[n(4)]
    pub last_usage_timestamp_ns: Option<u64>,
    #[n(5)]
    pub alias: Option<String>,
    #[n(6)]
    pub aaguid: Option<Vec<u8>>,
}
