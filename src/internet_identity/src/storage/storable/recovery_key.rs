use minicbor::{Decode, Encode};

#[derive(Encode, Decode, Clone)]
#[cbor(map)]
pub struct StorableRecoveryKey {
    #[cbor(n(0), with = "minicbor::bytes")]
    pub pubkey: Vec<u8>,
    #[n(1)]
    pub created_at_ns: Option<u64>,
    #[n(2)]
    pub last_usage_timestamp_ns: Option<u64>,
    #[n(3)]
    pub is_protected: Option<bool>,
}
