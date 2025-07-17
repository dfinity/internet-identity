use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use internet_identity_interface::internet_identity::types::MetadataEntryV2;
use minicbor::{Decode, Encode};
use serde_bytes::ByteBuf;
use std::borrow::Cow;
use std::collections::HashMap;

#[derive(Encode, Decode, Clone)]
pub enum StorableMetadataEntryV2 {
    #[n(0)]
    String(#[n(0)] String),
    #[n(1)]
    Bytes(#[n(0)] Vec<u8>),
    #[n(2)]
    Map(#[n(0)] HashMap<String, StorableMetadataEntryV2>),
}

impl Storable for StorableMetadataEntryV2 {
    fn to_bytes(&self) -> Cow<[u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableMetadataEntryV2");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<[u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorableMetadataEntryV2")
    }

    const BOUND: Bound = Bound::Unbounded;
}

impl From<MetadataEntryV2> for StorableMetadataEntryV2 {
    fn from(value: MetadataEntryV2) -> Self {
        match value {
            MetadataEntryV2::String(string) => StorableMetadataEntryV2::String(string),
            MetadataEntryV2::Bytes(buff) => StorableMetadataEntryV2::Bytes(buff.into_vec()),
            MetadataEntryV2::Map(map) => StorableMetadataEntryV2::Map(
                map.iter()
                    .map(|(k, v)| (k.clone(), v.clone().into()))
                    .collect(),
            ),
        }
    }
}

impl From<StorableMetadataEntryV2> for MetadataEntryV2 {
    fn from(value: StorableMetadataEntryV2) -> Self {
        match value {
            StorableMetadataEntryV2::String(string) => MetadataEntryV2::String(string),
            StorableMetadataEntryV2::Bytes(bytes) => MetadataEntryV2::Bytes(ByteBuf::from(bytes)),
            StorableMetadataEntryV2::Map(map) => MetadataEntryV2::Map(
                map.iter()
                    .map(|(k, v)| (k.clone(), v.clone().into()))
                    .collect(),
            ),
        }
    }
}
