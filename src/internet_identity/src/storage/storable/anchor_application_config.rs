use crate::storage::storable::account_number::StorableAccountNumber;
use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use internet_identity_interface::internet_identity::types::Timestamp;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

#[derive(Encode, Decode, Default, Clone, Ord, Eq, PartialEq, PartialOrd, Debug)]
#[cbor(map)]
pub struct AnchorApplicationConfig {
    #[n(0)]
    pub default_account_number: Option<StorableAccountNumber>, // None is the unreserved synthetic account
    #[n(1)]
    pub notifications_consented_at_ns: Option<Timestamp>, // None if the app may not notify
    #[n(2)]
    pub last_notified_at_ns: Option<Timestamp>, // None if the app never reached a browser
}

impl Storable for AnchorApplicationConfig {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode AnchorApplicationConfig");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode AnchorApplicationConfig")
    }

    const BOUND: Bound = Bound::Unbounded;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_roundtrip_through_storable() {
        let config = AnchorApplicationConfig {
            default_account_number: None,
            notifications_consented_at_ns: Some(1),
            last_notified_at_ns: Some(2),
        };
        assert_eq!(
            AnchorApplicationConfig::from_bytes(config.to_bytes()),
            config
        );
    }

    /// Configs written before `last_notified_at_ns` existed are CBOR maps without key 2,
    /// and decode as an app that never notified.
    #[test]
    fn should_decode_config_without_last_notified() {
        let mut buffer = Vec::new();
        let mut encoder = minicbor::Encoder::new(&mut buffer);
        encoder.map(1).unwrap();
        encoder.u8(1).unwrap().u64(7).unwrap();

        let decoded = AnchorApplicationConfig::from_bytes(Cow::Borrowed(&buffer));
        assert_eq!(decoded.notifications_consented_at_ns, Some(7));
        assert_eq!(decoded.last_notified_at_ns, None);
    }
}
