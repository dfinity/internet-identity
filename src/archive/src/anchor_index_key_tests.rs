use crate::AnchorIndexKey;
use ic_stable_structures::Storable;
use std::borrow::Cow;

#[test]
fn should_have_correct_length() {
    let index_key = AnchorIndexKey {
        anchor: 1234,
        timestamp: 5678,
        log_index: 23,
    };
    let bytes = index_key.to_bytes();
    assert_eq!(bytes.len(), 24);
    assert_eq!(
        bytes,
        hex::decode("00000000000004d2000000000000162e0000000000000017").unwrap()
    );
}

#[test]
fn should_deserialize_correctly() {
    let decoded = hex::decode("00000000000f434200000002e1b7ad6e00000000000003b1").unwrap();
    let index_key = AnchorIndexKey::from_bytes(Cow::from(decoded));

    assert_eq!(
        index_key,
        AnchorIndexKey {
            anchor: 1000258,
            timestamp: 12376845678,
            log_index: 945,
        }
    );
}
