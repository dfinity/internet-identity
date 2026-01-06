use crate::archive::{ArchiveData, ArchiveState};
use crate::openid::OpenIdCredential;
use crate::state::PersistentState;
use crate::stats::activity_stats::activity_counter::active_anchor_counter::ActiveAnchorCounter;
use crate::stats::activity_stats::{ActivityStats, CompletedActivityStats, OngoingActivityStats};
use crate::storage::account::{CreateAccountParams, ReadAccountParams};
use crate::storage::anchor::{Anchor, Device};
use crate::storage::{Header, StorageError, MAX_ENTRIES};
use crate::Storage;
use candid::Principal;
use ic_stable_structures::{Memory, VectorMemory};
use internet_identity_interface::internet_identity::types::{
    ArchiveConfig, DeviceProtection, KeyType, Purpose,
};
use pretty_assertions::assert_eq;
use serde_bytes::ByteBuf;
use std::collections::HashMap;

const HEADER_SIZE: usize = 58;

#[test]
fn should_match_actual_header_size() {
    // if this test fails, make sure the change was intentional and upgrade as well as rollback still work!
    assert_eq!(std::mem::size_of::<Header>(), HEADER_SIZE);
}

#[test]
fn should_report_max_number_of_entries_for_256gb() {
    // The maximum number of entries that could be supported by the canister without making any changes
    // is constant. This test now exists to make sure any dev is aware of the limit if making changes
    // to the underlying constants.
    assert_eq!(MAX_ENTRIES, 67_106_816);
}

#[test]
fn should_serialize_header_v9() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((1, 2), memory.clone());
    storage.update_salt([5u8; 32]);
    storage.flush();

    assert_eq!(storage.version(), 9);
    let mut buf = vec![0; HEADER_SIZE];
    memory.read(0, &mut buf);
    assert_eq!(buf, hex::decode("49494309000000000100000000000000020000000000000000100505050505050505050505050505050505050505050505050505050505050505").unwrap());
}

#[test]
fn should_recover_header_from_memory_v9() {
    let memory = VectorMemory::default();
    memory.grow(1);
    memory.write(0, &hex::decode("494943090500000040e2010000000000f1fb090000000000000843434343434343434343434343434343434343434343434343434343434343430002000000000000000000000000000000000000000000000000").unwrap());

    let storage = Storage::from_memory(memory);
    assert_eq!(storage.assigned_anchor_number_range(), (123456, 654321));
    assert_eq!(storage.salt().unwrap(), &[67u8; 32]);
    assert_eq!(storage.anchor_count(), 5);
    assert_eq!(storage.version(), 9);
}

#[test]
fn should_read_previous_write() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((12345, 678910), memory);
    let mut anchor = storage.allocate_anchor(0).unwrap();
    let anchor_number = anchor.anchor_number();

    anchor.add_device(sample_device()).unwrap();
    storage.create(anchor.clone()).unwrap();

    let read_anchor = storage.read(anchor_number).unwrap();
    assert_eq!(anchor, read_anchor);
}

#[test]
fn should_not_write_using_anchor_number_outside_allocated_range() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((123, 456), memory);
    storage.allocate_anchor(0).unwrap();

    let anchor = Anchor::new(222, 333);

    let result = storage.create(anchor);
    assert!(
        matches!(result, Err(StorageError::BadAnchorNumber(_))),
        "result = {:?}",
        result
    )
}

#[test]
fn should_not_read_using_anchor_number_outside_allocated_range() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((123, 456), memory);
    storage.allocate_anchor(0).unwrap();

    let result = storage.read(222);
    assert!(matches!(result, Err(StorageError::BadAnchorNumber(_))))
}

#[test]
fn should_save_and_restore_persistent_state() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((123, 456), memory);
    storage.flush();
    storage.allocate_anchor(0).unwrap();

    let persistent_state = sample_persistent_state();

    storage.write_persistent_state(&persistent_state);
    assert_eq!(storage.read_persistent_state(), persistent_state);
}

#[test]
fn should_read_default_persistent_state_from_new_storage() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory);
    storage.flush();

    assert_eq!(storage.read_persistent_state(), PersistentState::default());
}

#[test]
fn should_not_overwrite_persistent_state_with_next_anchor_v9() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory.clone());
    storage.flush();

    storage.allocate_anchor(0).unwrap();
    storage.write_persistent_state(&sample_persistent_state());
    assert_eq!(storage.read_persistent_state(), sample_persistent_state());

    let anchor = storage.allocate_anchor(0).unwrap();
    storage.create(anchor).unwrap();

    assert_eq!(storage.read_persistent_state(), sample_persistent_state());
}

#[test]
fn should_write_and_update_openid_credential_lookup() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory);

    let mut anchor = storage.allocate_anchor(0).unwrap();
    let openid_credential_0 = openid_credential(0);
    let openid_credential_1 = openid_credential(1);
    let openid_credential_2 = openid_credential(2);
    anchor
        .add_openid_credential(openid_credential_0.clone())
        .unwrap();
    anchor
        .add_openid_credential(openid_credential_1.clone())
        .unwrap();

    // Check if both anchor and OpenID credential lookups are written to storage
    storage.create(anchor.clone()).unwrap();
    assert_eq!(storage.read(anchor.anchor_number()).unwrap(), anchor);
    assert_eq!(
        storage
            .lookup_anchor_with_openid_credential(&openid_credential_0.key())
            .unwrap(),
        anchor.anchor_number()
    );
    assert_eq!(
        storage
            .lookup_anchor_with_openid_credential(&openid_credential_1.key())
            .unwrap(),
        anchor.anchor_number()
    );

    // Check if OpenID credential lookup is cleaned up from storage when anchor is written
    anchor
        .remove_openid_credential(&openid_credential_0.key())
        .unwrap();
    storage.update(anchor.clone()).unwrap();
    assert_eq!(
        storage.lookup_anchor_with_openid_credential(&openid_credential_0.key()),
        None
    );
    assert_eq!(
        storage
            .lookup_anchor_with_openid_credential(&openid_credential_1.key())
            .unwrap(),
        anchor.anchor_number()
    );

    // Check if OpenID credential lookup is written to storage when anchor is written
    anchor
        .add_openid_credential(openid_credential_2.clone())
        .unwrap();
    storage.update(anchor.clone()).unwrap();
    assert_eq!(
        storage.lookup_anchor_with_openid_credential(&openid_credential_0.key()),
        None
    );
    assert_eq!(
        storage
            .lookup_anchor_with_openid_credential(&openid_credential_1.key())
            .unwrap(),
        anchor.anchor_number()
    );
    assert_eq!(
        storage
            .lookup_anchor_with_openid_credential(&openid_credential_2.key())
            .unwrap(),
        anchor.anchor_number()
    );
}

#[test]
fn should_write_and_update_device_credential_lookup() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory);

    let mut anchor = storage.allocate_anchor(0).unwrap();
    let device_0 = Device {
        pubkey: ByteBuf::from(vec![0]),
        credential_id: Some(ByteBuf::from(vec![0])),
        ..sample_device()
    };
    let device_1 = Device {
        pubkey: ByteBuf::from(vec![1]),
        credential_id: Some(ByteBuf::from(vec![1])),
        ..sample_device()
    };
    let device_2 = Device {
        pubkey: ByteBuf::from(vec![2]),
        credential_id: Some(ByteBuf::from(vec![2])),
        ..sample_device()
    };
    anchor.add_device(device_0.clone()).unwrap();
    anchor.add_device(device_1.clone()).unwrap();

    // Check if both anchor and device credential lookups are written to storage
    storage.create(anchor.clone()).unwrap();
    assert_eq!(storage.read(anchor.anchor_number()).unwrap(), anchor);
    assert_eq!(
        storage
            .lookup_anchor_with_device_credential(&device_0.credential_id.clone().unwrap())
            .unwrap(),
        anchor.anchor_number()
    );
    assert_eq!(
        storage
            .lookup_anchor_with_device_credential(&device_1.credential_id.clone().unwrap())
            .unwrap(),
        anchor.anchor_number()
    );

    // Check if device credential lookup is cleaned up from storage when anchor is written
    anchor.remove_device(&device_0.pubkey).unwrap();
    storage.update(anchor.clone()).unwrap();
    assert_eq!(
        storage.lookup_anchor_with_device_credential(&device_0.credential_id.clone().unwrap()),
        None
    );
    assert_eq!(
        storage
            .lookup_anchor_with_device_credential(&device_1.credential_id.clone().unwrap())
            .unwrap(),
        anchor.anchor_number()
    );

    // Check if device credential lookup is written to storage when anchor is written
    anchor.add_device(device_2.clone()).unwrap();
    storage.update(anchor.clone()).unwrap();
    assert_eq!(
        storage.lookup_anchor_with_device_credential(&device_0.credential_id.clone().unwrap()),
        None
    );
    assert_eq!(
        storage
            .lookup_anchor_with_device_credential(&device_1.credential_id.clone().unwrap())
            .unwrap(),
        anchor.anchor_number()
    );
    assert_eq!(
        storage
            .lookup_anchor_with_device_credential(&device_2.credential_id.clone().unwrap())
            .unwrap(),
        anchor.anchor_number()
    );
}

#[test]
fn should_not_overwrite_device_credential_lookup() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory);

    let mut anchor_0 = storage.allocate_anchor(0).unwrap();
    let mut anchor_1 = storage.allocate_anchor(0).unwrap();
    let device_0 = Device {
        pubkey: ByteBuf::from(vec![0]),
        credential_id: Some(ByteBuf::from(vec![0])),
        ..sample_device()
    };
    let device_1 = Device {
        pubkey: ByteBuf::from(vec![1]),
        credential_id: device_0.credential_id.clone(),
        ..sample_device()
    };
    anchor_0.add_device(device_0.clone()).unwrap();
    anchor_1.add_device(device_1.clone()).unwrap();

    // Make sure that lookup of anchor_0 is not overwritten with anchor_1
    storage.create(anchor_0.clone()).unwrap();
    storage.create(anchor_1.clone()).unwrap();
    assert_eq!(
        storage
            .lookup_anchor_with_device_credential(&device_0.credential_id.clone().unwrap())
            .unwrap(),
        anchor_0.anchor_number()
    );
    // Make sure that lookup of anchor_0 is not remove by anchor_1
    anchor_1.remove_device(&device_1.pubkey).unwrap();
    storage.update(anchor_1.clone()).unwrap();
    assert_eq!(
        storage
            .lookup_anchor_with_device_credential(&device_0.credential_id.clone().unwrap())
            .unwrap(),
        anchor_0.anchor_number()
    );
}

#[test]
fn should_set_account_last_used() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory);
    let origin = "https://example.com".to_string();

    // Create an anchor
    let anchor = storage.allocate_anchor(0).unwrap();
    let anchor_number = anchor.anchor_number();
    storage.create(anchor).unwrap();

    // Create an additional account for this anchor and origin
    let account = storage
        .create_additional_account(CreateAccountParams {
            anchor_number,
            name: "Test Account".to_string(),
            origin: origin.clone(),
        })
        .unwrap();

    let account_number = account.account_number.unwrap();

    // Initially, last_used should be None
    let read_account = storage
        .read_account(ReadAccountParams {
            anchor_number,
            origin: &origin,
            account_number: Some(account_number),
            known_app_num: None,
        })
        .unwrap();
    assert_eq!(read_account.last_used, None);

    // Set last_used for the additional account
    let timestamp = 123456789u64;
    let result = storage.set_account_last_used(
        anchor_number,
        origin.clone(),
        Some(account_number),
        timestamp,
    );
    assert!(result.is_some());

    // Verify last_used was updated
    let read_account = storage
        .read_account(ReadAccountParams {
            anchor_number,
            origin: &origin,
            account_number: Some(account_number),
            known_app_num: None,
        })
        .unwrap();
    assert_eq!(read_account.last_used, Some(timestamp));

    // Update last_used again with a new timestamp
    let new_timestamp = 987654321u64;
    let result = storage.set_account_last_used(
        anchor_number,
        origin.clone(),
        Some(account_number),
        new_timestamp,
    );
    assert!(result.is_some());

    // Verify last_used was updated to the new timestamp
    let read_account = storage
        .read_account(ReadAccountParams {
            anchor_number,
            origin: &origin,
            account_number: Some(account_number),
            known_app_num: None,
        })
        .unwrap();
    assert_eq!(read_account.last_used, Some(new_timestamp));
}

#[test]
fn should_set_account_last_used_for_synthethic_account() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory);
    let origin = "https://example.com".to_string();

    // Create an anchor
    let anchor = storage.allocate_anchor(0).unwrap();
    let anchor_number = anchor.anchor_number();
    storage.create(anchor).unwrap();

    // Set last_used for the synthetic account (account_number = None)
    let timestamp = 555555u64;
    let result = storage.set_account_last_used(anchor_number, origin.clone(), None, timestamp);
    assert!(result.is_none());

    // Verify last_used was updated for the synthetic account
    let read_account = storage
        .read_account(ReadAccountParams {
            anchor_number,
            origin: &origin,
            account_number: None,
            known_app_num: None,
        })
        .unwrap();
    // Because the account reference doesn't exist, the `last_used` is not updated.
    assert_eq!(read_account.last_used, None);
}

#[test]
fn should_set_account_last_used_for_synthetic_account_with_reference() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory);
    let origin = "https://example.com".to_string();

    // Create an anchor
    let anchor = storage.allocate_anchor(0).unwrap();
    let anchor_number = anchor.anchor_number();
    storage.create(anchor).unwrap();

    // Create an additional account to force creation of account references
    storage
        .create_additional_account(CreateAccountParams {
            anchor_number,
            name: "Test Account".to_string(),
            origin: origin.clone(),
        })
        .unwrap();

    // Set last_used for the synthetic account (account_number = None)
    let timestamp = 555555u64;
    let result = storage.set_account_last_used(anchor_number, origin.clone(), None, timestamp);
    assert!(result.is_some());

    // Verify last_used was updated for the synthetic account
    let read_account = storage
        .read_account(ReadAccountParams {
            anchor_number,
            origin: &origin,
            account_number: None,
            known_app_num: None,
        })
        .unwrap();
    assert_eq!(read_account.last_used, Some(timestamp));
}

#[test]
fn should_return_none_when_setting_last_used_for_nonexistent_account() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory);
    let origin = "https://example.com".to_string();

    // Create an anchor
    let anchor = storage.allocate_anchor(0).unwrap();
    let anchor_number = anchor.anchor_number();
    storage.create(anchor).unwrap();

    // Try to set last_used for a non-existent account number
    let nonexistent_account_number = 99999u64;
    let timestamp = 123456u64;
    let result = storage.set_account_last_used(
        anchor_number,
        origin,
        Some(nonexistent_account_number),
        timestamp,
    );

    // Should return None because the account doesn't exist
    assert!(result.is_none());
}

#[test]
fn should_return_none_when_setting_last_used_for_nonexistent_origin() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory);

    // Create an anchor
    let anchor = storage.allocate_anchor(0).unwrap();
    let anchor_number = anchor.anchor_number();
    storage.create(anchor).unwrap();

    // Try to set last_used for an origin that hasn't been registered
    let nonexistent_origin = "https://nonexistent.com".to_string();
    let timestamp = 123456u64;
    let result = storage.set_account_last_used(anchor_number, nonexistent_origin, None, timestamp);

    // Should return None because the origin/application doesn't exist
    assert!(result.is_none());
}

fn sample_device() -> Device {
    Device {
        pubkey: ByteBuf::from("hello world, I am a public key"),
        alias: "my test device".to_string(),
        credential_id: Some(ByteBuf::from("this is the credential id")),
        aaguid: None,
        purpose: Purpose::Authentication,
        key_type: KeyType::Unknown,
        protection: DeviceProtection::Unprotected,
        origin: Some("https://id.ai".to_string()),
        last_usage_timestamp: Some(1234),
        metadata: None,
    }
}

fn openid_credential(n: u8) -> OpenIdCredential {
    OpenIdCredential {
        iss: "https://example.com".into(),
        sub: n.to_string(),
        aud: "example-aud".into(),
        last_usage_timestamp: Some(n.into()),
        metadata: HashMap::default(),
    }
}
fn sample_persistent_state() -> PersistentState {
    PersistentState {
        archive_state: ArchiveState::Created {
            data: ArchiveData {
                sequence_number: 39,
                archive_canister: Principal::from_text("2h5ob-7aaaa-aaaad-aacya-cai").unwrap(),
            },
            config: ArchiveConfig {
                module_hash: [99u8; 32],
                entries_buffer_limit: 10_000,
                polling_interval_ns: 60_000_000_000,
                entries_fetch_limit: 1_000,
            },
        },
        canister_creation_cycles_cost: 12_346_000_000,
        active_anchor_stats: ActivityStats {
            completed: CompletedActivityStats {
                daily_events: Some(ActiveAnchorCounter {
                    start_timestamp: 965485,
                    counter: 99,
                }),
                monthly_events: None,
            },
            ongoing: OngoingActivityStats {
                daily_events: ActiveAnchorCounter {
                    start_timestamp: 5648954321,
                    counter: 44,
                },
                monthly_events: vec![ActiveAnchorCounter {
                    start_timestamp: 549843248,
                    counter: 66,
                }],
            },
        },
        ..PersistentState::default()
    }
}

#[cfg(test)]
mod application_lookup_tests {
    use super::*;
    use crate::storage::storable::application::StorableOriginSha256;
    use ic_stable_structures::VectorMemory;
    use pretty_assertions::assert_eq;

    #[track_caller]
    fn assert_application_lookup<M: Memory + Clone>(
        storage: &Storage<M>,
        origin: &str,
        expected_new: Option<u64>,
    ) {
        let origin = origin.to_string();
        let origin_sha256 = StorableOriginSha256::from_origin(&origin);

        assert_eq!(
            storage.lookup_application_number_with_origin(&origin),
            expected_new,
            "Unexpected result from lookup_application_number_with_origin for origin {}",
            origin
        );
        assert_eq!(
            storage
                .lookup_application_with_origin_memory
                .get(&origin_sha256),
            expected_new,
            "Unexpected lookup_application_with_origin_memory entry for key {}",
            origin_sha256
        );
    }

    #[test]
    fn should_create_new_application_with_sha256_lookup() {
        let mut storage = Storage::new((10, 20), VectorMemory::default());
        let origin = "https://example.com".to_string();

        let app_number = storage.lookup_or_insert_application_number_with_origin(&origin);

        // Should create application number 0 for first application
        assert_eq!(app_number, 0);

        // Should exist in both old and new lookup maps
        assert_application_lookup(&storage, &origin, Some(0));

        // Should exist in applications storage
        let stored_app = storage.stable_application_memory.get(&0);
        assert!(stored_app.is_some());
        let app = stored_app.unwrap();
        assert_eq!(app.origin, origin);
        assert_eq!(app.stored_accounts, 0);
        assert_eq!(app.stored_account_references, 0);
    }

    #[test]
    fn should_return_existing_application_number() {
        let mut storage = Storage::new((10, 20), VectorMemory::default());
        let origin = "https://example.com".to_string();

        // Create application first time
        let app_number1 = storage.lookup_or_insert_application_number_with_origin(&origin);

        // Should return same application number on second call
        let app_number2 = storage.lookup_or_insert_application_number_with_origin(&origin);

        assert_eq!(app_number1, app_number2);
        assert_eq!(app_number1, 0);
    }

    #[test]
    fn should_create_different_numbers_for_different_origins() {
        let mut storage = Storage::new((10, 20), VectorMemory::default());

        let origin1 = "https://example.com".to_string();
        let origin2 = "https://different.com".to_string();
        let origin3 = "https://another.org".to_string();

        let app_num1 = storage.lookup_or_insert_application_number_with_origin(&origin1);
        let app_num2 = storage.lookup_or_insert_application_number_with_origin(&origin2);
        let app_num3 = storage.lookup_or_insert_application_number_with_origin(&origin3);

        assert_eq!(app_num1, 0);
        assert_eq!(app_num2, 1);
        assert_eq!(app_num3, 2);

        // All should be present in both lookup maps
        assert_application_lookup(&storage, &origin1, Some(0));
        assert_application_lookup(&storage, &origin2, Some(1));
        assert_application_lookup(&storage, &origin3, Some(2));
    }

    #[test]
    fn new_storage_should_have_empty_maps() {
        let storage = Storage::new((10, 20), VectorMemory::default());
        assert_eq!(storage.lookup_application_with_origin_memory.len(), 0);
    }

    #[test]
    fn should_handle_very_long_origins_with_sha256() {
        let mut storage = Storage::new((10, 20), VectorMemory::default());

        let long_origin = format!("https://{}.com", "a".repeat(20_000));

        let app_number = storage.lookup_or_insert_application_number_with_origin(&long_origin);
        assert_eq!(app_number, 0);

        // Should be findable in both maps
        assert_application_lookup(&storage, &long_origin, Some(0));

        // Application should be stored with full origin
        let stored_app = storage.stable_application_memory.get(&0).unwrap();
        assert_eq!(stored_app.origin, long_origin);
    }

    #[test]
    fn should_increment_application_counter_correctly() {
        let mut storage = Storage::new((10, 20), VectorMemory::default());

        let origins = [
            "https://app1.com".to_string(),
            "https://app2.com".to_string(),
            "https://app3.com".to_string(),
        ];

        for (i, origin) in origins.iter().enumerate() {
            let app_number = storage.lookup_or_insert_application_number_with_origin(origin);
            assert_eq!(app_number, i as u64);

            // Total application count should increment
            assert_eq!(storage.get_total_application_count(), (i + 1) as u64);
        }
    }

    #[test]
    fn should_preserve_existing_data_on_storage_restart() {
        let memory = VectorMemory::default();
        let origin = "https://persistent-test.com".to_string();

        // Create storage and add application
        {
            let mut storage = Storage::new((10, 20), memory.clone());
            let app_number = storage.lookup_or_insert_application_number_with_origin(&origin);
            assert_eq!(app_number, 0);
        }

        // Recreate storage from same memory
        let storage = Storage::from_memory(memory);

        // Should find existing application in old lookup
        assert_eq!(
            storage.lookup_application_number_with_origin(&origin),
            Some(0)
        );

        // Should find it in both maps
        assert_application_lookup(&storage, &origin, Some(0));

        // Application should still exist in storage
        let stored_app = storage.stable_application_memory.get(&0);
        assert!(stored_app.is_some());
    }
}

#[cfg(test)]
mod storable_origin_sha256_tests {
    use crate::storage::storable::application::StorableOriginSha256;
    use ic_stable_structures::Storable;
    use pretty_assertions::assert_eq;
    use std::borrow::Cow;

    #[test]
    fn should_create_different_hashes_for_different_origins() {
        let origin1 = "https://example.com".to_string();
        let origin2 = "https://different.com".to_string();

        let hash1 = StorableOriginSha256::from_origin(&origin1);
        let hash2 = StorableOriginSha256::from_origin(&origin2);

        assert_ne!(hash1, hash2);
    }

    #[test]
    fn should_create_same_hash_for_same_origin() {
        let origin = "https://example.com".to_string();

        let hash1 = StorableOriginSha256::from_origin(&origin);
        let hash2 = StorableOriginSha256::from_origin(&origin);

        assert_eq!(hash1, hash2);
    }

    #[test]
    fn should_be_storable_and_retrievable() {
        let origin = "https://storable-test.com".to_string();
        let original_hash = StorableOriginSha256::from_origin(&origin);

        // Test round-trip serialization
        let bytes = original_hash.to_bytes();
        let recovered_hash = StorableOriginSha256::from_bytes(bytes);

        assert_eq!(original_hash, recovered_hash);
    }

    #[test]
    fn should_handle_short_byte_arrays() {
        let short_bytes = [1, 2, 3, 4, 5];
        let hash = StorableOriginSha256::from_bytes(Cow::Borrowed(&short_bytes));

        // Should be padded with zeros
        let bytes = hash.to_bytes();
        assert_eq!(bytes.len(), 32);
        assert_eq!(&bytes[..5], &short_bytes[..]);
        assert_eq!(&bytes[5..], &[0u8; 27]);
    }

    #[test]
    fn should_handle_oversized_byte_arrays() {
        let oversized_bytes = [42u8; 40];
        let hash = StorableOriginSha256::from_bytes(Cow::Borrowed(&oversized_bytes));

        // Should be truncated to 32 bytes
        let bytes = hash.to_bytes();
        assert_eq!(bytes.len(), 32);
        assert_eq!(&bytes[..], &[42u8; 32]);
    }

    #[test]
    fn should_handle_empty_byte_array() {
        let empty_bytes: Vec<u8> = vec![];
        let hash = StorableOriginSha256::from_bytes(Cow::Borrowed(&empty_bytes));

        // Should be all zeros
        let bytes = hash.to_bytes();
        assert_eq!(bytes.len(), 32);
        assert_eq!(&bytes[..], &[0u8; 32]);
    }

    #[test]
    fn should_respect_storable_bound() {
        assert_eq!(StorableOriginSha256::BOUND.max_size(), 32);
        assert!(StorableOriginSha256::BOUND.is_fixed_size());
    }
}

#[cfg(test)]
mod sync_anchor_with_recovery_phrase_principal_index_tests {
    use super::*;
    use crate::storage::anchor::Device;
    use candid::Principal;
    use internet_identity_interface::internet_identity::types::{KeyType, PublicKey};
    use pretty_assertions::assert_eq;

    fn pubkey(n: u8) -> PublicKey {
        vec![n].into()
    }

    fn seed_phrase_device(pubkey: PublicKey) -> Device {
        Device {
            pubkey,
            alias: "seed".to_string(),
            credential_id: None,
            aaguid: None,
            purpose: Purpose::Recovery,
            key_type: KeyType::SeedPhrase,
            protection: DeviceProtection::Unprotected,
            origin: None,
            metadata: None,
            last_usage_timestamp: None,
        }
    }

    fn other_device(pubkey: PublicKey) -> Device {
        Device {
            pubkey,
            alias: "other".to_string(),
            credential_id: None,
            aaguid: None,
            purpose: Purpose::Authentication,
            key_type: KeyType::Unknown,
            protection: DeviceProtection::Unprotected,
            origin: None,
            metadata: None,
            last_usage_timestamp: None,
        }
    }

    fn pre_populate_index<M: Memory + Clone>(
        storage: &mut Storage<M>,
        anchor_number: u64,
        devices: &[Device],
    ) {
        for d in devices {
            let principal = Principal::self_authenticating(&d.pubkey);
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .insert(principal, anchor_number);
        }
    }

    #[test]
    fn adds_new_seed_phrase_principals() {
        let mut storage = Storage::new((0, 10), ic_stable_structures::DefaultMemoryImpl::default());
        let anchor_number = 1;
        let prev = vec![];
        let curr = vec![seed_phrase_device(pubkey(42)), other_device(pubkey(99))];

        storage.sync_anchor_with_recovery_phrase_principal_index(anchor_number, &prev, &curr);

        let principal = Principal::self_authenticating(pubkey(42));
        assert_eq!(
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .get(&principal),
            Some(anchor_number)
        );
        // Should not add non-seed phrase device
        let principal_other = Principal::self_authenticating(pubkey(99));
        assert_eq!(
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .get(&principal_other),
            None
        );

        // Bonus: What if the same recovery phrase is used again by another user?
        let another_anchor_number = 2;
        let prev = vec![];
        let curr = vec![seed_phrase_device(pubkey(42))];

        storage.sync_anchor_with_recovery_phrase_principal_index(
            another_anchor_number,
            &prev,
            &curr,
        );

        // Index should not change for this principal.
        assert_eq!(
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .get(&principal),
            Some(anchor_number)
        );
    }

    #[test]
    fn removes_old_seed_phrase_principals() {
        let mut storage = Storage::new((0, 10), ic_stable_structures::DefaultMemoryImpl::default());
        let anchor_number = 2;
        let prev = vec![seed_phrase_device(pubkey(1)), seed_phrase_device(pubkey(2))];
        let curr = vec![seed_phrase_device(pubkey(2))];

        pre_populate_index(&mut storage, anchor_number, &prev);

        storage.sync_anchor_with_recovery_phrase_principal_index(anchor_number, &prev, &curr);

        let principal_removed = Principal::self_authenticating(pubkey(1));
        let principal_kept = Principal::self_authenticating(pubkey(2));
        assert_eq!(
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .get(&principal_removed),
            None
        );
        assert_eq!(
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .get(&principal_kept),
            Some(anchor_number)
        );
    }

    #[test]
    fn no_change_if_same_devices() {
        let mut storage = Storage::new((0, 10), ic_stable_structures::DefaultMemoryImpl::default());
        let anchor_number = 3;
        let prev = vec![seed_phrase_device(pubkey(7))];
        let curr = vec![seed_phrase_device(pubkey(7))];

        pre_populate_index(&mut storage, anchor_number, &prev);

        storage.sync_anchor_with_recovery_phrase_principal_index(anchor_number, &prev, &curr);

        let principal = Principal::self_authenticating(pubkey(7));
        assert_eq!(
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .get(&principal),
            Some(anchor_number)
        );
    }

    #[test]
    fn handles_empty_current_and_previous() {
        let mut storage = Storage::new((0, 10), ic_stable_structures::DefaultMemoryImpl::default());
        let anchor_number = 4;
        let prev = vec![];
        let curr = vec![];

        storage.sync_anchor_with_recovery_phrase_principal_index(anchor_number, &prev, &curr);

        // Should remain empty
        assert_eq!(
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .iter()
                .count(),
            0
        );
    }

    #[test]
    fn adds_and_removes_seed_phrase_principals_in_single_call() {
        let mut storage = Storage::new((0, 10), ic_stable_structures::DefaultMemoryImpl::default());
        let anchor_number = 5;
        let prev = vec![
            seed_phrase_device(pubkey(1)),
            seed_phrase_device(pubkey(2)),
            seed_phrase_device(pubkey(3)),
        ];
        let curr = vec![
            seed_phrase_device(pubkey(2)),
            seed_phrase_device(pubkey(4)),
            seed_phrase_device(pubkey(5)),
        ];

        pre_populate_index(&mut storage, anchor_number, &prev);

        storage.sync_anchor_with_recovery_phrase_principal_index(anchor_number, &prev, &curr);

        // Devices 1 and 3 should be removed
        let principal_1 = Principal::self_authenticating(pubkey(1));
        let principal_3 = Principal::self_authenticating(pubkey(3));
        assert_eq!(
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .get(&principal_1),
            None
        );
        assert_eq!(
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .get(&principal_3),
            None
        );

        // Device 2 should remain
        let principal_2 = Principal::self_authenticating(pubkey(2));
        assert_eq!(
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .get(&principal_2),
            Some(anchor_number)
        );

        // Devices 4 and 5 should be added
        let principal_4 = Principal::self_authenticating(pubkey(4));
        let principal_5 = Principal::self_authenticating(pubkey(5));
        assert_eq!(
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .get(&principal_4),
            Some(anchor_number)
        );
        assert_eq!(
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .get(&principal_5),
            Some(anchor_number)
        );
    }

    #[test]
    fn removes_seed_phrase_principals_only_for_specified_anchor() {
        let mut storage = Storage::new((0, 10), ic_stable_structures::DefaultMemoryImpl::default());
        let anchor_number_1 = 10;
        let anchor_number_2 = 20;

        // Device present for both anchors
        let device_shared = seed_phrase_device(pubkey(42));
        let device_unique_1 = seed_phrase_device(pubkey(1));
        let device_unique_2 = seed_phrase_device(pubkey(2));

        // Pre-populate index for both anchors
        pre_populate_index(
            &mut storage,
            anchor_number_1,
            &[device_shared.clone(), device_unique_1.clone()],
        );
        pre_populate_index(
            &mut storage,
            anchor_number_2,
            &[device_shared.clone(), device_unique_2.clone()],
        );

        // Remove device_shared and device_unique_1 from anchor_number_1
        let prev = vec![device_shared.clone(), device_unique_1.clone()];
        let curr = vec![]; // all removed for anchor_number_1

        storage.sync_anchor_with_recovery_phrase_principal_index(anchor_number_1, &prev, &curr);

        let principal_shared = Principal::self_authenticating(&device_shared.pubkey);
        let principal_unique_1 = Principal::self_authenticating(&device_unique_1.pubkey);
        let principal_unique_2 = Principal::self_authenticating(&device_unique_2.pubkey);

        // Should be removed for anchor_number_1
        assert_eq!(
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .get(&principal_shared),
            Some(anchor_number_2)
        );
        assert_eq!(
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .get(&principal_unique_1),
            None
        );
        // Should remain for anchor_number_2
        assert_eq!(
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .get(&principal_unique_2),
            Some(anchor_number_2)
        );
    }

    #[test]
    fn write_moves_recovery_phrase_principal_between_anchors() {
        let mut storage = Storage::new((0, 10), ic_stable_structures::DefaultMemoryImpl::default());

        let d1 = other_device(pubkey(1));
        let d2 = seed_phrase_device(pubkey(2)); // recovery device
        let d3 = other_device(pubkey(3));

        let mut anchor_a = storage.allocate_anchor(111).unwrap();
        anchor_a.add_device(d1.clone()).unwrap();
        anchor_a.add_device(d2.clone()).unwrap();

        // Code under test (I)
        storage.create(anchor_a.clone()).unwrap();

        let mut anchor_b = storage.allocate_anchor(222).unwrap();
        anchor_b.add_device(d3.clone()).unwrap();

        // Code under test (II)
        storage.create(anchor_b.clone()).unwrap();

        let principal_d2 = Principal::self_authenticating(&d2.pubkey);

        // d2 should be indexed for anchor_a
        assert_eq!(
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .iter()
                .collect::<Vec<_>>(),
            vec![(principal_d2, anchor_a.anchor_number())]
        );

        // Remove d2 from anchor_a
        anchor_a.remove_device(&d2.pubkey).unwrap();

        // Code under test (III)
        storage.update(anchor_a).unwrap();

        // No recovery devices are left in the index.
        assert_eq!(
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .iter()
                .collect::<Vec<_>>(),
            vec![]
        );

        // Add d2 to anchor_b
        anchor_b.add_device(d2).unwrap();

        // Code under test (IV)
        storage.update(anchor_b.clone()).unwrap();

        // d2 should now be indexed for anchor_b only
        assert_eq!(
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .iter()
                .collect::<Vec<_>>(),
            vec![(principal_d2, anchor_b.anchor_number())]
        );
    }
}
