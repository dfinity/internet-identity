use crate::archive::{ArchiveData, ArchiveState};
use crate::openid::OpenIdCredential;
use crate::state::PersistentState;
use crate::stats::activity_stats::activity_counter::active_anchor_counter::ActiveAnchorCounter;
use crate::stats::activity_stats::{ActivityStats, CompletedActivityStats, OngoingActivityStats};
use crate::storage::account::{Account, AccountReference, Application};
use crate::storage::anchor::{Anchor, Device};
use crate::storage::{Header, StorageError, MAX_ENTRIES};
use crate::Storage;
use candid::Principal;
use ic_stable_structures::{Memory, VectorMemory};
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, ArchiveConfig, DeviceProtection, FrontendHostname, KeyType, Purpose,
};
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
    memory.write(0, &hex::decode("494943090500000040e2010000000000f1fb090000000000000843434343434343434343434343434343434343434343434343434343434343430002000000000000000000000000000000000000000000000").unwrap());

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
    let mut anchor = storage.allocate_anchor().unwrap();
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
    storage.allocate_anchor().unwrap();

    let anchor = Anchor::new(222);

    let result = storage.create(anchor);
    assert!(matches!(result, Err(StorageError::BadAnchorNumber(_))))
}

#[test]
fn should_not_read_using_anchor_number_outside_allocated_range() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((123, 456), memory);
    storage.allocate_anchor().unwrap();

    let result = storage.read(222);
    assert!(matches!(result, Err(StorageError::BadAnchorNumber(_))))
}

#[test]
fn should_save_and_restore_persistent_state() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((123, 456), memory);
    storage.flush();
    storage.allocate_anchor().unwrap();

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

    storage.allocate_anchor().unwrap();
    storage.write_persistent_state(&sample_persistent_state());
    assert_eq!(storage.read_persistent_state(), sample_persistent_state());

    let anchor = storage.allocate_anchor().unwrap();
    storage.create(anchor).unwrap();

    assert_eq!(storage.read_persistent_state(), sample_persistent_state());
}

#[test]
fn should_write_and_update_openid_credential_lookup() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory);

    let mut anchor = storage.allocate_anchor().unwrap();
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

    let mut anchor = storage.allocate_anchor().unwrap();
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

    let mut anchor_0 = storage.allocate_anchor().unwrap();
    let mut anchor_1 = storage.allocate_anchor().unwrap();
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
fn should_write_account() {
    // Setup storage
    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory);

    // 1. Define anchor number and origin
    let anchor_number: AnchorNumber = 10_000;
    let origin: FrontendHostname = "https://some.origin".to_string();
    let account_name = None;

    // 2. Save anchor to stable memory
    let anchor = storage.allocate_anchor().unwrap();

    storage.create(anchor).unwrap();

    let app_num = storage.lookup_or_insert_application_number_with_origin(&origin);
    let app = Application {
        origin: origin.to_string(),
        total_accounts: 0u64,
    };
    assert_eq!(
        storage.lookup_application_with_application_number(&app_num),
        Some(app),
    );

    // 3. Read anchor and check that application_accounts is empty
    let read_anchor_1 = storage.read(anchor_number).unwrap();
    assert!(
        read_anchor_1.application_accounts(app_num).is_none(),
        "Initial anchor should have no accounts"
    );

    // 4. Create new account
    let new_account = Account::new(anchor_number, origin.clone(), account_name.clone(), None);

    // 5. Check that get_account_by_id returns None
    // Create AccountReference for lookup
    let account_ref_lookup = AccountReference {
        account_number: None, // Default account ID
        anchor_number,
        last_used: None, // Not relevant for lookup by ID
    };
    assert!(
        storage.read_account(&account_ref_lookup, &origin).is_none(),
        "Account should not exist before writing"
    );

    // Reconstruct the expected account as it would be retrieved (including potentially updated last_used)
    // Account::new sets last_used to None. Assuming get_account_by_id does not modify it on read for this test.
    let expected_retrieved_account = Account::reconstruct(
        new_account.account_number, // None for default account
        new_account.anchor_number,
        new_account.origin.clone(),
        new_account.last_used, // Should be None initially
        new_account.name.clone(),
    );

    // 6. Write account using write_account
    storage.write_account(new_account).unwrap();

    // 7. Use get_account_by_id to read the account and check that it's present.
    let retrieved_account = storage
        .read_account(&account_ref_lookup, &origin)
        .expect("Account should exist after writing");

    assert_eq!(
        retrieved_account, expected_retrieved_account,
        "Retrieved account does not match written account"
    );

    // 8. Read anchor and check that the new account is present in application_accounts
    let read_anchor_2 = storage.read(anchor_number).unwrap();
    let application_accounts = read_anchor_2.application_accounts(app_num);
    assert!(
        application_accounts.is_some(),
        "Anchor should have accounts after writing"
    );
    for acc_ref in application_accounts.unwrap() {
        assert_eq!(acc_ref, expected_retrieved_account.to_reference());
    }
}

#[test]
fn should_list_accounts() {
    // Setup storage
    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory);

    // 1. Define anchor number and origin
    let anchor_number: AnchorNumber = 10_000;
    let origin: FrontendHostname = "https://some.origin".to_string();
    let account_name = None;

    // 2. Save anchor to stable memory
    let anchor = storage.allocate_anchor().unwrap();
    storage.create(anchor).unwrap();

    // Look up or insert application number
    let _app_num = storage.lookup_or_insert_application_number_with_origin(&origin);

    // 3. Create new account
    let new_account = Account::new(anchor_number, origin.clone(), account_name.clone(), None);
    let expected_account_ref = new_account.to_reference();

    // 4. Write account using write_account
    storage.write_account(new_account).unwrap();

    // 5. Use list_accounts to read the account and check that it's present.
    let listed_accounts = storage.list_accounts(&anchor_number, &origin);

    // 6. Assert that the list contains exactly one account and it matches the expected one
    assert_eq!(
        listed_accounts.len(),
        1,
        "Expected exactly one account to be listed"
    );
    assert_eq!(
        listed_accounts[0], expected_account_ref,
        "Listed account reference does not match the written account reference"
    );
}

fn sample_device() -> Device {
    Device {
        pubkey: ByteBuf::from("hello world, I am a public key"),
        alias: "my test device".to_string(),
        credential_id: Some(ByteBuf::from("this is the credential id")),
        purpose: Purpose::Authentication,
        key_type: KeyType::Unknown,
        protection: DeviceProtection::Unprotected,
        origin: None,
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
