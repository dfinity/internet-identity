use crate::archive::{ArchiveData, ArchiveState};
use crate::openid::OpenIdCredential;
use crate::state::PersistentState;
use crate::stats::activity_stats::activity_counter::active_anchor_counter::ActiveAnchorCounter;
use crate::stats::activity_stats::{ActivityStats, CompletedActivityStats, OngoingActivityStats};
use crate::storage::account::{InternalAccount, InternalAccountReference};
use crate::storage::anchor::{Anchor, Device};
use crate::storage::{CreateAdditionalAccountParams, Header, StorageError, UpdateAccountParams, MAX_ENTRIES};
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
fn should_write_additional_account() {
    // Setup storage
    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory);

    // 1. Define additional account parameters
    let anchor_number: AnchorNumber = 10_000;
    let origin: FrontendHostname = "https://some.origin".to_string();
    let account_name = "account name".to_string();

    // 2. Save anchor to stable memory
    let anchor = storage.allocate_anchor().unwrap();
    storage.create(anchor).unwrap();

    // 3. Additional account and application don't exist yet.
    let acc_ref = InternalAccountReference {
        account_number: None,
        anchor_number,
        last_used: None,
    };
    let additional_account_1 = storage.read_account(&acc_ref, &origin);
    assert!(
        additional_account_1.is_none(),
        "Initial anchor should have no accounts"
    );
    assert!(
        storage.lookup_application_with_origin(&origin).is_none(),
    );

    // 4. Create additional account
    let new_account_params = CreateAdditionalAccountParams {
        anchor_number,
        origin: origin.clone(),
        name: account_name.clone(),
    };
    storage.create_additional_account(new_account_params).unwrap();

    // 5. Check that read_account returns additional account and creates application.
    let account_ref_lookup = InternalAccountReference {
        account_number: Some(1), // First account created
        anchor_number,
        last_used: None,
    };
    let additional_account = storage.read_account(&account_ref_lookup, &origin).unwrap();
    let expected_account = InternalAccount {
        account_number: Some(1),
        anchor_number,
        origin: origin.clone(),
        name: Some(account_name.clone()),
        last_used: None,
    };
    assert_eq!(additional_account, expected_account);
    assert!(
        storage.lookup_application_with_origin(&origin).is_some(),
    );
}

#[test]
fn should_list_accounts() {
    // Setup storage
    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory);

    // 1. Define additional account parameters
    let anchor_number: AnchorNumber = 10_000;
    let origin: FrontendHostname = "https://some.origin".to_string();
    let account_name = "account name".to_string();

    // 2. Save anchor to stable memory
    let anchor = storage.allocate_anchor().unwrap();
    storage.create(anchor).unwrap();

    // 3. List accounts returns default account
    let listed_accounts = storage.list_accounts(&anchor_number, &origin).unwrap();
    assert_eq!(listed_accounts.len(), 1);
    assert!(listed_accounts[0].account_number.is_none());

    // 4. Create new account
    let new_account = CreateAdditionalAccountParams {
        anchor_number,
        origin: origin.clone(),
        name: account_name.clone(),
    };
    let expected_additional_account_ref = InternalAccountReference {
        account_number: Some(1),
        anchor_number,
        last_used: None,
    };
    let expected_default_account_ref = InternalAccountReference {
        account_number: None,
        anchor_number,
        last_used: None,
    };
    storage.create_additional_account(new_account).unwrap();

    // 5. List accounts returns default account
    let listed_accounts = storage.list_accounts(&anchor_number, &origin).unwrap();

    // 6. Assert that the list contains exactly two accounts and it matches the expected one
    assert_eq!(
        listed_accounts.len(),
        2,
        "Expected exactly two accounts to be listed"
    );
    assert_eq!(
        listed_accounts[0], expected_default_account_ref,
        "Default account reference is missing from the listed accounts."
    );
    assert_eq!(
        listed_accounts[1], expected_additional_account_ref,
        "Additional account reference is missing from the listed accounts."
    );
}

#[test]
fn should_list_identity_accounts() {
    // Setup storage
    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory);

    // 1. Define additional account parameters
    let anchor_number: AnchorNumber = 10_000;
    let account_name = "account name".to_string();
    let origin: FrontendHostname = "https://some.origin".to_string();
    let origin_2: FrontendHostname = "https://some-other.origin".to_string();

    // 2. Save anchor to stable memory
    let anchor = storage.allocate_anchor().unwrap();
    storage.create(anchor).unwrap();

    // 3. List accounts returns default account
    let listed_accounts = storage.list_identity_accounts(anchor_number);
    assert_eq!(listed_accounts.len(), 0);

    // 4. Create additional account
    let new_account_params = CreateAdditionalAccountParams {
        anchor_number,
        origin: origin.clone(),
        name: account_name.clone(),
    };
    storage.create_additional_account(new_account_params).unwrap();

    // 5. List accounts returns default account
    let listed_accounts = storage.list_identity_accounts(anchor_number);
    // Default account + additional account for the origin application.
    assert_eq!(listed_accounts.len(), 2);

    // 6. Create additional account
    let new_account_params = CreateAdditionalAccountParams {
        anchor_number,
        origin: origin_2.clone(),
        name: account_name.clone(),
    };
    storage.create_additional_account(new_account_params).unwrap();

    // 7. List accounts returns default account
    let listed_accounts = storage.list_identity_accounts(anchor_number);
    // Default account + additional account for the origin_2 application.
    assert_eq!(listed_accounts.len(), 4);
}

#[test]
fn should_update_default_account() {
    // Setup storage
    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory);

    // 1. Define parameters
    let anchor_number: AnchorNumber = 10_000;
    let origin: FrontendHostname = "https://some.origin".to_string();
    let account_name = "account name".to_string();

    // 2. Default account exists withuot creating it
    let default_account = storage.read_account(&InternalAccountReference { account_number: None, anchor_number, last_used: None }, &origin).unwrap();
    let expected_unreserved_account = InternalAccount {
        account_number: None,
        anchor_number,
        origin: origin.clone(),
        last_used: None,
        name: None,
    };
    assert_eq!(default_account, expected_unreserved_account);

    // 3. Update default account
    let updated_account_params = UpdateAccountParams {
        anchor_number,
        origin: origin.clone(),
        name: account_name.clone(),
        account_number: None,
    };
    let new_account_number = storage.update_account(updated_account_params).unwrap();

    // 4. Check that the default account has been created with the updated values.
    let updated_account = storage.read_account(&InternalAccountReference { account_number: Some(new_account_number), anchor_number, last_used: None }, &origin).unwrap();
    let expected_updated_account = InternalAccount {
        account_number: Some(new_account_number),
        anchor_number,
        origin: origin.clone(),
        last_used: None,
        name: Some(account_name),
    };
    assert_eq!(updated_account, expected_updated_account);
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
