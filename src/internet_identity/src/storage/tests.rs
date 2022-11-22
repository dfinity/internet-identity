use crate::archive::{ArchiveData, ArchiveInfo, ArchiveState};
use crate::state::{Anchor, DeviceDataInternal, PersistentState};
use crate::storage::{Header, PersistentStateError, StorageError};
use crate::Storage;
use candid::Principal;
use ic_stable_structures::{Memory, VectorMemory};
use internet_identity_interface::{DeviceProtection, KeyType, MigrationState, Purpose, UserNumber};
use serde_bytes::ByteBuf;

const WASM_PAGE_SIZE: u64 = 1 << 16;
const HEADER_SIZE: usize = 84;
const RESERVED_HEADER_BYTES_V1: u64 = 512;
const RESERVED_HEADER_BYTES_V3: u64 = 2 * WASM_PAGE_SIZE;
const PERSISTENT_STATE_MAGIC: [u8; 4] = *b"IIPS";

#[test]
fn should_match_actual_header_size() {
    // if this test fails, make sure the change was intentional and upgrade as well as rollback still work!
    assert_eq!(std::mem::size_of::<Header>(), HEADER_SIZE);
}

#[test]
fn should_report_max_number_of_entries_for_8gb() {
    let memory = VectorMemory::default();
    let storage = Storage::new((1, 2), memory);
    assert_eq!(storage.max_entries(), 3_774_873);
}

#[test]
fn should_serialize_header() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((1, 2), memory.clone());
    storage.update_salt([05u8; 32]);
    storage.flush();

    let mut buf = vec![0; HEADER_SIZE];
    memory.read(0, &mut buf);
    assert_eq!(buf, hex::decode("494943030000000001000000000000000200000000000000001005050505050505050505050505050505050505050505050505050505050505050000020000000000000000000000000000000000000000000000").unwrap());
}

#[test]
fn should_recover_header_from_memory_v1() {
    let memory = VectorMemory::default();
    memory.grow(1);
    memory.write(0, &hex::decode("494943010500000040e2010000000000f1fb090000000000000843434343434343434343434343434343434343434343434343434343434343430002000000000000000000000000000000000000000000000000").unwrap());

    let storage = Storage::from_memory(memory).unwrap();
    assert_eq!(storage.assigned_user_number_range(), (123456, 654321));
    assert_eq!(storage.salt().unwrap(), &[67u8; 32]);
    assert_eq!(storage.user_count(), 5);
    assert_eq!(storage.version(), 1);
}

#[test]
fn should_enforce_size_limit_v1() {
    let mut storage = layout_v1_storage((123456, 654321), VectorMemory::default());
    let user_number = storage.allocate_user_number().unwrap();
    let mut anchor = sample_anchor_record();
    anchor.devices.get_mut(0).unwrap().pubkey = ByteBuf::from(vec![1u8; 2048]);
    let result = storage.write(user_number, anchor);
    assert!(matches!(
        result,
        Err(StorageError::EntrySizeLimitExceeded(_))
    ));
}

#[test]
fn should_enforce_size_limit_v3() {
    let mut storage = Storage::new((123456, 654321), VectorMemory::default());
    let user_number1 = storage.allocate_user_number().unwrap();

    // anchor with size ~2200 bytes now fits v3 storage (see test above)
    let mut anchor1 = sample_anchor_record();
    anchor1.devices.get_mut(0).unwrap().pubkey = ByteBuf::from(vec![1u8; 2048]);
    let result = storage.write(user_number1, anchor1);
    assert!(matches!(result, Ok(())));

    // anchor with size ~4200 bytes is still rejected
    let user_number2 = storage.allocate_user_number().unwrap();
    let mut anchor2 = sample_anchor_record();
    anchor2.devices.get_mut(0).unwrap().pubkey = ByteBuf::from(vec![1u8; 4096]);
    let result = storage.write(user_number2, anchor2);
    assert!(matches!(
        result,
        Err(StorageError::EntrySizeLimitExceeded(_))
    ));
}

#[test]
fn should_update_header() {
    // genesis header
    const TEST_HEADER: &'static str = "494943010500000040e2010000000000f1fb09000000000000084343434343434343434343434343434343434343434343434343434343434343";
    let memory = VectorMemory::default();
    memory.grow(1);
    memory.write(0, &hex::decode(TEST_HEADER).unwrap());

    let mut storage = Storage::from_memory(memory.clone()).unwrap();
    storage.set_user_number_range((1234567, 5_000_000));
    storage.allocate_user_number();
    storage.flush();

    let mut buf = vec![0; HEADER_SIZE];
    memory.read(0, &mut buf);
    assert_eq!(buf, hex::decode("494943010600000087d6120000000000404b4c0000000000000843434343434343434343434343434343434343434343434343434343434343430002000000000000000000000000000000000000000000000000").unwrap());
}

#[test]
fn should_serialize_first_record_v1() {
    const EXPECTED_LENGTH: usize = 192;
    let memory = VectorMemory::default();
    let mut storage = layout_v1_storage((123, 456), memory.clone());
    let user_number = storage.allocate_user_number().unwrap();
    assert_eq!(user_number, 123u64);

    let anchor = sample_anchor_record();
    storage.write(user_number, anchor.clone()).unwrap();

    let mut buf = [0u8; EXPECTED_LENGTH];
    memory.read(RESERVED_HEADER_BYTES_V1, &mut buf);
    let decoded_from_memory: Vec<DeviceDataInternal> = candid::decode_one(&buf[2..]).unwrap();
    assert_eq!(decoded_from_memory, anchor.devices);
}

#[test]
fn should_serialize_first_record_v3() {
    const EXPECTED_LENGTH: usize = 192;
    let memory = VectorMemory::default();
    let mut storage = Storage::new((123, 456), memory.clone());
    let user_number = storage.allocate_user_number().unwrap();
    assert_eq!(user_number, 123u64);

    let anchor = sample_anchor_record();
    storage.write(user_number, anchor.clone()).unwrap();

    let mut buf = [0u8; EXPECTED_LENGTH];
    memory.read(RESERVED_HEADER_BYTES_V3, &mut buf);
    let decoded_from_memory: Vec<DeviceDataInternal> = candid::decode_one(&buf[2..]).unwrap();
    assert_eq!(decoded_from_memory, anchor.devices);
}

#[test]
fn should_serialize_subsequent_record_to_expected_memory_location_v1() {
    const EXPECTED_LENGTH: usize = 192;
    const EXPECTED_RECORD_OFFSET: u64 = 204_800; // 100 * max anchor size
    let memory = VectorMemory::default();
    let mut storage = layout_v1_storage((123, 456), memory.clone());
    for _ in 0..100 {
        storage.allocate_user_number().unwrap();
    }
    let user_number = storage.allocate_user_number().unwrap();
    assert_eq!(user_number, 223u64);

    let anchor = sample_anchor_record();
    storage.write(user_number, anchor.clone()).unwrap();

    let mut buf = [0u8; EXPECTED_LENGTH];
    memory.read(RESERVED_HEADER_BYTES_V1 + EXPECTED_RECORD_OFFSET, &mut buf);
    let decoded_from_memory: Vec<DeviceDataInternal> = candid::decode_one(&buf[2..]).unwrap();
    assert_eq!(decoded_from_memory, anchor.devices);
}

#[test]
fn should_serialize_subsequent_record_to_expected_memory_location_v3() {
    const EXPECTED_LENGTH: usize = 192;
    const EXPECTED_RECORD_OFFSET: u64 = 409_600; // 100 * max anchor size
    let memory = VectorMemory::default();
    let mut storage = Storage::new((123, 456), memory.clone());
    for _ in 0..100 {
        storage.allocate_user_number().unwrap();
    }
    let user_number = storage.allocate_user_number().unwrap();
    assert_eq!(user_number, 223u64);

    let anchor = sample_anchor_record();
    storage.write(user_number, anchor.clone()).unwrap();

    let mut buf = [0u8; EXPECTED_LENGTH];
    memory.read(RESERVED_HEADER_BYTES_V3 + EXPECTED_RECORD_OFFSET, &mut buf);
    let decoded_from_memory: Vec<DeviceDataInternal> = candid::decode_one(&buf[2..]).unwrap();
    assert_eq!(decoded_from_memory, anchor.devices);
}

#[test]
fn should_not_write_using_anchor_number_outside_allocated_range() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((123, 456), memory.clone());
    storage.allocate_user_number().unwrap();

    let result = storage.write(222, sample_anchor_record().clone());
    assert!(matches!(result, Err(StorageError::BadUserNumber(_))))
}

#[test]
fn should_deserialize_first_record() {
    let memory = VectorMemory::default();
    memory.grow(1);
    let mut storage = layout_v1_storage((123, 456), memory.clone());
    let user_number = storage.allocate_user_number().unwrap();
    assert_eq!(user_number, 123u64);

    let anchor = sample_anchor_record();
    let buf = candid::encode_one(&anchor.devices).unwrap();
    memory.write(512, &(buf.len() as u16).to_le_bytes());
    memory.write(514, &buf);

    let read_from_storage = storage.read(123).unwrap();
    assert_eq!(read_from_storage, anchor);
}

#[test]
fn should_deserialize_subsequent_record_at_expected_memory_location() {
    const EXPECTED_RECORD_OFFSET: u64 = 204_800; // 100 * max anchor size
    let memory = VectorMemory::default();
    memory.grow(4); // grow memory to accommodate a write to EXPECTED_RECORD_OFFSET
    let mut storage = layout_v1_storage((123, 456), memory.clone());
    for _ in 0..100 {
        storage.allocate_user_number().unwrap();
    }
    let user_number = storage.allocate_user_number().unwrap();
    assert_eq!(user_number, 223u64);

    let anchor = sample_anchor_record();
    let buf = candid::encode_one(&anchor.devices).unwrap();
    memory.write(
        512 + EXPECTED_RECORD_OFFSET,
        &(buf.len() as u16).to_le_bytes(),
    );
    memory.write(514 + EXPECTED_RECORD_OFFSET, &buf);

    let read_from_storage = storage.read(223).unwrap();
    assert_eq!(read_from_storage, anchor);
}

#[test]
fn should_not_read_using_anchor_number_outside_allocated_range() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((123, 456), memory.clone());
    storage.allocate_user_number().unwrap();

    let result = storage.read(222);
    assert!(matches!(result, Err(StorageError::BadUserNumber(_))))
}

#[test]
fn should_save_and_restore_persistent_state() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((123, 456), memory.clone());
    storage.flush();
    storage.allocate_user_number().unwrap();

    let persistent_state = sample_persistent_state();

    storage.write_persistent_state(&persistent_state);
    assert_eq!(storage.read_persistent_state().unwrap(), persistent_state);
}

#[test]
fn should_save_persistent_state_at_expected_memory_address_v1() {
    let memory = VectorMemory::default();
    let mut storage = layout_v1_storage((10_000, 3_784_873), memory.clone());
    storage.flush();

    storage.write_persistent_state(&sample_persistent_state());

    let mut buf = vec![0u8; 4];
    memory.read(RESERVED_HEADER_BYTES_V1, &mut buf);
    assert_eq!(buf, PERSISTENT_STATE_MAGIC);
}

#[test]
fn should_save_persistent_state_at_expected_memory_address_v3() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory.clone());
    storage.flush();

    storage.write_persistent_state(&sample_persistent_state());

    let mut buf = vec![0u8; 4];
    memory.read(RESERVED_HEADER_BYTES_V3, &mut buf);
    assert_eq!(buf, PERSISTENT_STATE_MAGIC);
}

#[test]
fn should_not_find_persistent_state() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory.clone());
    storage.flush();

    let result = storage.read_persistent_state();
    assert!(matches!(result, Err(PersistentStateError::NotFound)))
}

#[test]
fn should_not_find_persistent_state_on_magic_bytes_mismatch() {
    let memory = VectorMemory::default();

    let mut storage = Storage::new((10_000, 3_784_873), memory.clone());
    storage.flush();

    memory.write(RESERVED_HEADER_BYTES_V1, b"IIPX"); // correct magic bytes are IIPS

    let result = storage.read_persistent_state();
    assert!(matches!(result, Err(PersistentStateError::NotFound)))
}

#[test]
fn should_save_persistent_state_at_expected_memory_address_with_anchors_v1() {
    const EXPECTED_ADDRESS: u64 = RESERVED_HEADER_BYTES_V1 + 100 * 2048; // number of anchors is 100

    let memory = VectorMemory::default();
    let mut storage = layout_v1_storage((10_000, 3_784_873), memory.clone());
    storage.flush();

    for _ in 0..100 {
        storage.allocate_user_number().unwrap();
    }

    storage.write_persistent_state(&sample_persistent_state());

    let mut buf = vec![0u8; 4];
    memory.read(EXPECTED_ADDRESS, &mut buf);
    assert_eq!(buf, PERSISTENT_STATE_MAGIC);
}

#[test]
fn should_save_persistent_state_at_expected_memory_address_with_anchors_v3() {
    const EXPECTED_ADDRESS: u64 = RESERVED_HEADER_BYTES_V3 + 100 * 4096; // number of anchors is 100

    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory.clone());
    storage.flush();

    for _ in 0..100 {
        storage.allocate_user_number().unwrap();
    }

    storage.write_persistent_state(&sample_persistent_state());

    let mut buf = vec![0u8; 4];
    memory.read(EXPECTED_ADDRESS, &mut buf);
    assert_eq!(buf, PERSISTENT_STATE_MAGIC);
}

/// This tests verifies that address calculation is correct for 64bit addresses.
/// Note: this test takes about 8GB of memory.
#[test]
fn should_save_persistent_state_at_expected_memory_address_with_many_anchors() {
    let memory = VectorMemory::default();
    memory.grow(1);
    memory.write(0, &hex::decode("49494301C0C62D001027000000000000a9c039000000000000084343434343434343434343434343434343434343434343434343434343434343").unwrap());
    const EXPECTED_ADDRESS: u64 = RESERVED_HEADER_BYTES_V1 + 3_000_000 * 2048; // number of anchors is 100

    let mut storage = Storage::from_memory(memory.clone()).unwrap();
    storage.write_persistent_state(&sample_persistent_state());

    let mut buf = vec![0u8; 4];
    memory.read(EXPECTED_ADDRESS, &mut buf);
    assert_eq!(buf, PERSISTENT_STATE_MAGIC);
}

/// This test verifies that storage correctly reports `NotFound` if the persistent state address
/// lies outside of the allocated stable memory range. This can happen on upgrade from a version
/// that did not serialize a persistent state into stable memory.
#[test]
fn should_not_panic_on_unallocated_persistent_state_mem_address() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory.clone());
    storage.flush();
    for _ in 0..32 {
        storage.allocate_user_number();
    }

    assert!(matches!(
        storage.read_persistent_state(),
        Err(PersistentStateError::NotFound)
    ));
}

#[test]
fn should_overwrite_persistent_state_with_next_anchor() {
    const EXPECTED_ADDRESS: u64 = RESERVED_HEADER_BYTES_V1 + 2048; // only one anchor exists

    let memory = VectorMemory::default();
    let mut storage = layout_v1_storage((10_000, 3_784_873), memory.clone());
    storage.flush();

    storage.allocate_user_number().unwrap();
    storage.write_persistent_state(&sample_persistent_state());

    let mut buf = vec![0u8; 4];
    memory.read(EXPECTED_ADDRESS, &mut buf);
    assert_eq!(buf, PERSISTENT_STATE_MAGIC);

    let anchor = storage.allocate_user_number().unwrap();
    storage.write(anchor, sample_anchor_record()).unwrap();

    let mut buf = vec![0u8; 4];
    memory.read(EXPECTED_ADDRESS, &mut buf);
    assert_ne!(buf, PERSISTENT_STATE_MAGIC);

    assert!(matches!(
        storage.read_persistent_state(),
        Err(PersistentStateError::NotFound)
    ));
}

#[test]
fn should_read_previously_stored_persistent_state() {
    const EXPECTED_ADDRESS: u64 = RESERVED_HEADER_BYTES_V1 + 3 * 2048; // 3 anchors
    const PERSISTENT_STATE_BYTES: &'static str = "4949505388000000000000004449444c066c02cbc282b70501f7f5cbfb07786c02faafb5ac020291ecada008046e036d7b6b03d1d3dab70b78b5c2d2b70d7fc8bbeff50d056c02c7e8ccee037884fbf0820968010001206363636363636363636363636363636363636363636363636363636363636363022700000000000000010a00000000006000b001018002e1df02000000";

    let memory = VectorMemory::default();
    // allocate space for the writes
    memory.grow(1);

    // write header so the number of users is set
    memory.write(0, &hex::decode("494943010300000040e2010000000000f1fb09000000000000084343434343434343434343434343434343434343434343434343434343434343").unwrap());
    memory.write(
        EXPECTED_ADDRESS,
        &hex::decode(PERSISTENT_STATE_BYTES).unwrap(),
    );

    let storage = Storage::from_memory(memory).unwrap();

    assert_eq!(
        storage.read_persistent_state().unwrap(),
        sample_persistent_state()
    );
}

#[test]
fn should_stay_in_v1_if_no_migration_configured() {
    let memory = VectorMemory::default();
    let mut storage = layout_v1_storage((10_000, 3_784_873), memory.clone());
    storage.flush();
    for _ in 0..32 {
        storage.allocate_user_number();
    }

    assert!(matches!(
        storage.migration_state(),
        MigrationState::NotStarted
    ));
}

#[test]
fn should_start_migration_when_configuring() {
    let memory = VectorMemory::default();
    let mut storage = layout_v1_storage((10_000, 3_784_873), memory.clone());
    storage.flush();
    for _ in 0..32 {
        storage.allocate_user_number();
    }

    storage.configure_migration(100);

    assert_eq!(
        storage.migration_state(),
        MigrationState::Started {
            anchors_left: 32,
            batch_size: 100
        }
    );
}

#[test]
fn should_migrate_anchors() {
    let memory = VectorMemory::default();
    let mut storage = layout_v1_storage((10_000, 3_784_873), memory.clone());
    storage.flush();
    for _ in 0..32 {
        storage.allocate_user_number();
    }

    storage.configure_migration(100);
    storage.write(10_000, sample_anchor_record()).unwrap();

    assert_eq!(storage.migration_state(), MigrationState::Finished);
}

#[test]
fn should_load_anchors_from_memory_after_migration() {
    let memory = VectorMemory::default();
    let mut storage = layout_v1_storage((10_000, 3_784_873), memory.clone());
    storage.flush();
    for _ in 0..32 {
        storage.allocate_user_number();
    }

    // write some data to check for migration
    let mut test_anchor = sample_anchor_record();
    test_anchor.devices.push(DeviceDataInternal {
        pubkey: ByteBuf::from("recovery pub key"),
        alias: "my protected recovery phrase".to_string(),
        credential_id: None,
        purpose: Some(Purpose::Recovery),
        key_type: Some(KeyType::SeedPhrase),
        protection: Some(DeviceProtection::Protected),
    });
    storage.write(10_001, test_anchor.clone()).unwrap();

    storage.configure_migration(100);
    // write data to trigger migration
    storage.write(10_000, sample_anchor_record()).unwrap();

    assert_eq!(storage.migration_state(), MigrationState::Finished);

    let storage = Storage::from_memory(memory.clone()).unwrap();
    assert_eq!(storage.version(), 3);
    assert_eq!(storage.read(10_001).unwrap(), test_anchor);
}

#[test]
fn should_pause_migration_with_batch_size_0() {
    let memory = VectorMemory::default();
    let mut storage = layout_v1_storage((10_000, 3_784_873), memory.clone());
    storage.flush();
    for _ in 0..32 {
        storage.allocate_user_number();
    }

    storage.configure_migration(5);

    assert_eq!(
        storage.migration_state(),
        MigrationState::Started {
            anchors_left: 32,
            batch_size: 5
        }
    );

    storage.write(10_000, sample_anchor_record()).unwrap();

    assert_eq!(
        storage.migration_state(),
        MigrationState::Started {
            anchors_left: 27,
            batch_size: 5
        }
    );

    storage.configure_migration(0);

    assert_eq!(
        storage.migration_state(),
        MigrationState::Started {
            anchors_left: 27,
            batch_size: 0
        }
    );

    storage.write(10_000, sample_anchor_record()).unwrap();

    assert_eq!(
        storage.migration_state(),
        MigrationState::Started {
            anchors_left: 27,
            batch_size: 0
        }
    );
}

#[test]
fn should_recover_migration_state_from_memory() {
    let memory = VectorMemory::default();
    let mut storage = layout_v1_storage((10_000, 3_784_873), memory.clone());
    storage.flush();
    for _ in 0..32 {
        storage.allocate_user_number();
    }

    storage.configure_migration(5);
    storage.write(10_000, sample_anchor_record()).unwrap();

    assert_eq!(
        storage.migration_state(),
        MigrationState::Started {
            anchors_left: 27,
            batch_size: 5
        }
    );

    let storage = Storage::from_memory(memory.clone()).unwrap();
    assert_eq!(
        storage.migration_state(),
        MigrationState::Started {
            anchors_left: 27,
            batch_size: 5
        }
    );
}

#[test]
fn should_restart_paused_migration() {
    let memory = VectorMemory::default();
    let mut storage = layout_v1_storage((10_000, 3_784_873), memory.clone());
    storage.flush();
    for _ in 0..32 {
        storage.allocate_user_number();
    }

    storage.configure_migration(5);
    storage.write(10_000, sample_anchor_record()).unwrap();
    storage.configure_migration(0);

    assert_eq!(
        storage.migration_state(),
        MigrationState::Started {
            anchors_left: 27,
            batch_size: 0
        }
    );

    storage.configure_migration(100);
    storage.write(10_000, sample_anchor_record()).unwrap();
    assert_eq!(storage.migration_state(), MigrationState::Finished);
}

fn layout_v1_storage(
    anchor_range: (UserNumber, UserNumber),
    memory: VectorMemory,
) -> Storage<VectorMemory> {
    let mut storage = Storage::new(anchor_range, memory.clone());
    storage.flush();
    memory.write(3, &[1u8]); // fix version
    memory.write(24, &2048u32.to_le_bytes()); // fix entry size
    memory.write(58, &512u64.to_le_bytes()); // fix entry offset
    Storage::from_memory(memory.clone()).unwrap()
}

fn sample_anchor_record() -> Anchor {
    Anchor {
        devices: vec![DeviceDataInternal {
            pubkey: ByteBuf::from("hello world, I am a public key"),
            alias: "my test device".to_string(),
            credential_id: Some(ByteBuf::from("this is the credential id")),
            purpose: Some(Purpose::Authentication),
            key_type: Some(KeyType::Unknown),
            protection: Some(DeviceProtection::Protected),
        }],
    }
}

fn sample_persistent_state() -> PersistentState {
    let persistent_state = PersistentState {
        archive_info: ArchiveInfo {
            expected_module_hash: Some([99u8; 32]),
            state: ArchiveState::Created(ArchiveData {
                sequence_number: 39,
                archive_canister: Principal::from_text("2h5ob-7aaaa-aaaad-aacya-cai").unwrap(),
            }),
        },
        canister_creation_cycles_cost: 12_346_000_000,
    };
    persistent_state
}
