use crate::archive::{ArchiveData, ArchiveInfo, ArchiveState};
use crate::state::PersistentState;
use crate::storage::anchor::{Anchor, Device};
use crate::storage::{DeviceDataInternal, Header, PersistentStateError, StorageError};
use crate::Storage;
use candid::Principal;
use ic_stable_structures::{Memory, VectorMemory};
use internet_identity_interface::{
    AnchorNumber, DeviceProtection, KeyType, MigrationState, Purpose,
};
use serde_bytes::ByteBuf;

const WASM_PAGE_SIZE: u64 = 1 << 16;
const HEADER_SIZE: usize = 74;
const RESERVED_HEADER_BYTES: u64 = 2 * WASM_PAGE_SIZE;
const PERSISTENT_STATE_MAGIC: [u8; 4] = *b"IIPS";

#[test]
fn should_match_actual_header_size() {
    // if this test fails, make sure the change was intentional and upgrade as well as rollback still work!
    assert_eq!(std::mem::size_of::<Header>(), HEADER_SIZE);
}

#[test]
fn should_report_max_number_of_entries_for_32gb() {
    let memory = VectorMemory::default();
    let storage = Storage::new((1, 2), memory);
    assert_eq!(storage.max_entries(), 8_178_860);
}

#[test]
fn should_serialize_header_v3() {
    let memory = VectorMemory::default();
    let mut storage = layout_v3_storage((1, 2), memory.clone());
    storage.update_salt([05u8; 32]);
    storage.flush();

    let mut buf = vec![0; HEADER_SIZE];
    memory.read(0, &mut buf);
    assert_eq!(buf, hex::decode("4949430300000000010000000000000002000000000000000010050505050505050505050505050505050505050505050505050505050505050500000200000000000000000000000000").unwrap());
}

#[test]
fn should_serialize_header_v5() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((1, 2), memory.clone());
    storage.update_salt([05u8; 32]);
    storage.flush();

    let mut buf = vec![0; HEADER_SIZE];
    memory.read(0, &mut buf);
    assert_eq!(buf, hex::decode("4949430500000000010000000000000002000000000000000010050505050505050505050505050505050505050505050505050505050505050500000200000000000000000000000000").unwrap());
}

#[test]
fn should_recover_header_from_memory() {
    let memory = VectorMemory::default();
    memory.grow(1);
    memory.write(0, &hex::decode("494943030500000040e2010000000000f1fb090000000000000843434343434343434343434343434343434343434343434343434343434343430002000000000000000000000000000000000000000000000000").unwrap());

    let storage = Storage::from_memory(memory).unwrap();
    assert_eq!(storage.assigned_anchor_number_range(), (123456, 654321));
    assert_eq!(storage.salt().unwrap(), &[67u8; 32]);
    assert_eq!(storage.anchor_count(), 5);
    assert_eq!(storage.version(), 3);
}

#[test]
fn should_read_previous_write_v3() {
    let memory = VectorMemory::default();
    let mut storage = layout_v3_storage((12345, 678910), memory.clone());
    let (anchor_number, mut anchor) = storage.allocate_anchor().unwrap();

    anchor.add_device(sample_device()).unwrap();
    storage.write(anchor_number, anchor.clone()).unwrap();

    let read_anchor = storage.read(anchor_number).unwrap();
    assert_eq!(anchor, read_anchor);
}

#[test]
fn should_read_previous_write_v5() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((12345, 678910), memory.clone());
    let (anchor_number, mut anchor) = storage.allocate_anchor().unwrap();

    anchor.add_device(sample_device()).unwrap();
    storage.write(anchor_number, anchor.clone()).unwrap();

    let read_anchor = storage.read(anchor_number).unwrap();
    assert_eq!(anchor, read_anchor);
}

#[test]
fn should_serialize_first_record_v3() {
    const EXPECTED_LENGTH: usize = 192;
    let memory = VectorMemory::default();
    let mut storage = layout_v3_storage((123, 456), memory.clone());
    let (anchor_number, mut anchor) = storage.allocate_anchor().unwrap();
    assert_eq!(anchor_number, 123u64);

    anchor.add_device(sample_device()).unwrap();
    storage.write(anchor_number, anchor.clone()).unwrap();

    let mut buf = [0u8; EXPECTED_LENGTH];
    memory.read(RESERVED_HEADER_BYTES, &mut buf);
    let decoded_from_memory: Vec<Device> = candid::decode_one::<Vec<DeviceDataInternal>>(&buf[2..])
        .unwrap()
        .into_iter()
        .map(|d| Device::from(d))
        .collect();
    assert_eq!(&decoded_from_memory, anchor.devices());
}

#[test]
fn should_serialize_first_record_v5() {
    const EXPECTED_LENGTH: usize = 191;
    let memory = VectorMemory::default();
    let mut storage = Storage::new((123, 456), memory.clone());
    let (anchor_number, mut anchor) = storage.allocate_anchor().unwrap();
    assert_eq!(anchor_number, 123u64);

    anchor.add_device(sample_device()).unwrap();
    storage.write(anchor_number, anchor.clone()).unwrap();

    let mut buf = [0u8; EXPECTED_LENGTH];
    memory.read(RESERVED_HEADER_BYTES, &mut buf);
    let decoded_from_memory: Anchor = candid::decode_one(&buf[2..]).unwrap();
    assert_eq!(decoded_from_memory, anchor);
}

#[test]
fn should_serialize_subsequent_record_to_expected_memory_location_v3() {
    const EXPECTED_LENGTH: usize = 192;
    const EXPECTED_RECORD_OFFSET: u64 = 409_600; // 100 * max anchor size
    let memory = VectorMemory::default();
    let mut storage = layout_v3_storage((123, 456), memory.clone());
    for _ in 0..100 {
        storage.allocate_anchor().unwrap();
    }
    let (anchor_number, mut anchor) = storage.allocate_anchor().unwrap();
    assert_eq!(anchor_number, 223u64);

    anchor.add_device(sample_device()).unwrap();
    storage.write(anchor_number, anchor.clone()).unwrap();

    let mut buf = [0u8; EXPECTED_LENGTH];
    memory.read(RESERVED_HEADER_BYTES + EXPECTED_RECORD_OFFSET, &mut buf);
    let decoded_from_memory: Vec<Device> = candid::decode_one::<Vec<DeviceDataInternal>>(&buf[2..])
        .unwrap()
        .into_iter()
        .map(|d| Device::from(d))
        .collect();
    assert_eq!(&decoded_from_memory, anchor.devices());
}

#[test]
fn should_serialize_subsequent_record_to_expected_memory_location_v5() {
    const EXPECTED_LENGTH: usize = 191;
    const EXPECTED_RECORD_OFFSET: u64 = 409_600; // 100 * max anchor size
    let memory = VectorMemory::default();
    let mut storage = Storage::new((123, 456), memory.clone());
    for _ in 0..100 {
        storage.allocate_anchor().unwrap();
    }
    let (anchor_number, mut anchor) = storage.allocate_anchor().unwrap();
    assert_eq!(anchor_number, 223u64);

    anchor.add_device(sample_device()).unwrap();
    storage.write(anchor_number, anchor.clone()).unwrap();

    let mut buf = [0u8; EXPECTED_LENGTH];
    memory.read(RESERVED_HEADER_BYTES + EXPECTED_RECORD_OFFSET, &mut buf);
    let decoded_from_memory: Anchor = candid::decode_one(&buf[2..]).unwrap();
    assert_eq!(decoded_from_memory, anchor);
}

#[test]
fn should_not_write_using_anchor_number_outside_allocated_range() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((123, 456), memory.clone());
    let (_, anchor) = storage.allocate_anchor().unwrap();

    let result = storage.write(222, anchor);
    assert!(matches!(result, Err(StorageError::BadAnchorNumber(_))))
}

#[test]
fn should_deserialize_first_record_v3() {
    let memory = VectorMemory::default();
    memory.grow(3);
    let mut storage = layout_v3_storage((123, 456), memory.clone());
    let (anchor_number, mut anchor) = storage.allocate_anchor().unwrap();
    assert_eq!(anchor_number, 123u64);

    anchor.add_device(sample_device()).unwrap();
    let buf = candid::encode_one(&anchor.devices()).unwrap();
    memory.write(RESERVED_HEADER_BYTES, &(buf.len() as u16).to_le_bytes());
    memory.write(RESERVED_HEADER_BYTES + 2, &buf);

    let read_from_storage = storage.read(123).unwrap();
    assert_eq!(read_from_storage, anchor);
}

#[test]
fn should_deserialize_first_record_v5() {
    let memory = VectorMemory::default();
    memory.grow(3);
    let mut storage = Storage::new((123, 456), memory.clone());
    let (anchor_number, mut anchor) = storage.allocate_anchor().unwrap();
    assert_eq!(anchor_number, 123u64);

    anchor.add_device(sample_device()).unwrap();
    let buf = candid::encode_one(&anchor).unwrap();
    memory.write(RESERVED_HEADER_BYTES, &(buf.len() as u16).to_le_bytes());
    memory.write(RESERVED_HEADER_BYTES + 2, &buf);

    let read_from_storage = storage.read(123).unwrap();
    assert_eq!(read_from_storage, anchor);
}

#[test]
fn should_deserialize_subsequent_record_at_expected_memory_location_v3() {
    const EXPECTED_RECORD_OFFSET: u64 = 409_600; // 100 * max anchor size
    let memory = VectorMemory::default();
    memory.grow(9); // grow memory to accommodate a write to record 100
    let mut storage = layout_v3_storage((123, 456), memory.clone());
    for _ in 0..100 {
        storage.allocate_anchor().unwrap();
    }
    let (anchor_number, mut anchor) = storage.allocate_anchor().unwrap();
    assert_eq!(anchor_number, 223u64);

    anchor.add_device(sample_device()).unwrap();
    let buf = candid::encode_one(&anchor.devices()).unwrap();
    memory.write(
        RESERVED_HEADER_BYTES + EXPECTED_RECORD_OFFSET,
        &(buf.len() as u16).to_le_bytes(),
    );
    memory.write(RESERVED_HEADER_BYTES + 2 + EXPECTED_RECORD_OFFSET, &buf);

    let read_from_storage = storage.read(223).unwrap();
    assert_eq!(read_from_storage, anchor);
}

#[test]
fn should_deserialize_subsequent_record_at_expected_memory_location_v5() {
    const EXPECTED_RECORD_OFFSET: u64 = 409_600; // 100 * max anchor size
    let memory = VectorMemory::default();
    memory.grow(9); // grow memory to accommodate a write to record 100
    let mut storage = Storage::new((123, 456), memory.clone());
    for _ in 0..100 {
        storage.allocate_anchor().unwrap();
    }
    let (anchor_number, mut anchor) = storage.allocate_anchor().unwrap();
    assert_eq!(anchor_number, 223u64);

    anchor.add_device(sample_device()).unwrap();
    let buf = candid::encode_one(&anchor).unwrap();
    memory.write(
        RESERVED_HEADER_BYTES + EXPECTED_RECORD_OFFSET,
        &(buf.len() as u16).to_le_bytes(),
    );
    memory.write(RESERVED_HEADER_BYTES + 2 + EXPECTED_RECORD_OFFSET, &buf);

    let read_from_storage = storage.read(223).unwrap();
    assert_eq!(read_from_storage, anchor);
}

#[test]
fn should_not_read_using_anchor_number_outside_allocated_range() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((123, 456), memory.clone());
    storage.allocate_anchor().unwrap();

    let result = storage.read(222);
    assert!(matches!(result, Err(StorageError::BadAnchorNumber(_))))
}

#[test]
fn should_save_and_restore_persistent_state() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((123, 456), memory.clone());
    storage.flush();
    storage.allocate_anchor().unwrap();

    let persistent_state = sample_persistent_state();

    storage.write_persistent_state(&persistent_state);
    assert_eq!(storage.read_persistent_state().unwrap(), persistent_state);
}

#[test]
fn should_save_persistent_state_at_expected_memory_address() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory.clone());
    storage.flush();

    storage.write_persistent_state(&sample_persistent_state());

    let mut buf = vec![0u8; 4];
    memory.read(RESERVED_HEADER_BYTES, &mut buf);
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
    memory.grow(3);

    let mut storage = Storage::new((10_000, 3_784_873), memory.clone());
    storage.flush();

    memory.write(RESERVED_HEADER_BYTES, b"IIPX"); // correct magic bytes are IIPS

    let result = storage.read_persistent_state();
    assert!(matches!(result, Err(PersistentStateError::NotFound)))
}

#[test]
fn should_save_persistent_state_at_expected_memory_address_with_anchors() {
    const EXPECTED_ADDRESS: u64 = RESERVED_HEADER_BYTES + 100 * 4096; // number of anchors is 100

    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory.clone());
    storage.flush();

    for _ in 0..100 {
        storage.allocate_anchor().unwrap();
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
    memory.write(0, &hex::decode("4949430360E316001027000000000000a9c03900000000000010434343434343434343434343434343434343434343434343434343434343434300000200").unwrap());
    const EXPECTED_ADDRESS: u64 = RESERVED_HEADER_BYTES + 1_500_000 * 4096; // number of anchors is 1_500_000

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
        storage.allocate_anchor();
    }

    assert!(matches!(
        storage.read_persistent_state(),
        Err(PersistentStateError::NotFound)
    ));
}

#[test]
fn should_overwrite_persistent_state_with_next_anchor() {
    const EXPECTED_ADDRESS: u64 = RESERVED_HEADER_BYTES + 4096; // only one anchor exists

    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory.clone());
    storage.flush();

    storage.allocate_anchor().unwrap();
    storage.write_persistent_state(&sample_persistent_state());

    let mut buf = vec![0u8; 4];
    memory.read(EXPECTED_ADDRESS, &mut buf);
    assert_eq!(buf, PERSISTENT_STATE_MAGIC);

    let (anchor_number, anchor) = storage.allocate_anchor().unwrap();
    storage.write(anchor_number, anchor).unwrap();

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
    const EXPECTED_ADDRESS: u64 = RESERVED_HEADER_BYTES + 3 * 2048; // 3 anchors
    const PERSISTENT_STATE_BYTES: &'static str = "4949505388000000000000004449444c066c02cbc282b70501f7f5cbfb07786c02faafb5ac020291ecada008046e036d7b6b03d1d3dab70b78b5c2d2b70d7fc8bbeff50d056c02c7e8ccee037884fbf0820968010001206363636363636363636363636363636363636363636363636363636363636363022700000000000000010a00000000006000b001018002e1df02000000";

    let memory = VectorMemory::default();
    // allocate space for the writes
    memory.grow(3);

    // write header so the number of users is set
    memory.write(0, &hex::decode("494943030300000040e2010000000000f1fb090000000000000843434343434343434343434343434343434343434343434343434343434343430000020000000000").unwrap());
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
fn should_stay_in_v3_if_no_migration_configured() {
    let memory = VectorMemory::default();
    let mut storage = layout_v3_storage((10_000, 3_784_873), memory.clone());
    storage.flush();
    for _ in 0..32 {
        storage.allocate_anchor();
    }

    assert!(matches!(
        storage.migration_state(),
        MigrationState::NotStarted
    ));
}

#[test]
fn should_start_migration_when_configuring() {
    let memory = VectorMemory::default();
    let mut storage = layout_v3_storage((10_000, 3_784_873), memory.clone());
    storage.flush();
    for _ in 0..32 {
        storage.allocate_anchor();
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
    let mut storage = layout_v3_storage((10_000, 3_784_873), memory.clone());
    storage.flush();
    let (anchor_number, anchor) = storage.allocate_anchor().unwrap();
    for _ in 0..32 {
        storage.allocate_anchor();
    }

    storage.configure_migration(100);
    storage.write(anchor_number, anchor).unwrap();

    assert_eq!(storage.migration_state(), MigrationState::Finished);
}

#[test]
fn should_load_anchors_from_memory_after_migration() {
    let memory = VectorMemory::default();
    let mut storage = layout_v3_storage((10_000, 3_784_873), memory.clone());
    storage.flush();
    let (_, mut anchor) = storage.allocate_anchor().unwrap();
    for _ in 0..32 {
        storage.allocate_anchor();
    }

    // write some data to check for migration
    anchor.add_device(sample_device()).unwrap();
    anchor
        .add_device(Device {
            pubkey: ByteBuf::from("recovery pub key"),
            alias: "my protected recovery phrase".to_string(),
            credential_id: None,
            purpose: Purpose::Recovery,
            key_type: KeyType::SeedPhrase,
            protection: DeviceProtection::Protected,
        })
        .unwrap();
    storage.write(10_001, anchor.clone()).unwrap();

    storage.configure_migration(100);
    // write data to trigger migration
    storage.write(10_000, anchor.clone()).unwrap();

    assert_eq!(storage.migration_state(), MigrationState::Finished);

    let storage = Storage::from_memory(memory.clone()).unwrap();
    assert_eq!(storage.version(), 5);
    assert_eq!(storage.read(10_001).unwrap(), anchor);
}

#[test]
fn should_pause_migration_with_batch_size_0() {
    let memory = VectorMemory::default();
    let mut storage = layout_v3_storage((10_000, 3_784_873), memory.clone());
    storage.flush();
    let (_, anchor) = storage.allocate_anchor().unwrap();
    for _ in 0..31 {
        storage.allocate_anchor();
    }

    storage.configure_migration(5);

    assert_eq!(
        storage.migration_state(),
        MigrationState::Started {
            anchors_left: 32,
            batch_size: 5
        }
    );

    storage.write(10_000, anchor.clone()).unwrap();

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

    storage.write(10_000, anchor).unwrap();

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
    let mut storage = layout_v3_storage((10_000, 3_784_873), memory.clone());
    storage.flush();
    let (_, anchor) = storage.allocate_anchor().unwrap();
    for _ in 0..31 {
        storage.allocate_anchor();
    }
    storage.configure_migration(5);
    storage.write(10_000, anchor).unwrap();

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
    let mut storage = layout_v3_storage((10_000, 3_784_873), memory.clone());
    storage.flush();
    let (_, anchor) = storage.allocate_anchor().unwrap();
    for _ in 0..31 {
        storage.allocate_anchor();
    }

    storage.configure_migration(5);
    storage.write(10_000, anchor.clone()).unwrap();
    storage.configure_migration(0);

    assert_eq!(
        storage.migration_state(),
        MigrationState::Started {
            anchors_left: 27,
            batch_size: 0
        }
    );

    storage.configure_migration(100);
    storage.write(10_000, anchor).unwrap();
    assert_eq!(storage.migration_state(), MigrationState::Finished);
}

fn sample_device() -> Device {
    Device {
        pubkey: ByteBuf::from("hello world, I am a public key"),
        alias: "my test device".to_string(),
        credential_id: Some(ByteBuf::from("this is the credential id")),
        purpose: Purpose::Authentication,
        key_type: KeyType::Unknown,
        protection: DeviceProtection::Unprotected,
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

fn layout_v3_storage(
    anchor_range: (AnchorNumber, AnchorNumber),
    memory: VectorMemory,
) -> Storage<VectorMemory> {
    let mut storage = Storage::new(anchor_range, memory.clone());
    storage.flush();
    memory.write(3, &[3u8]); // fix version
    Storage::from_memory(memory.clone()).unwrap()
}
