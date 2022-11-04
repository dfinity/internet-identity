use crate::state::DeviceDataInternal;
use crate::storage::{Header, StorageError};
use crate::Storage;
use ic_stable_structures::{Memory, VectorMemory};
use internet_identity_interface::{DeviceProtection, KeyType, Purpose};
use serde_bytes::ByteBuf;

const HEADER_SIZE: usize = 58;

#[test]
fn should_match_actual_header_size() {
    // if this test fails, make sure the change was intentional and upgrade as well as rollback still work!
    assert_eq!(std::mem::size_of::<Header>(), HEADER_SIZE);
}

#[test]
fn should_report_max_number_of_entries_for_8gb() {
    let memory = VectorMemory::default();
    let storage = Storage::<(), VectorMemory>::new((1, 2), memory);
    assert_eq!(storage.max_entries(), 3774873);
}

#[test]
fn should_serialize_header() {
    let memory = VectorMemory::default();
    let mut storage = Storage::<(), VectorMemory>::new((1, 2), memory.clone());
    storage.update_salt([5u8; 32]);
    storage.flush();

    let mut buf = vec![0; HEADER_SIZE];
    memory.read(0, &mut buf);
    assert_eq!(buf, hex::decode("49494301000000000100000000000000020000000000000000080505050505050505050505050505050505050505050505050505050505050505").unwrap());
}

#[test]
fn should_recover_header_from_memory() {
    let memory = VectorMemory::default();
    memory.grow(1);
    memory.write(0, &hex::decode("494943010500000040e2010000000000f1fb09000000000000084343434343434343434343434343434343434343434343434343434343434343").unwrap());

    let storage = Storage::<(), VectorMemory>::from_memory(memory).unwrap();
    assert_eq!(storage.assigned_user_number_range(), (123456, 654321));
    assert_eq!(storage.salt().unwrap(), &[67u8; 32]);
    assert_eq!(storage.user_count(), 5);
}

#[test]
fn should_update_header() {
    let memory = VectorMemory::default();
    memory.grow(1);
    memory.write(0, &hex::decode("494943010500000040e2010000000000f1fb09000000000000084343434343434343434343434343434343434343434343434343434343434343").unwrap());

    let mut storage = Storage::<(), VectorMemory>::from_memory(memory.clone()).unwrap();
    storage.set_user_number_range((1234567, 5_000_000));
    storage.allocate_user_number();
    storage.flush();

    let mut buf = vec![0; HEADER_SIZE];
    memory.read(0, &mut buf);
    assert_eq!(buf, hex::decode("494943010600000087d6120000000000404b4c000000000000084343434343434343434343434343434343434343434343434343434343434343").unwrap());
}

#[test]
fn should_serialize_first_record() {
    let memory = VectorMemory::default();
    let mut storage =
        Storage::<Vec<DeviceDataInternal>, VectorMemory>::new((123, 456), memory.clone());
    let user_number = storage.allocate_user_number().unwrap();
    assert_eq!(user_number, 123u64);

    let device_vec = sample_anchor_record();
    storage.write(user_number, device_vec.clone()).unwrap();

    let mut buf = [0u8; 192];
    memory.read(512, &mut buf);
    let decoded_from_memory: Vec<DeviceDataInternal> = candid::decode_one(&buf[2..]).unwrap();
    assert_eq!(decoded_from_memory, device_vec);
}

#[test]
fn should_serialize_subsequent_record_to_expected_memory_location() {
    const EXPECTED_RECORD_OFFSET: u64 = 204_800; // 100 * max anchor size
    let memory = VectorMemory::default();
    let mut storage =
        Storage::<Vec<DeviceDataInternal>, VectorMemory>::new((123, 456), memory.clone());
    for _ in 0..100 {
        storage.allocate_user_number().unwrap();
    }
    let user_number = storage.allocate_user_number().unwrap();
    assert_eq!(user_number, 223u64);

    let device_vec = sample_anchor_record();
    storage.write(user_number, device_vec.clone()).unwrap();

    let mut buf = [0u8; 192];
    memory.read(512 + EXPECTED_RECORD_OFFSET, &mut buf);
    let decoded_from_memory: Vec<DeviceDataInternal> = candid::decode_one(&buf[2..]).unwrap();
    assert_eq!(decoded_from_memory, device_vec);
}

#[test]
fn should_not_write_using_anchor_number_outside_allocated_range() {
    let memory = VectorMemory::default();
    let mut storage =
        Storage::<Vec<DeviceDataInternal>, VectorMemory>::new((123, 456), memory.clone());
    storage.allocate_user_number().unwrap();

    let result = storage.write(222, sample_anchor_record().clone());
    assert!(matches!(result, Err(StorageError::BadUserNumber(_))))
}

#[test]
fn should_deserialize_first_record() {
    let memory = VectorMemory::default();
    memory.grow(1);
    let mut storage =
        Storage::<Vec<DeviceDataInternal>, VectorMemory>::new((123, 456), memory.clone());
    let user_number = storage.allocate_user_number().unwrap();
    assert_eq!(user_number, 123u64);

    let device_vec = sample_anchor_record();
    let buf = candid::encode_one(&device_vec).unwrap();
    memory.write(512, &(buf.len() as u16).to_le_bytes());
    memory.write(514, &buf);

    let read_from_storage = storage.read(123).unwrap();
    assert_eq!(read_from_storage, device_vec);
}

#[test]
fn should_deserialize_subsequent_record_at_expected_memory_location() {
    const EXPECTED_RECORD_OFFSET: u64 = 204_800; // 100 * max anchor size
    let memory = VectorMemory::default();
    memory.grow(4); // grow memory to accommodate a write to EXPECTED_RECORD_OFFSET
    let mut storage =
        Storage::<Vec<DeviceDataInternal>, VectorMemory>::new((123, 456), memory.clone());
    for _ in 0..100 {
        storage.allocate_user_number().unwrap();
    }
    let user_number = storage.allocate_user_number().unwrap();
    assert_eq!(user_number, 223u64);

    let device_vec = sample_anchor_record();
    let buf = candid::encode_one(&device_vec).unwrap();
    memory.write(
        512 + EXPECTED_RECORD_OFFSET,
        &(buf.len() as u16).to_le_bytes(),
    );
    memory.write(514 + EXPECTED_RECORD_OFFSET, &buf);

    let read_from_storage = storage.read(223).unwrap();
    assert_eq!(read_from_storage, device_vec);
}

#[test]
fn should_not_read_using_anchor_number_outside_allocated_range() {
    let memory = VectorMemory::default();
    let mut storage =
        Storage::<Vec<DeviceDataInternal>, VectorMemory>::new((123, 456), memory.clone());
    storage.allocate_user_number().unwrap();

    let result = storage.read(222);
    assert!(matches!(result, Err(StorageError::BadUserNumber(_))))
}

fn sample_anchor_record() -> Vec<DeviceDataInternal> {
    let device_vec = vec![DeviceDataInternal {
        pubkey: ByteBuf::from("hello world, I am a public key"),
        alias: "my test device".to_string(),
        credential_id: Some(ByteBuf::from("this is the credential id")),
        purpose: Some(Purpose::Authentication),
        key_type: Some(KeyType::Unknown),
        protection: Some(DeviceProtection::Protected),
    }];
    device_vec
}
