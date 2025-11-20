use ic_stable_structures::Memory;

use crate::{
    storage::{anchor::Anchor, Storage, StorageError},
    RECOVERY_PHRASE_MIGRATION_BATCH_ID, RECOVERY_PHRASE_MIGRATION_ERRORS,
    RECOVERY_PHRASE_MIGRATION_LAST_ANCHOR_ID,
};

impl<M: Memory + Clone> Storage<M> {
    /// Migrates the next batch of anchors to synchronize the following indices:
    /// - `lookup_anchor_with_recovery_phrase_principal_memory`
    /// - `lookup_anchor_with_device_credential_memory`
    ///
    /// The batch size is `batch_size` anchors.
    ///
    /// The first call to this function saves the ID of the last anchor to be migrated.
    ///
    /// Stop condition (indicated by `RECOVERY_PHRASE_MIGRATION_BATCH_ID` being set to `u64::MAX`):
    /// - Either all anchors up to the last anchor ID have been migrated, or
    /// - All anchors in the current batch failed to migrate.
    ///
    /// Aggregates errors encountered during migration in `RECOVERY_PHRASE_MIGRATION_ERRORS`
    /// (can be queried via a separate function `list_recovery_phrase_migration_errors`).
    ///
    /// Must not panic.
    pub fn sync_anchor_indices(&mut self, now_nanos: u64, batch_size: u64) {
        // This is 10000 in the production II canister.
        let (id_range_lo, _) = self.assigned_anchor_number_range();

        // How many anchors were created to this point.
        let anchor_count = self.anchor_count() as u64;

        // Lazily compute the bound on the ID of anchors to be migrated.
        let last_anchor_id = RECOVERY_PHRASE_MIGRATION_LAST_ANCHOR_ID.with_borrow(|x| *x);

        let last_anchor_id = if let Some(last_anchor_id) = last_anchor_id {
            // The ID of the last anchor that needs to be migrated. Does not change during
            // the migration. Note that anchors created during the migration don't need to be
            // migrated since the migration should be released together with index
            // synchronization code (see `sync_anchor_with_recovery_phrase_principal_index`).
            last_anchor_id
        } else {
            let Some(next_anchor_number) = id_range_lo.checked_add(anchor_count) else {
                let err = format!(
                    "id_range_lo={} + anchor_count={} overflowed",
                    id_range_lo, anchor_count
                );
                ic_cdk::println!("ERROR: {}", err);
                RECOVERY_PHRASE_MIGRATION_ERRORS.with_borrow_mut(|all_errors| {
                    all_errors.push(err);
                });

                // Ensure that migration is not retried endlessly.
                ic_cdk::println!("Recovery phrase migration CANCELED.");
                RECOVERY_PHRASE_MIGRATION_BATCH_ID.replace(u64::MAX);
                return;
            };

            // (ID of the last anchor to be migrated) = (next unallocated anchor number) - 1
            let last_anchor_id = next_anchor_number.saturating_sub(1);

            // Save this value for future invocations.
            RECOVERY_PHRASE_MIGRATION_LAST_ANCHOR_ID.replace(Some(last_anchor_id));

            last_anchor_id
        };

        let batch_id = RECOVERY_PHRASE_MIGRATION_BATCH_ID.with_borrow(|id| *id);

        let begin = id_range_lo.saturating_add(batch_size.saturating_mul(batch_id));

        // The end index is inclusive.
        let end = begin.saturating_add(batch_size).saturating_sub(1);

        // Stop *before* the next un-allocated anchor number.
        let end = std::cmp::min(end, last_anchor_id);

        let mut errors = vec![];

        // This condition is used to terminate the migration in the unlikely event that *all*
        // anchors within this entire batch failed to migrate.
        let mut batch_did_not_fail_completely = false;

        // This is where the index migration happens. For each anchor in the batch, read it
        // from storage and write it back, which updates the indices.
        for anchor_number in begin..=end {
            let (anchor, is_previously_written) = match self.read(anchor_number) {
                Ok(anchor) => (anchor, true),
                Err(StorageError::AnchorNotFound { .. }) => {
                    ic_cdk::println!("Marking {} as <DUMMY ANCHOR>", anchor_number);

                    // If an anchor is not found, write a dummy anchor to the memory
                    // to ensure other read attempts succeed (currently, each anchor number within
                    // the allocated range must have an entry in stable memory, but this invariant
                    // has been violated for some anchors due to a past bug).
                    let mut anchor = Anchor::new(anchor_number, now_nanos);

                    // Mark this anchor in case it needs to be easily identifiable using on-chain
                    // data in the future. Safe to unwrap while the name is shorter than
                    // `MAX_NAME_LENGTH` bytes.
                    anchor.set_name(Some("<DUMMY ANCHOR>".to_string())).unwrap();
                    (anchor, false)
                }
                Err(err) => {
                    let err = format!("r#{}:{:?}", anchor_number, err);
                    errors.push(err);
                    continue;
                }
            };

            match self.write(anchor, is_previously_written) {
                Ok(_) => (),
                Err(err) => {
                    let err = format!("w#{}:{:?}", anchor_number, err);
                    errors.push(err);
                    continue;
                }
            }

            batch_did_not_fail_completely = true;
        }

        // Whether more batches are to be migrated.
        let has_more_work = end < last_anchor_id;

        if errors.is_empty() {
            ic_cdk::println!("Successfully migrated batch {}..{}", begin, end);
        } else {
            ic_cdk::println!(
                "{} errors while migrating batch {}..{}\n Last error: {:?}",
                errors.len(),
                begin,
                end,
                errors.last(),
            );

            RECOVERY_PHRASE_MIGRATION_ERRORS.with_borrow_mut(|all_errors| {
                all_errors.extend(errors);
            });
        }

        if has_more_work && batch_did_not_fail_completely {
            // Mark progress.
            RECOVERY_PHRASE_MIGRATION_BATCH_ID
                .with_borrow_mut(|id| *id = batch_id.saturating_add(1));
        } else {
            ic_cdk::println!("Recovery phrase migration COMPLETED.");
            RECOVERY_PHRASE_MIGRATION_BATCH_ID.replace(u64::MAX);
        }
    }
}

#[cfg(test)]
mod sync_anchor_indices_tests {
    use super::*;
    use crate::storage::anchor::Device;
    use candid::Principal;
    use ic_stable_structures::DefaultMemoryImpl;
    use internet_identity_interface::internet_identity::types::{
        DeviceProtection, KeyType, Purpose,
    };

    fn reset_migration_state() {
        crate::RECOVERY_PHRASE_MIGRATION_BATCH_ID.with_borrow_mut(|id| *id = 0);
        crate::RECOVERY_PHRASE_MIGRATION_LAST_ANCHOR_ID.with_borrow_mut(|id| *id = None);
        crate::RECOVERY_PHRASE_MIGRATION_ERRORS.with_borrow_mut(|errs| errs.clear());
    }

    #[track_caller]
    fn assert_migration_completed(expected_errors: Vec<String>) {
        let batch_id = crate::RECOVERY_PHRASE_MIGRATION_BATCH_ID.with_borrow(|id| *id);
        assert_eq!(batch_id, u64::MAX);

        let errors = crate::RECOVERY_PHRASE_MIGRATION_ERRORS.with(|errs| errs.borrow().clone());
        assert_eq!(errors, expected_errors);
    }

    fn pubkey(n: u8) -> Vec<u8> {
        vec![n]
    }

    fn recovery_phrase(pubkey: Vec<u8>) -> Device {
        Device {
            pubkey: pubkey.clone().into(),
            alias: "recovery".to_string(),
            credential_id: None,
            aaguid: None,
            purpose: Purpose::Recovery,
            key_type: KeyType::SeedPhrase,
            protection: DeviceProtection::Unprotected,
            origin: None,
            metadata: None,
            last_usage_timestamp: Some(0),
        }
    }

    fn other_device(pubkey: Vec<u8>) -> Device {
        Device {
            pubkey: pubkey.clone().into(),
            alias: "regular".to_string(),
            credential_id: None,
            aaguid: None,
            purpose: Purpose::Authentication,
            key_type: KeyType::Unknown,
            protection: DeviceProtection::Unprotected,
            origin: None,
            metadata: None,
            last_usage_timestamp: Some(0),
        }
    }

    #[test]
    fn migrates_all_anchors_in_batch() {
        let mut storage = Storage::new((1, 9), DefaultMemoryImpl::default());

        // Create 3 anchors with different devices
        let mut a1 = storage.allocate_anchor(111).unwrap();
        let mut a2 = storage.allocate_anchor(222).unwrap();
        let mut a3 = storage.allocate_anchor(333).unwrap();

        a1.add_device(recovery_phrase(pubkey(1))).unwrap();
        a2.add_device(other_device(pubkey(2))).unwrap();
        a3.add_device(recovery_phrase(pubkey(3))).unwrap();
        a3.add_device(other_device(pubkey(4))).unwrap();

        storage.create(a1.clone()).unwrap();
        storage.create(a2.clone()).unwrap();
        storage.create(a3.clone()).unwrap();

        const BATCH_SIZE: u64 = 3;

        reset_migration_state();

        // Run migration
        storage.sync_anchor_indices(0, BATCH_SIZE);

        // Check that recovery phrase principals are indexed for anchors 1 and 3
        let principal_1 = Principal::self_authenticating(pubkey(1));
        let principal_2 = Principal::self_authenticating(pubkey(2));
        let principal_3 = Principal::self_authenticating(pubkey(3));

        assert_eq!(
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .get(&principal_1),
            Some(1)
        );
        assert_eq!(
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .get(&principal_3),
            Some(3)
        );
        // Anchor 2 has no recovery device, so nothing indexed for its pubkey
        assert_eq!(
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .get(&principal_2),
            None
        );

        assert_migration_completed(vec![]);
    }

    #[test]
    fn migrates_multiple_batches_correctly() {
        let mut storage = Storage::new((1, 9), DefaultMemoryImpl::default());

        // Create 3 anchors with different devices
        let mut a1 = storage.allocate_anchor(111).unwrap();
        let mut a2 = storage.allocate_anchor(222).unwrap();
        let mut a3 = storage.allocate_anchor(333).unwrap();

        a1.add_device(recovery_phrase(pubkey(1))).unwrap();
        a2.add_device(other_device(pubkey(2))).unwrap();
        a3.add_device(recovery_phrase(pubkey(3))).unwrap();
        a3.add_device(other_device(pubkey(4))).unwrap();

        storage.create(a1.clone()).unwrap();
        storage.create(a2.clone()).unwrap();
        storage.create(a3.clone()).unwrap();

        storage
            .lookup_anchor_with_recovery_phrase_principal_memory
            .clear_new();

        // Not all anchors will fit into the first batch
        const BATCH_SIZE: u64 = 2;

        reset_migration_state();

        // Run migration (1)
        storage.sync_anchor_indices(0, BATCH_SIZE);

        // Check that recovery phrase principals are indexed for anchors 1 and 3
        let principal_1 = Principal::self_authenticating(pubkey(1));
        let principal_2 = Principal::self_authenticating(pubkey(2));
        let principal_3 = Principal::self_authenticating(pubkey(3));

        assert_eq!(
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .get(&principal_1),
            Some(1)
        );
        // Anchor 2 has no recovery device, so nothing indexed for its pubkey
        assert_eq!(
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .get(&principal_2),
            None
        );
        // Anchor 3 not migrated yet
        assert_eq!(
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .get(&principal_3),
            None
        );

        // Run migration (2)
        storage.sync_anchor_indices(0, BATCH_SIZE);

        // Now anchor 3 should be migrated
        assert_eq!(
            storage
                .lookup_anchor_with_recovery_phrase_principal_memory
                .get(&principal_3),
            Some(3)
        );

        assert_migration_completed(vec![]);
    }

    #[test]
    fn completes_migration_when_no_more_work() {
        let mut storage = Storage::new((1, 9), DefaultMemoryImpl::default());
        let mut a1 = storage.allocate_anchor(111).unwrap();

        a1.add_device(recovery_phrase(pubkey(1))).unwrap();

        storage.create(a1).unwrap();

        const BATCH_SIZE: u64 = 1;
        reset_migration_state();

        // Run migration
        storage.sync_anchor_indices(0, BATCH_SIZE);

        assert_migration_completed(vec![]);
    }
}
