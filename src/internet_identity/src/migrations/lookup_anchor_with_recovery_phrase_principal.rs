use ic_stable_structures::Memory;

use crate::{
    storage::Storage, RECOVERY_PHRASE_MIGRATION_BATCH_ID, RECOVERY_PHRASE_MIGRATION_BATCH_SIZE,
    RECOVERY_PHRASE_MIGRATION_ERRORS, RECOVERY_PHRASE_MIGRATION_LAST_ANCHOR_ID,
};

impl<M: Memory + Clone> Storage<M> {
    /// Migrates the next batch of anchors to synchronize the following indices:
    /// `lookup_anchor_with_recovery_phrase_principal_memory_wrapper`
    /// `lookup_anchor_with_device_credential_memory`
    ///
    /// The batch size is `RECOVERY_PHRASE_MIGRATION_BATCH_SIZE` anchors.
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
    pub fn sync_anchor_indices(&mut self) {
        // This is 10000 in the production II canister.
        let (id_range_lo, _) = self.assigned_anchor_number_range();

        // How many anchors were created to this point.
        let anchor_count = self.anchor_count() as u64;

        // Lazily compute the bound on the ID of anchors to be migrated.
        let last_achor_id = RECOVERY_PHRASE_MIGRATION_LAST_ANCHOR_ID.with_borrow(|x| x.clone());

        let next_anchor_number = if let Some(last_anchor_id) = last_achor_id {
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
                RECOVERY_PHRASE_MIGRATION_ERRORS.with(|errors| {
                    errors.borrow_mut().push(err);
                });
                return;
            };

            // (ID of the last anchor to be migrated) = (next unallocated anchor number) - 1
            RECOVERY_PHRASE_MIGRATION_LAST_ANCHOR_ID.with_borrow_mut(|x| {
                *x = Some(next_anchor_number.saturating_sub(1));
            });

            next_anchor_number
        };

        let batch_id = RECOVERY_PHRASE_MIGRATION_BATCH_ID.with(|id| *id.borrow());

        let begin = id_range_lo
            .saturating_add(RECOVERY_PHRASE_MIGRATION_BATCH_SIZE.saturating_mul(batch_id));

        let end = begin.saturating_add(RECOVERY_PHRASE_MIGRATION_BATCH_SIZE);

        // Stop *before* the next un-allocated anchor number.
        let end = std::cmp::min(end, next_anchor_number);

        let mut errors = vec![];

        for anchor_number in begin..end {
            let anchor = match self.read(anchor_number) {
                Ok(anchor) => anchor,
                Err(err) => {
                    let err = format!("r#{}:{:?}", anchor_number, err);
                    errors.push(err);
                    continue;
                }
            };

            match self.update(anchor) {
                Ok(_) => (),
                Err(err) => {
                    let err = format!("w#{}:{:?}", anchor_number, err);
                    errors.push(err);
                }
            }
        }

        // Whether more batches are to be migrated.
        let has_more_work = end < next_anchor_number;

        // This condition is used to terminate the migration in the unlikely event that *all*
        // anchors within this entire batch failed to migrate.
        let batch_did_not_fail_completely =
            errors.len() < RECOVERY_PHRASE_MIGRATION_BATCH_SIZE as usize;

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
                all_errors.extend(errors.into_iter());
            });
        }

        if has_more_work && batch_did_not_fail_completely {
            // Mark progress.
            RECOVERY_PHRASE_MIGRATION_BATCH_ID
                .with_borrow_mut(|id| *id = batch_id.saturating_add(1));
        } else {
            ic_cdk::println!("Recovery phrase migration COMPLETED.");
            RECOVERY_PHRASE_MIGRATION_BATCH_ID.with_borrow_mut(|id| *id = u64::MAX);
        }
    }
}
