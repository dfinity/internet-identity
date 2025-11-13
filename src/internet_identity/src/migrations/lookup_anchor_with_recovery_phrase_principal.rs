use ic_stable_structures::Memory;

use crate::{
    storage::Storage, RECOVERY_PHRASE_MIGRATION_BATCH_ID, RECOVERY_PHRASE_MIGRATION_BATCH_SIZE,
    RECOVERY_PHRASE_MIGRATION_ERRORS, RECOVERY_PHRASE_MIGRATION_LAST_ANCHOR_ID,
};

impl<M: Memory + Clone> Storage<M> {
    pub fn maybe_apply_lookup_anchor_with_recovery_phrase_principal_migration(&mut self) {
        let (id_range_lo, _) = self.assigned_anchor_number_range();
        let anchor_count = self.anchor_count() as u64;

        let next_anchor_number = if let Some(last_anchor_id) =
            RECOVERY_PHRASE_MIGRATION_LAST_ANCHOR_ID.with(|x| x.borrow().clone())
        {
            last_anchor_id + 1
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

            RECOVERY_PHRASE_MIGRATION_LAST_ANCHOR_ID.with_borrow_mut(|x| {
                *x = Some(next_anchor_number - 1);
            });

            next_anchor_number
        };

        let begin = id_range_lo
            + RECOVERY_PHRASE_MIGRATION_BATCH_SIZE
                * RECOVERY_PHRASE_MIGRATION_BATCH_ID.with(|id| *id.borrow());

        let end = begin + RECOVERY_PHRASE_MIGRATION_BATCH_SIZE;

        // Stop before the next un-allocated anchor number.
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

        let num_errors = errors.len();

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

        if end < next_anchor_number && num_errors < RECOVERY_PHRASE_MIGRATION_BATCH_SIZE as usize {
            // Mark progress.
            RECOVERY_PHRASE_MIGRATION_BATCH_ID.with_borrow_mut(|id| *id += 1);
        } else {
            ic_cdk::println!("Recovery phrase migration COMPLETED.");
            RECOVERY_PHRASE_MIGRATION_BATCH_ID.with_borrow_mut(|id| *id = u64::MAX);
        }
    }
}
