use crate::storage::storable::application::{
    StorableApplication, StorableOriginHash, StorableOriginSha256,
};
use crate::storage::Storage;
use ic_stable_structures::Memory;
use std::collections::BTreeMap;

impl<M: Memory + Clone> Storage<M> {
    fn log_after_migration(
        &self,
        migrated_entries: &BTreeMap<String, StorableOriginSha256>,
        missing_apps: &BTreeMap<StorableOriginHash, u64>,
        app_collisions: &BTreeMap<StorableOriginSha256, u64>,
    ) {
        // Log any missing applications
        if !missing_apps.is_empty() {
            ic_cdk::println!(
                "ERROR: Skipped {} apps with IDs in the old map but without an app entry.",
                missing_apps.len()
            );
            ic_cdk::println!(
                "INFO: missing_apps:\n{}",
                missing_apps
                    .iter()
                    .map(|(k, v)| format!("{}: {}\n", k, v))
                    .collect::<Vec<_>>()
                    .join("")
            );
        }

        // Log the main invariants
        if self.lookup_application_with_origin_memory_old.len()
            != self.lookup_application_with_origin_memory.len()
        {
            ic_cdk::println!(
                "ERROR: There were **{}** old map entries but **{}** new map entries.",
                self.lookup_application_with_origin_memory_old.len(),
                self.lookup_application_with_origin_memory.len()
            );
            ic_cdk::println!(
                "INFO: Migrated origins (origin -> new hash):\n{}",
                migrated_entries
                    .iter()
                    .map(|(k, v)| format!("{}:{}\n", k, v))
                    .collect::<Vec<_>>()
                    .join("\n")
            );
            ic_cdk::println!(
                "INFO: lookup_application_with_origin_memory_old:\n{}",
                self.lookup_application_with_origin_memory_old
                    .iter()
                    .map(|(k, v)| format!("{}:{}\n", k, v))
                    .collect::<Vec<_>>()
                    .join("\n")
            );
        } else {
            ic_cdk::println!(
                "INFO: Successfully migrated {} entries.",
                self.lookup_application_with_origin_memory.len()
            );
        }

        // Log all migrated entries (compactly)
        ic_cdk::println!(
            "INFO: lookup_application_with_origin_memory:\n{}",
            self.lookup_application_with_origin_memory
                .iter()
                .map(|(k, v)| format!("{}:{}\n", k, v))
                .collect::<Vec<_>>()
                .join("\n")
        );

        if !app_collisions.is_empty() {
            ic_cdk::println!(
                "WARNING: Discovered {} app collisions (not present in the old map).",
                app_collisions.len()
            );
            ic_cdk::println!(
                "INFO: app_collisions:\n{}",
                app_collisions
                    .iter()
                    .map(|(k, v)| format!("{}:{}\n", k, v))
                    .collect::<Vec<_>>()
                    .join("\n")
            );
        } else {
            ic_cdk::println!(
                "INFO: Migration completed SUCCESSFULLY without discovering any app collisions."
            );
        }
    }

    /// This function implements the Application Origin Hash Collision Resistance data migration,
    /// as specified in ID-343. This code is temporary, and it has an effect only once (the effect
    /// being that `lookup_application_with_origin_memory` is populated with entries corresponding
    /// to the existing applications).
    ///
    /// In the unlikely event that there already was a collision, this function populates
    /// the abovementioned map also by directly scanning `stable_application_memory`.
    ///
    /// The return type is an optional tuple containing the migrated entries, missing apps, and app
    /// collisions (where None indicates that the migration had already run before).
    #[allow(clippy::type_complexity)]
    pub fn maybe_apply_lookup_application_with_origin_map_data_migration(
        &mut self,
    ) -> Option<(
        BTreeMap<String, StorableOriginSha256>, // migrated_entries
        BTreeMap<StorableOriginHash, u64>,      // missing_apps
        BTreeMap<StorableOriginSha256, u64>,    // app_collisions
    )> {
        if !self.lookup_application_with_origin_memory.is_empty() {
            return None;
        }

        // Used only for validation in the unlikely event that the migration is incomplete.
        let mut migrated_entries = BTreeMap::<String, StorableOriginSha256>::new();

        let mut missing_apps = BTreeMap::<StorableOriginHash, u64>::new();

        for (key, app_id) in self.lookup_application_with_origin_memory_old.iter() {
            let Some(StorableApplication { origin, .. }) =
                self.stable_application_memory.get(&app_id)
            else {
                missing_apps.insert(key, app_id);
                continue;
            };

            let new_key = StorableOriginSha256::from_origin(&origin);

            migrated_entries.insert(origin, new_key.clone());

            self.lookup_application_with_origin_memory
                .insert(new_key, app_id);
        }

        let mut app_collisions = BTreeMap::<StorableOriginSha256, u64>::new();

        for (add_id, StorableApplication { origin, .. }) in self.stable_application_memory.iter() {
            let new_key = StorableOriginSha256::from_origin(&origin);

            if self
                .lookup_application_with_origin_memory
                .get(&new_key)
                .is_some()
            {
                // No work to be done, key already exists in the new map
                continue;
            }

            self.lookup_application_with_origin_memory
                .insert(new_key.clone(), add_id);

            app_collisions.insert(new_key, add_id);
        }

        self.log_after_migration(&migrated_entries, &missing_apps, &app_collisions);

        Some((migrated_entries, missing_apps, app_collisions))
    }
}

#[cfg(test)]
mod tests {
    use crate::storage::storable::application::{
        StorableApplication, StorableOriginHash, StorableOriginSha256,
    };
    use crate::Storage;
    use ic_stable_structures::VectorMemory;
    use std::collections::{BTreeMap, HashMap};

    // Helper function to avoid repetitive conversions
    fn create_origin_keys(origin: &str) -> (StorableOriginHash, StorableOriginSha256) {
        let origin_string = origin.to_string();
        (
            StorableOriginHash::from_origin(&origin_string),
            StorableOriginSha256::from_origin(&origin_string),
        )
    }

    #[test]
    fn should_skip_migration_if_new_map_not_empty() {
        let mut storage = Storage::new((10, 20), VectorMemory::default());

        let origin = "https://already-migrated.com";
        let (_, sha256_key) = create_origin_keys(origin);
        storage
            .lookup_application_with_origin_memory
            .insert(sha256_key, 42);

        let old_origin = "https://should-not-migrate.com";
        let (old_key, _) = create_origin_keys(old_origin);
        storage
            .lookup_application_with_origin_memory_old
            .insert(old_key, 99);

        // Smoke test
        assert_eq!(storage.lookup_application_with_origin_memory.len(), 1);
        assert_eq!(storage.lookup_application_with_origin_memory_old.len(), 1);

        // Record initial state
        let new_map_before = storage
            .lookup_application_with_origin_memory
            .iter()
            .map(|(k, _)| k)
            .collect::<Vec<_>>();
        let old_map_before = storage
            .lookup_application_with_origin_memory_old
            .iter()
            .map(|(k, _)| k)
            .collect::<Vec<_>>();

        // Run migration
        let result = storage.maybe_apply_lookup_application_with_origin_map_data_migration();

        // Should return None (migration skipped)
        assert_eq!(result, None);

        // Should not have changed anything
        let new_map_after = storage
            .lookup_application_with_origin_memory
            .iter()
            .map(|(k, _)| k)
            .collect::<Vec<_>>();
        let old_map_after = storage
            .lookup_application_with_origin_memory_old
            .iter()
            .map(|(k, _)| k)
            .collect::<Vec<_>>();

        assert_eq!(new_map_before, new_map_after);
        assert_eq!(old_map_before, old_map_after);
    }

    #[test]
    fn should_migrate_single_entry_successfully() {
        let mut storage = Storage::new((10, 20), VectorMemory::default());
        let origin = "https://single-test.com";

        // Create application in storage
        let app = StorableApplication {
            origin: origin.to_string(),
            stored_accounts: 5,
            stored_account_references: 3,
        };
        let app_id = 100u64;
        storage.stable_application_memory.insert(app_id, app);

        // Add entry to old lookup map
        let (old_key, new_key) = create_origin_keys(origin);
        storage
            .lookup_application_with_origin_memory_old
            .insert(old_key.clone(), app_id);

        // Ensure new map is empty
        assert!(storage.lookup_application_with_origin_memory.is_empty());

        // Run migration
        let result = storage.maybe_apply_lookup_application_with_origin_map_data_migration();

        let (migrated_entries, missing_apps, app_collisions) = result.unwrap();

        // Assert return values
        assert_eq!(
            migrated_entries,
            BTreeMap::from([(origin.to_string(), new_key.clone())])
        );
        assert!(missing_apps.is_empty());
        assert!(app_collisions.is_empty());

        // Verify migration worked
        assert_eq!(
            storage
                .lookup_application_with_origin_memory
                .iter()
                .collect::<Vec<_>>(),
            vec![(new_key, app_id)]
        );
        assert_eq!(
            storage
                .lookup_application_with_origin_memory_old
                .iter()
                .collect::<Vec<_>>(),
            vec![(old_key, app_id)]
        );
    }

    #[test]
    fn should_migrate_multiple_entries_successfully() {
        let mut storage = Storage::new((10, 20), VectorMemory::default());
        let origins = vec![
            "https://app1.com",
            "https://app2.org",
            "https://app3.net",
            "https://different-domain.io",
        ];

        let mut expected_migrations = HashMap::new();
        let mut expected_migrated_entries = BTreeMap::new();

        // Create applications and old lookup entries
        for (i, origin) in origins.iter().enumerate() {
            let app_id = i as u64 + 200; // Start from 200 to avoid conflicts

            let app = StorableApplication {
                origin: origin.to_string(),
                stored_accounts: i as u64 * 2,
                stored_account_references: i as u64,
            };
            storage.stable_application_memory.insert(app_id, app);

            let (old_key, new_key) = create_origin_keys(origin);
            storage
                .lookup_application_with_origin_memory_old
                .insert(old_key, app_id);

            expected_migrations.insert(origin.to_string(), app_id);
            expected_migrated_entries.insert(origin.to_string(), new_key);
        }

        // Smoke test
        assert_eq!(
            storage.lookup_application_with_origin_memory_old.len(),
            origins.len() as u64
        );
        assert_eq!(storage.lookup_application_with_origin_memory.len(), 0_u64);

        // Ensure new map is empty
        assert!(storage.lookup_application_with_origin_memory.is_empty());

        // Run migration
        let result = storage.maybe_apply_lookup_application_with_origin_map_data_migration();

        let (migrated_entries, missing_apps, app_collisions) = result.unwrap();

        // Assert return values
        assert_eq!(migrated_entries, expected_migrated_entries);
        assert!(missing_apps.is_empty());
        assert!(app_collisions.is_empty());

        // Verify all entries were migrated
        assert_eq!(
            storage.lookup_application_with_origin_memory.len(),
            origins.len() as u64
        );
        assert_eq!(
            storage.lookup_application_with_origin_memory_old.len(),
            origins.len() as u64
        );

        // Verify each entry was migrated correctly
        for (origin, expected_app_id) in expected_migrations {
            let (old_key, new_key) = create_origin_keys(&origin);

            assert_eq!(
                storage.lookup_application_with_origin_memory.get(&new_key),
                Some(expected_app_id),
                "Failed to migrate origin: {}",
                origin
            );

            assert_eq!(
                storage
                    .lookup_application_with_origin_memory_old
                    .get(&old_key),
                Some(expected_app_id),
                "Migration deleted old map entry for origin: {}",
                origin
            );
        }
    }

    #[test]
    fn should_handle_missing_application_gracefully() {
        let mut storage = Storage::new((10, 20), VectorMemory::default());
        let origin1 = "https://exists.com";
        let origin2 = "https://missing.com";

        // Create only one application (origin1)
        let app1 = StorableApplication {
            origin: origin1.to_string(),
            stored_accounts: 10,
            stored_account_references: 5,
        };
        let app_id1 = 300u64;
        let app_id2 = 301u64; // This app doesn't exist in stable_application_memory

        storage.stable_application_memory.insert(app_id1, app1);

        // Add both to old lookup map
        let (old_key1, new_key1) = create_origin_keys(origin1);
        let (old_key2, _) = create_origin_keys(origin2);
        storage
            .lookup_application_with_origin_memory_old
            .insert(old_key1, app_id1);
        storage
            .lookup_application_with_origin_memory_old
            .insert(old_key2.clone(), app_id2); // Missing app

        let old_map_before = storage
            .lookup_application_with_origin_memory_old
            .iter()
            .map(|(k, _)| k)
            .collect::<Vec<_>>();

        // Run migration
        let result = storage.maybe_apply_lookup_application_with_origin_map_data_migration();

        let (migrated_entries, missing_apps, app_collisions) = result.unwrap();

        // Assert return values
        assert_eq!(
            migrated_entries,
            BTreeMap::from([(origin1.to_string(), new_key1.clone())])
        );
        assert_eq!(missing_apps, BTreeMap::from([(old_key2, app_id2)]));
        assert!(app_collisions.is_empty());

        // Should only migrate the existing application, so the counts should be different
        // (2 old entries, 1 new entry)
        assert_eq!(storage.lookup_application_with_origin_memory.len(), 1);
        assert_eq!(storage.lookup_application_with_origin_memory_old.len(), 2);

        // Verify only the valid entry was migrated
        let (_, new_key1) = create_origin_keys(origin1);
        assert_eq!(
            storage.lookup_application_with_origin_memory.get(&new_key1),
            Some(app_id1)
        );

        // Missing application should not be in new map
        let (_, new_key2) = create_origin_keys(origin2);
        assert_eq!(
            storage.lookup_application_with_origin_memory.get(&new_key2),
            None
        );

        // Old map should remain unchanged.
        let old_map_after = storage
            .lookup_application_with_origin_memory_old
            .iter()
            .map(|(k, _)| k)
            .collect::<Vec<_>>();

        assert_eq!(old_map_before, old_map_after);
    }

    #[test]
    fn should_handle_empty_old_map() {
        let mut storage = Storage::new((10, 20), VectorMemory::default());

        // Ensure both maps are empty
        assert!(storage.lookup_application_with_origin_memory.is_empty());
        assert!(storage.lookup_application_with_origin_memory_old.is_empty());

        // Run migration
        let result = storage.maybe_apply_lookup_application_with_origin_map_data_migration();

        let (migrated_entries, missing_apps, app_collisions) = result.unwrap();

        // Assert return values are all empty
        assert!(migrated_entries.is_empty());
        assert!(missing_apps.is_empty());
        assert!(app_collisions.is_empty());

        // Both maps should remain empty
        assert!(storage.lookup_application_with_origin_memory.is_empty());
        assert!(storage.lookup_application_with_origin_memory_old.is_empty());
    }

    #[test]
    fn should_handle_hash_collisions_correctly() {
        let mut storage = Storage::new((10, 20), VectorMemory::default());

        // These origins have hash prefix collisions with 8-byte hash but not with full SHA-256.
        // Example taken from https://github.com/yugt/sha256-prefix-collision
        let origin1 = "08RTz8".to_string();
        let origin2 = "4iRDWF".to_string();
        let origins = vec![origin1.clone(), origin2.clone()];

        let mut expected_apps = HashMap::new();

        // Create applications
        for (i, origin) in origins.iter().enumerate() {
            let app_id = i as u64 + 400;
            let app = StorableApplication {
                origin: origin.to_string(),
                stored_accounts: i as u64 + 1,
                stored_account_references: i as u64,
            };
            storage
                .stable_application_memory
                .insert(app_id, app.clone());
            expected_apps.insert(origin.to_string(), (app_id, app));

            // Add to old map (unless the key has already been used)
            let (old_key, _) = create_origin_keys(origin);
            if storage
                .lookup_application_with_origin_memory_old
                .get(&old_key)
                .is_none()
            {
                storage
                    .lookup_application_with_origin_memory_old
                    .insert(old_key, app_id);
            }
        }

        // Smoke test: there actually was a collision
        assert_eq!(origins.len(), 2);
        assert_eq!(storage.stable_application_memory.len(), 2);
        assert_eq!(storage.lookup_application_with_origin_memory_old.len(), 1);

        // Run migration
        let result = storage.maybe_apply_lookup_application_with_origin_map_data_migration();

        let (migrated_entries, missing_apps, app_collisions) = result.unwrap();

        // Assert return values
        let (_, expected_new_key1) = create_origin_keys(&origin1);
        let (_, expected_new_key2) = create_origin_keys(&origin2);
        assert_eq!(
            migrated_entries,
            BTreeMap::from([(origin1, expected_new_key1)])
        );
        assert!(missing_apps.is_empty());
        assert_eq!(app_collisions, BTreeMap::from([(expected_new_key2, 401)]));

        // All entries should be migrated successfully
        assert_eq!(
            storage.lookup_application_with_origin_memory.len(),
            origins.len() as u64
        );

        // Verify each origin maps to correct application
        for (origin, (expected_app_id, expected_app)) in expected_apps {
            let (_, new_key) = create_origin_keys(&origin);
            let migrated_app_id = storage.lookup_application_with_origin_memory.get(&new_key);
            assert_eq!(migrated_app_id, Some(expected_app_id));

            // Verify the application data is correct
            let stored_app = storage
                .stable_application_memory
                .get(&expected_app_id)
                .unwrap();
            assert_eq!(stored_app, expected_app);
        }
    }
}
