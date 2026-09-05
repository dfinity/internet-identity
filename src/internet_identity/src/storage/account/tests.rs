use crate::storage::account::Account;
use crate::storage::account::AccountKey;
use crate::storage::storable::account_reference_list::StorableAccountReferenceList;
use crate::storage::storable::application::StorableApplication;
use crate::Storage;
use ic_stable_structures::VectorMemory;
use internet_identity_interface::internet_identity::types::{AnchorNumber, FrontendHostname};

use super::AccountsCounter;

fn assert_empty_counters(storage: &Storage<VectorMemory>, anchor_number: AnchorNumber) {
    assert_eq!(
        storage.get_account_counter(anchor_number),
        AccountsCounter::default()
    );
    assert_eq!(
        storage.get_total_accounts_counter(),
        AccountsCounter::default()
    );
}

#[test]
fn should_create_a_named_account() {
    // Setup storage
    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory);

    // 1. Define additional account parameters
    let anchor_number: AnchorNumber = 10_000;
    let origin: FrontendHostname = "https://some.origin".to_string();
    let account_name = "account name".to_string();

    // 2. Additional account and application don't exist yet.
    let read_params = AccountKey {
        anchor_number,
        origin: origin.clone(),
        account_number: Some(1),
    };
    let additional_account_1 = storage.read_account(&read_params);
    assert!(
        additional_account_1.is_none(),
        "Additional account should not exist yet"
    );
    assert!(
        storage
            .lookup_application_number_with_origin(&origin)
            .is_none(),
        "Application should not exist yet"
    );
    assert_empty_counters(&storage, anchor_number);

    // 3. Create additional account
    storage
        .create_account(anchor_number, origin.clone(), account_name.clone())
        .unwrap();

    // 5. Check that read_account returns additional account, creates application and updates counters.
    let additional_account = storage.read_account(&read_params).unwrap();
    let expected_account = Account {
        account_number: Some(1),
        anchor_number,
        origin: origin.clone(),
        name: Some(account_name.clone()),
        last_used: None,
        seed_from_anchor: None,
    };
    assert_eq!(additional_account, expected_account);
    assert_eq!(
        storage.lookup_application_with_origin(&origin).unwrap(),
        StorableApplication {
            origin: origin.clone(),
            stored_accounts: 1,
            stored_account_references: 2,
            tombstones: 0,
        }
    );
    assert_eq!(
        storage.get_account_counter(anchor_number),
        AccountsCounter {
            stored_accounts: 1,
            stored_account_references: 2,
        }
    );
    assert_eq!(
        storage.get_total_accounts_counter(),
        AccountsCounter {
            stored_accounts: 1,
            stored_account_references: 2,
        }
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
    let anchor = storage.allocate_anchor(0).unwrap();
    storage.write(anchor).unwrap();

    // 3. List accounts returns default account
    let listed_accounts = storage.list_accounts(anchor_number, &origin);
    assert_eq!(listed_accounts.len(), 1);
    assert!(listed_accounts[0].account_number.is_none());
    assert_empty_counters(&storage, anchor_number);

    // 4. Create new account
    let expected_additional_account = Account::new(
        anchor_number,
        origin.clone(),
        Some(account_name.clone()),
        Some(1),
    );
    let expected_default_account = Account::synthetic(anchor_number, origin.clone());
    storage
        .create_account(anchor_number, origin.clone(), account_name.clone())
        .unwrap();

    // 5. List accounts returns default account
    let listed_accounts = storage.list_accounts(anchor_number, &origin);

    // 6. Assert that the list contains exactly two accounts and it matches the expected one
    assert_eq!(
        listed_accounts.len(),
        2,
        "Expected exactly two accounts to be listed"
    );
    assert_eq!(
        listed_accounts[0], expected_default_account,
        "Default account reference is missing from the listed accounts."
    );
    assert_eq!(
        listed_accounts[1], expected_additional_account,
        "Additional account reference is missing from the listed accounts."
    );
    assert_eq!(
        storage.get_account_counter(anchor_number),
        AccountsCounter {
            stored_accounts: 1,
            stored_account_references: 2,
        }
    );
    assert_eq!(
        storage.get_total_accounts_counter(),
        AccountsCounter {
            stored_accounts: 1,
            stored_account_references: 2,
        }
    );
}

#[test]
fn should_list_all_identity_accounts() {
    // Setup storage
    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory);

    // 1. Define additional account parameters
    let anchor_number: AnchorNumber = 10_000;
    let account_name = "account name".to_string();
    let origin: FrontendHostname = "https://some.origin".to_string();
    let origin_2: FrontendHostname = "https://some-other.origin".to_string();

    // 2. Save anchor to stable memory
    let anchor = storage.allocate_anchor(0).unwrap();
    storage.write(anchor).unwrap();

    // 3. List accounts returns default account
    let listed_accounts = storage.list_identity_account_references(anchor_number);
    assert_eq!(listed_accounts.len(), 0);

    // 4. Create additional account
    storage
        .create_account(anchor_number, origin.clone(), account_name.clone())
        .unwrap();

    // 5. List accounts returns default account
    let listed_accounts = storage.list_identity_account_references(anchor_number);
    // Default account + additional account for the origin application.
    assert_eq!(listed_accounts.len(), 2);

    // 6. Create additional account
    storage
        .create_account(anchor_number, origin_2.clone(), account_name.clone())
        .unwrap();

    // 7. List accounts returns default account
    let listed_accounts = storage.list_identity_account_references(anchor_number);
    // Default account + additional account for the origin_2 application.
    assert_eq!(listed_accounts.len(), 4);

    assert_eq!(
        storage.get_account_counter(anchor_number),
        AccountsCounter {
            stored_accounts: 2,
            stored_account_references: 4,
        }
    );
    assert_eq!(
        storage.get_total_accounts_counter(),
        AccountsCounter {
            stored_accounts: 2,
            stored_account_references: 4,
        }
    );
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
    let initial_accounts = storage.list_accounts(anchor_number, &origin);
    let expected_unreserved_account = Account::synthetic(anchor_number, origin.clone());
    assert_eq!(initial_accounts, vec![expected_unreserved_account]);

    // 3. Update default account
    let mut account_to_update = storage
        .read_account(&AccountKey {
            anchor_number,
            origin: origin.clone(),
            account_number: None,
        })
        .unwrap();
    account_to_update.name = Some(account_name.clone());
    let new_account = storage.write_account(account_to_update).unwrap();

    // 4. Check that the default account has been created with the updated values.
    assert_eq!(
        new_account,
        Account::new_full(
            anchor_number,
            origin,
            Some(account_name),
            new_account.account_number,
            None,
            Some(anchor_number),
        )
    );
    assert_eq!(
        storage.get_account_counter(anchor_number),
        AccountsCounter {
            stored_accounts: 1,
            stored_account_references: 1,
        }
    );
    assert_eq!(
        storage.get_total_accounts_counter(),
        AccountsCounter {
            stored_accounts: 1,
            stored_account_references: 1,
        }
    );
}

#[test]
fn should_update_additional_account() {
    // Setup storage
    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory);

    // 1. Define additional account parameters
    let anchor_number: AnchorNumber = 10_000;
    let origin: FrontendHostname = "https://some.origin".to_string();
    let account_name = "account name".to_string();
    let new_account_name = "new account name".to_string();
    let account_number = 1;

    // 2. Additional account and application don't exist yet.
    let read_params = AccountKey {
        anchor_number,
        origin: origin.clone(),
        account_number: Some(account_number),
    };
    let additional_account_1 = storage.read_account(&read_params);
    assert!(
        additional_account_1.is_none(),
        "Additional account should not exist yet"
    );
    assert!(
        storage
            .lookup_application_number_with_origin(&origin)
            .is_none(),
        "Application should not exist yet"
    );

    // 3. Create additional account
    storage
        .create_account(anchor_number, origin.clone(), account_name.clone())
        .unwrap();
    assert!(storage.read_account(&read_params).is_some());

    // 4. Update additional account
    let mut account_to_update = storage
        .read_account(&AccountKey {
            anchor_number,
            origin: origin.clone(),
            account_number: Some(1),
        })
        .unwrap();
    account_to_update.name = Some(new_account_name.clone());
    let updated_account = storage.write_account(account_to_update).unwrap();

    // 5. Check that the additional account has been created with the updated values.
    assert_eq!(
        updated_account,
        Account {
            account_number: Some(1),
            anchor_number,
            origin: origin.clone(),
            last_used: None,
            name: Some(new_account_name),
            seed_from_anchor: None,
        }
    );
    assert_eq!(
        storage.get_account_counter(anchor_number),
        AccountsCounter {
            stored_accounts: 1,
            stored_account_references: 2,
        }
    );
    assert_eq!(
        storage.get_total_accounts_counter(),
        AccountsCounter {
            stored_accounts: 1,
            stored_account_references: 2,
        }
    );
}

#[test]
fn should_count_accounts_different_anchors() {
    // Setup storage
    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory);

    // --- Anchor 1 ---
    let anchor_1 = storage.allocate_anchor(0).unwrap();
    storage.write(anchor_1.clone()).unwrap();
    let anchor_number_1 = anchor_1.anchor_number();
    let origin_1: FrontendHostname = "https://origin1.com".to_string();
    let account_name_1 = "account_anchor1".to_string();

    // List accounts for anchor 1 - should return 1 (default)
    let accounts_anchor_1_initial = storage.list_accounts(anchor_number_1, &origin_1);
    assert_eq!(
        accounts_anchor_1_initial.len(),
        1,
        "Initial list for anchor 1 should have 1 account"
    );
    assert!(
        accounts_anchor_1_initial[0].account_number.is_none(),
        "Initial account should be default"
    );
    assert_empty_counters(&storage, anchor_number_1);

    // Check counters for anchor 1 - should be 0
    assert_eq!(
        storage.get_account_counter(anchor_number_1),
        AccountsCounter::default(),
        "Counters for anchor 1 should be 0"
    );
    assert_eq!(
        storage.get_total_accounts_counter(),
        AccountsCounter::default(),
        "Total counters should be 0"
    );

    // Create an additional account for anchor 1
    storage
        .create_account(anchor_number_1, origin_1.clone(), account_name_1.clone())
        .unwrap();

    // List accounts for anchor 1 - should return 2
    let accounts_anchor_1_after_add = storage.list_accounts(anchor_number_1, &origin_1);
    assert_eq!(
        accounts_anchor_1_after_add.len(),
        2,
        "List for anchor 1 after additional account should have 2 accounts"
    );

    // Check counters for anchor 1 and total counters
    let expected_counters_anchor_1 = AccountsCounter {
        stored_accounts: 1,
        stored_account_references: 2,
    };
    assert_eq!(
        storage.get_account_counter(anchor_number_1),
        expected_counters_anchor_1,
        "Counters for anchor 1 after additional account mismatch"
    );
    assert_eq!(
        storage.get_total_accounts_counter(),
        expected_counters_anchor_1,
        "Total counters after anchor 1 additional account mismatch"
    );

    // --- Anchor 2 ---
    let anchor_2 = storage.allocate_anchor(0).unwrap();
    storage.write(anchor_2.clone()).unwrap();
    let anchor_number_2 = anchor_2.anchor_number();
    let origin_2: FrontendHostname = "https://origin2.com".to_string();
    let account_name_2 = "account_anchor2".to_string();

    // List accounts for anchor 2 - should return 1 (default)
    let accounts_anchor_2_initial = storage.list_accounts(anchor_number_2, &origin_2);
    assert_eq!(
        accounts_anchor_2_initial.len(),
        1,
        "Initial list for anchor 2 should have 1 account"
    );
    assert!(
        accounts_anchor_2_initial[0].account_number.is_none(),
        "Initial account for anchor 2 should be default"
    );

    // Check counters for anchor 2 - should be 0 (total counters still reflect anchor 1)
    assert_eq!(
        storage.get_account_counter(anchor_number_2),
        AccountsCounter::default(),
        "Counters for anchor 2 should be default 0"
    );

    // Create an additional account for anchor 2
    storage
        .create_account(anchor_number_2, origin_2.clone(), account_name_2.clone())
        .unwrap();

    // List accounts for anchor 2 - should return 2
    let accounts_anchor_2_after_add = storage.list_accounts(anchor_number_2, &origin_2);
    assert_eq!(
        accounts_anchor_2_after_add.len(),
        2,
        "List for anchor 2 after additional account should have 2 accounts"
    );

    // Check counters for anchor 2
    let expected_counters_anchor_2 = AccountsCounter {
        stored_accounts: 1,
        stored_account_references: 2,
    };
    assert_eq!(
        storage.get_account_counter(anchor_number_2),
        expected_counters_anchor_2,
        "Counters for anchor 2 after additional account mismatch"
    );

    // Check total counters - should be sum of anchor 1 and anchor 2
    let expected_total_counters = AccountsCounter {
        stored_accounts: 2,           // 1 from anchor_1 + 1 from anchor_2
        stored_account_references: 4, // 2 from anchor_1 + 2 from anchor_2
    };
    assert_eq!(
        storage.get_total_accounts_counter(),
        expected_total_counters,
        "Total counters after anchor 2 additional account mismatch"
    );
}

#[test]
fn should_not_read_a_default_account_from_an_empty_reference_list() {
    // Setup storage
    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory);

    // 1. Define parameters
    let anchor_number: AnchorNumber = 10_000;
    let origin: FrontendHostname = "https://some.origin".to_string();

    // 2. Create application but with empty account reference list
    let app_num = storage
        .lookup_or_insert_application_number_with_origin(&origin)
        .unwrap();
    storage.stable_account_reference_list_memory.insert(
        (anchor_number, app_num),
        StorableAccountReferenceList::tombstone_for_testing(),
    );

    // 3. Try to read default account
    let read_params = AccountKey {
        anchor_number,
        origin: origin.clone(),
        account_number: None,
    };

    assert_eq!(storage.read_account(&read_params), None);
}

#[test]
fn should_read_a_synthetic_default_account_when_no_reference_list_exists() {
    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory);

    let anchor_number: AnchorNumber = 10_000;
    let origin: FrontendHostname = "https://some.origin".to_string();
    // The origin is known, but this identity has no row under it.
    storage
        .lookup_or_insert_application_number_with_origin(&origin)
        .unwrap();

    let default_account = storage
        .read_account(&AccountKey {
            anchor_number,
            origin: origin.clone(),
            account_number: None,
        })
        .unwrap();

    assert_eq!(
        default_account,
        Account::synthetic(anchor_number, origin.clone())
    );
}

#[test]
fn should_not_read_account_from_wrong_anchor() {
    // Setup storage
    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory);

    // 1. Define parameters for two different anchors
    let anchor_number_1: AnchorNumber = 10_000;
    let anchor_number_2: AnchorNumber = 10_001;
    let origin: FrontendHostname = "https://some.origin".to_string();
    let account_name = "account name".to_string();

    // 2. Create account for first anchor
    storage
        .create_account(anchor_number_1, origin.clone(), account_name)
        .unwrap();

    // 3. Try to read the account with second anchor
    let read_params = AccountKey {
        anchor_number: anchor_number_2,
        origin: origin.clone(),
        account_number: Some(1),
    };
    let account = storage.read_account(&read_params);

    // 4. Verify we get None since the account doesn't belong to anchor_number_2
    assert!(
        account.is_none(),
        "Should not be able to read account from wrong anchor"
    );
}
