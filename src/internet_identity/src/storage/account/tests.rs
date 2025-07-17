use crate::storage::account::Account;
use crate::storage::storable::application::StorableApplication;
use crate::storage::{CreateAccountParams, ReadAccountParams, UpdateAccountParams};
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
fn should_create_additional_account() {
    // Setup storage
    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory);

    // 1. Define additional account parameters
    let anchor_number: AnchorNumber = 10_000;
    let origin: FrontendHostname = "https://some.origin".to_string();
    let account_name = "account name".to_string();

    // 2. Additional account and application don't exist yet.
    let read_params = ReadAccountParams {
        account_number: Some(1), // First account created
        anchor_number,
        origin: &origin,
        known_app_num: None,
    };
    let additional_account_1 = storage.read_account(read_params.clone());
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
    let new_account_params = CreateAccountParams {
        anchor_number,
        origin: origin.clone(),
        name: account_name.clone(),
    };
    storage
        .create_additional_account(new_account_params)
        .unwrap();

    // 5. Check that read_account returns additional account, creates application and updates counters.
    let additional_account = storage.read_account(read_params).unwrap();
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
    let anchor = storage.allocate_anchor().unwrap();
    storage.create(anchor).unwrap();

    // 3. List accounts returns default account
    let listed_accounts = storage.list_accounts(anchor_number, &origin);
    assert_eq!(listed_accounts.len(), 1);
    assert!(listed_accounts[0].account_number.is_none());
    assert_empty_counters(&storage, anchor_number);

    // 4. Create new account
    let new_account = CreateAccountParams {
        anchor_number,
        origin: origin.clone(),
        name: account_name.clone(),
    };
    let expected_additional_account =
        Account::new(anchor_number, origin.clone(), Some(account_name), Some(1));
    let expected_default_account = Account::new(anchor_number, origin.clone(), None, None);
    storage.create_additional_account(new_account).unwrap();

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
    let anchor = storage.allocate_anchor().unwrap();
    storage.create(anchor).unwrap();

    // 3. List accounts returns default account
    let listed_accounts = storage.list_identity_account_references(anchor_number);
    assert_eq!(listed_accounts.len(), 0);

    // 4. Create additional account
    let new_account_params = CreateAccountParams {
        anchor_number,
        origin: origin.clone(),
        name: account_name.clone(),
    };
    storage
        .create_additional_account(new_account_params)
        .unwrap();

    // 5. List accounts returns default account
    let listed_accounts = storage.list_identity_account_references(anchor_number);
    // Default account + additional account for the origin application.
    assert_eq!(listed_accounts.len(), 2);

    // 6. Create additional account
    let new_account_params = CreateAccountParams {
        anchor_number,
        origin: origin_2.clone(),
        name: account_name.clone(),
    };
    storage
        .create_additional_account(new_account_params)
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
    let expected_unreserved_account = Account::new(anchor_number, origin.clone(), None, None);
    assert_eq!(initial_accounts, vec![expected_unreserved_account]);

    // 3. Update default account
    let updated_account_params = UpdateAccountParams {
        anchor_number,
        origin: origin.clone(),
        name: account_name.clone(),
        account_number: None,
    };
    let new_account = storage.update_account(updated_account_params).unwrap();

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
    let read_params = ReadAccountParams {
        account_number: Some(account_number), // First account created is 1
        anchor_number,
        origin: &origin,
        known_app_num: None,
    };
    let additional_account_1 = storage.read_account(read_params.clone());
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
    let new_account_params = CreateAccountParams {
        anchor_number,
        origin: origin.clone(),
        name: account_name.clone(),
    };
    storage
        .create_additional_account(new_account_params)
        .unwrap();
    assert!(storage.read_account(read_params.clone()).is_some());

    // 4. Update additional account
    let updated_account_params = UpdateAccountParams {
        anchor_number,
        origin: origin.clone(),
        name: new_account_name.clone(),
        account_number: Some(1),
    };
    let updated_account = storage.update_account(updated_account_params).unwrap();

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
    let anchor_1 = storage.allocate_anchor().unwrap();
    storage.create(anchor_1.clone()).unwrap();
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
    let create_params_1 = CreateAccountParams {
        anchor_number: anchor_number_1,
        origin: origin_1.clone(),
        name: account_name_1.clone(),
    };
    storage.create_additional_account(create_params_1).unwrap();

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
    let anchor_2 = storage.allocate_anchor().unwrap();
    storage.create(anchor_2.clone()).unwrap();
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
    let create_params_2 = CreateAccountParams {
        anchor_number: anchor_number_2,
        origin: origin_2.clone(),
        name: account_name_2.clone(),
    };
    storage.create_additional_account(create_params_2).unwrap();

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

// XXX WARNING: this functionality exists for the case that a user might have moved/deleted a default account
// and then reached the maximum accounts limit. If we don't return a synthetic default account here,
// they would be locked out of their account.
// However: if we implement account transfers at some point, and default accounts can be transfered,
// this would allow a user to regain access to their transferred default account.
#[test]
fn should_read_default_account_with_empty_reference_list() {
    // Setup storage
    let memory = VectorMemory::default();
    let mut storage = Storage::new((10_000, 3_784_873), memory);

    // 1. Define parameters
    let anchor_number: AnchorNumber = 10_000;
    let origin: FrontendHostname = "https://some.origin".to_string();

    // 2. Create application but with empty account reference list
    let app_num = storage.lookup_or_insert_application_number_with_origin(&origin);
    storage.stable_account_reference_list_memory.insert(
        (anchor_number, app_num),
        vec![].into(), // Empty reference list
    );

    // 3. Try to read default account
    let read_params = ReadAccountParams {
        account_number: None,
        anchor_number,
        origin: &origin,
        known_app_num: Some(app_num),
    };
    let default_account = storage.read_account(read_params).unwrap();

    // 4. Verify we get a synthetic default account
    let expected_account = Account::new(anchor_number, origin, None, None);
    assert_eq!(default_account, expected_account);
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
    let create_params = CreateAccountParams {
        anchor_number: anchor_number_1,
        origin: origin.clone(),
        name: account_name,
    };
    storage.create_additional_account(create_params).unwrap();

    // 3. Try to read the account with second anchor
    let read_params = ReadAccountParams {
        account_number: Some(1),        // First account created
        anchor_number: anchor_number_2, // Different anchor
        origin: &origin,
        known_app_num: None,
    };
    let account = storage.read_account(read_params);

    // 4. Verify we get None since the account doesn't belong to anchor_number_2
    assert!(
        account.is_none(),
        "Should not be able to read account from wrong anchor"
    );
}
