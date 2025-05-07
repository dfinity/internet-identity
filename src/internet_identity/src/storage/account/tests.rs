use crate::storage::account::{Account, AccountReference};
use crate::storage::{CreateAccountParams, ReadAccountParams, UpdateAccountParams};
use crate::Storage;
use ic_stable_structures::VectorMemory;
use internet_identity_interface::internet_identity::types::{AnchorNumber, FrontendHostname};

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
        origin: origin.clone(),
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

    // 5. Check that read_account returns additional account and creates application.
    let additional_account = storage.read_account(read_params).unwrap();
    let expected_account = Account {
        account_number: Some(1),
        anchor_number,
        origin: origin.clone(),
        name: Some(account_name.clone()),
        last_used: None,
    };
    assert_eq!(additional_account, expected_account);
    assert!(storage
        .lookup_application_number_with_origin(&origin)
        .is_some(),);
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
    let new_account = CreateAccountParams {
        anchor_number,
        origin: origin.clone(),
        name: account_name.clone(),
    };
    let expected_additional_account_ref = AccountReference {
        account_number: Some(1),
        last_used: None,
    };
    let expected_default_account_ref = AccountReference {
        account_number: None,
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
    let listed_accounts = storage.list_identity_accounts(anchor_number);
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
    let listed_accounts = storage.list_identity_accounts(anchor_number);
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
    let initial_accounts = storage.list_accounts(&anchor_number, &origin).unwrap();
    let expected_unreserved_account = AccountReference {
        account_number: None,
        last_used: None,
    };
    assert_eq!(initial_accounts, vec![expected_unreserved_account]);

    // 3. Update default account
    let updated_account_params = UpdateAccountParams {
        anchor_number,
        origin: origin.clone(),
        name: account_name.clone(),
        account_number: None,
    };
    let new_account_number = storage.update_account(updated_account_params).unwrap();

    // 4. Check that the default account has been created with the updated values.
    let updated_accounts = storage.list_accounts(&anchor_number, &origin).unwrap();
    let expected_updated_account = AccountReference {
        account_number: Some(new_account_number),
        last_used: None,
    };
    assert_eq!(updated_accounts, vec![expected_updated_account]);
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
        origin: origin.clone(),
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
    let update_account_return_value = storage.update_account(updated_account_params).unwrap();

    assert_eq!(update_account_return_value, account_number);

    // 5. Check that the additional account has been created with the updated values.
    let updated_account = storage
        .read_account(ReadAccountParams {
            account_number: Some(update_account_return_value),
            anchor_number,
            origin: origin.clone(),
        })
        .unwrap();
    let expected_updated_account = Account {
        account_number: Some(update_account_return_value),
        anchor_number,
        origin: origin.clone(),
        last_used: None,
        name: Some(new_account_name),
    };
    assert_eq!(updated_account, expected_updated_account);
}
