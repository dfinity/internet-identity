use crate::{
    state::{storage_borrow, storage_borrow_mut},
    storage::account::{
        Account, AccountReference, AccountsCounter, CreateAccountParams, ReadAccountParams,
    },
};
use internet_identity_interface::internet_identity::types::{
    AccountNumber, AnchorNumber, CreateAccountError, FrontendHostname,
};

const MAX_ANCHOR_ACCOUNTS: usize = 500;

#[allow(dead_code)]
pub fn anchor_has_account(
    anchor_number: AnchorNumber,
    origin: &FrontendHostname,
    account_number: Option<AccountNumber>,
) -> Option<AccountReference> {
    // check if anchor has acc
    storage_borrow(|storage| {
        storage
            .lookup_application_number_with_origin(origin)
            .and_then(|application_number| {
                storage.has_account_reference(anchor_number, application_number, account_number)
            })
    })
}

pub fn get_accounts_for_origin(
    anchor_number: &AnchorNumber,
    origin: &FrontendHostname,
) -> Vec<Account> {
    storage_borrow(|storage| {
        storage
            .list_accounts(anchor_number, origin)
            .iter()
            .filter_map(|acc_ref| {
                storage.read_account(ReadAccountParams {
                    account_number: &acc_ref.account_number,
                    anchor_number,
                    origin,
                })
            })
            .collect()
    })
}

pub fn create_account_for_origin(
    anchor_number: AnchorNumber,
    origin: FrontendHostname,
    name: String,
) -> Result<Account, CreateAccountError> {
    storage_borrow_mut(|storage| {
        let AccountsCounter {
            stored_accounts,
            stored_account_references: _,
        } = storage.get_account_counter(anchor_number);

        if stored_accounts >= MAX_ANCHOR_ACCOUNTS as u64 {
            return Err(CreateAccountError::MaximumAccountNumberReached);
        }

        storage
            .create_additional_account(CreateAccountParams {
                anchor_number,
                origin,
                name,
            })
            .map_err(|err| CreateAccountError::InternalCanisterError(format!("{}", err)))
    })
}

#[test]
fn should_create_account_for_origin() {
    use crate::state::{storage_borrow_mut, storage_replace};
    use crate::storage::Storage;
    use ic_stable_structures::VectorMemory;

    storage_replace(Storage::new((0, 10000), VectorMemory::default()));
    let anchor = storage_borrow_mut(|storage| storage.allocate_anchor().unwrap());
    let origin = "https://example.com".to_string();
    let name = "Alice".to_string();

    assert_eq!(
        create_account_for_origin(anchor.anchor_number(), origin.clone(), name.clone()),
        Ok(Account {
            account_number: Some(1),
            anchor_number: anchor.anchor_number(),
            origin,
            last_used: None,
            name: Some(name)
        })
    );
}

#[test]
fn should_fail_to_create_accounts_above_max() {
    use crate::state::{storage_borrow_mut, storage_replace};
    use crate::storage::Storage;
    use ic_stable_structures::VectorMemory;

    storage_replace(Storage::new((0, 10000), VectorMemory::default()));
    let anchor = storage_borrow_mut(|storage| storage.allocate_anchor().unwrap());
    let name = "Alice".to_string();
    for i in 0..=MAX_ANCHOR_ACCOUNTS {
        let origin = format!("https://example-{}.com", i);
        let result =
            create_account_for_origin(anchor.anchor_number(), origin.clone(), name.clone());
        if i == MAX_ANCHOR_ACCOUNTS {
            assert_eq!(result, Err(CreateAccountError::MaximumAccountNumberReached))
        }
    }
}

#[test]
fn should_get_accounts_for_origin() {
    use crate::state::{storage_borrow_mut, storage_replace};
    use crate::storage::Storage;
    use ic_stable_structures::VectorMemory;

    storage_replace(Storage::new((0, 10000), VectorMemory::default()));
    let anchor = storage_borrow_mut(|storage| storage.allocate_anchor().unwrap());
    let origin = "https://example.com".to_string();
    let name = "Alice".to_string();
    let name_two = "Bob".to_string();
    let anchor_number = anchor.anchor_number();

    let _ = create_account_for_origin(anchor_number, origin.clone(), name.clone());
    let _ = create_account_for_origin(anchor_number, origin.clone(), name_two.clone());

    assert_eq!(
        get_accounts_for_origin(&anchor_number, &origin),
        vec![
            Account {
                account_number: None, // default account gets created when additional account gets created
                anchor_number,
                origin: origin.clone(),
                last_used: None,
                name: None
            },
            Account {
                account_number: Some(1),
                anchor_number,
                origin: origin.clone(),
                last_used: None,
                name: Some("Alice".to_string())
            },
            Account {
                account_number: Some(2),
                anchor_number,
                origin,
                last_used: None,
                name: Some("Bob".to_string())
            }
        ]
    )
}

#[test]
fn should_only_get_own_accounts_for_origin() {
    use crate::state::{storage_borrow_mut, storage_replace};
    use crate::storage::Storage;
    use ic_stable_structures::VectorMemory;

    storage_replace(Storage::new((0, 10000), VectorMemory::default()));
    let anchor = storage_borrow_mut(|storage| storage.allocate_anchor().unwrap());
    let anchor_two = storage_borrow_mut(|storage| storage.allocate_anchor().unwrap());
    let origin = "https://example.com".to_string();
    let name = "Alice".to_string();
    let name_two = "Bob".to_string();
    let anchor_number = anchor.anchor_number();
    let anchor_number_two = anchor_two.anchor_number();

    let _ = create_account_for_origin(anchor_number, origin.clone(), name.clone());
    let _ = create_account_for_origin(anchor_number_two, origin.clone(), name_two.clone());

    assert_eq!(
        get_accounts_for_origin(&anchor_number, &origin),
        vec![
            Account {
                account_number: None, // default account gets created when additional account gets created
                anchor_number,
                origin: origin.clone(),
                last_used: None,
                name: None
            },
            Account {
                account_number: Some(1), // because of how allocate_account_number is implemented, this starts at 1
                anchor_number,
                origin: origin.clone(),
                last_used: None,
                name: Some("Alice".to_string())
            },
        ]
    );

    assert_eq!(
        get_accounts_for_origin(&anchor_number_two, &origin),
        vec![
            Account {
                account_number: None, // default account gets created when additional account gets created
                anchor_number: anchor_number_two,
                origin: origin.clone(),
                last_used: None,
                name: None
            },
            Account {
                account_number: Some(2),
                anchor_number: anchor_number_two,
                origin,
                last_used: None,
                name: Some("Bob".to_string())
            }
        ]
    )
}
