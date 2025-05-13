use crate::{
    state::{storage_borrow, storage_borrow_mut},
    storage::{
        account::{Account, CreateAccountParams, ReadAccountParams, UpdateAccountParams},
        StorageError,
    },
};
use internet_identity_interface::internet_identity::types::{
    AccountNumber, AccountUpdate, AnchorNumber, CreateAccountError, FrontendHostname,
    UpdateAccountError,
};

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
        storage
            .create_additional_account(CreateAccountParams {
                anchor_number,
                origin,
                name,
            })
            .map_err(|err| CreateAccountError::InternalCanisterError(format!("{}", err)))
    })
}

pub fn update_account_for_origin(
    anchor_number: AnchorNumber,
    account_number: Option<AccountNumber>,
    origin: FrontendHostname,
    update: AccountUpdate,
) -> Result<Account, UpdateAccountError> {
    match update.name {
        Some(name) => storage_borrow_mut(|storage| {
            storage
                .update_account(UpdateAccountParams {
                    account_number,
                    anchor_number,
                    name,
                    origin: origin.clone(),
                })
                .and_then(|acc_num| {
                    storage
                        .read_account(ReadAccountParams {
                            account_number: &Some(acc_num),
                            anchor_number: &anchor_number,
                            origin: &origin,
                        })
                        .ok_or(StorageError::AccountNotFound {
                            account_number: acc_num,
                        })
                })
                .map_err(|err| UpdateAccountError::InternalCanisterError(format!("{}", err)))
        }),
        None => Err(UpdateAccountError::InternalCanisterError(
            "No name was provided.".to_string(),
        )),
    }
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
            name: Some(name),
            seed_from_anchor: None
        })
    );
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
                name: None,
                seed_from_anchor: None
            },
            Account {
                account_number: Some(1),
                anchor_number,
                origin: origin.clone(),
                last_used: None,
                name: Some("Alice".to_string()),
                seed_from_anchor: None
            },
            Account {
                account_number: Some(2),
                anchor_number,
                origin,
                last_used: None,
                name: Some("Bob".to_string()),
                seed_from_anchor: None
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
                name: None,
                seed_from_anchor: None
            },
            Account {
                account_number: Some(1), // because of how allocate_account_number is implemented, this starts at 1
                anchor_number,
                origin: origin.clone(),
                last_used: None,
                name: Some("Alice".to_string()),
                seed_from_anchor: None
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
                name: None,
                seed_from_anchor: None
            },
            Account {
                account_number: Some(2),
                anchor_number: anchor_number_two,
                origin,
                last_used: None,
                name: Some("Bob".to_string()),
                seed_from_anchor: None
            }
        ]
    )
}

#[test]
fn should_update_account_for_origin() {
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
                name: None,
                seed_from_anchor: None
            },
            Account {
                account_number: Some(1),
                anchor_number,
                origin: origin.clone(),
                last_used: None,
                name: Some("Alice".to_string()),
                seed_from_anchor: None
            },
            Account {
                account_number: Some(2),
                anchor_number,
                origin: origin.clone(),
                last_used: None,
                name: Some("Bob".to_string()),
                seed_from_anchor: None
            }
        ]
    );

    assert_eq!(
        update_account_for_origin(
            anchor_number,
            Some(1),
            origin.clone(),
            AccountUpdate {
                name: Some("Becky".to_string())
            }
        ),
        Ok(Account {
            account_number: Some(1),
            anchor_number,
            origin: origin.clone(),
            last_used: None,
            name: Some("Becky".to_string()),
            seed_from_anchor: None
        })
    );

    assert_eq!(
        get_accounts_for_origin(&anchor_number, &origin),
        vec![
            Account {
                account_number: None, // default account gets created when additional account gets created
                anchor_number,
                origin: origin.clone(),
                last_used: None,
                name: None,
                seed_from_anchor: None
            },
            Account {
                account_number: Some(1),
                anchor_number,
                origin: origin.clone(),
                last_used: None,
                name: Some("Becky".to_string()),
                seed_from_anchor: None
            },
            Account {
                account_number: Some(2),
                anchor_number,
                origin,
                last_used: None,
                name: Some("Bob".to_string()),
                seed_from_anchor: None
            }
        ]
    );
}

#[test]
fn should_update_default_account_for_origin() {
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
                name: None,
                seed_from_anchor: None
            },
            Account {
                account_number: Some(1),
                anchor_number,
                origin: origin.clone(),
                last_used: None,
                name: Some("Alice".to_string()),
                seed_from_anchor: None
            },
            Account {
                account_number: Some(2),
                anchor_number,
                origin: origin.clone(),
                last_used: None,
                name: Some("Bob".to_string()),
                seed_from_anchor: None
            }
        ]
    );

    assert_eq!(
        update_account_for_origin(
            anchor_number,
            None,
            origin.clone(),
            AccountUpdate {
                name: Some("Becky".to_string())
            }
        ),
        Ok(Account {
            account_number: Some(3),
            anchor_number,
            origin: origin.clone(),
            last_used: None,
            name: Some("Becky".to_string()),
            seed_from_anchor: Some(anchor_number)
        })
    );

    assert_eq!(
        get_accounts_for_origin(&anchor_number, &origin),
        vec![
            Account {
                account_number: Some(3),
                anchor_number,
                origin: origin.clone(),
                last_used: None,
                name: Some("Becky".to_string()),
                seed_from_anchor: Some(anchor_number)
            },
            Account {
                account_number: Some(1),
                anchor_number,
                origin: origin.clone(),
                last_used: None,
                name: Some("Alice".to_string()),
                seed_from_anchor: None
            },
            Account {
                account_number: Some(2),
                anchor_number,
                origin,
                last_used: None,
                name: Some("Bob".to_string()),
                seed_from_anchor: None
            }
        ]
    );
}
