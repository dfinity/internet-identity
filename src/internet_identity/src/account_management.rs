use crate::{
    delegation::{
        self, add_delegation_signature, check_frontend_length, delegation_bookkeeping,
        der_encode_canister_sig_key,
    },
    ii_domain::IIDomain,
    state::{self, storage_borrow, storage_borrow_mut},
    storage::{
        account::{
            Account, AccountDelegationError, AccountReference, AccountsCounter,
            CreateAccountParams, ReadAccountParams, UpdateAccountParams,
        },
        StorageError,
    },
    update_root_hash,
};
use ic_canister_sig_creation::hash_bytes;
use ic_cdk::{api::time, caller};
use ic_certification::Hash;
use internet_identity_interface::internet_identity::types::{
    AccountNumber, AccountUpdate, AnchorNumber, CreateAccountError, FrontendHostname, SessionKey,
    Timestamp, UpdateAccountError, UserKey,
};
use serde_bytes::ByteBuf;

const MAX_ANCHOR_ACCOUNTS: usize = 500;

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
    anchor_number: AnchorNumber,
    origin: &FrontendHostname,
) -> Vec<Account> {
    storage_borrow(|storage| {
        storage
            .list_accounts(anchor_number, origin)
            .iter()
            .filter_map(|acc_ref| {
                storage.read_account(ReadAccountParams {
                    account_number: acc_ref.account_number,
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
            return Err(CreateAccountError::AccountLimitReached);
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

pub fn update_account_for_origin(
    anchor_number: AnchorNumber,
    account_number: Option<AccountNumber>,
    origin: FrontendHostname,
    update: AccountUpdate,
) -> Result<Account, UpdateAccountError> {
    match update.name {
        Some(name) => storage_borrow_mut(|storage| {
            // If the account to be updated is a default account
            // Check if whe have reached account limit
            // Because editing a default account turns it into a stored account
            if account_number.is_none() {
                let AccountsCounter {
                    stored_accounts,
                    stored_account_references: _,
                } = storage.get_account_counter(anchor_number);

                if stored_accounts >= MAX_ANCHOR_ACCOUNTS as u64 {
                    return Err(UpdateAccountError::AccountLimitReached);
                }
            }

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
                            account_number: Some(acc_num),
                            anchor_number,
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

/// Create `Hash` used for a delegation that can make calls on behalf of an `Account`.
/// If the `Account` is a non-stored default account or has a `seed_from_anchor` (and thus is a stored default account),
/// the respective anchor number will be used as a seed input. Otherwise, the `AccountNumber` is used.
///
/// # Arguments
///
/// * `account` is the `Account` we're using for this delegation
fn calculate_account_delegation_seed(account: Account) -> Hash {
    let salt = state::salt();
    let mut blob: Vec<u8> = vec![];
    blob.push(salt.len() as u8);
    blob.extend_from_slice(&salt);

    // If this is a non-stored default account, we derive from frontend and anchor
    if account.account_number.is_none() {
        return delegation::calculate_seed(account.anchor_number, &account.origin);
    }

    match account.get_seed_anchor() {
        Some(seed_from_anchor) => {
            // If this is a stored default account, we derive from frontend and anchor
            delegation::calculate_seed(seed_from_anchor, &account.origin)
        }
        None => {
            // If this is an added account, we derive from the account number.
            let salt = state::salt();

            let mut blob: Vec<u8> = vec![];
            blob.push(salt.len() as u8);
            blob.extend_from_slice(&salt);

            let account_number_str = account.account_number.unwrap().to_string(); // XXX: this should be safe because an account without a seed_from_anchor must always have an account_number
            let account_number_blob = account_number_str.bytes();
            blob.push(account_number_blob.len() as u8);
            blob.extend(account_number_blob);

            hash_bytes(blob)
        }
    }
}

pub async fn prepare_account_delegation(
    anchor_number: AnchorNumber,
    origin: FrontendHostname,
    account_number: Option<AccountNumber>,
    session_key: SessionKey,
    max_ttl: Option<u64>,
    ii_domain: &Option<IIDomain>,
) -> Result<(UserKey, Timestamp), AccountDelegationError> {
    // If the anchor doesn't own this account, we return unauthorized.
    if anchor_has_account(anchor_number, &origin, account_number).is_none() {
        return Err(AccountDelegationError::Unauthorized(caller()));
    }
    state::ensure_salt_set().await;
    check_frontend_length(&origin);

    let session_duration_ns = u64::min(
        max_ttl.unwrap_or(crate::delegation::DEFAULT_EXPIRATION_PERIOD_NS),
        crate::delegation::MAX_EXPIRATION_PERIOD_NS,
    );
    let expiration = time().saturating_add(session_duration_ns);

    let account = storage_borrow(|storage| {
        storage.read_account(ReadAccountParams {
            account_number,
            anchor_number,
            origin: &origin,
        })
    })
    .ok_or(AccountDelegationError::InternalCanisterError(
        "Could not retrieve account".to_string(),
    ))?;

    let seed = calculate_account_delegation_seed(account);

    state::signature_map_mut(|sigs| {
        add_delegation_signature(sigs, session_key, seed.as_ref(), expiration);
    });
    update_root_hash();

    delegation_bookkeeping(origin, ii_domain.clone(), session_duration_ns);

    Ok((
        ByteBuf::from(der_encode_canister_sig_key(seed.to_vec())),
        expiration,
    ))
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
        Ok(Account::new_with_seed_anchor(
            anchor.anchor_number(),
            origin,
            Some(name),
            Some(1),
            None
        ))
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
            assert_eq!(result, Err(CreateAccountError::AccountLimitReached))
        }
    }
}

#[test]
fn should_fail_to_update_default_accounts_above_max() {
    use crate::state::{storage_borrow_mut, storage_replace};
    use crate::storage::Storage;
    use ic_stable_structures::VectorMemory;

    storage_replace(Storage::new((0, 10000), VectorMemory::default()));
    let anchor = storage_borrow_mut(|storage| storage.allocate_anchor().unwrap());
    let name = "Alice".to_string();
    for i in 0..MAX_ANCHOR_ACCOUNTS {
        let origin = format!("https://example-{}.com", i);
        let _ = create_account_for_origin(anchor.anchor_number(), origin.clone(), name.clone());
    }
    let result = update_account_for_origin(
        anchor.anchor_number(),
        None,
        "https://example-1.com".to_string(),
        AccountUpdate {
            name: Some("Gabriel".to_string()),
        },
    );
    assert_eq!(result, Err(UpdateAccountError::AccountLimitReached))
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
        get_accounts_for_origin(anchor_number, &origin),
        vec![
            Account::new_with_seed_anchor(anchor_number, origin.clone(), None, None, None),
            Account::new_with_seed_anchor(
                anchor_number,
                origin.clone(),
                Some("Alice".to_string()),
                Some(1),
                None
            ),
            Account::new_with_seed_anchor(
                anchor_number,
                origin.clone(),
                Some("Bob".to_string()),
                Some(2),
                None
            ),
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
        get_accounts_for_origin(anchor_number, &origin),
        vec![
            Account::new_with_seed_anchor(anchor_number, origin.clone(), None, None, None),
            Account::new_with_seed_anchor(
                anchor_number,
                origin.clone(),
                Some("Alice".to_string()),
                Some(1),
                None
            ),
        ]
    );

    assert_eq!(
        get_accounts_for_origin(anchor_number_two, &origin),
        vec![
            Account::new_with_seed_anchor(anchor_number_two, origin.clone(), None, None, None),
            Account::new_with_seed_anchor(
                anchor_number_two,
                origin.clone(),
                Some("Bob".to_string()),
                Some(2),
                None
            ),
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
        get_accounts_for_origin(anchor_number, &origin),
        vec![
            Account::new_with_seed_anchor(anchor_number, origin.clone(), None, None, None),
            Account::new_with_seed_anchor(
                anchor_number,
                origin.clone(),
                Some("Alice".to_string()),
                Some(1),
                None
            ),
            Account::new_with_seed_anchor(
                anchor_number,
                origin.clone(),
                Some("Bob".to_string()),
                Some(2),
                None
            ),
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
        Ok(Account::new_with_seed_anchor(
            anchor_number,
            origin.clone(),
            Some("Becky".to_string()),
            Some(1),
            None
        ))
    );

    assert_eq!(
        get_accounts_for_origin(anchor_number, &origin),
        vec![
            Account::new_with_seed_anchor(anchor_number, origin.clone(), None, None, None),
            Account::new_with_seed_anchor(
                anchor_number,
                origin.clone(),
                Some("Becky".to_string()),
                Some(1),
                None
            ),
            Account::new_with_seed_anchor(
                anchor_number,
                origin.clone(),
                Some("Bob".to_string()),
                Some(2),
                None
            ),
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
        get_accounts_for_origin(anchor_number, &origin),
        vec![
            Account::new_with_seed_anchor(anchor_number, origin.clone(), None, None, None),
            Account::new_with_seed_anchor(
                anchor_number,
                origin.clone(),
                Some("Alice".to_string()),
                Some(1),
                None
            ),
            Account::new_with_seed_anchor(
                anchor_number,
                origin.clone(),
                Some("Bob".to_string()),
                Some(2),
                None
            ),
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
        Ok(Account::new_with_seed_anchor(
            anchor_number,
            origin.clone(),
            Some("Becky".to_string()),
            Some(3),
            Some(anchor_number)
        ))
    );

    assert_eq!(
        get_accounts_for_origin(anchor_number, &origin),
        vec![
            Account::new_with_seed_anchor(
                anchor_number,
                origin.clone(),
                Some("Becky".to_string()),
                Some(3),
                Some(anchor_number)
            ),
            Account::new_with_seed_anchor(
                anchor_number,
                origin.clone(),
                Some("Alice".to_string()),
                Some(1),
                None
            ),
            Account::new_with_seed_anchor(
                anchor_number,
                origin.clone(),
                Some("Bob".to_string()),
                Some(2),
                None
            ),
        ]
    );
}
