use crate::{
    delegation::{
        add_delegation_signature, check_frontend_length, delegation_bookkeeping,
        der_encode_canister_sig_key,
    },
    ii_domain::IIDomain,
    state::{self, storage_borrow, storage_borrow_mut},
    storage::{
        account::{
            Account, AccountDelegationError, AccountsCounter, CreateAccountParams,
            PrepareAccountDelegation, ReadAccountParams, UpdateAccountParams,
        },
        Storage,
    },
    update_root_hash,
};
use ic_canister_sig_creation::{
    delegation_signature_msg, signature_map::CanisterSigInputs, DELEGATION_SIG_DOMAIN,
};
use ic_cdk::{api::time, caller};
use ic_stable_structures::DefaultMemoryImpl;
use internet_identity_interface::internet_identity::types::{
    AccountNumber, AccountUpdate, AnchorNumber, CheckMaxAccountError, CreateAccountError,
    Delegation, FrontendHostname, SessionKey, SignedDelegation, Timestamp, UpdateAccountError,
};
use serde_bytes::ByteBuf;

const MAX_ANCHOR_ACCOUNTS: usize = 500;

pub fn get_accounts_for_origin(
    anchor_number: AnchorNumber,
    origin: &FrontendHostname,
) -> Vec<Account> {
    storage_borrow(|storage| storage.list_accounts(anchor_number, origin))
}

pub fn create_account_for_origin(
    anchor_number: AnchorNumber,
    origin: FrontendHostname,
    name: String,
) -> Result<Account, CreateAccountError> {
    storage_borrow_mut(|storage| {
        check_or_rebuild_max_anchor_accounts(storage, anchor_number, true)
            .map_err(Into::<CreateAccountError>::into)?;

        storage
            .create_additional_account(CreateAccountParams {
                anchor_number,
                name,
                origin,
            })
            .map_err(|err| CreateAccountError::InternalCanisterError(format!("{err}")))
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
                if let Err(err) = check_or_rebuild_max_anchor_accounts(storage, anchor_number, true)
                {
                    return Err(err.into());
                }
            }

            storage
                .update_account(UpdateAccountParams {
                    account_number,
                    anchor_number,
                    name,
                    origin: origin.clone(),
                })
                .map_err(|err| UpdateAccountError::InternalCanisterError(format!("{}", err)))
        }),
        None => Err(UpdateAccountError::InternalCanisterError(
            "No name was provided.".to_string(),
        )),
    }
}

pub async fn prepare_account_delegation(
    anchor_number: AnchorNumber,
    origin: FrontendHostname,
    account_number: Option<AccountNumber>,
    session_key: SessionKey,
    max_ttl: Option<u64>,
    ii_domain: &Option<IIDomain>,
) -> Result<PrepareAccountDelegation, AccountDelegationError> {
    state::ensure_salt_set().await;
    check_frontend_length(&origin);

    let account = storage_borrow(|storage| {
        storage
            .read_account(ReadAccountParams {
                account_number,
                anchor_number,
                origin: &origin,
            })
            .ok_or(AccountDelegationError::Unauthorized(caller()))
    })?;

    let session_duration_ns = u64::min(
        max_ttl.unwrap_or(crate::delegation::DEFAULT_EXPIRATION_PERIOD_NS),
        crate::delegation::MAX_EXPIRATION_PERIOD_NS,
    );
    let expiration = time().saturating_add(session_duration_ns);
    let seed = account.calculate_seed();

    state::signature_map_mut(|sigs| {
        add_delegation_signature(sigs, session_key, seed.as_ref(), expiration);
    });
    update_root_hash();

    delegation_bookkeeping(origin, ii_domain.clone(), session_duration_ns);

    Ok(PrepareAccountDelegation {
        user_key: ByteBuf::from(der_encode_canister_sig_key(seed.to_vec())),
        expiration,
    })
}

pub fn get_account_delegation(
    anchor_number: AnchorNumber,
    origin: &FrontendHostname,
    account_number: Option<AccountNumber>,
    session_key: SessionKey,
    expiration: Timestamp,
) -> Result<SignedDelegation, AccountDelegationError> {
    check_frontend_length(origin);

    storage_borrow(|storage| {
        let account = storage
            .read_account(ReadAccountParams {
                account_number,
                anchor_number,
                origin,
            })
            .ok_or(AccountDelegationError::Unauthorized(caller()))?;

        state::assets_and_signatures(|certified_assets, sigs| {
            let inputs = CanisterSigInputs {
                domain: DELEGATION_SIG_DOMAIN,
                seed: &account.calculate_seed(),
                message: &delegation_signature_msg(&session_key, expiration, None),
            };
            match sigs.get_signature_as_cbor(&inputs, Some(certified_assets.root_hash())) {
                Ok(signature) => Ok(SignedDelegation {
                    delegation: Delegation {
                        pubkey: session_key,
                        expiration,
                        targets: None,
                    },
                    signature: ByteBuf::from(signature),
                }),
                Err(_) => Err(AccountDelegationError::NoSuchDelegation),
            }
        })
    })
}

/// Checks whether the stored number of accounts as per the counter exceeds the maximum permitted number.
/// If it does, it rebuilds the counter. If it still exceeds, it will return an error.
fn check_or_rebuild_max_anchor_accounts(
    storage: &mut Storage<DefaultMemoryImpl>,
    anchor_number: AnchorNumber,
    first_time: bool, // required for safe recursion
) -> Result<(), CheckMaxAccountError> {
    let AccountsCounter {
        stored_accounts,
        stored_account_references: _,
    } = storage.get_account_counter(anchor_number);

    if stored_accounts >= MAX_ANCHOR_ACCOUNTS as u64 {
        // check whether we actually have reached the number
        if first_time {
            storage.rebuild_identity_account_counters(anchor_number);
            return check_or_rebuild_max_anchor_accounts(storage, anchor_number, false);
        } else {
            return Err(CheckMaxAccountError::AccountLimitReached);
        }
    }
    Ok(())
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
        Ok(Account::new_full(
            anchor.anchor_number(),
            origin,
            Some(name),
            Some(1),
            None,
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
        } else {
            assert!(result.is_ok())
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
        let create_result =
            create_account_for_origin(anchor.anchor_number(), origin.clone(), name.clone());

        assert!(create_result.is_ok())
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
            Account::new(anchor_number, origin.clone(), None, None),
            Account::new_full(
                anchor_number,
                origin.clone(),
                Some("Alice".to_string()),
                Some(1),
                None,
                None
            ),
            Account::new_full(
                anchor_number,
                origin.clone(),
                Some("Bob".to_string()),
                Some(2),
                None,
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
            Account::new(anchor_number, origin.clone(), None, None),
            Account::new_full(
                anchor_number,
                origin.clone(),
                Some("Alice".to_string()),
                Some(1),
                None,
                None
            ),
        ]
    );

    assert_eq!(
        get_accounts_for_origin(anchor_number_two, &origin),
        vec![
            Account::new(anchor_number_two, origin.clone(), None, None),
            Account::new_full(
                anchor_number_two,
                origin.clone(),
                Some("Bob".to_string()),
                Some(2),
                None,
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
            Account::new(anchor_number, origin.clone(), None, None),
            Account::new_full(
                anchor_number,
                origin.clone(),
                Some("Alice".to_string()),
                Some(1),
                None,
                None
            ),
            Account::new_full(
                anchor_number,
                origin.clone(),
                Some("Bob".to_string()),
                Some(2),
                None,
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
        Ok(Account::new_full(
            anchor_number,
            origin.clone(),
            Some("Becky".to_string()),
            Some(1),
            None,
            None
        ))
    );

    assert_eq!(
        get_accounts_for_origin(anchor_number, &origin),
        vec![
            Account::new(anchor_number, origin.clone(), None, None),
            Account::new_full(
                anchor_number,
                origin.clone(),
                Some("Becky".to_string()),
                Some(1),
                None,
                None
            ),
            Account::new_full(
                anchor_number,
                origin.clone(),
                Some("Bob".to_string()),
                Some(2),
                None,
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
            Account::new(anchor_number, origin.clone(), None, None),
            Account::new_full(
                anchor_number,
                origin.clone(),
                Some("Alice".to_string()),
                Some(1),
                None,
                None
            ),
            Account::new_full(
                anchor_number,
                origin.clone(),
                Some("Bob".to_string()),
                Some(2),
                None,
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
        Ok(Account::new_full(
            anchor_number,
            origin.clone(),
            Some("Becky".to_string()),
            Some(3),
            None,
            Some(anchor_number)
        ))
    );

    assert_eq!(
        get_accounts_for_origin(anchor_number, &origin),
        vec![
            Account::new_full(
                anchor_number,
                origin.clone(),
                Some("Becky".to_string()),
                Some(3),
                None,
                Some(anchor_number)
            ),
            Account::new_full(
                anchor_number,
                origin.clone(),
                Some("Alice".to_string()),
                Some(1),
                None,
                None
            ),
            Account::new_full(
                anchor_number,
                origin.clone(),
                Some("Bob".to_string()),
                Some(2),
                None,
                None
            ),
        ]
    );
}
