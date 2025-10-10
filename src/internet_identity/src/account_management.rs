#[cfg(not(test))]
use crate::anchor_management::post_operation_bookkeeping;
use crate::{
    delegation::{
        add_delegation_signature, check_frontend_length, delegation_bookkeeping,
        der_encode_canister_sig_key,
    },
    ii_domain::IIDomain,
    state::{self, storage_borrow, storage_borrow_mut},
    storage::{
        account::{
            validate_account_name, Account, AccountDelegationError, AccountsCounter,
            CreateAccountParams, PrepareAccountDelegation, ReadAccountParams, UpdateAccountParams,
        },
        storable::anchor_application_config::AnchorApplicationConfig,
        Storage,
    },
    update_root_hash,
};
use ic_canister_sig_creation::{
    delegation_signature_msg, signature_map::CanisterSigInputs, DELEGATION_SIG_DOMAIN,
};
use ic_cdk::{api::time, caller};
use ic_stable_structures::DefaultMemoryImpl;
use internet_identity_interface::{
    archive::types::{Operation, Private},
    internet_identity::types::{
        AccountInfo, AccountNumber, AccountUpdate, AnchorNumber, ApplicationNumber,
        CheckMaxAccountError, CreateAccountError, Delegation, FrontendHostname,
        GetDefaultAccountError, SessionKey, SetDefaultAccountError, SignedDelegation, Timestamp,
        UpdateAccountError,
    },
};
#[cfg(test)]
use pretty_assertions::assert_eq;
use serde_bytes::ByteBuf;

const MAX_ANCHOR_ACCOUNTS: usize = 500;

pub fn get_accounts_for_origin(
    anchor_number: AnchorNumber,
    origin: &FrontendHostname,
) -> Vec<Account> {
    storage_borrow(|storage| storage.list_accounts(anchor_number, origin))
}

/// Helper function to read an account by application number, unlike `storage.read_account` this
/// returns `Result` instead of `Option`.
///
/// This function is applicable only to numbered accounts; synthetic accounts are not currently
/// stored in the memory and thus cannot be fetched from the storage.
fn try_read_account_info(
    anchor_number: AnchorNumber,
    origin: FrontendHostname,
    application_number: ApplicationNumber,
    account_number: AccountNumber,
) -> Result<AccountInfo, String> {
    let Some(account) = storage_borrow(|storage| {
        storage.read_account(ReadAccountParams {
            account_number: Some(account_number),
            anchor_number,
            origin: &origin,
            known_app_num: Some(application_number),
        })
    }) else {
        let message = format!(
            "Account #{} does not exist for anchor {} and origin {}.",
            account_number, anchor_number, origin
        );

        return Err(message);
    };

    Ok(account.to_info())
}

/// Best effort to determine the default account for the given (anchor, origin).
/// - If the application is not found, returns a "synthetic" account.
/// - If no default account stored, returns a "synthetic" account.
/// - Else, read from storage, returning an Err result if not found.
///
/// An Err case indicates internal inconsistency in the canister state.
pub fn get_default_account_for_origin(
    anchor_number: AnchorNumber,
    origin: FrontendHostname,
) -> Result<AccountInfo, GetDefaultAccountError> {
    let Some(application_number) =
        storage_borrow(|storage| storage.lookup_application_number_with_origin(&origin))
    else {
        return Ok(Account::synthetic(anchor_number, origin).to_info());
    };

    let AnchorApplicationConfig {
        default_account_number,
    } = storage_borrow(|storage| {
        storage.lookup_anchor_application_config(anchor_number, application_number)
    });

    let Some(default_account_number) = default_account_number else {
        return Ok(Account::synthetic(anchor_number, origin).to_info());
    };

    let account = try_read_account_info(
        anchor_number,
        origin,
        application_number,
        default_account_number,
    )
    .map_err(GetDefaultAccountError::InternalCanisterError)?;

    Ok(account)
}

/// Sets the default account for the given (anchor, origin) to the specified `account_number`.
///
/// If the `account_number` is `None`, then the synthetic account is returned, and also `None`
/// is stored (just so that the previous default account is not retained).
///
/// If this is the first time an origin is seen for the anchor, a new application number is created.
pub fn set_default_account_for_origin(
    anchor_number: AnchorNumber,
    origin: FrontendHostname,
    account_number: Option<AccountNumber>,
) -> Result<AccountInfo, SetDefaultAccountError> {
    let application_number = storage_borrow_mut(|storage| {
        storage.lookup_or_insert_application_number_with_origin(&origin)
    });

    let account = if let Some(account_number) = account_number {
        try_read_account_info(
            anchor_number,
            origin.clone(),
            application_number,
            account_number,
        )
        .map_err(|_| SetDefaultAccountError::NoSuchAccount {
            anchor_number,
            origin,
        })?
    } else {
        Account::synthetic(anchor_number, origin).to_info()
    };

    let config = AnchorApplicationConfig {
        default_account_number: account_number,
    };

    storage_borrow_mut(|storage| {
        storage.set_anchor_application_config(anchor_number, application_number, config);
    });

    Ok(account)
}

pub fn create_account_for_origin(
    anchor_number: AnchorNumber,
    origin: FrontendHostname,
    name: String,
) -> Result<Account, CreateAccountError> {
    validate_account_name(&name).map_err(Into::<CreateAccountError>::into)?;
    let created_account = storage_borrow_mut(|storage| {
        check_or_rebuild_max_anchor_accounts(
            storage,
            anchor_number,
            MAX_ANCHOR_ACCOUNTS as u64,
            true,
        )
        .map_err(Into::<CreateAccountError>::into)?;

        storage
            .create_additional_account(CreateAccountParams {
                anchor_number,
                name: name.clone(),
                origin,
            })
            .map_err(|err| CreateAccountError::InternalCanisterError(format!("{err}")))
    })?;

    post_account_operation_bookkeeping(
        anchor_number,
        Operation::CreateAccount {
            name: Private::Redacted,
        },
    );

    Ok(created_account)
}

pub fn update_account_for_origin(
    anchor_number: AnchorNumber,
    account_number: Option<AccountNumber>,
    origin: FrontendHostname,
    update: AccountUpdate,
) -> Result<Account, UpdateAccountError> {
    match update.name {
        Some(new_name) => {
            validate_account_name(&new_name).map_err(Into::<UpdateAccountError>::into)?;
            let (updated_account, old_account_name) =
                // Type annotation was necessary for the compiler to infer the correct type
                storage_borrow_mut(|storage| -> Result<(Account, Option<String>), UpdateAccountError> {
                    // If the account to be updated is a default account
                    // Check if we have reached account limit
                    // Because editing a default account turns it into a stored account
                    if account_number.is_none() {
                        check_or_rebuild_max_anchor_accounts(
                            storage,
                            anchor_number,
                            MAX_ANCHOR_ACCOUNTS as u64,
                            true,
                        )
                        .map_err(Into::<UpdateAccountError>::into)?
                    }

                    let old_account = storage
                        .read_account(ReadAccountParams {
                            account_number,
                            anchor_number,
                            origin: &origin,
                            known_app_num: None
                        })
                        .expect("Updating an unreadable account should be impossible!");

                    let updated_account = storage
                        .update_account(UpdateAccountParams {
                            account_number,
                            anchor_number,
                            name: new_name.clone(),
                            origin: origin.clone(),
                        })
                        .map_err(|err| UpdateAccountError::InternalCanisterError(err.to_string()))?;

                    Ok((updated_account, old_account.name))
                })?;

            // No account number meant that the account was a default account and was created before being updated.
            if account_number.is_none() {
                post_account_operation_bookkeeping(
                    anchor_number,
                    Operation::CreateAccount {
                        name: Private::Redacted,
                    },
                );
            }

            let name = if updated_account.name == old_account_name {
                None
            } else {
                Some(Private::Redacted)
            };
            post_account_operation_bookkeeping(anchor_number, Operation::UpdateAccount { name });

            Ok(updated_account)
        }
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
                known_app_num: None,
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
                known_app_num: None,
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
    max_anchor_accounts: u64,
    first_time: bool, // required for safe recursion
) -> Result<(), CheckMaxAccountError> {
    let AccountsCounter {
        stored_accounts,
        stored_account_references: _,
    } = storage.get_account_counter(anchor_number);

    if stored_accounts >= max_anchor_accounts {
        // check whether we actually have reached the number
        if first_time {
            storage.rebuild_identity_account_counters(anchor_number);
            return check_or_rebuild_max_anchor_accounts(
                storage,
                anchor_number,
                max_anchor_accounts,
                false,
            );
        } else {
            return Err(CheckMaxAccountError::AccountLimitReached);
        }
    }
    Ok(())
}

#[cfg(not(test))]
#[allow(dead_code)]
fn post_account_operation_bookkeeping(anchor_number: AnchorNumber, operation: Operation) {
    post_operation_bookkeeping(anchor_number, operation)
}

// Bookkeeping fails outside of canisters, so we work around it for the unit tests.
#[cfg(test)]
fn post_account_operation_bookkeeping(_anchor_number: AnchorNumber, _operation: Operation) {}

#[test]
fn should_create_account_for_origin() {
    use crate::state::{storage_borrow_mut, storage_replace};
    use crate::storage::Storage;
    use ic_stable_structures::VectorMemory;

    storage_replace(Storage::new((0, 10000), VectorMemory::default()));
    let anchor = storage_borrow_mut(|storage| storage.allocate_anchor(0).unwrap());
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
    let anchor = storage_borrow_mut(|storage| storage.allocate_anchor(0).unwrap());
    let name = "Alice".to_string();
    for i in 0..=MAX_ANCHOR_ACCOUNTS {
        let origin = format!("https://example-{i}.com");
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
    let anchor = storage_borrow_mut(|storage| storage.allocate_anchor(0).unwrap());
    let name = "Alice".to_string();
    for i in 0..MAX_ANCHOR_ACCOUNTS {
        let origin = format!("https://example-{i}.com");
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
    let anchor = storage_borrow_mut(|storage| storage.allocate_anchor(0).unwrap());
    let origin = "https://example.com".to_string();
    let name = "Alice".to_string();
    let name_two = "Bob".to_string();
    let anchor_number = anchor.anchor_number();

    let _ = create_account_for_origin(anchor_number, origin.clone(), name.clone());
    let _ = create_account_for_origin(anchor_number, origin.clone(), name_two.clone());

    assert_eq!(
        get_accounts_for_origin(anchor_number, &origin),
        vec![
            Account::synthetic(anchor_number, origin.clone()),
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
    let anchor = storage_borrow_mut(|storage| storage.allocate_anchor(0).unwrap());
    let anchor_two = storage_borrow_mut(|storage| storage.allocate_anchor(0).unwrap());
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
            Account::synthetic(anchor_number, origin.clone()),
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
            Account::synthetic(anchor_number_two, origin.clone()),
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
    let anchor = storage_borrow_mut(|storage| storage.allocate_anchor(0).unwrap());
    let origin = "https://example.com".to_string();
    let name = "Alice".to_string();
    let name_two = "Bob".to_string();
    let anchor_number = anchor.anchor_number();

    let _ = create_account_for_origin(anchor_number, origin.clone(), name.clone());
    let _ = create_account_for_origin(anchor_number, origin.clone(), name_two.clone());

    assert_eq!(
        get_accounts_for_origin(anchor_number, &origin),
        vec![
            Account::synthetic(anchor_number, origin.clone()),
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
            Account::synthetic(anchor_number, origin.clone()),
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
    let anchor = storage_borrow_mut(|storage| storage.allocate_anchor(0).unwrap());
    let origin = "https://example.com".to_string();
    let name = "Alice".to_string();
    let name_two = "Bob".to_string();
    let anchor_number = anchor.anchor_number();

    let _ = create_account_for_origin(anchor_number, origin.clone(), name.clone());
    let _ = create_account_for_origin(anchor_number, origin.clone(), name_two.clone());

    assert_eq!(
        get_accounts_for_origin(anchor_number, &origin),
        vec![
            Account::synthetic(anchor_number, origin.clone()),
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

#[test]
// This test is to make sure that the check_or_rebuild_max_anchor_accounts function correctly errors
// It should error when the counters are at or above max and argument 'first_time' is false
fn should_fail_check_or_rebuild_when_not_first_time() {
    use crate::state::{storage_borrow_mut, storage_replace};
    use crate::storage::Storage;
    use ic_stable_structures::VectorMemory;

    storage_replace(Storage::new((0, 10000), VectorMemory::default()));
    let anchor = storage_borrow_mut(|storage| storage.allocate_anchor(0).unwrap());

    // create faulty counter entries
    storage_borrow_mut(|storage| {
        storage.set_counters_for_testing(
            anchor.anchor_number(),
            MAX_ANCHOR_ACCOUNTS as u64,
            MAX_ANCHOR_ACCOUNTS as u64,
        );
        let res = check_or_rebuild_max_anchor_accounts(
            storage,
            anchor.anchor_number(),
            MAX_ANCHOR_ACCOUNTS as u64,
            false,
        );
        assert!(res.is_err())
    });
}

#[test]
fn should_properly_recalculate_faulty_account_counter() {
    use crate::state::{storage_borrow_mut, storage_replace};
    use crate::storage::Storage;
    use ic_stable_structures::VectorMemory;

    storage_replace(Storage::new((0, 10000), VectorMemory::default()));
    let anchor = storage_borrow_mut(|storage| storage.allocate_anchor(0).unwrap());
    let name = "Alice".to_string();

    // create faulty counter entries
    storage_borrow_mut(|storage| {
        storage.set_counters_for_testing(
            anchor.anchor_number(),
            MAX_ANCHOR_ACCOUNTS as u64,
            MAX_ANCHOR_ACCOUNTS as u64,
        )
    });

    for i in 0..=MAX_ANCHOR_ACCOUNTS {
        let origin = format!("https://example-{i}.com");
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
fn should_properly_recalculate_faulty_account_counter_when_updating() {
    use crate::state::{storage_borrow_mut, storage_replace};
    use crate::storage::Storage;
    use ic_stable_structures::VectorMemory;

    storage_replace(Storage::new((0, 10000), VectorMemory::default()));
    let anchor = storage_borrow_mut(|storage| storage.allocate_anchor(0).unwrap());

    // create faulty counter entries
    storage_borrow_mut(|storage| {
        storage.set_counters_for_testing(
            anchor.anchor_number(),
            MAX_ANCHOR_ACCOUNTS as u64,
            MAX_ANCHOR_ACCOUNTS as u64,
        )
    });

    let result = update_account_for_origin(
        anchor.anchor_number(),
        None,
        "https://example-1.com".to_string(),
        AccountUpdate {
            name: Some("Gabriel".to_string()),
        },
    );
    assert!(result.is_ok())
}

#[test]
fn should_increment_discrepancy_counter() {
    use crate::state::{storage_borrow_mut, storage_replace};
    use crate::storage::Storage;
    use ic_stable_structures::VectorMemory;

    storage_replace(Storage::new((0, 10000), VectorMemory::default()));
    let anchor = storage_borrow_mut(|storage| storage.allocate_anchor(0).unwrap());

    // create faulty counter entries
    storage_borrow_mut(|storage| {
        storage.set_counters_for_testing(
            anchor.anchor_number(),
            MAX_ANCHOR_ACCOUNTS as u64,
            MAX_ANCHOR_ACCOUNTS as u64,
        )
    });

    storage_borrow(|storage| {
        let discrepancy_counter_before = storage.get_discrepancy_counter();
        assert_eq!(discrepancy_counter_before.account_counter_rebuilds, 0);
    });

    let result = update_account_for_origin(
        anchor.anchor_number(),
        None,
        "https://example-1.com".to_string(),
        AccountUpdate {
            name: Some("Gabriel".to_string()),
        },
    );
    assert!(result.is_ok());

    storage_borrow(|storage| {
        let discrepancy_counter_after = storage.get_discrepancy_counter();
        assert_eq!(discrepancy_counter_after.account_counter_rebuilds, 1);
    });
}

#[test]
fn should_get_default_account_for_origin() {
    use crate::state::{storage_borrow_mut, storage_replace};
    use crate::storage::Storage;
    use ic_stable_structures::VectorMemory;

    storage_replace(Storage::new((0, 10000), VectorMemory::default()));
    let anchor = storage_borrow_mut(|storage| storage.allocate_anchor(0).unwrap());
    let origin = "https://example.com".to_string();
    let anchor_number = anchor.anchor_number();

    create_account_for_origin(anchor_number, origin.clone(), "Alice".to_string()).unwrap();
    create_account_for_origin(anchor_number, origin.clone(), "Bob".to_string()).unwrap();

    // Smoke test
    assert_eq!(
        get_accounts_for_origin(anchor_number, &origin),
        vec![
            Account::synthetic(anchor_number, origin.clone()),
            Account::new(
                anchor_number,
                origin.clone(),
                Some("Alice".to_string()),
                Some(1)
            ),
            Account::new(
                anchor_number,
                origin.clone(),
                Some("Bob".to_string()),
                Some(2)
            ),
        ]
    );

    let test_cases = [
        (
            "GET without ever calling SET (getter works from the start)",
            None,
            Ok(AccountInfo {
                account_number: None,
                origin: origin.clone(),
                last_used: None,
                name: None,
            }),
        ),
        (
            "SET to Some(1) (Alice), then GET (default can be customized)",
            Some((
                Some(1),
                Ok(AccountInfo {
                    account_number: Some(1),
                    origin: origin.clone(),
                    last_used: None,
                    name: Some("Alice".to_string()),
                }),
            )),
            Ok(AccountInfo {
                account_number: Some(1),
                origin: origin.clone(),
                last_used: None,
                name: Some("Alice".to_string()),
            }),
        ),
        (
            "SET to Some(0) (no such account), then GET (error is returned by SET, GET returns what was there before)",
            Some((
                Some(0),
                Err(SetDefaultAccountError::NoSuchAccount {
                    anchor_number,
                    origin: origin.clone(),
                }),
            )),
            Ok(AccountInfo {
                account_number: Some(1),
                origin: origin.clone(),
                last_used: None,
                name: Some("Alice".to_string()),
            }),
        ),
        (
            "SET to Some(42) (non-existent), then GET (error is returned by SET, GET returns what was there before)",
            Some((
                Some(42),
                Err(SetDefaultAccountError::NoSuchAccount {
                    anchor_number,
                    origin: origin.clone(),
                }),
            )),
            Ok(AccountInfo {
                account_number: Some(1),
                origin: origin.clone(),
                last_used: None,
                name: Some("Alice".to_string()),
            }),
        ),
        (
            "SET to Some(2) (Bob), then GET (default can be changed a second time)",
            Some((
                Some(2),
                Ok(AccountInfo {
                    account_number: Some(2),
                    origin: origin.clone(),
                    last_used: None,
                    name: Some("Bob".to_string()),
                }),
            )),
            Ok(AccountInfo {
                account_number: Some(2),
                origin: origin.clone(),
                last_used: None,
                name: Some("Bob".to_string()),
            }),
        ),
        (
            "SET to None (reset to default), then GET (can set default to None again)",
            Some((
                None,
                Ok(AccountInfo {
                    account_number: None,
                    origin: origin.clone(),
                    last_used: None,
                    name: None,
                }),
            )),
            Ok(AccountInfo {
                account_number: None,
                origin: origin.clone(),
                last_used: None,
                name: None,
            }),
        ),
    ];

    // Run code under test
    for (label, default_account_number_and_expected_set_result, expected_get_result) in test_cases {
        if let Some((default_account_number, expected_set_result)) =
            default_account_number_and_expected_set_result
        {
            let result = set_default_account_for_origin(
                anchor_number,
                origin.clone(),
                default_account_number,
            );

            assert_eq!(
                result, expected_set_result,
                "{label}: unexpected SET result"
            );
        }

        let result = get_default_account_for_origin(anchor_number, origin.clone());

        assert_eq!(
            result, expected_get_result,
            "{label}: unexpected GET result"
        );
    }
}

#[test]
fn can_get_default_before_update_account_for_origin() {
    use crate::state::{storage_borrow_mut, storage_replace};
    use crate::storage::Storage;
    use ic_stable_structures::VectorMemory;

    storage_replace(Storage::new((0, 10000), VectorMemory::default()));
    let anchor = storage_borrow_mut(|storage| storage.allocate_anchor(0).unwrap());
    let origin = "https://example.com".to_string();
    let anchor_number = anchor.anchor_number();

    // Run code under test
    let result = get_default_account_for_origin(anchor_number, origin.clone());

    assert_eq!(
        result,
        Ok(Account::synthetic(anchor_number, origin.clone()).to_info())
    );
}

#[test]
fn should_get_updated_default_account_after_modification() {
    use crate::state::{storage_borrow_mut, storage_replace};
    use crate::storage::Storage;
    use ic_stable_structures::VectorMemory;

    storage_replace(Storage::new((0, 10000), VectorMemory::default()));
    let anchor = storage_borrow_mut(|storage| storage.allocate_anchor(0).unwrap());
    let origin = "https://example.com".to_string();
    let anchor_number = anchor.anchor_number();

    // Update the default account (this should give it a name and account number)
    update_account_for_origin(
        anchor_number,
        None, // None means default account
        origin.clone(),
        AccountUpdate {
            name: Some("Default Account".to_string()),
        },
    )
    .unwrap();

    // Run code under test
    let result = get_default_account_for_origin(anchor_number, origin.clone());

    assert_eq!(
        result,
        Ok(AccountInfo {
            account_number: Some(1),
            origin: origin.clone(),
            last_used: None,
            name: Some("Default Account".to_string()),
        })
    );
}

#[test]
fn should_succeed_get_default_account_for_nonexistent_anchor() {
    use crate::state::storage_replace;
    use crate::storage::Storage;
    use ic_stable_structures::VectorMemory;

    storage_replace(Storage::new((0, 10000), VectorMemory::default()));
    let nonexistent_anchor = 99999;
    let origin = "https://example.com".to_string();

    // Run code under test
    let result = get_default_account_for_origin(nonexistent_anchor, origin);

    assert_eq!(
        result,
        Ok(AccountInfo {
            account_number: None,
            origin: "https://example.com".to_string(),
            last_used: None,
            name: None,
        })
    );
}

#[test]
fn should_get_default_account_for_different_origins() {
    use crate::state::{storage_borrow_mut, storage_replace};
    use crate::storage::Storage;
    use ic_stable_structures::VectorMemory;

    storage_replace(Storage::new((0, 10000), VectorMemory::default()));
    let anchor = storage_borrow_mut(|storage| storage.allocate_anchor(0).unwrap());
    let origin1 = "https://app1.com".to_string();
    let origin2 = "https://app2.com".to_string();
    let anchor_number = anchor.anchor_number();

    // Create accounts for both origins
    create_account_for_origin(anchor_number, origin1.clone(), "Alice".to_string()).unwrap();
    create_account_for_origin(anchor_number, origin2.clone(), "Bob".to_string()).unwrap();

    // Run code under test
    let result1 = get_default_account_for_origin(anchor_number, origin1.clone());
    let result2 = get_default_account_for_origin(anchor_number, origin2.clone());

    assert_eq!(
        result1,
        Ok(AccountInfo {
            account_number: None,
            origin: origin1.clone(),
            last_used: None,
            name: None,
        })
    );

    assert_eq!(
        result2,
        Ok(AccountInfo {
            account_number: None,
            origin: origin2.clone(),
            last_used: None,
            name: None,
        })
    );
}
