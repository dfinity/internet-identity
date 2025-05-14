use crate::{
    state::storage_borrow,
    storage::account::{Account, ReadAccountParams},
};
use internet_identity_interface::internet_identity::types::{AnchorNumber, FrontendHostname};

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
