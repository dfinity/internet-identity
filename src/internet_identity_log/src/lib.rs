use candid::Principal;
use ic_cdk::api;
use ic_cdk_macros::query;

// TODO: implement canister.

#[query]
fn whoami() -> Principal {
    api::caller()
}
