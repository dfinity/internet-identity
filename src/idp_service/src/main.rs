use ic_cdk::api::call::reject;
use ic_cdk_macros::{query, update};
use std::cell::RefCell;
use std::collections::HashMap;

type UserId = u64;
type CredentialId = Vec<u8>;
type Alias = String;
type PublicKey = Vec<u8>;
type Entry = (Alias, PublicKey, Option<CredentialId>);

thread_local! {
    static MAP: RefCell<HashMap<UserId, Vec<Entry>>> = RefCell::new(HashMap::new());
}

#[update]
fn register(
    user_id: UserId,
    alias: Alias,
    public_key: PublicKey,
    credential_id: Option<CredentialId>,
) {
    MAP.with(|m| {
        let mut m = m.borrow_mut();
        if m.get(&user_id).is_some() {
            reject("this user is already registered");
            return;
        }
        m.insert(user_id, vec![(alias, public_key, credential_id)]);
    })
}

#[update]
fn add(user_id: UserId, alias: Alias, public_key: PublicKey, credential: Option<CredentialId>) {
    MAP.with(|m| {
        let mut m = m.borrow_mut();
        if let Some(entries) = m.get_mut(&user_id) {
            for e in entries.iter_mut() {
                if e.1 == public_key {
                    e.0 = alias;
                    e.2 = credential;
                    return;
                }
            }
            entries.push((alias, public_key, credential))
        } else {
            reject("this user is not registered yet");
        }
    })
}

#[update]
fn remove(user_id: UserId, public_key: PublicKey) {
    MAP.with(|m| {
        if let Some(entries) = m.borrow_mut().get_mut(&user_id) {
            if let Some(i) = entries.iter().position(|e| e.1 == public_key) {
                entries.swap_remove(i as usize);
            }
        }
    })
}

#[query]
fn lookup(user_id: UserId) -> Vec<Entry> {
    MAP.with(|m| m.borrow().get(&user_id).cloned().unwrap_or_default())
}

fn main() {}
