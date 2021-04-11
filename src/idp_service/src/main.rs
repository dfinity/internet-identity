use certified_map::RbTree;
use hashtree::HashTree;
use ic_cdk::api::{data_certificate, set_certified_data};
use ic_cdk_macros::{init, query, update};
use serde::Serialize;
use std::cell::RefCell;
use std::collections::HashMap;

type UserId = u64;
type CredentialId = Vec<u8>;
type PublicKey = Vec<u8>;
type Alias = String;
type Entry = (Alias, PublicKey, Option<CredentialId>);

thread_local! {
    static MAP: RefCell<HashMap<UserId, Vec<Entry>>> = RefCell::new(HashMap::new());
    static SIGS: RefCell<RbTree> = RefCell::new(RbTree::new());
}

fn update_root_hash(t: &RbTree) {
    let prefixed_root_hash = hashtree::labeled_hash(b"sig", &t.root_hash());
    set_certified_data(&prefixed_root_hash[..]);
}

fn sign_payload(t: &mut RbTree, payload: Vec<u8>) {
    t.insert(payload, vec![]);
    update_root_hash(t);
}

fn remove_payload_signature(t: &mut RbTree, payload: Vec<u8>) {
    t.insert(payload, vec![]);
    update_root_hash(t);
}

fn get_payload_signature<'a>(t: &'a RbTree, payload: &[u8]) -> Option<Vec<u8>> {
    #[derive(Serialize)]
    struct Sig<'a> {
        #[serde(with = "serde_bytes")]
        certificate: Vec<u8>,
        tree: HashTree<'a>,
    }

    if t.get(payload).is_none() {
        return None;
    }

    let sig = Sig {
        certificate: data_certificate()?,
        tree: hashtree::labeled(b"sig", t.witness(payload)),
    };

    let mut cbor = serde_cbor::ser::Serializer::new(Vec::new());
    cbor.self_describe().unwrap();
    sig.serialize(&mut cbor).unwrap();
    Some(cbor.into_inner())
}

#[update]
fn register(user_id: UserId, alias: Alias, pk: PublicKey, credential_id: Option<CredentialId>) {
    MAP.with(|m| {
        let mut m = m.borrow_mut();
        if m.get(&user_id).is_some() {
            panic!("this user is already registered");
        }
        m.insert(user_id, vec![(alias, pk, credential_id)]);
    })
}

#[update]
fn add(user_id: UserId, alias: Alias, pk: PublicKey, credential: Option<CredentialId>) {
    MAP.with(|m| {
        let mut m = m.borrow_mut();
        if let Some(entries) = m.get_mut(&user_id) {
            for e in entries.iter_mut() {
                if e.1 == pk {
                    e.0 = alias;
                    e.2 = credential;
                    return;
                }
            }
            entries.push((alias, pk, credential))
        } else {
            panic!("this user is not registered yet");
        }
    })
}

#[update]
fn remove(user_id: UserId, pk: PublicKey) {
    MAP.with(|m| {
        if let Some(entries) = m.borrow_mut().get_mut(&user_id) {
            if let Some(i) = entries.iter().position(|e| e.1 == pk) {
                entries.swap_remove(i as usize);
            }
        }
    })
}

#[query]
fn lookup(user_id: UserId) -> Vec<Entry> {
    MAP.with(|m| m.borrow().get(&user_id).cloned().unwrap_or_default())
}

#[init]
fn init() {
    SIGS.with(|sigs| update_root_hash(&sigs.borrow()));
}

fn main() {}
