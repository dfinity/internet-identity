use hash::hash_bytes;
use hashtree::{Hash, HashTree};
use ic_cdk::api::{data_certificate, set_certified_data, time, trap};
use ic_cdk::export::candid::{CandidType, Deserialize, Principal};
use ic_cdk::storage::{stable_restore, stable_save};
use ic_cdk_macros::{init, post_upgrade, pre_upgrade, query, update};
use idp_service::signature_map::SignatureMap;
use serde::Serialize;
use std::cell::RefCell;
use std::collections::HashMap;

const DEFAULT_EXPIRATION_PERIOD_NS: u64 = 31_536_000_000_000_000;

type UserId = u64;
type CredentialId = Vec<u8>;
type PublicKey = Vec<u8>;
type Alias = String;
type Entry = (Alias, PublicKey, Timestamp, Option<CredentialId>);
type Timestamp = u64;
type Signature = Vec<u8>;

#[derive(Clone, Debug, CandidType, Deserialize)]
struct Delegation {
    pubkey: PublicKey,
    expiration: Timestamp,
    targets: Option<Vec<Principal>>,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
struct SignedDelegation {
    delegation: Delegation,
    signature: Signature,
}

mod hash;

#[derive(Clone, Debug, CandidType, Deserialize)]
struct HeaderField {
    key: String,
    value: String,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
struct HttpRequest {
    method: String,
    url: String,
    headers: Vec<HeaderField>,
    body: Vec<u8>,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
struct HttpResponse {
    status_code: u16,
    headers: Vec<HeaderField>,
    body: Vec<u8>,
}

struct State {
    map: RefCell<HashMap<UserId, Vec<Entry>>>,
    sigs: RefCell<SignatureMap>,
}

impl Default for State {
    fn default() -> Self {
        Self {
            map: RefCell::new(HashMap::default()),
            sigs: RefCell::new(SignatureMap::default()),
        }
    }
}

thread_local! {
    static STATE: State = State::default();
    static ASSETS: RefCell<HashMap<String, Vec<u8>>> = RefCell::new(HashMap::default());
}

fn update_root_hash(m: &SignatureMap) {
    let prefixed_root_hash = hashtree::labeled_hash(b"sig", &m.root_hash());
    set_certified_data(&prefixed_root_hash[..]);
}

fn get_signature(
    sigs: &SignatureMap,
    user_id: UserId,
    pk: PublicKey,
    expiration: Timestamp,
) -> Option<Vec<u8>> {
    let certificate = data_certificate()?;
    let msg_hash = hash_bytes(delegation_signature_msg(&Delegation {
        pubkey: pk,
        expiration,
        targets: None,
    }));
    let witness = sigs.witness(seed_hash(user_id), msg_hash)?;
    let tree = HashTree::Labeled(&b"sig"[..], Box::new(witness));

    #[derive(Serialize)]
    struct Sig<'a> {
        #[serde(with = "serde_bytes")]
        certificate: Vec<u8>,
        tree: HashTree<'a>,
    }

    let sig = Sig { certificate, tree };

    let mut cbor = serde_cbor::ser::Serializer::new(Vec::new());
    cbor.self_describe().unwrap();
    sig.serialize(&mut cbor).unwrap();
    Some(cbor.into_inner())
}

fn add_signature(sigs: &mut SignatureMap, user_id: UserId, pk: PublicKey, expiration: Timestamp) {
    let msg_hash = hash_bytes(delegation_signature_msg(&Delegation {
        pubkey: pk,
        expiration,
        targets: None,
    }));
    sigs.put(seed_hash(user_id), msg_hash);
    update_root_hash(&sigs);
}

fn remove_signature(
    sigs: &mut SignatureMap,
    user_id: UserId,
    pk: PublicKey,
    expiration: Timestamp,
) {
    let msg_hash = hash_bytes(delegation_signature_msg(&Delegation {
        pubkey: pk,
        expiration,
        targets: None,
    }));
    sigs.delete(seed_hash(user_id), msg_hash);
    update_root_hash(sigs);
}

#[update]
fn register(user_id: UserId, alias: Alias, pk: PublicKey, credential_id: Option<CredentialId>) {
    STATE.with(|s| {
        let mut m = s.map.borrow_mut();
        if m.get(&user_id).is_some() {
            trap("This user is already registered");
        }
        let expiration = time() as u64 + DEFAULT_EXPIRATION_PERIOD_NS;
        m.insert(
            user_id,
            vec![(alias, pk.clone(), expiration, credential_id)],
        );
        add_signature(&mut s.sigs.borrow_mut(), user_id, pk, expiration);
    })
}

#[update]
fn add(user_id: UserId, alias: Alias, pk: PublicKey, credential: Option<CredentialId>) {
    STATE.with(|s| {
        let mut m = s.map.borrow_mut();
        if let Some(entries) = m.get_mut(&user_id) {
            let expiration = time() as u64 + DEFAULT_EXPIRATION_PERIOD_NS;
            for e in entries.iter_mut() {
                if e.1 == pk {
                    e.0 = alias;
                    e.2 = expiration;
                    e.3 = credential;
                    add_signature(&mut s.sigs.borrow_mut(), user_id, pk, expiration);
                    return;
                }
            }
            entries.push((alias, pk.clone(), expiration, credential));
            add_signature(&mut s.sigs.borrow_mut(), user_id, pk, expiration);
        } else {
            trap("This user is not registered yet");
        }
    })
}

#[update]
fn remove(user_id: UserId, pk: PublicKey) {
    STATE.with(|s| {
        let mut remove_user = false;
        if let Some(entries) = s.map.borrow_mut().get_mut(&user_id) {
            if let Some(i) = entries.iter().position(|e| e.1 == pk) {
                let (_, _, expiration, _) = entries.swap_remove(i as usize);
                remove_signature(&mut s.sigs.borrow_mut(), user_id, pk, expiration);
                remove_user = entries.is_empty();
            }
        }
        if remove_user {
            s.map.borrow_mut().remove(&user_id);
        }
    })
}

#[query]
fn lookup(user_id: UserId) -> Vec<Entry> {
    STATE.with(|s| s.map.borrow().get(&user_id).cloned().unwrap_or_default())
}

#[query]
fn http_request(req: HttpRequest) -> HttpResponse {
    let parts: Vec<&str> = req.url.split("?").collect();
    let asset = parts[0].to_string();

    ASSETS.with(|a| match a.borrow().get(&asset) {
        Some(value) => HttpResponse {
            status_code: 200,
            headers: vec![],
            body: value.clone(),
        },
        None => HttpResponse {
            status_code: 404,
            headers: vec![],
            body: format!("Asset {} not found.", asset).as_bytes().into(),
        },
    })
}

#[query]
fn get_delegation(user_id: UserId, pubkey: PublicKey) -> SignedDelegation {
    STATE.with(|state| {
        let mut m = state.map.borrow_mut();
        if let Some(entries) = m.get_mut(&user_id) {
            if let Some((_, _, expiration, _)) = entries.iter().find(|e| e.1 == pubkey) {
                let signature =
                    get_signature(&state.sigs.borrow(), user_id, pubkey.clone(), *expiration)
                        .unwrap_or_else(|| trap("No signature found"));
                return SignedDelegation {
                    delegation: Delegation {
                        pubkey,
                        expiration: *expiration,
                        targets: None,
                    },
                    signature,
                };
            }
        }
        trap("User ID and public key pair not found.");
    })
}

#[init]
fn init() {
    STATE.with(|state| update_root_hash(&state.sigs.borrow()));
    ASSETS.with(|a| {
        let mut a = a.borrow_mut();

        a.insert(
            "/sample-asset.txt".to_string(),
            include_str!("../../frontend/assets/sample-asset.txt")
                .as_bytes()
                .into(),
        );
    });
}

#[pre_upgrade]
fn persist_data() {
    STATE.with(|s| {
        if let Err(err) = stable_save((s.map.take(),)) {
            ic_cdk::trap(&format!(
                "An error occurred while saving data to stable memory: {}",
                err
            ));
        }
    })
}

#[post_upgrade]
fn retrieve_data() {
    match stable_restore::<(HashMap<UserId, Vec<Entry>>,)>() {
        Ok((map,)) => {
            STATE.with(|s| {
                // Restore user map.
                s.map.replace(map);

                // Recompute the signatures based on the user map.
                let mut sigs = SignatureMap::default();
                for (user_id, entries) in s.map.borrow().iter() {
                    for (_, pk, expiration, _) in entries.iter() {
                        add_signature(&mut sigs, *user_id, pk.clone(), *expiration);
                    }
                }
                s.sigs.replace(sigs);
            });
        }
        Err(err) => ic_cdk::trap(&format!(
            "An error occurred while retrieving data from stable memory: {}",
            err
        )),
    }
}

fn seed_hash(user_id: UserId) -> Hash {
    hash::hash_string(user_id.to_string().as_str())
}

fn delegation_signature_msg(d: &Delegation) -> Hash {
    use hash::Value;

    let mut m = HashMap::new();
    m.insert("pubkey", Value::Bytes(d.pubkey.as_slice()));
    m.insert("expiration", Value::U64(d.expiration));
    if let Some(targets) = d.targets.as_ref() {
        let mut arr = Vec::with_capacity(targets.len());
        for t in targets.iter() {
            arr.push(Value::Bytes(t.as_ref()));
        }
        m.insert("targets", Value::Array(arr));
    }
    let map_hash = hash::hash_of_map(m);
    hash::hash_with_domain(b"ic-request-auth-delegation", &map_hash)
}

fn main() {}
