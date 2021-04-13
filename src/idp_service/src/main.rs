use hashtree::{Hash, HashTree};
use ic_cdk::api::{data_certificate, set_certified_data};
use ic_cdk::export::candid::{CandidType, Deserialize, Principal};
use ic_cdk_macros::{init, query, update};
use idp_service::signature_map::SignatureMap;
use serde::Serialize;
use std::cell::RefCell;
use std::collections::HashMap;

type UserId = u64;
type CredentialId = Vec<u8>;
type PublicKey = Vec<u8>;
type Alias = String;
type Entry = (Alias, PublicKey, Option<CredentialId>);
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
    assets: RefCell<HashMap<String, Vec<u8>>>,
}

impl Default for State {
    fn default() -> Self {
        Self {
            map: RefCell::new(HashMap::default()),
            sigs: RefCell::new(SignatureMap::default()),
            assets: RefCell::new(HashMap::default()),
        }
    }
}

thread_local! {
    static STATE: State = State::default();
}

fn update_root_hash(m: &SignatureMap) {
    let prefixed_root_hash = hashtree::labeled_hash(b"sig", &m.root_hash());
    set_certified_data(&prefixed_root_hash[..]);
}

#[allow(dead_code)]
fn get_signature(m: &SignatureMap, seed_hash: Hash, msg_hash: Hash) -> Option<Vec<u8>> {
    let certificate = data_certificate()?;
    let witness = m.witness(seed_hash, msg_hash)?;
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

#[update]
fn register(user_id: UserId, alias: Alias, pk: PublicKey, credential_id: Option<CredentialId>) {
    STATE.with(|s| {
        let mut m = s.map.borrow_mut();
        if m.get(&user_id).is_some() {
            panic!("this user is already registered");
        }
        m.insert(user_id, vec![(alias, pk, credential_id)]);
    })
}

#[update]
fn add(user_id: UserId, alias: Alias, pk: PublicKey, credential: Option<CredentialId>) {
    STATE.with(|s| {
        let mut m = s.map.borrow_mut();
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
    STATE.with(|s| {
        if let Some(entries) = s.map.borrow_mut().get_mut(&user_id) {
            if let Some(i) = entries.iter().position(|e| e.1 == pk) {
                entries.swap_remove(i as usize);
            }
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

    STATE.with(|s| match s.assets.borrow().get(&asset) {
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
fn get_delegation(
    user_id: UserId,
    pubkey: PublicKey,
    expiration: Timestamp,
    targets: Option<Vec<Principal>>,
) -> SignedDelegation {
    STATE.with(|s| {
        if let Some(entries) = s.map.borrow_mut().get_mut(&user_id) {
            if entries.iter().position(|e| e.1 == pubkey) == None {
                panic!("User ID and public key pair not found.");
            }
        }
    });

    SignedDelegation {
        delegation: Delegation {
            pubkey,
            expiration,
            targets,
        },
        signature: Vec::new(), // empty signature for now.
    }
}

#[init]
fn init() {
    STATE.with(|state| update_root_hash(&state.sigs.borrow()));
    STATE.with(|state| {
        let mut a = state.assets.borrow_mut();

        a.insert(
            "/sample-asset.txt".to_string(),
            include_str!("../../frontend/assets/sample-asset.txt")
                .as_bytes()
                .into(),
        );
        a.insert(
            "/main.css".to_string(),
            include_str!("../../frontend/assets/main.css")
                .as_bytes()
                .into(),
        );
        a.insert(
            "/logo.png".to_string(),
            include_bytes!("../../frontend/assets/logo.png")
                .to_owned()
                .into(),
        );
    });
}

#[allow(dead_code)]
fn delegation_hash(d: &Delegation) -> Hash {
    use hash::{hash_of_map, Value};

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
    hash_of_map(m)
}

fn main() {}
