use hashtree::{Hash, HashTree};
use ic_cdk::api::{caller, data_certificate, set_certified_data, time, trap};
use ic_cdk::export::candid::{CandidType, Deserialize, Func, Principal};
use ic_cdk::storage::{stable_restore, stable_save};
use ic_cdk_macros::{init, post_upgrade, pre_upgrade, query, update};
use idp_service::signature_map::SignatureMap;
use serde::Serialize;
use std::cell::{Cell, RefCell};
use std::collections::HashMap;

const DEFAULT_EXPIRATION_PERIOD_NS: u64 = 31_536_000_000_000_000;
const DEFAULT_SIGNATURE_EXPIRATION_PERIOD_NS: u64 = 600_000_000_000;

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

#[derive(Clone, Debug, CandidType, Deserialize)]
enum GetDelegationResponse {
    #[serde(rename = "delegation")]
    Delegation(SignedDelegation),
    #[serde(rename = "request_delegation_explicitly")]
    RequestDelegationExplicitly,
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
    headers: Vec<(String, String)>,
    body: Vec<u8>,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
struct HttpResponse {
    status_code: u16,
    headers: Vec<HeaderField>,
    body: Vec<u8>,
    streaming_strategy: Option<StreamingStrategy>,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
struct Token {}

#[derive(Clone, Debug, CandidType, Deserialize)]
enum StreamingStrategy {
    Callback { callback: Func, token: Token },
}

#[derive(Clone, Debug, CandidType, Deserialize)]
struct StreamingCallbackHttpResponse {
    body: Vec<u8>,
    token: Option<Token>,
}

struct State {
    next_user_id: Cell<UserId>,
    map: RefCell<HashMap<UserId, Vec<Entry>>>,
    sigs: RefCell<SignatureMap>,
}

impl Default for State {
    fn default() -> Self {
        Self {
            next_user_id: Cell::new(10000),
            map: RefCell::new(HashMap::default()),
            sigs: RefCell::new(SignatureMap::default()),
        }
    }
}

thread_local! {
    static STATE: State = State::default();
    static ASSETS: RefCell<HashMap<String, Vec<u8>>> = RefCell::new(HashMap::default());
}

#[update]
fn register(alias: Alias, pk: PublicKey, credential_id: Option<CredentialId>) -> UserId {
    check_entry_limits(alias.as_str(), pk.as_slice(), credential_id.as_ref());

    STATE.with(|s| {
        if caller() != Principal::self_authenticating(pk.clone()) {
            ic_cdk::trap(&format!(
                "{} could not be authenticated against {:?}",
                caller(),
                pk
            ));
        }

        prune_expired_signatures(&mut s.sigs.borrow_mut());

        let mut m = s.map.borrow_mut();
        let user_id = s.next_user_id.get();
        s.next_user_id.replace(user_id + 1);

        let expiration = time() as u64 + DEFAULT_EXPIRATION_PERIOD_NS;
        m.insert(
            user_id,
            vec![(alias, pk.clone(), expiration, credential_id)],
        );
        add_signature(&mut s.sigs.borrow_mut(), user_id, pk, expiration);

        user_id
    })
}

#[update]
fn add(user_id: UserId, alias: Alias, pk: PublicKey, credential: Option<CredentialId>) {
    const MAX_ENTRIES_PER_USER: usize = 10;

    check_entry_limits(alias.as_str(), pk.as_slice(), credential.as_ref());

    STATE.with(|s| {
        let mut m = s.map.borrow_mut();
        if let Some(entries) = m.get_mut(&user_id) {
            trap_if_not_authenticated(entries.iter().map(|e| &e.1));

            let expiration = time() as u64 + DEFAULT_EXPIRATION_PERIOD_NS;
            for e in entries.iter_mut() {
                if e.1 == pk {
                    e.0 = alias;
                    e.2 = expiration;
                    e.3 = credential;
                    add_signature(&mut s.sigs.borrow_mut(), user_id, pk, expiration);
                    prune_expired_signatures(&mut s.sigs.borrow_mut());
                    return;
                }
            }

            if entries.len() >= MAX_ENTRIES_PER_USER {
                trap(&format!(
                    "at most {} authentication information entries are allowed per user",
                    MAX_ENTRIES_PER_USER,
                ));
            }

            entries.push((alias, pk.clone(), expiration, credential));
            add_signature(&mut s.sigs.borrow_mut(), user_id, pk, expiration);
            prune_expired_signatures(&mut s.sigs.borrow_mut());
        } else {
            trap(&format!("User {} is not registered yet", user_id));
        }
    })
}

#[update]
fn request_delegation(user_id: UserId, pk: PublicKey) {
    STATE.with(|s| {
        let mut m = s.map.borrow_mut();
        if let Some(entries) = m.get_mut(&user_id) {
            for e in entries.iter_mut() {
                if e.1 == pk {
                    let expiration = time() as u64 + DEFAULT_EXPIRATION_PERIOD_NS;
                    e.2 = expiration;

                    let mut sigs = s.sigs.borrow_mut();
                    add_signature(&mut sigs, user_id, pk, expiration);
                    prune_expired_signatures(&mut sigs);
                    return;
                }
            }
        }
        trap(&format!(
            "Unknown combination of user_id {} and public_key {}",
            user_id,
            hex::encode(&pk[..])
        ));
    })
}

#[update]
fn remove(user_id: UserId, pk: PublicKey) {
    STATE.with(|s| {
        prune_expired_signatures(&mut s.sigs.borrow_mut());

        let mut remove_user = false;
        if let Some(entries) = s.map.borrow_mut().get_mut(&user_id) {
            trap_if_not_authenticated(entries.iter().map(|e| &e.1));

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
            streaming_strategy: None,
        },
        None => HttpResponse {
            status_code: 404,
            headers: vec![],
            body: format!("Asset {} not found.", asset).as_bytes().into(),
            streaming_strategy: None,
        },
    })
}

#[query]
fn get_delegation(user_id: UserId, pubkey: PublicKey) -> GetDelegationResponse {
    STATE.with(|state| {
        let mut m = state.map.borrow_mut();
        if let Some(entries) = m.get_mut(&user_id) {
            if let Some((_, _, expiration, _)) = entries.iter().find(|e| e.1 == pubkey) {
                match get_signature(&state.sigs.borrow(), user_id, pubkey.clone(), *expiration) {
                    Some(signature) => {
                        return GetDelegationResponse::Delegation(SignedDelegation {
                            delegation: Delegation {
                                pubkey,
                                expiration: *expiration,
                                targets: None,
                            },
                            signature,
                        });
                    }
                    None => {
                        return GetDelegationResponse::RequestDelegationExplicitly;
                    }
                }
            }
        }
        trap("User ID and public key pair not found.");
    })
}

// used both in init and post_upgrade
fn init_assets() {
    ASSETS.with(|a| {
        let mut a = a.borrow_mut();

        a.insert(
            "/index.html".to_string(),
            include_str!("../../../dist/index.html")
                .as_bytes()
                .into(),
        );
        a.insert(
            "/".to_string(),
            include_str!("../../../dist/index.html")
                .as_bytes()
                .into(),
        );
        a.insert(
            "/authorize".to_string(),
            include_str!("../../../dist/index.html")
                .as_bytes()
                .into(),
        );
        a.insert(
            "/index.js".to_string(),
            include_str!("../../../dist/index.js")
                .as_bytes()
                .into(),
        );
        a.insert(
            "/manage".to_string(),
            include_str!("../../../dist/manage.html")
                .as_bytes()
                .into(),
        );
        a.insert(
            "/manage.js".to_string(),
            include_str!("../../../dist/manage.js")
                .as_bytes()
                .into(),
        );
    });
}

#[init]
fn init() {
    STATE.with(|state| update_root_hash(&state.sigs.borrow()));
    init_assets();
}

#[pre_upgrade]
fn persist_data() {
    STATE.with(|s| {
        let map = s.map.replace(Default::default());
        if let Err(err) = stable_save((map, s.next_user_id.get())) {
            ic_cdk::trap(&format!(
                "An error occurred while saving data to stable memory: {}",
                err
            ));
        }
    })
}

#[post_upgrade]
fn retrieve_data() {
    init_assets();
    match stable_restore::<(HashMap<UserId, Vec<Entry>>, UserId)>() {
        Ok((map, user_id)) => {
            STATE.with(|s| {
                // Restore user map.
                s.map.replace(map);
                s.next_user_id.replace(user_id);

                // We drop all the signatures on upgrade, users will
                // re-request them if needed.
                update_root_hash(&s.sigs.borrow());
            });
        }
        Err(err) => ic_cdk::trap(&format!(
            "An error occurred while retrieving data from stable memory: {}",
            err
        )),
    }
}

fn hash_seed(user_id: UserId) -> Hash {
    hash::hash_string(user_id.to_string().as_str())
}

fn delegation_signature_msg_hash(d: &Delegation) -> Hash {
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
    let certificate = data_certificate().unwrap_or_else(|| {
        trap("data certificate is only available in query calls");
    });
    let msg_hash = delegation_signature_msg_hash(&Delegation {
        pubkey: pk,
        expiration,
        targets: None,
    });
    let witness = sigs.witness(hash_seed(user_id), msg_hash)?;
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
    let msg_hash = delegation_signature_msg_hash(&Delegation {
        pubkey: pk,
        expiration,
        targets: None,
    });
    let expires_at = time() as u64 + DEFAULT_SIGNATURE_EXPIRATION_PERIOD_NS;
    sigs.put(hash_seed(user_id), msg_hash, expires_at);
    update_root_hash(&sigs);
}

fn remove_signature(
    sigs: &mut SignatureMap,
    user_id: UserId,
    pk: PublicKey,
    expiration: Timestamp,
) {
    let msg_hash = delegation_signature_msg_hash(&Delegation {
        pubkey: pk,
        expiration,
        targets: None,
    });
    sigs.delete(hash_seed(user_id), msg_hash);
    update_root_hash(sigs);
}

/// Removes a batch of expired signatures from the signature map.
///
/// This function is supposed to piggy back on update calls to
/// amortize the cost of tree pruning.  Each operation on the signature map
/// will prune at most MAX_SIGS_TO_PRUNE other signatures.
fn prune_expired_signatures(sigs: &mut SignatureMap) {
    const MAX_SIGS_TO_PRUNE: usize = 10;
    let num_pruned = sigs.prune_expired(time() as u64, MAX_SIGS_TO_PRUNE);

    if num_pruned > 0 {
        update_root_hash(sigs);
    }
}

// Checks if the caller is authenticated against any of the public keys provided
// and traps if not.
fn trap_if_not_authenticated<'a>(public_keys: impl Iterator<Item = &'a PublicKey>) {
    for pk in public_keys {
        if caller() == Principal::self_authenticating(pk) {
            return;
        }
    }
    ic_cdk::trap(&format!("{} could not be authenticated.", caller()))
}

fn check_entry_limits(alias: &str, pk: &[u8], credential_id: Option<&CredentialId>) {
    const ALIAS_LEN_LIMIT: usize = 64;
    const PK_LEN_LIMIT: usize = 100;
    const CREDENTIAL_ID_LEN_LIMIT: usize = 200;

    let n = alias.len();
    if n > ALIAS_LEN_LIMIT {
        trap(&format!(
            "alias length {} exceeds the limit of {} bytes",
            n, ALIAS_LEN_LIMIT,
        ));
    }

    let n = pk.len();
    if n > PK_LEN_LIMIT {
        trap(&format!(
            "public key length {} exceeds the limit of {} bytes",
            n, PK_LEN_LIMIT,
        ));
    }

    let n = credential_id.map(|bytes| bytes.len()).unwrap_or_default();
    if n > CREDENTIAL_ID_LEN_LIMIT {
        trap(&format!(
            "credential id length {} exceeds the limit of {} bytes",
            n, CREDENTIAL_ID_LEN_LIMIT,
        ));
    }
}

fn main() {}
