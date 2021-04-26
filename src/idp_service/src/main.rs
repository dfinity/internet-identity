use hashtree::{Hash, HashTree};
use ic_cdk::api::call::call;
use ic_cdk::api::{caller, data_certificate, id, set_certified_data, time, trap};
use ic_cdk::export::candid::{CandidType, Deserialize, Func, Principal};
use ic_cdk_macros::{init, post_upgrade, query, update};
use idp_service::nonce_cache::NonceCache;
use idp_service::signature_map::SignatureMap;
use serde::Serialize;
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryInto;
use storage::{Salt, Storage};

const fn secs_to_nanos(secs: u64) -> u64 {
    secs * 1_000_000_000
}

const DEFAULT_EXPIRATION_PERIOD_NS: u64 = 31_536_000_000_000_000;
const DEFAULT_SIGNATURE_EXPIRATION_PERIOD_NS: u64 = secs_to_nanos(600);
const POW_NONCE_LIFETIME: u64 = secs_to_nanos(300);

type UserNumber = u64;
type CredentialId = Vec<u8>;
type PublicKey = Vec<u8>;
type DeviceKey = PublicKey;
type UserKey = PublicKey;
type SessionKey = PublicKey;
type FrontendHostname = String;
type Timestamp = u64;
type Signature = Vec<u8>;

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct DeviceData {
    pubkey: DeviceKey,
    alias: String,
    credential_id: Option<CredentialId>,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
struct ProofOfWork {
    timestamp: Timestamp,
    nonce: u64,
}

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
    #[serde(rename = "signed_delegation")]
    SignedDelegation(SignedDelegation),
    #[serde(rename = "no_such_delegation")]
    NoSuchDelegation,
}

mod hash;
mod storage;

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

#[derive(Clone, Debug, CandidType, Deserialize)]
struct InternetIdentityStats {
    assigned_user_number_range: (UserNumber, UserNumber),
    users_registered: u64,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
struct InternetIdentityInit {
    assigned_user_number_range: (UserNumber, UserNumber),
}

struct State {
    nonce_cache: RefCell<NonceCache>,
    storage: RefCell<Storage<Vec<DeviceData>>>,
    sigs: RefCell<SignatureMap>,
}

impl Default for State {
    fn default() -> Self {
        Self {
            nonce_cache: RefCell::new(NonceCache::default()),
            storage: RefCell::new(Storage::new((10_000, 8_000_000_000))),
            sigs: RefCell::new(SignatureMap::default()),
        }
    }
}

thread_local! {
    static STATE: State = State::default();
    static ASSETS: RefCell<HashMap<String, (Vec<HeaderField>, Vec<u8>)>> = RefCell::new(HashMap::default());
}

#[update]
async fn init_salt() {
    STATE.with(|s| {
        if s.storage.borrow().salt().is_some() {
            trap("Salt already set");
        }
    });

    let res: Vec<u8> = match call(Principal::management_canister(), "raw_rand", ()).await {
        Ok((res,)) => res,
        Err((_, err)) => trap(&format!("failed to get salt: {}", err)),
    };
    let salt: Salt = res[..].try_into().unwrap_or_else(|_| {
        trap(&format!(
            "expected raw randomness to be of length 32, got {}",
            res.len()
        ));
    });

    STATE.with(|s| {
        let mut store = s.storage.borrow_mut();
        store.update_salt(salt); // update_salt() traps if salt has already been set
    });
}

#[update]
async fn register(device_data: DeviceData, pow: ProofOfWork) -> UserNumber {
    check_entry_limits(&device_data);
    let now = time() as u64;
    check_proof_of_work(&pow, now);

    if caller() != Principal::self_authenticating(device_data.pubkey.clone()) {
        ic_cdk::trap(&format!(
            "{} could not be authenticated against {:?}",
            caller(),
            device_data.pubkey
        ));
    }

    ensure_salt_set().await;

    STATE.with(|s| {
        let mut nonce_cache = s.nonce_cache.borrow_mut();
        if nonce_cache.contains(pow.timestamp, pow.nonce) {
            trap(&format!(
                "the combination of timestamp {} and nonce {} has already been used",
                pow.timestamp, pow.nonce,
            ));
        }
        nonce_cache.prune_expired(now.saturating_sub(POW_NONCE_LIFETIME));

        let mut store = s.storage.borrow_mut();
        let user_number = store
            .allocate_user_number()
            .unwrap_or_else(|| trap("failed to allocate a new user number"));
        store
            .write(user_number, vec![device_data])
            .unwrap_or_else(|err| trap(&format!("failed to store user device data: {}", err)));

        nonce_cache.add(pow.timestamp, pow.nonce);

        user_number
    })
}

#[update]
async fn add(user_number: UserNumber, device_data: DeviceData) {
    const MAX_ENTRIES_PER_USER: usize = 10;

    check_entry_limits(&device_data);

    ensure_salt_set().await;

    STATE.with(|s| {
        let mut entries = s.storage.borrow().read(user_number).unwrap_or_else(|err| {
            trap(&format!(
                "failed to read device data of user {}: {}",
                user_number, err
            ))
        });

        trap_if_not_authenticated(entries.iter().map(|e| &e.pubkey));

        for e in entries.iter_mut() {
            if e.pubkey == device_data.pubkey {
                trap("Device already added.");
            }
        }

        if entries.len() >= MAX_ENTRIES_PER_USER {
            trap(&format!(
                "at most {} authentication information entries are allowed per user",
                MAX_ENTRIES_PER_USER,
            ));
        }

        entries.push(device_data);
        s.storage
            .borrow()
            .write(user_number, entries)
            .unwrap_or_else(|err| {
                trap(&format!(
                    "failed to write device data of user {}: {}",
                    user_number, err
                ))
            });

        prune_expired_signatures(&mut s.sigs.borrow_mut());
    })
}

#[update]
async fn remove(user_number: UserNumber, device_key: DeviceKey) {
    ensure_salt_set().await;
    STATE.with(|s| {
        prune_expired_signatures(&mut s.sigs.borrow_mut());

        let mut entries = s.storage.borrow().read(user_number).unwrap_or_else(|err| {
            trap(&format!(
                "failed to read device data of user {}: {}",
                user_number, err
            ))
        });

        trap_if_not_authenticated(entries.iter().map(|e| &e.pubkey));

        if let Some(i) = entries.iter().position(|e| e.pubkey == device_key) {
            entries.swap_remove(i as usize);
        }

        s.storage
            .borrow()
            .write(user_number, entries)
            .unwrap_or_else(|err| {
                trap(&format!(
                    "failed to persist device data of user {}: {}",
                    user_number, err
                ))
            });
    })
}

#[query]
fn lookup(user_number: UserNumber) -> Vec<DeviceData> {
    STATE.with(|s| s.storage.borrow().read(user_number).unwrap_or_default())
}

#[update]
async fn prepare_delegation(
    user_number: UserNumber,
    frontend: FrontendHostname,
    session_key: SessionKey,
) -> (UserKey, Timestamp) {
    ensure_salt_set().await;

    STATE.with(|s| {
        let entries = s.storage.borrow().read(user_number).unwrap_or_else(|err| {
            trap(&format!(
                "failed to read device data of user {}: {}",
                user_number, err
            ))
        });

        trap_if_not_authenticated(entries.iter().map(|e| &e.pubkey));

        check_frontend_length(&frontend);

        let expiration = (time() as u64).saturating_add(DEFAULT_EXPIRATION_PERIOD_NS);

        let seed = calculate_seed(user_number, &frontend);
        let mut sigs = s.sigs.borrow_mut();
        add_signature(&mut sigs, session_key, seed, expiration);
        prune_expired_signatures(&mut sigs);

        (der_encode_canister_sig_key(seed.to_vec()), expiration)
    })
}

#[query]
fn get_delegation(
    user_number: UserNumber,
    frontend: FrontendHostname,
    session_key: SessionKey,
    expiration: Timestamp,
) -> GetDelegationResponse {
    check_frontend_length(&frontend);

    STATE.with(|state| {
        match get_signature(
            &state.sigs.borrow(),
            session_key.clone(),
            calculate_seed(user_number, &frontend),
            expiration,
        ) {
            Some(signature) => GetDelegationResponse::SignedDelegation(SignedDelegation {
                delegation: Delegation {
                    pubkey: session_key,
                    expiration,
                    targets: None,
                },
                signature,
            }),
            None => GetDelegationResponse::NoSuchDelegation,
        }
    })
}

#[query]
fn http_request(req: HttpRequest) -> HttpResponse {
    let parts: Vec<&str> = req.url.split("?").collect();
    let asset = parts[0].to_string();

    ASSETS.with(|a| match a.borrow().get(&asset) {
        Some((headers, value)) => HttpResponse {
            status_code: 200,
            headers: headers.clone(),
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
fn stats() -> InternetIdentityStats {
    STATE.with(|state| {
        let storage = state.storage.borrow();
        InternetIdentityStats {
            assigned_user_number_range: storage.assigned_user_number_range(),
            users_registered: storage.user_count() as u64,
        }
    })
}

// used both in init and post_upgrade
fn init_assets() {
    ASSETS.with(|a| {
        let mut a = a.borrow_mut();

        a.insert(
            "/index.html".to_string(),
            (vec![], include_bytes!("../../../dist/index.html").to_vec()),
        );
        a.insert(
            "/".to_string(),
            (vec![], include_bytes!("../../../dist/index.html").to_vec()),
        );
        a.insert(
            "/authorize".to_string(),
            (vec![], include_bytes!("../../../dist/index.html").to_vec()),
        );
        a.insert(
            "/index.js".to_string(),
            (
                vec![HeaderField {
                    key: "Content-Encoding".to_string(),
                    value: "gzip".to_string(),
                }],
                include_bytes!("../../../dist/index.js.gz").to_vec(),
            ),
        );
        a.insert(
            "/glitch-loop.webp".to_string(),
            (
                vec![],
                include_bytes!("../../../dist/glitch-loop.webp").to_vec(),
            ),
        );
        a.insert(
            "/favicon.ico".to_string(),
            (vec![], include_bytes!("../../../dist/favicon.ico").to_vec()),
        );
    });
}

#[init]
fn init(maybe_arg: Option<InternetIdentityInit>) {
    STATE.with(|state| {
        if let Some(arg) = maybe_arg {
            state
                .storage
                .replace(Storage::new(arg.assigned_user_number_range));
        }
        state.storage.borrow().flush();
        update_root_hash(&state.sigs.borrow());
    });
    init_assets();
}

#[post_upgrade]
fn retrieve_data() {
    init_assets();
    STATE.with(|s| {
        match Storage::from_stable_memory() {
            Some(storage) => {
                s.storage.replace(storage);
            }
            None => {
                s.storage.borrow().flush();
            }
        }

        // We drop all the signatures on upgrade, users will
        // re-request them if needed.
        update_root_hash(&s.sigs.borrow());
    });
}

fn calculate_seed(user_number: UserNumber, frontend: &FrontendHostname) -> Hash {
    let salt = STATE
        .with(|s| s.storage.borrow().salt().cloned())
        .unwrap_or_else(|| trap("Salt is not set. Try calling init_salt() to set it"));

    let mut blob: Vec<u8> = vec![];
    blob.push(salt.len() as u8);
    blob.extend_from_slice(&salt);

    let user_number_str = user_number.to_string();
    let user_number_blob = user_number_str.bytes();
    blob.push(user_number_blob.len() as u8);
    blob.extend(user_number_blob);

    blob.push(frontend.bytes().len() as u8);
    blob.extend(frontend.bytes());

    hash::hash_bytes(blob)
}

fn der_encode_canister_sig_key(seed: Vec<u8>) -> Vec<u8> {
    let my_canister_id: Vec<u8> = id().as_ref().to_vec();

    let mut bitstring: Vec<u8> = vec![];
    bitstring.push(my_canister_id.len() as u8);
    bitstring.extend(my_canister_id);
    bitstring.extend(seed);

    let mut der: Vec<u8> = vec![];
    // sequence of length 17 + the bit string length
    der.push(0x30);
    der.push(17 + bitstring.len() as u8);
    der.extend(vec![
        // sequence of length 12 for the OID
        0x30, 0x0C, // OID 1.3.6.1.4.1.56387.1.2
        0x06, 0x0A, 0x2B, 0x06, 0x01, 0x04, 0x01, 0x83, 0xB8, 0x43, 0x01, 0x02,
    ]);
    // BIT string of given length
    der.push(0x03);
    der.push(1 + bitstring.len() as u8);
    der.push(0x00);
    der.extend(bitstring);
    der
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
    pk: PublicKey,
    seed: Hash,
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
    let witness = sigs.witness(hash::hash_bytes(seed), msg_hash)?;

    let witness_hash = witness.reconstruct();
    let root_hash = sigs.root_hash();
    if witness_hash != root_hash {
        trap(&format!(
            "internal error: signature map computed an invalid hash tree, witness hash is {}, root hash is {}",
            hex::encode(&witness_hash),
            hex::encode(&root_hash)
        ));
    }

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

fn add_signature(sigs: &mut SignatureMap, pk: PublicKey, seed: Hash, expiration: Timestamp) {
    let msg_hash = delegation_signature_msg_hash(&Delegation {
        pubkey: pk,
        expiration,
        targets: None,
    });
    let expires_at = (time() as u64).saturating_add(DEFAULT_SIGNATURE_EXPIRATION_PERIOD_NS);
    sigs.put(hash::hash_bytes(seed), msg_hash, expires_at);
    update_root_hash(&sigs);
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

fn check_entry_limits(device_data: &DeviceData) {
    const ALIAS_LEN_LIMIT: usize = 64;
    const PK_LEN_LIMIT: usize = 100;
    const CREDENTIAL_ID_LEN_LIMIT: usize = 200;

    let n = device_data.alias.len();
    if n > ALIAS_LEN_LIMIT {
        trap(&format!(
            "alias length {} exceeds the limit of {} bytes",
            n, ALIAS_LEN_LIMIT,
        ));
    }

    let n = device_data.pubkey.len();
    if n > PK_LEN_LIMIT {
        trap(&format!(
            "public key length {} exceeds the limit of {} bytes",
            n, PK_LEN_LIMIT,
        ));
    }

    let n = device_data
        .credential_id
        .as_ref()
        .map(|bytes| bytes.len())
        .unwrap_or_default();
    if n > CREDENTIAL_ID_LEN_LIMIT {
        trap(&format!(
            "credential id length {} exceeds the limit of {} bytes",
            n, CREDENTIAL_ID_LEN_LIMIT,
        ));
    }
}

fn check_frontend_length(frontend: &FrontendHostname) {
    const FRONTEND_HOSTNAME_LIMIT: usize = 255;

    let n = frontend.len();
    if frontend.len() > FRONTEND_HOSTNAME_LIMIT {
        trap(&format!(
            "frontend hostname {} exceeds the limit of {} bytes",
            n, FRONTEND_HOSTNAME_LIMIT,
        ));
    }
}

fn check_proof_of_work(pow: &ProofOfWork, now: Timestamp) {
    use cubehash::CubeHash;

    const DIFFICULTY: usize = 2;

    if pow.timestamp < now.saturating_sub(POW_NONCE_LIFETIME) {
        trap(&format!(
            "proof of work timestamp {} is too old, current time: {}",
            pow.timestamp, now
        ));
    }
    if pow.timestamp > now.saturating_add(POW_NONCE_LIFETIME) {
        trap(&format!(
            "proof of work timestamp {} is too far in future, current time: {}",
            pow.timestamp, now
        ));
    }

    let mut hasher = CubeHash::new();
    let domain = b"ic-proof-of-work";
    hasher.update(&[domain.len() as u8]);
    hasher.update(&domain[..]);
    hasher.update(&pow.timestamp.to_le_bytes());
    hasher.update(&pow.nonce.to_le_bytes());

    let id = ic_cdk::api::id();
    hasher.update(&[id.as_slice().len() as u8]);
    hasher.update(id.as_slice());

    let hash = hasher.finalize();
    if !hash[0..DIFFICULTY].iter().all(|b| *b == 0) {
        trap("proof of work hash check failed");
    }
}

// Checks if salt is empty and calls `init_salt` to set it.
async fn ensure_salt_set() {
    let salt = STATE.with(|s| s.storage.borrow().salt().cloned());
    if salt.is_none() {
        init_salt().await;
    }

    STATE.with(|s| {
        if s.storage.borrow().salt().is_none() {
            trap("Salt is not set. Try calling init_salt() to set it");
        }
    });
}

fn main() {}
