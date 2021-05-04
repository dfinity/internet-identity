use certified_map::{AsHashTree, RbTree};
use hashtree::{Hash, HashTree};
use ic_cdk::api::call::call;
use ic_cdk::api::{caller, data_certificate, id, set_certified_data, time, trap};
use ic_cdk::export::candid::{CandidType, Deserialize, Func, Principal};
use ic_cdk_macros::{init, post_upgrade, query, update};
use idp_service::metrics_encoder::MetricsEncoder;
use idp_service::nonce_cache::NonceCache;
use idp_service::signature_map::SignatureMap;
use serde::Serialize;
use serde_bytes::ByteBuf;
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryInto;
use storage::{Salt, Storage};

const fn secs_to_nanos(secs: u64) -> u64 {
    secs * 1_000_000_000
}

// 30 mins
const DEFAULT_EXPIRATION_PERIOD_NS: u64 = secs_to_nanos(30 * 60);
// 8 days
const MAX_EXPIRATION_PERIOD_NS: u64 = secs_to_nanos(8 * 24 * 60 * 60);
// 10 mins
const DEFAULT_SIGNATURE_EXPIRATION_PERIOD_NS: u64 = secs_to_nanos(600);
// 5 mins
const POW_NONCE_LIFETIME: u64 = secs_to_nanos(300);

const LABEL_ASSETS: &[u8] = b"http_assets";
const LABEL_SIG: &[u8] = b"sig";

type UserNumber = u64;
type CredentialId = ByteBuf;
type PublicKey = ByteBuf;
type DeviceKey = PublicKey;
type UserKey = PublicKey;
type SessionKey = PublicKey;
type FrontendHostname = String;
type Timestamp = u64;
type Signature = ByteBuf;

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

#[derive(Clone, Debug, CandidType, Deserialize)]
enum RegisterResponse {
    #[serde(rename = "registered")]
    Registered { user_number: UserNumber },
    #[serde(rename = "canister_full")]
    CanisterFull,
}

mod hash;
mod storage;

type HeaderField = (String, String);

#[derive(Clone, Debug, CandidType, Deserialize)]
struct HttpRequest {
    method: String,
    url: String,
    headers: Vec<(String, String)>,
    body: ByteBuf,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
struct HttpResponse {
    status_code: u16,
    headers: Vec<HeaderField>,
    body: ByteBuf,
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
    body: ByteBuf,
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

type AssetHashes = RbTree<&'static str, Hash>;

struct State {
    nonce_cache: RefCell<NonceCache>,
    storage: RefCell<Storage<Vec<DeviceData>>>,
    sigs: RefCell<SignatureMap>,
    asset_hashes: RefCell<AssetHashes>,
}

impl Default for State {
    fn default() -> Self {
        Self {
            nonce_cache: RefCell::new(NonceCache::default()),
            storage: RefCell::new(Storage::new((10_000, 8_000_000_000))),
            sigs: RefCell::new(SignatureMap::default()),
            asset_hashes: RefCell::new(AssetHashes::default()),
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
async fn register(device_data: DeviceData, pow: ProofOfWork) -> RegisterResponse {
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
        match store.allocate_user_number() {
            Some(user_number) => {
                store
                    .write(user_number, vec![device_data])
                    .unwrap_or_else(|err| {
                        trap(&format!("failed to store user device data: {}", err))
                    });
                nonce_cache.add(pow.timestamp, pow.nonce);
                RegisterResponse::Registered { user_number }
            }
            None => RegisterResponse::CanisterFull,
        }
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

        prune_expired_signatures(&s.asset_hashes.borrow(), &mut s.sigs.borrow_mut());
    })
}

#[update]
async fn remove(user_number: UserNumber, device_key: DeviceKey) {
    ensure_salt_set().await;
    STATE.with(|s| {
        prune_expired_signatures(&s.asset_hashes.borrow(), &mut s.sigs.borrow_mut());

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
    max_time_to_live: Option<u64>,
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

        let delta = u64::min(
            max_time_to_live.unwrap_or(DEFAULT_EXPIRATION_PERIOD_NS),
            MAX_EXPIRATION_PERIOD_NS,
        );
        let expiration = (time() as u64).saturating_add(delta);

        let seed = calculate_seed(user_number, &frontend);
        let mut sigs = s.sigs.borrow_mut();
        add_signature(&mut sigs, session_key, seed, expiration);
        update_root_hash(&s.asset_hashes.borrow(), &sigs);
        prune_expired_signatures(&s.asset_hashes.borrow(), &mut sigs);

        (
            ByteBuf::from(der_encode_canister_sig_key(seed.to_vec())),
            expiration,
        )
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
        let entries = state
            .storage
            .borrow()
            .read(user_number)
            .unwrap_or_else(|err| {
                trap(&format!(
                    "failed to read device data of user {}: {}",
                    user_number, err
                ))
            });

        trap_if_not_authenticated(entries.iter().map(|e| &e.pubkey));

        match get_signature(
            &state.asset_hashes.borrow(),
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
                signature: ByteBuf::from(signature),
            }),
            None => GetDelegationResponse::NoSuchDelegation,
        }
    })
}

fn encode_metrics(w: &mut MetricsEncoder<Vec<u8>>) -> std::io::Result<()> {
    STATE.with(|s| {
        w.encode_counter(
            "internet_identity_user_count",
            s.storage.borrow().user_count() as f64,
            "Number of users registered in this canister.",
        )?;
        w.encode_gauge(
            "internet_identity_signature_count",
            s.sigs.borrow().len() as f64,
            "Number of active signatures issued by this canister.",
        )?;
        Ok(())
    })
}

#[query]
fn http_request(req: HttpRequest) -> HttpResponse {
    let parts: Vec<&str> = req.url.split("?").collect();
    match parts[0] {
        "/metrics" => {
            let mut writer = MetricsEncoder::new(vec![]);
            match encode_metrics(&mut writer) {
                Ok(()) => {
                    let body = writer.into_inner();
                    HttpResponse {
                        status_code: 200,
                        headers: vec![
                            ("Content-Type".to_string(), "text/plain".to_string()),
                            ("Content-Length".to_string(), body.len().to_string()),
                        ],
                        body: ByteBuf::from(body),
                        streaming_strategy: None,
                    }
                }
                Err(err) => HttpResponse {
                    status_code: 500,
                    headers: vec![],
                    body: ByteBuf::from(format!("Failed to encode metrics: {}", err)),
                    streaming_strategy: None,
                },
            }
        }
        probably_an_asset => {
            let certificate_header = STATE.with(|s| {
                make_asset_certificate_header(
                    &s.asset_hashes.borrow(),
                    &s.sigs.borrow(),
                    probably_an_asset,
                )
            });

            ASSETS.with(|a| match a.borrow().get(probably_an_asset) {
                Some((headers, value)) => {
                    let mut headers = headers.clone();
                    headers.push(certificate_header);

                    HttpResponse {
                        status_code: 200,
                        headers,
                        body: ByteBuf::from(value.clone()),
                        streaming_strategy: None,
                    }
                }
                None => HttpResponse {
                    status_code: 404,
                    headers: vec![certificate_header],
                    body: ByteBuf::from(format!("Asset {} not found.", probably_an_asset)),
                    streaming_strategy: None,
                },
            })
        }
    }
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
    STATE.with(|s| {
        let mut asset_hashes = s.asset_hashes.borrow_mut();

        ASSETS.with(|a| {
            let mut assets = a.borrow_mut();

            let mut add_asset = |name, headers, bytes| {
                asset_hashes.insert(name, hash::hash_bytes(&bytes));
                assets.insert(name.to_string(), (headers, bytes));
            };

            add_asset(
                "/index.html",
                vec![],
                include_bytes!("../../../dist/index.html").to_vec(),
            );
            add_asset(
                "/",
                vec![],
                include_bytes!("../../../dist/index.html").to_vec(),
            );
            add_asset(
                "/authorize",
                vec![],
                include_bytes!("../../../dist/index.html").to_vec(),
            );
            add_asset(
                "/index.js",
                vec![("Content-Encoding".to_string(), "gzip".to_string())],
                include_bytes!("../../../dist/index.js.gz").to_vec(),
            );
            add_asset(
                "/glitch-loop.webp",
                vec![],
                include_bytes!("../../../dist/glitch-loop.webp").to_vec(),
            );
            add_asset(
                "/favicon.ico",
                vec![],
                include_bytes!("../../../dist/favicon.ico").to_vec(),
            );
        });
    });
}

#[init]
fn init(maybe_arg: Option<InternetIdentityInit>) {
    init_assets();
    STATE.with(|state| {
        if let Some(arg) = maybe_arg {
            state
                .storage
                .replace(Storage::new(arg.assigned_user_number_range));
        }
        state.storage.borrow().flush();
        update_root_hash(&state.asset_hashes.borrow(), &state.sigs.borrow());
    });
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
        update_root_hash(&s.asset_hashes.borrow(), &s.sigs.borrow());
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

fn update_root_hash(a: &AssetHashes, m: &SignatureMap) {
    use hashtree::{fork_hash, labeled_hash};

    let prefixed_root_hash = fork_hash(
        &labeled_hash(LABEL_ASSETS, &a.root_hash()),
        &labeled_hash(LABEL_SIG, &m.root_hash()),
    );
    set_certified_data(&prefixed_root_hash[..]);
}

fn get_signature(
    asset_hashes: &AssetHashes,
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

    let tree = hashtree::fork(
        HashTree::Pruned(hashtree::labeled_hash(
            LABEL_ASSETS,
            &asset_hashes.root_hash(),
        )),
        hashtree::labeled(&LABEL_SIG[..], witness),
    );

    #[derive(Serialize)]
    struct Sig<'a> {
        certificate: ByteBuf,
        tree: HashTree<'a>,
    }

    let sig = Sig {
        certificate: ByteBuf::from(certificate),
        tree,
    };

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
}

fn make_asset_certificate_header(
    asset_hashes: &AssetHashes,
    sigs: &SignatureMap,
    asset_name: &str,
) -> (String, String) {
    let certificate = data_certificate().unwrap_or_else(|| {
        trap("data certificate is only available in query calls");
    });
    let witness = asset_hashes.witness(asset_name.as_bytes());
    let tree = hashtree::fork(
        hashtree::labeled(LABEL_ASSETS, witness),
        HashTree::Pruned(hashtree::labeled_hash(LABEL_SIG, &sigs.root_hash())),
    );
    let mut serializer = serde_cbor::ser::Serializer::new(vec![]);
    serializer.self_describe().unwrap();
    tree.serialize(&mut serializer)
        .unwrap_or_else(|e| trap(&format!("failed to serialize a hash tree: {}", e)));
    (
        "IC-Certificate".to_string(),
        format!(
            "certificate=:{}:, tree=:{}:",
            base64::encode(&certificate),
            base64::encode(&serializer.into_inner())
        ),
    )
}

/// Removes a batch of expired signatures from the signature map.
///
/// This function is supposed to piggy back on update calls to
/// amortize the cost of tree pruning.  Each operation on the signature map
/// will prune at most MAX_SIGS_TO_PRUNE other signatures.
fn prune_expired_signatures(asset_hashes: &AssetHashes, sigs: &mut SignatureMap) {
    const MAX_SIGS_TO_PRUNE: usize = 10;
    let num_pruned = sigs.prune_expired(time() as u64, MAX_SIGS_TO_PRUNE);

    if num_pruned > 0 {
        update_root_hash(asset_hashes, sigs);
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
