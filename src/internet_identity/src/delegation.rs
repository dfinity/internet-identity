use crate::ii_domain::IIDomain;
use crate::stats::event_stats::{
    update_event_based_stats, Event, EventData, PrepareDelegationEvent,
};
use crate::{state, DAY_NS, MINUTE_NS};
use candid::Principal;
use ic_canister_sig_creation::signature_map::{CanisterSigInputs, SignatureMap};
use ic_canister_sig_creation::{CanisterSigPublicKey, DELEGATION_SIG_DOMAIN};
use ic_cdk::{id, trap};
use ic_certification::Hash;
use internet_identity_interface::internet_identity::types::*;
use sha2::{Digest, Sha256};
use std::net::IpAddr;

// The expiration used for delegations if none is specified
// (calculated as now() + this)
pub const DEFAULT_EXPIRATION_PERIOD_NS: u64 = 30 * MINUTE_NS;

// The maximum expiration time for delegation
// (calculated as now() + this)
pub const MAX_EXPIRATION_PERIOD_NS: u64 = 30 * DAY_NS;

// The prefix used in all account seed calculations to avoid collisions
// with the primary acount seed calculations based on anchor number.
const ACCOUNT_SEED_PREFIX: &str = "<account>";

/// Update metrics and the list of latest front-end origins.
pub fn delegation_bookkeeping(
    frontend: FrontendHostname,
    ii_domain: Option<IIDomain>,
    session_duration_ns: u64,
) {
    state::usage_metrics_mut(|metrics| {
        metrics.delegation_counter += 1;
    });
    if !is_dev_frontend(&frontend) {
        update_event_based_stats(EventData {
            event: Event::PrepareDelegation(PrepareDelegationEvent {
                ii_domain: ii_domain.clone(),
                frontend: frontend.clone(),
                session_duration_ns,
            }),
        });
    }
}

/// Filter out derivation origins that most likely point to development setups.
/// This is not bulletproof but given the data we collected so far it should be good for now.
fn is_dev_frontend(frontend: &FrontendHostname) -> bool {
    if frontend.starts_with("http://") || frontend.contains("localhost") {
        // we don't care about insecure origins or localhost
        return true;
    }

    // let's check for local IP addresses
    if let Some(hostname) = frontend
        .strip_prefix("https://")
        .and_then(|s| s.split(':').next())
    {
        return match hostname.parse::<IpAddr>() {
            Ok(IpAddr::V4(addr)) => addr.is_private() || addr.is_loopback(),
            Ok(IpAddr::V6(addr)) => addr.is_loopback(),
            Err(_) => false,
        };
    }
    false
}

pub fn get_principal(anchor_number: AnchorNumber, frontend: FrontendHostname) -> Principal {
    check_frontend_length(&frontend);

    let seed = calculate_anchor_seed(anchor_number, &frontend);
    let public_key = der_encode_canister_sig_key(seed.to_vec());
    Principal::self_authenticating(public_key)
}

pub fn calculate_anchor_seed(anchor_number: AnchorNumber, frontend: &FrontendHostname) -> Hash {
    let salt = state::salt();

    let mut blob: Vec<u8> = vec![];
    blob.push(salt.len() as u8);
    blob.extend_from_slice(&salt);

    let anchor_number_str = anchor_number.to_string();
    let anchor_number_blob = anchor_number_str.bytes();
    blob.push(anchor_number_blob.len() as u8);
    blob.extend(anchor_number_blob);

    blob.push(frontend.len() as u8);
    blob.extend(frontend.bytes());

    hash_bytes(blob)
}

/// Calculate a seed only from an `AccountNumber` and `FrontendHostname`.
/// This is only called when we're not dealing with a default account.
/// The anchor number is not included because accounts are not tied to specific anchors.
pub fn calculate_account_seed(account_number: AccountNumber, frontend: &FrontendHostname) -> Hash {
    let salt = state::salt();

    let mut blob: Vec<u8> = vec![];
    blob.push(salt.len() as u8);
    blob.extend_from_slice(&salt);

    blob.push(ACCOUNT_SEED_PREFIX.len() as u8);
    blob.extend(ACCOUNT_SEED_PREFIX.bytes());

    let account_number_str = account_number.to_string();
    let account_number_blob = account_number_str.bytes();
    blob.push(account_number_blob.len() as u8);
    blob.extend(account_number_blob);

    blob.push(frontend.len() as u8);
    blob.extend(frontend.bytes());

    hash_bytes(blob)
}

fn hash_bytes(value: impl AsRef<[u8]>) -> Hash {
    let mut hasher = Sha256::new();
    hasher.update(value.as_ref());
    hasher.finalize().into()
}

pub(crate) fn der_encode_canister_sig_key(seed: Vec<u8>) -> Vec<u8> {
    let my_canister_id = id();
    CanisterSigPublicKey::new(my_canister_id, seed).to_der()
}

/// Adds a delegation signature for `pk` to the signature map. `permissions`
/// is the delegation's optional `permissions` field (folded into the signed
/// message when present) — pass `access.permissions()` of a
/// [`DelegationAccess`], or `None` for flows that never restrict.
pub fn add_delegation_signature(
    sigs: &mut SignatureMap,
    pk: PublicKey,
    seed: &[u8],
    expiration: Timestamp,
    permissions: Option<&str>,
) {
    let inputs = CanisterSigInputs {
        domain: DELEGATION_SIG_DOMAIN,
        seed,
        message: &delegation_signature_msg_with_permissions(&pk, expiration, None, permissions),
    };
    sigs.add_signature(&inputs);
}

/// The value of a delegation's `permissions` field that restricts the
/// sender to query calls: the IC rejects update calls authenticated
/// through such a delegation.
pub const DELEGATION_PERMISSIONS_QUERIES: &str = "queries";

/// The access level a delegation grants. Typed (rather than a bare bool) so
/// call sites read as `DelegationAccess::ReadOnly`, not a blind `true`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DelegationAccess {
    /// Update-capable: the delegation carries no `permissions` field.
    Unrestricted,
    /// Queries-only: the delegation carries `permissions = "queries"`, so the
    /// IC rejects update calls authenticated through it.
    ReadOnly,
}

impl DelegationAccess {
    /// Maps a `read_only` flag (e.g. the persisted MCP grant field, which is
    /// stored as a bool).
    pub fn from_read_only(read_only: bool) -> Self {
        if read_only {
            DelegationAccess::ReadOnly
        } else {
            DelegationAccess::Unrestricted
        }
    }

    /// The delegation's `permissions` field value for this access level.
    pub fn permissions(self) -> Option<&'static str> {
        match self {
            DelegationAccess::Unrestricted => None,
            DelegationAccess::ReadOnly => Some(DELEGATION_PERMISSIONS_QUERIES),
        }
    }
}

impl From<Permissions> for DelegationAccess {
    fn from(permissions: Permissions) -> Self {
        match permissions {
            Permissions::All => DelegationAccess::Unrestricted,
            Permissions::Queries => DelegationAccess::ReadOnly,
        }
    }
}

impl From<Option<Permissions>> for DelegationAccess {
    /// Maps the endpoints' trailing `permissions : opt Permissions` candid
    /// argument. An omitted argument means unrestricted: this preserves the
    /// original behavior for callers of the pre-feature form and matches the
    /// interface spec's default for an absent `permissions` field. First-party
    /// callers always pass an explicit value.
    fn from(permissions: Option<Permissions>) -> Self {
        permissions.map_or(DelegationAccess::Unrestricted, DelegationAccess::from)
    }
}

/// Like `ic_canister_sig_creation::delegation_signature_msg`, but
/// additionally supports the delegation's optional `permissions` field
/// (see the IC interface specification). Produces the identical message
/// when `permissions` is `None`.
pub fn delegation_signature_msg_with_permissions(
    pubkey: &[u8],
    expiration: Timestamp,
    targets: Option<&Vec<Vec<u8>>>,
    permissions: Option<&str>,
) -> Vec<u8> {
    use ic_representation_independent_hash::{representation_independent_hash, Value};

    let mut m: Vec<(String, Value)> = vec![
        ("pubkey".into(), Value::Bytes(pubkey.to_vec())),
        ("expiration".into(), Value::Number(expiration)),
    ];
    if let Some(targets) = targets {
        m.push((
            "targets".into(),
            Value::Array(targets.iter().map(|t| Value::Bytes(t.to_vec())).collect()),
        ));
    }
    if let Some(permissions) = permissions {
        m.push(("permissions".into(), Value::String(permissions.to_string())));
    }
    representation_independent_hash(m.as_slice()).to_vec()
}

pub(crate) fn check_frontend_length(frontend: &FrontendHostname) {
    const FRONTEND_HOSTNAME_LIMIT: usize = 255;

    let n = frontend.len();
    if frontend.len() > FRONTEND_HOSTNAME_LIMIT {
        trap(&format!(
            "frontend hostname {n} exceeds the limit of {FRONTEND_HOSTNAME_LIMIT} bytes",
        ));
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const SESSION_PUBKEY: &[u8] = b"test session public key";
    const EXPIRATION: Timestamp = 1_700_000_000_000_000_000;

    /// Without `permissions`, the message must be byte-identical to the
    /// upstream helper's: legacy (unrestricted) delegations must keep
    /// verifying against signatures produced before this feature existed.
    #[test]
    fn should_match_upstream_msg_when_permissions_absent() {
        assert_eq!(
            delegation_signature_msg_with_permissions(SESSION_PUBKEY, EXPIRATION, None, None),
            ic_canister_sig_creation::delegation_signature_msg(SESSION_PUBKEY, EXPIRATION, None),
        );
    }

    /// Pins the signable of a queries-only delegation: the representation-
    /// independent hash of `{pubkey, expiration, permissions: "queries"}`
    /// (the canister signature then covers the delegation signature domain
    /// separator plus these bytes). Agents and the replica recompute this
    /// independently, so any drift in the composition silently invalidates
    /// every read-only delegation — this test makes drift a visible failure
    /// and doubles as a reference vector for agent implementers.
    #[test]
    fn should_pin_queries_permissions_msg() {
        let msg = delegation_signature_msg_with_permissions(
            SESSION_PUBKEY,
            EXPIRATION,
            None,
            Some(DELEGATION_PERMISSIONS_QUERIES),
        );
        assert_eq!(
            hex::encode(&msg),
            "061d792a31da1cedbbb4ed1a4234c12645d021e6a81ef1598bbe27124f33a4c6",
            "queries-only delegation signable changed; \
             existing read-only delegations would stop verifying"
        );
    }
}
