//! The MCP *registration* delegation: connect-handshake hardening (phase 2).
//!
//! Instead of the frontend fetching the MCP server's session key and calling
//! [`crate::mcp::register`] with it, the connect flow now:
//!
//! 1. mints a short-lived, single-use canister-signed delegation `P_reg -> X`
//!    ([`prepare`] + [`get`]), where `X` is a registration key the MCP server
//!    generated for the browser session and `P_reg` is a synthetic principal
//!    derived from a per-connect seed folding in the anchor and `X`;
//! 2. delivers that delegation to the MCP frontend over a URL fragment (see the
//!    `/mcp` frontend), never a server-visible channel;
//! 3. lets the MCP server redeem it: authenticated by the `P_reg -> X` chain
//!    (so `caller()` *is* `P_reg`), it calls [`register_v2`] with the long-lived
//!    session key `S` it generated.
//!
//! What the user consented to — the anchor and the read-only choice — is
//! recorded on an index entry ([`StorableMcpRegistration`]) at [`prepare`] time,
//! under full user authorization, and recovered from `caller()` at
//! [`register_v2`] time. So the server can neither register a session for a
//! different anchor nor upgrade a read-only session to full access, and II never
//! binds a key it merely received. The entry is deleted on the first successful
//! [`register_v2`] (single-use); a boundary retry with the same `S` is served
//! idempotently from the grant that first call created.

use crate::authz_utils::check_authorization;
use crate::delegation::{add_delegation_signature, der_encode_canister_sig_key};
use crate::storage::storable::mcp_registration::StorableMcpRegistration;
use crate::{state, update_root_hash};
use candid::Principal;
use ic_canister_sig_creation::{
    delegation_signature_msg, signature_map::CanisterSigInputs, DELEGATION_SIG_DOMAIN,
};
use ic_cdk::api::time;
use ic_cdk::caller;
use ic_certification::Hash;
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, Delegation, McpRegistrationV2, Permissions, PrepareMcpRegistrationDelegation,
    SessionKey, SignedDelegation, Timestamp,
};
use serde_bytes::ByteBuf;
use sha2::{Digest, Sha256};

/// Lifetime of a registration delegation. Long enough to cover the browser hops
/// (II frontend `get` query, top-level navigation to the pinned callback, the
/// MCP server redeeming the chain) and short enough to bound how long a leaked
/// or abandoned entry is useful.
pub const MCP_REGISTRATION_DELEGATION_TTL_NS: u64 = 90 * 1_000_000_000;

/// Seed for the registration principal `P_reg`. Folds a fixed-length hash of the
/// registration key `X` into the per-anchor seed, so `P_reg` is unique per
/// connect (each connect has a fresh `X`) without a separate nonce. A dedicated
/// domain separator keeps this principal namespace disjoint from the session,
/// account, and email-recovery seeds (pure seed hygiene — II is the only signer
/// that could ever collide).
fn mcp_registration_seed(anchor_number: AnchorNumber, registration_key: &[u8]) -> Hash {
    const DOMAIN_SEPARATOR: &[u8] = b"mcp-registration";

    let salt = state::salt();
    let anchor_bytes = anchor_number.to_le_bytes();
    let key_hash = Sha256::digest(registration_key);

    let mut blob: Vec<u8> = Vec::new();
    blob.push(salt.len() as u8);
    blob.extend_from_slice(&salt);
    blob.push(DOMAIN_SEPARATOR.len() as u8);
    blob.extend_from_slice(DOMAIN_SEPARATOR);
    blob.push(anchor_bytes.len() as u8);
    blob.extend_from_slice(&anchor_bytes);
    blob.push(key_hash.len() as u8);
    blob.extend_from_slice(&key_hash);

    let mut hasher = Sha256::new();
    hasher.update(&blob);
    hasher.finalize().into()
}

fn read_only_from(permissions: Option<Permissions>) -> bool {
    matches!(permissions, Some(Permissions::Queries))
}

fn permissions_of(read_only: bool) -> Permissions {
    if read_only {
        Permissions::Queries
    } else {
        Permissions::All
    }
}

/// `prepare_mcp_registration_delegation`: mint the `P_reg -> X` delegation and
/// record the consent (anchor, read-only choice, grant lifetime) on an index
/// entry. Authenticated as the user (full authorization) — only the consenting
/// user can create a registration delegation for their anchor.
pub async fn prepare(
    anchor_number: AnchorNumber,
    registration_key: SessionKey,
    permissions: Option<Permissions>,
    grant_ttl_ns: u64,
) -> Result<PrepareMcpRegistrationDelegation, String> {
    check_authorization(anchor_number)
        .map_err(|err| format!("{} could not be authenticated.", err.principal))?;
    state::ensure_salt_set().await;

    let now = time();
    let expiration = now.saturating_add(MCP_REGISTRATION_DELEGATION_TTL_NS);
    let seed = mcp_registration_seed(anchor_number, &registration_key);
    let principal = Principal::self_authenticating(der_encode_canister_sig_key(seed.to_vec()));

    // Reject a duplicate live entry for the same P_reg (the same registration key
    // being prepared twice concurrently). Fresh connects use a fresh key, so this
    // never fires in normal use; it just refuses to overwrite an in-flight one.
    if let Some(existing) =
        state::storage_borrow(|storage| storage.lookup_mcp_registration(principal))
    {
        if existing.expires_at_ns > now {
            return Err(
                "MCP registration failed: a registration for this key is already in progress."
                    .to_string(),
            );
        }
    }

    let read_only = read_only_from(permissions);

    state::signature_map_mut(|sigs| {
        // No queries-only restriction on the registration delegation itself: it
        // authenticates one update (mcp_register_v2). The user's read-only choice
        // is recorded on the index entry below, not folded into this signature.
        add_delegation_signature(sigs, registration_key, &seed, expiration, None);
    });
    update_root_hash();

    state::storage_borrow_mut(|storage| {
        storage.insert_mcp_registration(
            principal,
            StorableMcpRegistration {
                anchor_number,
                read_only,
                grant_ttl_ns,
                expires_at_ns: expiration,
            },
        );
    });

    Ok(PrepareMcpRegistrationDelegation {
        user_key: ByteBuf::from(der_encode_canister_sig_key(seed.to_vec())),
        expiration,
    })
}

/// `get_mcp_registration_delegation`: fetch the certified `P_reg -> X`
/// delegation prepared above. Authenticated as the user, like [`prepare`].
pub fn get(
    anchor_number: AnchorNumber,
    registration_key: SessionKey,
    expiration: Timestamp,
) -> Result<SignedDelegation, String> {
    check_authorization(anchor_number)
        .map_err(|err| format!("{} could not be authenticated.", err.principal))?;

    // No delegation could exist before the salt was initialised (`prepare` awaits
    // `ensure_salt_set`); skip the seed derivation, which would trap on
    // `state::salt()`, and report the delegation as absent instead.
    let salt_initialised = state::storage_borrow(|storage| storage.salt().is_some());
    if !salt_initialised {
        return Err("MCP registration failed: no such delegation.".to_string());
    }

    let seed = mcp_registration_seed(anchor_number, &registration_key);
    state::assets_and_signatures(|certified_assets, sigs| {
        let inputs = CanisterSigInputs {
            domain: DELEGATION_SIG_DOMAIN,
            seed: &seed,
            message: &delegation_signature_msg(&registration_key, expiration, None),
        };
        match sigs.get_signature_as_cbor(&inputs, Some(certified_assets.root_hash())) {
            Ok(signature) => Ok(SignedDelegation {
                delegation: Delegation {
                    pubkey: registration_key,
                    expiration,
                    targets: None,
                    permissions: None,
                },
                signature: ByteBuf::from(signature),
            }),
            Err(_) => Err("MCP registration failed: no such delegation.".to_string()),
        }
    })
}

/// `mcp_register_v2`: bind the MCP server's long-lived session key `S` to the
/// consenting anchor. Authenticated by the `P_reg -> X` chain (so `caller()` is
/// `P_reg`); the anchor and read-only choice come from the index entry, never
/// from an argument. Single-use: the entry is deleted on success, and a boundary
/// retry with the same `S` is served idempotently from the resulting grant.
pub fn register_v2(session_key: SessionKey) -> Result<McpRegistrationV2, String> {
    let caller = caller();
    let now = time();

    match state::storage_borrow(|storage| storage.lookup_mcp_registration(caller)) {
        Some(entry) if entry.expires_at_ns > now => {
            // First redemption. Bind S with the recorded anchor + read-only
            // choice (config/one-session-per-anchor invariants live in
            // `mcp::register`), then delete the entry to make the chain
            // single-use.
            let registration = crate::mcp::register(
                entry.anchor_number,
                session_key,
                entry.grant_ttl_ns,
                entry.read_only,
            )?;
            state::storage_borrow_mut(|storage| storage.remove_mcp_registration(caller));
            Ok(McpRegistrationV2 {
                expiration: registration.expiration,
                permissions: permissions_of(entry.read_only),
            })
        }
        Some(_) => {
            // Expired: clean it up and report it gone.
            state::storage_borrow_mut(|storage| storage.remove_mcp_registration(caller));
            Err("MCP registration failed: the registration delegation has expired.".to_string())
        }
        None => {
            // No live entry: either already used (single-use spent) or a boundary
            // retry of a call that already committed. Serve the retry idempotently
            // from the grant the first call created, keyed by S's principal.
            let principal = Principal::self_authenticating(&session_key);
            match state::storage_borrow(|storage| storage.lookup_mcp_grant(principal)) {
                Some(grant) if grant.expires_at_ns > now => Ok(McpRegistrationV2 {
                    expiration: grant.expires_at_ns,
                    permissions: permissions_of(grant.read_only),
                }),
                _ => Err(
                    "MCP registration failed: no registration delegation for this caller."
                        .to_string(),
                ),
            }
        }
    }
}
