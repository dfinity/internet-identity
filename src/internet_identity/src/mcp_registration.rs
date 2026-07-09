//! The MCP *registration* delegation: connect-handshake hardening (phase 2).
//!
//! Instead of the frontend fetching the MCP server's session key and calling
//! [`crate::mcp::register`] with it, the connect flow now:
//!
//! 1. mints a short-lived canister-signed delegation `P_reg -> Y` via
//!    [`prepare`] and [`get`], where `Y` is an ephemeral registration key the
//!    *II frontend* generates for this connect (its private half never leaves
//!    the browser) and `P_reg` is a synthetic principal derived from a seed
//!    folding the anchor, the consent parameters (read-only choice, grant
//!    lifetime), and the trusted server URL from the anchor's synced config;
//! 2. lets the frontend extend the chain locally with a second, browser-signed
//!    hop `Y -> X` to the MCP server's registration key `X` (from the connect
//!    link), and deliver the full chain — plus the consent parameters — to the
//!    server's declared callback over a URL fragment;
//! 3. lets the MCP server redeem it: authenticated by the `P_reg -> Y -> X`
//!    chain (so `caller()` *is* `P_reg`), it calls [`register_v2`] echoing the
//!    consent parameters, with the long-lived session key `S` it generated.
//!
//! **Nothing is stored.** [`register_v2`] re-derives the seed from its
//! arguments and the anchor's *current* config, and compares the resulting
//! principal to `caller()` — derive-and-compare, like the OpenID delegation
//! principals. Only the authenticated user could have obtained a delegation
//! for that exact seed, so a match proves the presented parameters are the
//! consented ones: the server can neither register for a different anchor nor
//! alter the read-only choice or grant lifetime, because any altered tuple
//! derives a different principal. Folding the trusted server URL means a
//! config change (new trusted server, or MCP disabled) between consent and
//! redemption also invalidates the delegation — derivation no longer matches.
//!
//! The two-hop chain shape is load-bearing, not an implementation detail:
//! everything this canister signs transits the IC in the clear (update
//! arguments, and the [`get`] query response passes the answering replica and
//! the API boundary nodes), so the canister-signed hop must never be
//! redeemable on its own. Delegating to the browser-held `Y` keeps it inert to
//! any transport-level observer; the only redeemable artifact — the full chain
//! — is assembled inside the consenting browser and never transits the IC.
//!
//! With no stored entry there is no single-use marker: a delegation is
//! redeemable until it expires ([`MCP_REGISTRATION_DELEGATION_TTL_NS`], 5
//! minutes). That is bounded by the same facts that bound a grant: the chain
//! is only ever delivered to the trusted server's declared callback, only the
//! holder of `X`'s private key can redeem it, and the anchor holds at most one
//! session at a time (a re-registration replaces the previous grant). Retries
//! are trivially idempotent — re-registering the same key re-binds it.

use crate::authz_utils::check_authorization;
use crate::delegation::{add_delegation_signature, der_encode_canister_sig_key};
use crate::{mcp, state, update_root_hash};
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

/// Lifetime of a registration delegation: 5 minutes. Long enough to cover the
/// browser hops (II frontend `get` query, top-level navigation to the server's
/// declared callback, the MCP server redeeming the chain) and short enough to
/// bound how long an abandoned or superseded delegation stays redeemable.
/// Deliberately not shorter: delegation expiry is checked at ingress against
/// the receiving node's clock, and IC gateways/libraries permit ~5 minutes of
/// clock drift — a tighter window could be partly (or wholly) consumed by skew
/// between this subnet's time and the validator's.
pub const MCP_REGISTRATION_DELEGATION_TTL_NS: u64 = 5 * 60 * 1_000_000_000;

/// Default session-grant lifetime when the caller omits `max_ttl`: 1 hour,
/// matching the frontend's default connect TTL. `mcp::register` still clamps to
/// [10 min, 30 days]. Named explicitly (rather than falling through to the clamp
/// minimum) so an omitted argument has a documented, unsurprising meaning. Both
/// [`prepare`] and [`register_v2`] resolve an omitted value to this constant
/// *before* deriving the seed, so the two sides always fold the same number.
pub const DEFAULT_MCP_GRANT_TTL_NS: u64 = 60 * 60 * 1_000_000_000;

/// Seed for the registration principal `P_reg`: folds the anchor, the consent
/// parameters (read-only choice, resolved grant lifetime), and the trusted
/// server URL from the anchor's synced config. No per-connect nonce and no
/// key material — the delegated-to key `Y` lives in the signed *message*, not
/// the seed — so [`register_v2`] can re-derive the principal purely from its
/// call arguments plus current state and compare it to `caller()`
/// (derive-and-compare; nothing is stored). Every field is length-prefixed and
/// the URL is hashed to fixed length, and a dedicated domain separator keeps
/// this principal namespace disjoint from the session, account, and
/// email-recovery seeds.
fn mcp_registration_seed(
    anchor_number: AnchorNumber,
    read_only: bool,
    grant_ttl_ns: u64,
    trusted_url: &str,
) -> Hash {
    const DOMAIN_SEPARATOR: &[u8] = b"mcp-registration";

    let salt = state::salt();
    let anchor_bytes = anchor_number.to_le_bytes();
    let read_only_byte = [read_only as u8];
    let ttl_bytes = grant_ttl_ns.to_le_bytes();
    let url_hash = Sha256::digest(trusted_url.as_bytes());

    let mut blob: Vec<u8> = Vec::new();
    blob.push(salt.len() as u8);
    blob.extend_from_slice(&salt);
    blob.push(DOMAIN_SEPARATOR.len() as u8);
    blob.extend_from_slice(DOMAIN_SEPARATOR);
    blob.push(anchor_bytes.len() as u8);
    blob.extend_from_slice(&anchor_bytes);
    blob.push(read_only_byte.len() as u8);
    blob.extend_from_slice(&read_only_byte);
    blob.push(ttl_bytes.len() as u8);
    blob.extend_from_slice(&ttl_bytes);
    blob.push(url_hash.len() as u8);
    blob.extend_from_slice(&url_hash);

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

/// The anchor's trusted server URL, required by every entry point here: the
/// seed folds it, so no registration delegation can exist (or be redeemed)
/// without a live trusted-server config — and any config change invalidates
/// in-flight delegations by changing the derivation.
fn trusted_url(anchor_number: AnchorNumber) -> Result<String, String> {
    let config = mcp::get_mcp_config(anchor_number);
    match (config.enabled, config.url) {
        (true, Some(url)) => Ok(url),
        _ => Err(
            "MCP registration failed: no trusted MCP server is enabled for this identity."
                .to_string(),
        ),
    }
}

/// `prepare_mcp_registration_delegation`: mint the `P_reg -> Y` delegation
/// (`Y` = the frontend's ephemeral, browser-held registration key — see the
/// module doc for why the canister never delegates to a link-supplied key).
/// The consent (anchor, read-only choice, grant lifetime, trusted server) is
/// folded into the seed rather than stored. Authenticated as the user (full
/// authorization) — only the consenting user can create a registration
/// delegation for their anchor.
pub async fn prepare(
    anchor_number: AnchorNumber,
    registration_key: SessionKey,
    permissions: Option<Permissions>,
    max_ttl: Option<u64>,
) -> Result<PrepareMcpRegistrationDelegation, String> {
    check_authorization(anchor_number)
        .map_err(|err| format!("{} could not be authenticated.", err.principal))?;
    // A degenerate empty key would make the signed message trivially
    // predictable (mirrors `mcp::register`'s check).
    if registration_key.is_empty() {
        return Err("MCP registration failed: empty registration key.".to_string());
    }
    let url = trusted_url(anchor_number)?;
    state::ensure_salt_set().await;

    // Session-grant lifetime the user chose at connect; an omitted value means
    // the documented default, not the clamp minimum. `mcp::register` clamps.
    let grant_ttl_ns = max_ttl.unwrap_or(DEFAULT_MCP_GRANT_TTL_NS);
    let read_only = read_only_from(permissions);

    let expiration = time().saturating_add(MCP_REGISTRATION_DELEGATION_TTL_NS);
    let seed = mcp_registration_seed(anchor_number, read_only, grant_ttl_ns, &url);

    state::signature_map_mut(|sigs| {
        // No queries-only restriction on the registration delegation itself: it
        // authenticates one update (mcp_register_v2). The user's read-only
        // choice is folded into the seed above, not into this signature.
        add_delegation_signature(sigs, registration_key, &seed, expiration, None);
    });
    update_root_hash();

    Ok(PrepareMcpRegistrationDelegation {
        user_key: ByteBuf::from(der_encode_canister_sig_key(seed.to_vec())),
        expiration,
    })
}

/// `get_mcp_registration_delegation`: fetch the certified `P_reg -> Y`
/// delegation prepared above, for the frontend to extend browser-side with the
/// `Y -> X` hop and deliver. Takes the same consent parameters as [`prepare`]
/// because the seed must be re-derived from call arguments (this is a query —
/// it can look nothing up that isn't derivable). Authenticated as the user.
pub fn get(
    anchor_number: AnchorNumber,
    registration_key: SessionKey,
    permissions: Option<Permissions>,
    max_ttl: Option<u64>,
    expiration: Timestamp,
) -> Result<SignedDelegation, String> {
    check_authorization(anchor_number)
        .map_err(|err| format!("{} could not be authenticated.", err.principal))?;

    // No delegation could exist before the salt was initialised (`prepare`
    // awaits `ensure_salt_set`); skip the seed derivation, which would trap on
    // `state::salt()`, and report the delegation as absent instead.
    let salt_initialised = state::storage_borrow(|storage| storage.salt().is_some());
    if !salt_initialised {
        return Err("MCP registration failed: no such delegation.".to_string());
    }
    let url = trusted_url(anchor_number)?;

    let grant_ttl_ns = max_ttl.unwrap_or(DEFAULT_MCP_GRANT_TTL_NS);
    let read_only = read_only_from(permissions);
    let seed = mcp_registration_seed(anchor_number, read_only, grant_ttl_ns, &url);
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
/// consenting anchor. Authenticated by the `P_reg -> Y -> X` chain (so
/// `caller()` is `P_reg`), and authorized by derive-and-compare: the seed is
/// re-derived from the presented parameters and the anchor's *current* config,
/// and must land exactly on `caller()`. A tuple the user never consented to —
/// a different anchor, an upgraded access level, a longer lifetime, or a
/// config that has since changed — derives a different principal and is
/// rejected. Nothing is stored or consumed: the delegation is redeemable until
/// it expires (see the module doc for why that is bounded).
pub fn register_v2(
    anchor_number: AnchorNumber,
    session_key: SessionKey,
    permissions: Option<Permissions>,
    max_ttl: Option<u64>,
) -> Result<McpRegistrationV2, String> {
    // Before the salt exists no delegation was ever minted, and deriving would
    // trap; report the caller as unauthorized instead.
    let salt_initialised = state::storage_borrow(|storage| storage.salt().is_some());
    if !salt_initialised {
        return Err(
            "MCP registration failed: not authorized by a registration delegation.".to_string(),
        );
    }
    let url = trusted_url(anchor_number)?;

    let grant_ttl_ns = max_ttl.unwrap_or(DEFAULT_MCP_GRANT_TTL_NS);
    let read_only = read_only_from(permissions);
    let seed = mcp_registration_seed(anchor_number, read_only, grant_ttl_ns, &url);
    let p_reg = Principal::self_authenticating(der_encode_canister_sig_key(seed.to_vec()));
    if caller() != p_reg {
        return Err(
            "MCP registration failed: not authorized by a registration delegation.".to_string(),
        );
    }

    // The tuple is authenticated; bind S under it (config /
    // one-session-per-anchor invariants and the TTL clamp live in
    // `mcp::register`).
    let registration = mcp::register(anchor_number, session_key, grant_ttl_ns, read_only)?;
    Ok(McpRegistrationV2 {
        expiration: registration.expiration,
        permissions: permissions_of(read_only),
    })
}
