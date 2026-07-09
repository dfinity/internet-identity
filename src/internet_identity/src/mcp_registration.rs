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
//!    link), and deliver the full chain — plus the echoed consent parameters —
//!    to the server's declared callback over a URL fragment;
//! 3. lets the MCP server redeem it: authenticated by the `P_reg -> Y -> X`
//!    chain (so `caller()` *is* `P_reg`), it calls [`register_v2`] echoing the
//!    read-only choice and grant lifetime, with the long-lived session key `S`
//!    it generated.
//!
//! **What is stored is deliberately minimal: only the anchor.** At [`prepare`]
//! time (authenticated as the user) a tiny index entry
//! ([`StorableMcpRegistration`]) records `P_reg -> anchor`. [`register_v2`]
//! looks that up by `caller()` (== `P_reg`) to recover the anchor
//! **server-side** — so the anchor number, the one identifier II otherwise
//! never hands a relying party, is never a `register_v2` argument and never
//! reaches the MCP server (which has no use for it: every later `mcp_*` call
//! recovers the anchor from the session-key grant).
//!
//! The rest of the consent — the read-only choice and the grant lifetime — is
//! *not* stored. It is folded into `P_reg`'s seed, so the server remembers and
//! echoes those two, and [`register_v2`] validates them by re-deriving `P_reg`
//! from (recovered anchor, echoed params, current trusted-server URL) and
//! comparing to `caller()`: an altered echo derives a different principal and
//! is rejected, so the server can neither upgrade a read-only session nor
//! stretch the grant. Folding the trusted server URL means a config change
//! (new trusted server, or MCP disabled) between consent and redemption also
//! invalidates the delegation — derivation no longer matches.
//!
//! The two-hop chain shape is load-bearing, not an implementation detail:
//! everything this canister signs transits the IC in the clear (update
//! arguments, and the [`get`] query response passes the answering replica and
//! the API boundary nodes), so the canister-signed hop must never be
//! redeemable on its own. Delegating to the browser-held `Y` keeps it inert to
//! any transport-level observer; the only redeemable artifact — the full chain
//! — is assembled inside the consenting browser and never transits the IC.
//!
//! The delegation is multi-use within its short lifetime
//! ([`MCP_REGISTRATION_DELEGATION_TTL_NS`], 5 minutes): a boundary retry simply
//! re-binds. That is bounded by the same facts that bound a grant: the chain is
//! only ever delivered to the trusted server's declared callback, only the
//! holder of `X`'s private key can redeem it, and the anchor holds at most one
//! session at a time (a re-registration replaces the previous grant). The index
//! entry is retained until a lookup finds it expired, then pruned.

use crate::authz_utils::check_authorization;
use crate::delegation::{add_delegation_signature, der_encode_canister_sig_key};
use crate::storage::storable::mcp_registration::StorableMcpRegistration;
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
/// matching the frontend's default connect TTL. Named explicitly (rather than
/// falling through to the clamp minimum) so an omitted argument has a
/// documented, unsurprising meaning.
pub const DEFAULT_MCP_GRANT_TTL_NS: u64 = 60 * 60 * 1_000_000_000;

/// The effective session-grant lifetime for a requested `max_ttl`: the
/// documented default when omitted, clamped to `mcp::register`'s [10 min,
/// 30 days] bounds. Every entry point resolves the argument through this
/// *before* deriving the seed, so the TTL folded into `P_reg` is the one the
/// grant actually gets — if the seed folded the raw request instead, a
/// below-minimum value would pass derive-and-compare yet mint a grant
/// *longer* than the consent-bound number once `mcp::register` clamps it.
fn resolve_grant_ttl(max_ttl: Option<u64>) -> u64 {
    max_ttl
        .unwrap_or(DEFAULT_MCP_GRANT_TTL_NS)
        .clamp(mcp::MCP_GRANT_MIN_TTL_NS, mcp::MCP_GRANT_MAX_TTL_NS)
}

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
/// The full consent (anchor, read-only choice, grant lifetime, trusted server)
/// is folded into the seed; only the *anchor* is additionally stored (keyed by
/// `P_reg`), so [`register_v2`] can recover it server-side rather than take it
/// from the server. Authenticated as the user (full authorization) — only the
/// consenting user can create a registration delegation for their anchor.
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

    // The effective session-grant lifetime (defaulted and clamped), so the
    // seed binds the number the grant will actually get.
    let grant_ttl_ns = resolve_grant_ttl(max_ttl);
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

    // Record only the anchor, keyed by P_reg, so register_v2 recovers it
    // server-side (never from an argument the server controls, so the anchor
    // number is never disclosed to the MCP server). read-only / grant TTL stay
    // in the seed, validated at redemption by re-derivation.
    let user_key = der_encode_canister_sig_key(seed.to_vec());
    let p_reg = Principal::self_authenticating(&user_key);
    state::storage_borrow_mut(|storage| {
        storage.insert_mcp_registration(
            p_reg,
            StorableMcpRegistration {
                anchor_number,
                expires_at_ns: expiration,
            },
        );
    });

    Ok(PrepareMcpRegistrationDelegation {
        user_key: ByteBuf::from(user_key),
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

    let grant_ttl_ns = resolve_grant_ttl(max_ttl);
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
/// `caller()` is `P_reg`). The anchor is recovered **server-side** from the
/// index entry keyed by `caller()` — never taken as an argument, so it is
/// never disclosed to the server. The read-only choice and grant lifetime *are*
/// arguments (the server echoes what it remembered), authorized by
/// derive-and-compare: the seed is re-derived from (recovered anchor, echoed
/// params, the anchor's *current* trusted-server URL) and must land exactly on
/// `caller()`. An echo the user never consented to — an upgraded access level,
/// a longer lifetime, or a config that has since changed — derives a different
/// principal and is rejected. Multi-use within the delegation's short lifetime:
/// the entry is retained (a retry re-binds), and removed once found expired.
pub fn register_v2(
    session_key: SessionKey,
    permissions: Option<Permissions>,
    max_ttl: Option<u64>,
) -> Result<McpRegistrationV2, String> {
    // The one error this public method reports for anything short of an
    // authorized redemption. In particular a missing entry, an expired entry,
    // and a disabled/changed config are all indistinguishable from a derivation
    // mismatch: `register_v2` can be called by anyone, and a distinct error
    // would be an oracle for probing anchors or their MCP config. (`prepare` /
    // `get` keep specific errors — they authenticate as the identity first.)
    const NOT_AUTHORIZED: &str =
        "MCP registration failed: not authorized by a registration delegation.";

    // Recover the consented anchor from the index entry keyed by caller()
    // (== P_reg). No entry ⇒ this caller never received a registration
    // delegation. An expired entry authorizes nothing and is pruned. (Also
    // covers the pre-salt case: no entry could exist before the salt was set.)
    let p_reg = caller();
    let Some(entry) = state::storage_borrow(|storage| storage.lookup_mcp_registration(p_reg))
    else {
        return Err(NOT_AUTHORIZED.to_string());
    };
    if time() > entry.expires_at_ns {
        state::storage_borrow_mut(|storage| storage.remove_mcp_registration(p_reg));
        return Err(NOT_AUTHORIZED.to_string());
    }
    let anchor_number = entry.anchor_number;

    // The anchor's current trusted-server URL is folded into the seed, so a
    // config change (or disable) since consent breaks the re-derivation below.
    let Ok(url) = trusted_url(anchor_number) else {
        return Err(NOT_AUTHORIZED.to_string());
    };

    // Validate the echoed read-only / grant-lifetime by re-deriving P_reg from
    // (recovered anchor, echoed params, current URL) and comparing to caller().
    // The anchor is trusted (recovered, not echoed); only the two echoed values
    // and the current config can make this diverge from caller().
    let grant_ttl_ns = resolve_grant_ttl(max_ttl);
    let read_only = read_only_from(permissions);
    let seed = mcp_registration_seed(anchor_number, read_only, grant_ttl_ns, &url);
    if p_reg != Principal::self_authenticating(der_encode_canister_sig_key(seed.to_vec())) {
        return Err(NOT_AUTHORIZED.to_string());
    }

    // Consent is authenticated; bind S under it (config /
    // one-session-per-anchor invariants and the TTL clamp live in
    // `mcp::register`). The entry is retained (multi-use); a retry re-binds.
    let registration = mcp::register(anchor_number, session_key, grant_ttl_ns, read_only)?;
    Ok(McpRegistrationV2 {
        expiration: registration.expiration,
        permissions: permissions_of(read_only),
    })
}
