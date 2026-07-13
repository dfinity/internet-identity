//! The MCP *registration* delegation: connect-handshake hardening (phase 2).
//!
//! Instead of the frontend fetching the MCP server's session key and calling
//! [`crate::mcp::register`] with it, the connect flow now:
//!
//! 1. mints a short-lived canister-signed delegation `P_reg -> Y` via
//!    [`prepare`] and [`get`], where `Y` is an ephemeral registration key the
//!    *II frontend* generates for this connect (its private half never leaves
//!    the browser) and `P_reg` is a synthetic principal derived from a *random
//!    per-connect nonce* (`raw_rand`), under a dedicated domain separator;
//! 2. lets the frontend extend the chain locally with a second, browser-signed
//!    hop `Y -> X` to the MCP server's registration key `X` (from the connect
//!    link), and deliver the full chain to the server's declared callback over
//!    a URL fragment;
//! 3. lets the MCP server redeem it: authenticated by the `P_reg -> Y -> X`
//!    chain (so `caller()` *is* `P_reg`), it calls [`register_v2`] with the
//!    long-lived session key `S` it generated — and nothing else.
//!
//! **The consent lives in storage, not in the seed.** At [`prepare`] time
//! (authenticated as the user) the canister records an index entry
//! ([`StorableMcpRegistration`]) keyed by `P_reg`, holding the whole consent:
//! the anchor, the read-only choice, the resolved grant lifetime, and the
//! trusted server URL from the anchor's synced config. Because `P_reg` is
//! derived from a random nonce (not from the consent), [`prepare`] and [`get`]
//! don't have to agree on a deterministic derivation — [`prepare`] returns the
//! `user_key` (`= der(seed)`), and [`get`] recovers the seed straight back out
//! of it to fetch the certified signature. [`register_v2`] then recovers the
//! entire consent by looking up `caller()` (`== P_reg`): it takes **no** anchor,
//! read-only, or TTL argument, so the MCP server sends nothing but its own key
//! and cannot alter any consented value. The anchor number in particular — the
//! one identifier II otherwise never hands a relying party — is never disclosed
//! to the server (every later `mcp_*` call recovers the anchor from the
//! session-key grant, so the server has no use for it).
//!
//! A config change between consent and redemption is caught at [`register_v2`]:
//! the entry's stored trusted URL must still equal the anchor's *current*
//! trusted URL, so switching or disabling the trusted server invalidates an
//! in-flight delegation.
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
    delegation_signature_msg, signature_map::CanisterSigInputs, CanisterSigPublicKey,
    DELEGATION_SIG_DOMAIN,
};
use ic_cdk::api::time;
use ic_cdk::caller;
use ic_certification::Hash;
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, Delegation, McpRegistrationV2, Permissions, PrepareMcpRegistrationDelegation,
    SessionKey, SignedDelegation, Timestamp, UserKey,
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
/// 30 days] bounds. [`prepare`] resolves the argument through this and *stores*
/// the result, so the grant `register_v2` mints runs exactly the recorded
/// lifetime.
fn resolve_grant_ttl(max_ttl: Option<u64>) -> u64 {
    max_ttl
        .unwrap_or(DEFAULT_MCP_GRANT_TTL_NS)
        .clamp(mcp::MCP_GRANT_MIN_TTL_NS, mcp::MCP_GRANT_MAX_TTL_NS)
}

/// Seed for the registration principal `P_reg`: a fixed domain separator plus a
/// random per-connect `nonce` (`raw_rand`), both length-prefixed. The nonce is
/// the only entropy — `P_reg` carries no consent or key material, so the
/// consent must be stored (see the module doc) and the delegated-to key `Y`
/// lives in the signed *message*. The domain separator keeps this principal
/// namespace disjoint from the session, account, and email-recovery seeds.
fn mcp_registration_seed(nonce: &[u8]) -> Hash {
    const DOMAIN_SEPARATOR: &[u8] = b"mcp-registration";

    let mut blob: Vec<u8> = Vec::new();
    blob.push(DOMAIN_SEPARATOR.len() as u8);
    blob.extend_from_slice(DOMAIN_SEPARATOR);
    blob.push(nonce.len() as u8);
    blob.extend_from_slice(nonce);

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

/// The anchor's trusted server URL. Required at [`prepare`] (there is no
/// registration delegation without a trusted server to connect) and recorded on
/// the index entry; [`register_v2`] compares the stored URL against this again,
/// so a config change or disable between consent and redemption is caught.
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
/// `P_reg` is derived from a fresh random nonce, and the whole consent (anchor,
/// read-only choice, resolved grant lifetime, trusted server URL) is recorded
/// on the index entry keyed by `P_reg`, so [`register_v2`] recovers it
/// server-side. Authenticated as the user (full authorization) — only the
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

    // Effective (defaulted + clamped) grant lifetime, stored so the grant runs
    // exactly this long; and a fresh random nonce (raw_rand) that makes P_reg
    // per-connect-unique and unpredictable.
    let grant_ttl_ns = resolve_grant_ttl(max_ttl);
    let read_only = read_only_from(permissions);
    let nonce = crate::random_salt().await;

    let expiration = time().saturating_add(MCP_REGISTRATION_DELEGATION_TTL_NS);
    let seed = mcp_registration_seed(&nonce);

    state::signature_map_mut(|sigs| {
        // No queries-only restriction on the registration delegation itself: it
        // authenticates one update (mcp_register_v2). The read-only choice is
        // recorded on the index entry below, not on this signature.
        add_delegation_signature(sigs, registration_key, &seed, expiration, None);
    });
    update_root_hash();

    // Record the whole consent, keyed by P_reg, so register_v2 recovers it
    // server-side — the server passes only its session key, and the anchor
    // number is never disclosed to it.
    let user_key = der_encode_canister_sig_key(seed.to_vec());
    let p_reg = Principal::self_authenticating(&user_key);
    state::storage_borrow_mut(|storage| {
        storage.insert_mcp_registration(
            p_reg,
            StorableMcpRegistration {
                anchor_number,
                read_only,
                grant_ttl_ns,
                trusted_url: url,
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
/// `Y -> X` hop and deliver. `user_key` is the value [`prepare`] returned; the
/// seed is recovered straight out of it (the delegation's own public key), so
/// this query needs no deterministic re-derivation. Authenticated as the user,
/// and scoped to that user's own registration: the entry keyed by `P_reg`
/// (`= self_authenticating(user_key)`) must belong to `anchor_number`, so an
/// authenticated identity can only fetch *its own* delegation, never another
/// anchor's given that anchor's `(user_key, Y, expiration)`.
pub fn get(
    anchor_number: AnchorNumber,
    registration_key: SessionKey,
    user_key: UserKey,
    expiration: Timestamp,
) -> Result<SignedDelegation, String> {
    // Both a bad/foreign `user_key` and a genuinely absent signature report the
    // same thing: the method never distinguishes "no delegation" from "someone
    // else's delegation".
    const NO_SUCH_DELEGATION: &str = "MCP registration failed: no such delegation.";

    check_authorization(anchor_number)
        .map_err(|err| format!("{} could not be authenticated.", err.principal))?;

    // Recover the seed from `user_key` (the DER canister-signature public key
    // `prepare` returned). A malformed key reports the delegation as absent.
    let Ok(seed) = CanisterSigPublicKey::try_from(user_key.as_ref()).map(|pk| pk.seed) else {
        return Err(NO_SUCH_DELEGATION.to_string());
    };

    // Scope the fetch to the authenticated anchor. `check_authorization` proves
    // the caller controls `anchor_number`, but not that this `user_key`'s entry
    // is *theirs*: without this check any authenticated identity could fetch
    // another anchor's certified `P_reg -> Y` hop by presenting its
    // `(user_key, Y, expiration)`. The hop is inert without the browser-held
    // `priv(Y)` and transits the IC in the clear regardless, so this is
    // least-privilege hardening rather than a plugged leak — but there is no
    // legitimate reason to serve one anchor another's delegation.
    let p_reg = Principal::self_authenticating(&user_key);
    let owned_by_anchor = state::storage_borrow(|storage| {
        storage
            .lookup_mcp_registration(p_reg)
            .is_some_and(|entry| entry.anchor_number == anchor_number)
    });
    if !owned_by_anchor {
        return Err(NO_SUCH_DELEGATION.to_string());
    }

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
            Err(_) => Err(NO_SUCH_DELEGATION.to_string()),
        }
    })
}

/// `mcp_register_v2`: bind the MCP server's long-lived session key `S` to the
/// consenting anchor. Authenticated by the `P_reg -> Y -> X` chain (so
/// `caller()` is `P_reg`). The entire consent — anchor, read-only choice, grant
/// lifetime — is recovered from the index entry keyed by `caller()`, so the
/// server passes **only** `session_key`: it cannot name a different anchor,
/// upgrade the access level, or stretch the grant, and the anchor number is
/// never disclosed to it. A config change since consent (the entry's stored
/// trusted URL no longer matches the anchor's current one, or MCP was disabled)
/// invalidates the delegation. Multi-use within the delegation's short
/// lifetime: the entry is retained (a retry re-binds), and removed once found
/// expired.
pub fn register_v2(session_key: SessionKey) -> Result<McpRegistrationV2, String> {
    // The one error this public method reports for anything short of an
    // authorized redemption. A missing entry, an expired entry, and a
    // disabled/changed config are all indistinguishable: `register_v2` can be
    // called by anyone, and a distinct error would be an oracle for probing
    // anchors or their MCP config. (`prepare` / `get` keep specific errors —
    // they authenticate as the identity first.)
    const NOT_AUTHORIZED: &str =
        "MCP registration failed: not authorized by a registration delegation.";

    // Recover the consent from the index entry keyed by caller() (== P_reg).
    // No entry ⇒ this caller never received a registration delegation. An
    // expired entry authorizes nothing and is pruned.
    let p_reg = caller();
    let Some(entry) = state::storage_borrow(|storage| storage.lookup_mcp_registration(p_reg))
    else {
        return Err(NOT_AUTHORIZED.to_string());
    };
    if time() > entry.expires_at_ns {
        state::storage_borrow_mut(|storage| storage.remove_mcp_registration(p_reg));
        return Err(NOT_AUTHORIZED.to_string());
    }

    // The trusted server the user consented to must still be the anchor's
    // current one — otherwise a switch or disable since consent has invalidated
    // this delegation.
    match trusted_url(entry.anchor_number) {
        Ok(current_url) if current_url == entry.trusted_url => {}
        _ => return Err(NOT_AUTHORIZED.to_string()),
    }

    // Consent is authenticated; bind S under it (config /
    // one-session-per-anchor invariants and the TTL clamp live in
    // `mcp::register`). The entry is retained (multi-use); a retry re-binds.
    let registration = mcp::register(
        entry.anchor_number,
        session_key,
        entry.grant_ttl_ns,
        entry.read_only,
    )?;
    Ok(McpRegistrationV2 {
        expiration: registration.expiration,
        permissions: permissions_of(entry.read_only),
    })
}
