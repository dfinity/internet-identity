use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

/// A pending MCP *registration* delegation: the value of the registration index
/// (see [`crate::storage`]), keyed by the registration principal `P_reg` the
/// index entry was minted for.
///
/// Written by the authenticated `prepare_mcp_registration_delegation` (so only
/// the consenting user can create one), it records what a later
/// `mcp_register_v2` — authenticated by the `P_reg -> X` delegation chain, so
/// its `caller()` *is* `P_reg` — needs to bind the session key the MCP server
/// generates: the anchor to bind it to (recovered here, never taken from a call
/// argument), the read-only choice the user made at connect, and the session
/// grant's requested lifetime. The entry is deleted on the first successful
/// `mcp_register_v2`, so the registration delegation is single-use; a boundary
/// retry with the same key is served idempotently from the resulting grant.
#[derive(Encode, Decode, Clone, Debug, Eq, PartialEq)]
#[cbor(map)]
pub struct StorableMcpRegistration {
    /// The anchor the eventual session grant is bound to. Recovered here rather
    /// than trusted from an argument: `mcp_register_v2`'s caller is `P_reg`, and
    /// only this entry ties `P_reg` back to an anchor, so a chain minted for one
    /// anchor can never register a session for another.
    #[n(0)]
    pub anchor_number: u64,
    /// Whether the session's per-app delegations are queries-only. Chosen by the
    /// authenticated user at `prepare` time and applied at `mcp_register_v2`, so
    /// the MCP server cannot upgrade a read-only session to full access.
    #[n(1)]
    pub read_only: bool,
    /// The requested session-grant lifetime in nanoseconds (clamped again by
    /// [`crate::mcp::register`]). Chosen by the user at connect.
    #[n(2)]
    pub grant_ttl_ns: u64,
    /// Expiration of the *registration delegation itself* (nanoseconds since the
    /// epoch), short by design. Past this the entry authorizes nothing; it also
    /// bounds how long a stale entry lingers before it can be pruned.
    #[n(3)]
    pub expires_at_ns: u64,
    /// Whether this registration delegation has already been redeemed. Retained
    /// (rather than deleted) after a successful `mcp_register_v2` so a boundary
    /// retry can be recognized and answered — but only for the same caller
    /// (`P_reg`, the map key) and the same key (see `registered_key`).
    #[n(4)]
    pub used: bool,
    /// The session key `S` bound on the first successful redemption (empty until
    /// then). A retry is idempotent only when it presents this exact key; a
    /// different key with a `used` entry is rejected (single-use).
    #[n(5)]
    pub registered_key: Vec<u8>,
}

impl Storable for StorableMcpRegistration {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableMcpRegistration");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorableMcpRegistration")
    }

    const BOUND: Bound = Bound::Unbounded;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_roundtrip_through_storable() {
        for (read_only, used, registered_key) in [
            (false, false, vec![]),
            (true, false, vec![]),
            (true, true, b"bound session key".to_vec()),
        ] {
            let entry = StorableMcpRegistration {
                anchor_number: 10_000,
                read_only,
                grant_ttl_ns: 24 * 60 * 60 * 1_000_000_000,
                expires_at_ns: 1_234_567_890_000_000_000,
                used,
                registered_key,
            };
            let decoded = StorableMcpRegistration::from_bytes(entry.to_bytes());
            assert_eq!(decoded, entry);
        }
    }
}
