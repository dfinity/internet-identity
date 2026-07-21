use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

/// A pending MCP *registration* delegation: the value of the registration index
/// (see [`crate::storage`]), keyed by the registration principal `P_reg` the
/// index entry was minted for.
///
/// Written by the authenticated `prepare_mcp_registration_delegation` (so only
/// the consenting user can create one), it records the **whole consent** that a
/// later `mcp_register_v2` — authenticated by the registration delegation chain
/// rooted at `P_reg`, so its `caller()` *is* `P_reg` — needs to bind the
/// session key the MCP server generates. Because `P_reg` is derived from a
/// random per-connect nonce (not from the consent), the consent can't be
/// re-derived and so is stored here in full: the anchor to bind to, the
/// read-only choice, the resolved grant lifetime, and a hash of the trusted
/// server URL consented to. `mcp_register_v2` takes *only* the session key and
/// recovers all of this from the entry — so the server can neither name a
/// different anchor nor alter the access level or lifetime, and never learns
/// the anchor number. The stored URL hash lets `mcp_register_v2` reject a
/// delegation whose trusted server was switched or disabled since consent
/// (equality is all the check needs, so a hash suffices and bounds the entry
/// size).
///
/// The entry is retained after a successful redemption (the delegation is
/// multi-use within its short lifetime; a retry re-binds), and removed once a
/// lookup finds it expired.
#[derive(Encode, Decode, Clone, Debug, Eq, PartialEq)]
#[cbor(map)]
pub struct StorableMcpRegistration {
    /// The anchor the eventual session grant is bound to. Recovered here rather
    /// than trusted from an argument: `mcp_register_v2`'s caller is `P_reg`, and
    /// only this entry ties `P_reg` back to an anchor, so a chain minted for one
    /// anchor can never register a session for another — and the anchor number
    /// is never disclosed to the MCP server.
    #[n(0)]
    pub anchor_number: u64,
    /// Whether the session's per-app delegations are queries-only. Chosen by the
    /// authenticated user at `prepare` and applied at `mcp_register_v2`, so the
    /// MCP server cannot upgrade a read-only session to full access.
    #[n(1)]
    pub read_only: bool,
    /// The effective session-grant lifetime in nanoseconds (already defaulted
    /// and clamped to [10 min, 30 days] at `prepare`). `mcp_register_v2` binds
    /// the grant for exactly this long.
    #[n(2)]
    pub grant_ttl_ns: u64,
    /// `SHA-256` of the trusted server URL the user consented to (the anchor's
    /// synced config at `prepare`). `mcp_register_v2` requires it to still equal
    /// the hash of the anchor's current trusted URL, so switching or disabling
    /// the trusted server between consent and redemption invalidates the
    /// delegation. Stored as a fixed-size hash rather than the URL verbatim: the
    /// comparison only needs equality, and hashing caps the per-connect entry at
    /// 32 bytes so a large (attacker-set) trusted URL can't amplify it.
    #[cbor(n(3), with = "minicbor::bytes")]
    pub trusted_url_hash: Vec<u8>,
    /// Expiration of the *registration delegation itself* (nanoseconds since the
    /// epoch), short by design. Past this the entry authorizes nothing; it also
    /// bounds how long a stale entry lingers before it can be pruned.
    #[n(4)]
    pub expires_at_ns: u64,
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
        for (read_only, trusted_url_hash) in [(false, vec![0xab; 32]), (true, vec![0x12; 32])] {
            let entry = StorableMcpRegistration {
                anchor_number: 10_000,
                read_only,
                grant_ttl_ns: 24 * 60 * 60 * 1_000_000_000,
                trusted_url_hash,
                expires_at_ns: 1_234_567_890_000_000_000,
            };
            let decoded = StorableMcpRegistration::from_bytes(entry.to_bytes());
            assert_eq!(decoded, entry);
        }
    }
}
