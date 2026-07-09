use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

/// A pending MCP *registration* delegation: the value of the registration index
/// (see [`crate::storage`]), keyed by the registration principal `P_reg` the
/// index entry was minted for.
///
/// Deliberately minimal — it stores only what cannot be re-derived at
/// redemption: the **anchor** to bind the session to. Written by the
/// authenticated `prepare_mcp_registration_delegation` (so only the consenting
/// user can create one), it lets a later `mcp_register_v2` — authenticated by
/// the registration delegation chain rooted at `P_reg`, so its `caller()` *is*
/// `P_reg` — recover the anchor **server-side**, rather than take it as a call
/// argument. That keeps the anchor number out of the MCP server's hands (the
/// server never needs it, and it is the one identifier II otherwise never
/// exposes to a relying party).
///
/// The rest of the consent — the read-only choice and the grant lifetime — is
/// *not* stored: it is folded into `P_reg`'s seed, so `mcp_register_v2` takes
/// those two as arguments (the server remembers and echoes them) and validates
/// them by re-deriving `P_reg` and comparing to `caller()`. An altered echo
/// derives a different principal and is rejected, so the server can neither
/// upgrade a read-only session nor stretch the grant.
///
/// The entry authorizes redemption for as long as the registration delegation
/// is valid (a boundary retry within that window simply re-binds — the
/// delegation is multi-use, bounded by its short expiry and the
/// one-session-per-anchor invariant). It is removed once a lookup finds it
/// expired.
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
    /// Expiration of the *registration delegation itself* (nanoseconds since the
    /// epoch), short by design. Past this the entry authorizes nothing; it also
    /// bounds how long a stale entry lingers before it can be pruned.
    #[n(1)]
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
        let entry = StorableMcpRegistration {
            anchor_number: 10_000,
            expires_at_ns: 1_234_567_890_000_000_000,
        };
        let decoded = StorableMcpRegistration::from_bytes(entry.to_bytes());
        assert_eq!(decoded, entry);
    }
}
