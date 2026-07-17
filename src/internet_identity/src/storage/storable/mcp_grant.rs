use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

/// An active MCP session grant: the value of the grant map (see
/// [`crate::storage`]), keyed by the MCP server's session-key principal.
/// Written by the connect flow (`mcp_register_v2`); the server-facing
/// `mcp_*` methods authorize a caller by looking up its grant and checking it
/// has not expired. At most one live grant exists per anchor — registering a
/// new session replaces the previous one through
/// [`super::mcp_config::StorableMcpConfig::session_principal`].
#[derive(Encode, Decode, Clone, Debug, Eq, PartialEq)]
#[cbor(map)]
pub struct StorableMcpGrant {
    /// The anchor that registered this session (and whose config change
    /// revokes it). Stored in the value so revocation can verify ownership —
    /// a stale config pointer must never delete another anchor's grant.
    #[n(0)]
    pub anchor_number: u64,
    /// Expiration in nanoseconds since the epoch: the grant authorizes
    /// nothing once `time()` reaches this.
    #[n(1)]
    pub expires_at_ns: u64,
    /// Whether the per-app delegations this session mints are restricted to
    /// query calls (queries-only). Chosen once at connect (`mcp_register_v2`) and
    /// applied to every delegation the session mints — read-only is a property
    /// of the whole MCP session, not a per-call flag. Both `prepare` and `get`
    /// read it, since the access level is folded into the delegation signature.
    #[n(2)]
    pub read_only: bool,
}

impl Storable for StorableMcpGrant {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableMcpGrant");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorableMcpGrant")
    }

    const BOUND: Bound = Bound::Unbounded;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_roundtrip_through_storable() {
        for read_only in [false, true] {
            let grant = StorableMcpGrant {
                anchor_number: 10_000,
                expires_at_ns: 1_234_567_890_000_000_000,
                read_only,
            };
            let decoded = StorableMcpGrant::from_bytes(grant.to_bytes());
            assert_eq!(decoded, grant);
        }
    }
}
