use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use internet_identity_interface::internet_identity::types::AnchorNumber;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

/// An MCP-server access grant, keyed in stable storage by the principal II
/// derives for the anchor at the MCP server's origin (the principal the
/// server's standing delegation carries, recovered from `caller()`). Holding
/// the grant here — rather than a bare anchor number plus a parallel read-only
/// index — means a single lookup by the caller's principal yields everything the
/// `mcp_*_account_delegation` methods need: the MCP server never passes an
/// anchor number, and `read_only` travels with the grant it restricts.
///
/// The `anchor_number` cannot be dropped: a principal is a one-way hash, and the
/// anchor is required to derive the per-app account seeds the server acts under.
#[derive(Encode, Decode, Clone, Debug, Eq, PartialEq)]
#[cbor(map)]
pub struct StorableMcpAccess {
    /// The anchor that authorized this MCP-server principal.
    #[n(0)]
    pub anchor_number: AnchorNumber,
    /// Whether the per-app delegations minted for this grant are restricted to
    /// query calls (queries-only). Chosen at connect, persisted with the grant.
    #[n(1)]
    pub read_only: bool,
}

impl Storable for StorableMcpAccess {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableMcpAccess");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorableMcpAccess")
    }

    const BOUND: Bound = Bound::Unbounded;
}

#[cfg(test)]
mod test {
    use super::*;

    /// The grant must round-trip: a decode that loses or flips `read_only`
    /// would silently change the privilege of every delegation the MCP
    /// server mints after an upgrade.
    #[test]
    fn should_round_trip_mcp_access() {
        for read_only in [false, true] {
            let access = StorableMcpAccess {
                anchor_number: 10_000,
                read_only,
            };
            let decoded = StorableMcpAccess::from_bytes(access.to_bytes());
            assert_eq!(access, decoded);
        }
    }

    /// Pins the serialized layout (minicbor map, `0 => anchor_number`,
    /// `1 => read_only`). Renumbering the fields would decode existing
    /// grants wrongly; this test makes that a visible failure instead.
    #[test]
    fn should_keep_stable_wire_format() {
        let access = StorableMcpAccess {
            anchor_number: 10_000,
            read_only: true,
        };
        assert_eq!(
            access.to_bytes().as_ref(),
            [0xa2, 0x00, 0x19, 0x27, 0x10, 0x01, 0xf5],
            "serialized layout changed; existing grants would decode wrongly"
        );
        let decoded = StorableMcpAccess::from_bytes(Cow::Borrowed(&[
            0xa2, 0x00, 0x19, 0x27, 0x10, 0x01, 0xf5,
        ]));
        assert_eq!(decoded, access);
    }
}
