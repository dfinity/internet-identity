use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use minicbor::{Decode, Encode};
use std::borrow::Cow;

/// Per-anchor trusted-MCP-server configuration, persisted in its own stable map
/// (see [`crate::storage`]) so it syncs across all of the identity's devices —
/// unlike the CLI-access toggle, which is device-local. It is written via the
/// authenticated `mcp_set_config` method and read by the `/mcp` connect flow
/// (verify-at-connect) and the Settings UI via `mcp_get_config`. Kept separate
/// from the core anchor record so it never touches anchor serialization.
///
/// `Default` is the disabled, no-server state, returned for anchors that have
/// never written a config.
#[derive(Encode, Decode, Clone, Default, Debug, Eq, PartialEq)]
#[cbor(map)]
pub struct StorableMcpConfig {
    /// Master toggle for the feature on this identity.
    #[n(0)]
    pub enabled: bool,
    /// The trusted server URL, kept verbatim so the Settings UI can display and
    /// re-probe a path-based endpoint like `https://host/mcp`. Trust matching in
    /// the connect flow is by origin. `None` when no server is set.
    #[n(1)]
    pub url: Option<String>,
}

impl Storable for StorableMcpConfig {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorableMcpConfig");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorableMcpConfig")
    }

    const BOUND: Bound = Bound::Unbounded;
}
