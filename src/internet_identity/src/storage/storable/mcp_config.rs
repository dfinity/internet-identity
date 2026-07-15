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
///
/// It also carries the anchor's in-flight *registration delegations*
/// ([`pending_registrations`](StorableMcpConfig::pending_registrations)) — the
/// per-anchor state that bounds how many an anchor can hold at once. This rides
/// the config entry (which already exists per anchor) rather than a separate
/// map, so it adds no new per-anchor growth surface.
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
    /// Raw principal bytes of the MCP session currently registered for this
    /// identity (the key of the grant map, see [`crate::storage`]), or `None`
    /// when no session is registered. The grant map is keyed by principal, so
    /// this forward pointer is what lets a config change revoke the active
    /// session in O(1) — and what enforces at most one session per identity:
    /// `mcp_register` replaces the previous grant through it.
    #[cbor(n(2), with = "minicbor::bytes")]
    pub session_principal: Option<Vec<u8>>,
    /// The anchor's in-flight MCP registration delegations (see
    /// `prepare_mcp_registration_delegation`): the registration principal
    /// `P_reg` of each and the delegation's expiry. `prepare` prunes the expired
    /// ones (removing them from the registration index too) and refuses to mint
    /// a new one once this list is at the per-anchor cap — so a single anchor
    /// can't flood the registration index. Kept small (bounded by that cap).
    /// `None` on configs written before this field existed, decoded as empty.
    #[n(3)]
    pub pending_registrations: Option<Vec<StorablePendingRegistration>>,
}

/// One of an anchor's in-flight MCP registration delegations, tracked on its
/// [`StorableMcpConfig`] to bound how many the anchor can hold at once and to
/// let `prepare` reclaim the anchor's own expired ones synchronously.
#[derive(Encode, Decode, Clone, Debug, Eq, PartialEq)]
#[cbor(map)]
pub struct StorablePendingRegistration {
    /// Raw bytes of the registration principal `P_reg` (the key of the
    /// registration index entry this tracks).
    #[cbor(n(0), with = "minicbor::bytes")]
    pub principal: Vec<u8>,
    /// Expiry (ns since epoch) of the registration delegation, copied from the
    /// index entry so `prepare` can prune without a per-entry lookup.
    #[n(1)]
    pub expires_at_ns: u64,
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_roundtrip_through_storable() {
        let config = StorableMcpConfig {
            enabled: true,
            url: Some("https://mcp.example.com/mcp".to_string()),
            session_principal: Some(vec![1, 2, 3, 4]),
            pending_registrations: Some(vec![
                StorablePendingRegistration {
                    principal: vec![7; 29],
                    expires_at_ns: 42,
                },
                StorablePendingRegistration {
                    principal: vec![8; 29],
                    expires_at_ns: 43,
                },
            ]),
        };
        let decoded = StorableMcpConfig::from_bytes(config.to_bytes());
        assert_eq!(decoded, config);
    }

    /// Configs written before the `session_principal` field existed are CBOR
    /// maps without key 2 — they must decode with the field `None`.
    #[test]
    fn should_decode_config_without_session_principal() {
        // Encode the pre-`session_principal` shape: a CBOR map with keys 0..=1.
        let mut buffer = Vec::new();
        let mut encoder = minicbor::Encoder::new(&mut buffer);
        encoder.map(2).unwrap();
        encoder.u8(0).unwrap().bool(true).unwrap();
        encoder
            .u8(1)
            .unwrap()
            .str("https://mcp.example.com/mcp")
            .unwrap();

        let decoded = StorableMcpConfig::from_bytes(Cow::Borrowed(&buffer));
        assert!(decoded.enabled);
        assert_eq!(decoded.url, Some("https://mcp.example.com/mcp".to_string()));
        assert_eq!(decoded.session_principal, None);
        // A field added later (key 3) is likewise absent -> empty.
        assert_eq!(decoded.pending_registrations, None);
    }

    /// Configs written before the `pending_registrations` field existed are
    /// CBOR maps without key 3 — they must decode with the field `None`.
    #[test]
    fn should_decode_config_without_pending_registrations() {
        // Encode the pre-`pending_registrations` shape: a map with keys 0..=2.
        let mut buffer = Vec::new();
        let mut encoder = minicbor::Encoder::new(&mut buffer);
        encoder.map(3).unwrap();
        encoder.u8(0).unwrap().bool(true).unwrap();
        encoder
            .u8(1)
            .unwrap()
            .str("https://mcp.example.com/mcp")
            .unwrap();
        encoder.u8(2).unwrap().bytes(&[5; 29]).unwrap();

        let decoded = StorableMcpConfig::from_bytes(Cow::Borrowed(&buffer));
        assert!(decoded.enabled);
        assert_eq!(decoded.session_principal, Some(vec![5; 29]));
        assert_eq!(decoded.pending_registrations, None);
    }

    /// Rollback safety: bytes written by the new encoder (with key 2 present)
    /// must remain decodable when the unknown key is skipped — minicbor's
    /// `#[cbor(map)]` derive ignores unknown map keys, which is what a
    /// rolled-back (pre-`session_principal`) wasm relies on. Cheaply
    /// approximated here by decoding new-encoder bytes and checking the
    /// pre-existing fields survive untouched.
    #[test]
    fn should_keep_preexisting_fields_when_session_principal_present() {
        let config = StorableMcpConfig {
            enabled: true,
            url: Some("https://mcp.example.com/mcp".to_string()),
            session_principal: Some(vec![5; 29]),
            pending_registrations: None,
        };
        let decoded = StorableMcpConfig::from_bytes(config.to_bytes());
        assert!(decoded.enabled);
        assert_eq!(decoded.url, config.url);
        assert_eq!(decoded.session_principal, Some(vec![5; 29]));
    }
}
