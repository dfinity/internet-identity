use ic_stable_structures::storable::Bound;
use ic_stable_structures::Storable;
use internet_identity_interface::internet_identity::types::{FrontendHostname, Timestamp};
use minicbor::{Decode, Encode};
use std::borrow::Cow;

/// Per-`(anchor, origin)` marker that the user has granted a specific
/// dApp permission to send them push notifications on this identity.
///
/// The presence of an entry means "granted"; a revoke removes the key
/// outright rather than storing a `granted: bool`. `granted_at_ns` is
/// kept for future auditing (e.g. showing the user "granted 3 days ago"
/// in the Settings UI).
///
/// `origin` duplicates the plaintext of the map key's `StorableOriginSha256`
/// (a one-way hash) so `push_list_consented_origins` can list the anchor's
/// consented dApps without a reverse hash lookup.
///
/// `Option<>` on `origin` is a schema-evolution hedge: an earlier revision
/// of this struct shipped without the field, and rows written under that
/// shape still exist on staging canisters that were upgraded rather than
/// reinstalled. Decoding one of those rows now yields `origin: None` —
/// the list handler filters those out.
#[derive(Encode, Decode, Default, Clone)]
#[cbor(map)]
pub struct StorablePushConsent {
    #[n(0)]
    pub granted_at_ns: Timestamp,
    #[n(1)]
    pub origin: Option<FrontendHostname>,
}

impl Storable for StorablePushConsent {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        let mut buffer = Vec::new();
        minicbor::encode(self, &mut buffer).expect("failed to encode StorablePushConsent");
        Cow::Owned(buffer)
    }

    fn from_bytes(bytes: Cow<'_, [u8]>) -> Self {
        minicbor::decode(&bytes).expect("failed to decode StorablePushConsent")
    }

    const BOUND: Bound = Bound::Unbounded;
}
