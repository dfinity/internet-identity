use crate::internet_identity::types::{AuthnMethod, AuthnMethodData, PublicKeyAuthn, WebAuthn};
use candid::Principal;

impl AuthnMethodData {
    /// Returns the principal of this authentication method (i.e. the caller principal observed in
    /// the II canister when signing canister calls with the given authentication method).
    pub fn principal(&self) -> Principal {
        let pubkey = match self.authn_method {
            AuthnMethod::WebAuthn(WebAuthn { ref pubkey, .. }) => pubkey,
            AuthnMethod::PubKey(PublicKeyAuthn { ref pubkey }) => pubkey,
        };
        Principal::self_authenticating(pubkey)
    }
}
