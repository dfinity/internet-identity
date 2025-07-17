use crate::internet_identity::types::{
    AuthnMethod, AuthnMethodData, PublicKey, PublicKeyAuthn, WebAuthn,
};
use candid::Principal;

impl AuthnMethodData {
    /// Returns the principal of this authentication method (i.e. the caller principal observed in
    /// the II canister when signing canister calls with the given authentication method).
    pub fn principal(&self) -> Principal {
        Principal::self_authenticating(self.public_key())
    }

    /// Returns the public key of this authentication method.
    pub fn public_key(&self) -> PublicKey {
        match self.authn_method {
            AuthnMethod::WebAuthn(WebAuthn { ref pubkey, .. }) => pubkey,
            AuthnMethod::PubKey(PublicKeyAuthn { ref pubkey }) => pubkey,
        }
        .clone()
    }
}
