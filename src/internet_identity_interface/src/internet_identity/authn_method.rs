use crate::internet_identity::types::{
    AuthnMethod, AuthnMethodData, AuthnMethodProtection, PublicKeyAuthn, Purpose, WebAuthn,
};
use candid::Principal;
use serde_bytes::ByteBuf;

impl AuthnMethodData {
    pub fn principal(&self) -> Principal {
        let pubkey = match self.authn_method {
            AuthnMethod::WebAuthn(WebAuthn { ref pubkey, .. }) => pubkey,
            AuthnMethod::PubKey(PublicKeyAuthn { ref pubkey }) => pubkey,
        };
        Principal::self_authenticating(pubkey)
    }

    pub fn eq_ignoring_last_authentication(&self, other: &Self) -> bool {
        let a = Self {
            last_authentication: None,
            ..self.clone()
        };
        let b = Self {
            last_authentication: None,
            ..other.clone()
        };
        a == b
    }

    pub fn test_authn_method() -> Self {
        AuthnMethodData {
            authn_method: AuthnMethod::PubKey(PublicKeyAuthn {
                pubkey: ByteBuf::from(vec![0; 32]),
            }),
            metadata: Default::default(),
            protection: AuthnMethodProtection::Unprotected,
            purpose: Purpose::Authentication,
            last_authentication: None,
        }
    }
}
