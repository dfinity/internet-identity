//! The Web Push channel: one subscription per browser, and the pool of VAPID JWTs that
//! browser signed for it.
//!
//! Callers reach this through `main.rs`, which validates and authorizes first, so
//! everything here acts on a browser the caller has already been shown to be.

mod subscription;
mod validation;

pub use subscription::{remove_subscription, set_subscription};
pub use validation::{
    ValidatedRemoveWebPushSubscriptionRequest, ValidatedSetWebPushSubscriptionRequest,
};

#[cfg(test)]
pub(crate) mod fixtures {
    use super::validation::{ValidatedSetWebPushSubscriptionRequest, JWT_SIG_LEN};
    use crate::state::storage_borrow;
    use internet_identity_interface::internet_identity::types::{
        AnchorNumber, BrowserId, SetWebPushSubscriptionRequest, Timestamp,
    };
    use serde_bytes::ByteBuf;

    /// A real, fixed SEC1 point: a length-only fixture cannot pass the curve check.
    pub(crate) fn valid_vapid_key() -> Vec<u8> {
        use p256::elliptic_curve::sec1::ToEncodedPoint;
        let secret = p256::SecretKey::from_slice(&[1u8; 32]).expect("fixed scalar is valid");
        secret
            .public_key()
            .to_encoded_point(false)
            .as_bytes()
            .to_vec()
    }

    pub(crate) fn valid_pool() -> Vec<Vec<u8>> {
        vec![vec![3u8; JWT_SIG_LEN]; 3]
    }

    /// Well-formed defaults, so a field added to the request doesn't touch every test.
    pub(crate) fn request(
        anchor_number: AnchorNumber,
        endpoint: &str,
        jwt_issued_at_ns: Timestamp,
    ) -> SetWebPushSubscriptionRequest {
        SetWebPushSubscriptionRequest {
            anchor_number,
            endpoint: endpoint.to_string(),
            vapid_public_key: ByteBuf::from(valid_vapid_key()),
            jwt_signatures: valid_pool().into_iter().map(ByteBuf::from).collect(),
            jwt_issued_at_ns,
        }
    }

    pub(crate) fn validated(
        anchor_number: AnchorNumber,
        endpoint: &str,
        jwt_issued_at_ns: Timestamp,
    ) -> ValidatedSetWebPushSubscriptionRequest {
        ValidatedSetWebPushSubscriptionRequest {
            anchor_number,
            endpoint: endpoint.to_string(),
            vapid_public_key: valid_vapid_key(),
            jwt_signatures: valid_pool(),
            jwt_issued_at_ns,
        }
    }

    pub(crate) fn stored_endpoint(
        anchor_number: AnchorNumber,
        browser_id: BrowserId,
    ) -> Option<String> {
        storage_borrow(|storage| {
            storage
                .webpush_subscription(anchor_number, browser_id)
                .map(|row| row.endpoint)
        })
    }
}
