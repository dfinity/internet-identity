use crate::storage::storable::timestamp::StorableTimestamp;
use minicbor::{Decode, Encode};

/// Where a push goes, and what its payload is sealed against. Held by a browser entry,
/// by a Home Screen app's record, which has one per identity, and by an identity's config
/// for an app, one per installed web app of that app.
///
/// Two states because a subscribe takes two calls: the first mints the key the browser
/// encrypts `auth` to, the second spends it. A re-subscribe passes back through
/// `Pending`, so nothing outside this module reads one without asking for the ready half.
#[derive(Encode, Decode, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum StorableWebPushSubscription {
    /// A key fetch waiting for the subscribe it was minted for. Expires on its own.
    #[n(0)]
    Pending {
        /// The private half of the key the browser encrypts `auth` to.
        #[cbor(n(0), with = "minicbor::bytes")]
        secret_key: Vec<u8>,
        /// Expanded into this subscription's first sender key once a subscribe arrives.
        #[cbor(n(1), with = "minicbor::bytes")]
        sender_seed: Vec<u8>,
        #[n(2)]
        minted_at_ns: StorableTimestamp,
    },
    /// Subscribed, and what a wake-up is posted and sealed against.
    #[n(1)]
    Ready {
        /// Relay endpoint URL (≤512 bytes, validated at insert). Mutable: the browser
        /// rewrites it whenever the push service hands it a new one.
        #[n(0)]
        endpoint: String,
        /// Written per (re-)subscribe, so the manage screen can say since when.
        #[n(1)]
        created_at_ns: StorableTimestamp,
        /// RFC 8291 §3.4 input keying material, derived from `subscriber_key`, `auth`
        /// and the sender key below. Re-derived only when a pass refreshes that key.
        #[cbor(n(2), with = "minicbor::bytes")]
        payload_ikm: Vec<u8>,
        /// The public half of that sender key, carried as the `keyid` the browser derives
        /// its own half against (uncompressed SEC1 P-256, 65 bytes).
        #[cbor(n(3), with = "minicbor::bytes")]
        payload_keyid: Vec<u8>,
        /// The subscription's own `p256dh`, kept because every refresh runs the ECDH
        /// against it again.
        #[cbor(n(4), with = "minicbor::bytes")]
        subscriber_key: Vec<u8>,
        /// The subscription's `auth` secret, which arrived encrypted and is the salt of
        /// every derivation of `payload_ikm`.
        #[cbor(n(5), with = "minicbor::bytes")]
        auth: Vec<u8>,
        /// When `payload_ikm` was last derived. A pass refreshes the oldest it reaches.
        #[n(6)]
        last_rotated_ns: StorableTimestamp,
        /// The pushes posted through this subscription since a notification was last
        /// fetched through it.
        #[n(7)]
        unanswered_pushes: Option<StorableUnansweredPushCount>,
        /// When Internet Identity last submitted its own notification through this
        /// subscription to check it, where no fetch through it has answered that since.
        #[n(8)]
        probe_sent_at_ns: Option<StorableTimestamp>,
    },
}

/// How many pushes went unanswered through one subscription, and when the first of them
/// was posted.
#[derive(Encode, Decode, Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
#[cbor(map)]
pub struct StorableUnansweredPushCount {
    #[n(0)]
    pub count: u32,
    #[n(1)]
    pub first_sent_at_ns: StorableTimestamp,
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    /// The ready half as a canister without the counter and the probe writes and reads it.
    #[derive(Encode, Decode, Debug, Eq, PartialEq)]
    enum StorableWebPushSubscriptionWithoutCounter {
        #[n(1)]
        Ready {
            #[n(0)]
            endpoint: String,
            #[n(1)]
            created_at_ns: StorableTimestamp,
            #[cbor(n(2), with = "minicbor::bytes")]
            payload_ikm: Vec<u8>,
            #[cbor(n(3), with = "minicbor::bytes")]
            payload_keyid: Vec<u8>,
            #[cbor(n(4), with = "minicbor::bytes")]
            subscriber_key: Vec<u8>,
            #[cbor(n(5), with = "minicbor::bytes")]
            auth: Vec<u8>,
            #[n(6)]
            last_rotated_ns: StorableTimestamp,
        },
    }

    #[test]
    fn a_subscription_written_without_the_counter_reads_with_none() {
        let mut bytes = Vec::new();
        minicbor::encode(without_counter(), &mut bytes).expect("encoding a subscription");

        let decoded_subscription: StorableWebPushSubscription =
            minicbor::decode(&bytes).expect("decoding it as the current shape");

        assert_eq!(
            decoded_subscription,
            StorableWebPushSubscription::Ready {
                endpoint: "https://push.example/a".to_string(),
                created_at_ns: 1,
                payload_ikm: vec![2; 32],
                payload_keyid: vec![3; 65],
                subscriber_key: vec![4; 65],
                auth: vec![5; 16],
                last_rotated_ns: 6,
                unanswered_pushes: None,
                probe_sent_at_ns: None,
            }
        );
    }

    #[test]
    fn a_subscription_written_with_the_counter_reads_without_it() {
        let mut bytes = Vec::new();
        minicbor::encode(
            StorableWebPushSubscription::Ready {
                endpoint: "https://push.example/a".to_string(),
                created_at_ns: 1,
                payload_ikm: vec![2; 32],
                payload_keyid: vec![3; 65],
                subscriber_key: vec![4; 65],
                auth: vec![5; 16],
                last_rotated_ns: 6,
                unanswered_pushes: Some(StorableUnansweredPushCount {
                    count: 3,
                    first_sent_at_ns: 7,
                }),
                probe_sent_at_ns: Some(8),
            },
            &mut bytes,
        )
        .expect("encoding a subscription");

        let decoded_subscription: StorableWebPushSubscriptionWithoutCounter =
            minicbor::decode(&bytes).expect("decoding it as the shape without the counter");

        assert_eq!(decoded_subscription, without_counter());
    }

    fn without_counter() -> StorableWebPushSubscriptionWithoutCounter {
        StorableWebPushSubscriptionWithoutCounter::Ready {
            endpoint: "https://push.example/a".to_string(),
            created_at_ns: 1,
            payload_ikm: vec![2; 32],
            payload_keyid: vec![3; 65],
            subscriber_key: vec![4; 65],
            auth: vec![5; 16],
            last_rotated_ns: 6,
        }
    }
}
