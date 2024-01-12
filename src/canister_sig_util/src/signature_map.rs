//! Maintains anchor signatures and expirations.
use crate::{hash_bytes, CanisterSig};
use ic_cdk::api::{data_certificate, time};
use ic_certification::{
    fork, labeled, leaf, leaf_hash, pruned, AsHashTree, Hash, HashTree, RbTree,
};
use serde::Serialize;
use serde_bytes::ByteBuf;
use std::borrow::Cow;
use std::collections::BinaryHeap;

const MINUTE_NS: u64 = 60 * 1_000_000_000;

// The expiration used for signatures.
#[allow(clippy::identity_op)]
const SIGNATURE_EXPIRATION_PERIOD_NS: u64 = 1 * MINUTE_NS;

pub const LABEL_SIG: &[u8] = b"sig";
#[derive(Default)]
struct Unit;

impl AsHashTree for Unit {
    fn root_hash(&self) -> Hash {
        leaf_hash(&b""[..])
    }
    fn as_hash_tree(&self) -> HashTree {
        leaf(Cow::from(&b""[..]))
    }
}

#[derive(PartialEq, Eq)]
struct SigExpiration {
    expires_at: u64,
    seed_hash: Hash,
    msg_hash: Hash,
}

impl Ord for SigExpiration {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // BinaryHeap is a max heap, but we want expired entries
        // first, hence the inversed order.
        other.expires_at.cmp(&self.expires_at)
    }
}

impl PartialOrd for SigExpiration {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Default)]
pub struct SignatureMap {
    certified_map: RbTree<Hash, RbTree<Hash, Unit>>,
    expiration_queue: BinaryHeap<SigExpiration>,
}

impl SignatureMap {
    fn put(&mut self, seed: &[u8], message_hash: Hash, signature_expires_at: u64) {
        let seed_hash = hash_bytes(seed);
        if self.certified_map.get(&seed_hash[..]).is_none() {
            let mut submap = RbTree::new();
            submap.insert(message_hash, Unit);
            self.certified_map.insert(seed_hash, submap);
        } else {
            self.certified_map.modify(&seed_hash[..], |submap| {
                submap.insert(message_hash, Unit);
            });
        }
        self.expiration_queue.push(SigExpiration {
            seed_hash,
            msg_hash: message_hash,
            expires_at: signature_expires_at,
        });
    }

    pub fn delete(&mut self, seed_hash: Hash, message_hash: Hash) {
        let mut is_empty = false;
        self.certified_map.modify(&seed_hash[..], |m| {
            m.delete(&message_hash[..]);
            is_empty = m.is_empty();
        });
        if is_empty {
            self.certified_map.delete(&seed_hash[..]);
        }
    }

    pub fn prune_expired(&mut self, now: u64, max_to_prune: usize) -> usize {
        let mut num_pruned = 0;

        for _step in 0..max_to_prune {
            if let Some(expiration) = self.expiration_queue.peek() {
                if expiration.expires_at > now {
                    return num_pruned;
                }
            }
            if let Some(expiration) = self.expiration_queue.pop() {
                self.delete(expiration.seed_hash, expiration.msg_hash);
            }
            num_pruned += 1;
        }

        num_pruned
    }

    /// Retrieves from this map and returns canister signature for the specified `seed` and `message_hash`.
    /// If the canister uses
    /// [certified_data](https://internetcomputer.org/docs/current/references/ic-interface-spec/#system-api-certified-data)
    /// to manage certified assets, the caller should provide also the root hash of the assets subtree
    /// (cf. `asset_util`-crate).
    /// The returned value (if found) is a CBOR-serialised `CanisterSig`.
    pub fn get_signature_as_cbor(
        &self,
        seed: &[u8],
        message_hash: Hash,
        maybe_certified_assets_root_hash: Option<Hash>,
    ) -> Result<Vec<u8>, String> {
        let certificate = data_certificate()
            .ok_or("data certificate is only available in query calls".to_string())?;
        let witness = self
            .witness(seed, message_hash)
            .ok_or("missing witness".to_string())?;

        let witness_hash = witness.digest();
        let root_hash = self.root_hash();
        if witness_hash != root_hash {
            return Err(format!(
                "internal error: signature map computed an invalid hash tree, witness hash is {}, root hash is {}",
                hex::encode(witness_hash),
                hex::encode(root_hash)
            ));
        }

        let sigs_tree = labeled(LABEL_SIG, witness);
        let tree = match maybe_certified_assets_root_hash {
            Some(certified_assets_root_hash) => fork(pruned(certified_assets_root_hash), sigs_tree),
            None => sigs_tree,
        };

        let sig = CanisterSig {
            certificate: ByteBuf::from(certificate),
            tree,
        };

        let mut cbor = serde_cbor::ser::Serializer::new(Vec::new());
        cbor.self_describe().unwrap();
        sig.serialize(&mut cbor).unwrap();
        Ok(cbor.into_inner())
    }

    /// Adds to this map a canister signature for the specified `seed` and `message_hash`
    pub fn add_signature(&mut self, seed: &[u8], message_hash: Hash) {
        let expires_at = time().saturating_add(SIGNATURE_EXPIRATION_PERIOD_NS);
        self.put(seed, message_hash, expires_at);
    }

    pub fn len(&self) -> usize {
        self.expiration_queue.len()
    }

    pub fn is_empty(&self) -> bool {
        self.expiration_queue.is_empty()
    }

    pub fn root_hash(&self) -> Hash {
        self.certified_map.root_hash()
    }

    pub fn witness(&self, seed: &[u8], message_hash: Hash) -> Option<HashTree> {
        let seed_hash = hash_bytes(seed);
        self.certified_map
            .get(&seed_hash[..])?
            .get(&message_hash[..])?;
        let witness = self
            .certified_map
            .nested_witness(&seed_hash[..], |nested| nested.witness(&message_hash[..]));
        Some(witness)
    }
}

#[cfg(test)]
mod test;
