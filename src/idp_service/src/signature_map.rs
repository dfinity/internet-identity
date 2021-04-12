use certified_map::{AsHashTree, RbTree};
use hashtree::{leaf_hash, Hash, HashTree};

struct Found;

impl AsHashTree for Found {
    fn root_hash(&self) -> Hash {
        leaf_hash(&b""[..])
    }
    fn as_hash_tree(&self) -> HashTree<'_> {
        HashTree::Leaf(&b""[..])
    }
}

#[derive(Default)]
pub struct SignatureMap(RbTree<RbTree<Found>>);

impl SignatureMap {
    pub fn put(&mut self, seed: Vec<u8>, message: Vec<u8>) {
        if self.0.get(&seed[..]).is_none() {
            let mut submap = RbTree::new();
            submap.insert(message, Found);
            self.0.insert(seed, submap);
        } else {
            self.0.modify(&seed[..], |submap| {
                submap.insert(message, Found);
            });
        }
    }

    pub fn delete(&mut self, seed: &[u8], message: &[u8]) {
        self.0.modify(seed, |m| {
            m.delete(message);
        });
    }

    pub fn root_hash(&self) -> Hash {
        self.0.root_hash()
    }

    pub fn witness(&self, seed: &[u8], message: &[u8]) -> Option<HashTree<'_>> {
        self.0.get(seed)?.get(message)?;
        let witness = self
            .0
            .nested_witness(seed, |nested| nested.witness(message));
        Some(witness)
    }
}
