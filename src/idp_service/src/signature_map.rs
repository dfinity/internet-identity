use certified_map::{AsHashTree, RbTree};
use hashtree::{leaf_hash, Hash, HashTree};

struct Unit;

impl AsHashTree for Unit {
    fn root_hash(&self) -> Hash {
        leaf_hash(&b""[..])
    }
    fn as_hash_tree(&self) -> HashTree<'_> {
        HashTree::Leaf(&b""[..])
    }
}

#[derive(Default)]
pub struct SignatureMap(RbTree<Hash, RbTree<Hash, Unit>>);

impl SignatureMap {
    pub fn put(&mut self, seed: Hash, message: Hash) {
        if self.0.get(&seed[..]).is_none() {
            let mut submap = RbTree::new();
            submap.insert(message, Unit);
            self.0.insert(seed, submap);
        } else {
            self.0.modify(&seed[..], |submap| {
                submap.insert(message, Unit);
            });
        }
    }

    pub fn delete(&mut self, seed: Hash, message: Hash) {
        let mut is_empty = false;
        self.0.modify(&seed[..], |m| {
            m.delete(&message[..]);
            is_empty = m.is_empty();
        });
        if is_empty {
            self.0.delete(&seed[..]);
        }
    }

    pub fn root_hash(&self) -> Hash {
        self.0.root_hash()
    }

    pub fn witness(&self, seed: Hash, message: Hash) -> Option<HashTree<'_>> {
        self.0.get(&seed[..])?.get(&message[..])?;
        let witness = self
            .0
            .nested_witness(&seed[..], |nested| nested.witness(&message[..]));
        Some(witness)
    }
}
