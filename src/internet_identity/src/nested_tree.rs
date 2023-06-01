//! This module has been copied from https://github.com/dfinity/sdk/blob/master/src/canisters/frontend/ic-certified-assets/src/asset_certification/tree.rs
//! (commit d6a3cb89dc3bfb2c07500e1fc781667e3b0cda5a).
//! TODO: Replace with library, once the trust team has extracted a reusable certification library.
#![allow(dead_code)] // we don't need all the features provided here

use ic_certified_map::{AsHashTree, HashTree, RbTree};

pub trait NestedTreeKeyRequirements: Clone + AsRef<[u8]> + 'static {}
pub trait NestedTreeValueRequirements: AsHashTree + 'static {}
impl<T> NestedTreeKeyRequirements for T where T: Clone + AsRef<[u8]> + 'static {}
impl<T> NestedTreeValueRequirements for T where T: AsHashTree + 'static {}

#[derive(Debug, Clone)]
pub enum NestedTree<K: NestedTreeKeyRequirements, V: NestedTreeValueRequirements> {
    Leaf(V),
    Nested(RbTree<K, NestedTree<K, V>>),
}

impl<K: NestedTreeKeyRequirements, V: NestedTreeValueRequirements> Default for NestedTree<K, V> {
    fn default() -> Self {
        NestedTree::Nested(RbTree::<K, NestedTree<K, V>>::new())
    }
}

impl<K: NestedTreeKeyRequirements, V: NestedTreeValueRequirements> AsHashTree for NestedTree<K, V> {
    fn root_hash(&self) -> ic_certified_map::Hash {
        match self {
            NestedTree::Leaf(a) => a.root_hash(),
            NestedTree::Nested(tree) => tree.root_hash(),
        }
    }

    fn as_hash_tree(&self) -> HashTree<'_> {
        match self {
            NestedTree::Leaf(a) => a.as_hash_tree(),
            NestedTree::Nested(tree) => tree.as_hash_tree(),
        }
    }
}

impl<K: NestedTreeKeyRequirements, V: NestedTreeValueRequirements> NestedTree<K, V> {
    pub fn get(&self, path: &[K]) -> Option<&V> {
        if let Some(key) = path.get(0) {
            match self {
                NestedTree::Leaf(_) => None,
                NestedTree::Nested(tree) => tree
                    .get(key.as_ref())
                    .and_then(|child| child.get(&path[1..])),
            }
        } else {
            match self {
                NestedTree::Leaf(value) => Some(value),
                NestedTree::Nested(_) => None,
            }
        }
    }

    /// Returns true if there is a leaf at the specified path
    pub fn contains_leaf(&self, path: &[K]) -> bool {
        if let Some(key) = path.get(0) {
            match self {
                NestedTree::Leaf(_) => false,
                NestedTree::Nested(tree) => tree
                    .get(key.as_ref())
                    .map(|child| child.contains_leaf(&path[1..]))
                    .unwrap_or(false),
            }
        } else {
            matches!(self, NestedTree::Leaf(_))
        }
    }

    /// Returns true if there is a leaf or a subtree at the specified path
    pub fn contains_path(&self, path: &[K]) -> bool {
        if let Some(key) = path.get(0) {
            match self {
                NestedTree::Leaf(_) => false,
                NestedTree::Nested(tree) => tree
                    .get(key.as_ref())
                    .map(|child| child.contains_path(&path[1..]))
                    .unwrap_or(false),
            }
        } else {
            true
        }
    }

    pub fn insert(&mut self, path: &[K], value: V) {
        if let Some(key) = path.get(0) {
            match self {
                NestedTree::Leaf(_) => {
                    *self = NestedTree::default();
                    self.insert(path, value);
                }
                NestedTree::Nested(tree) => {
                    if tree.get(key.as_ref()).is_some() {
                        tree.modify(key.as_ref(), |child| child.insert(&path[1..], value));
                    } else {
                        tree.insert(key.clone(), NestedTree::default());
                        self.insert(path, value);
                    }
                }
            }
        } else {
            *self = NestedTree::Leaf(value);
        }
    }

    pub fn delete(&mut self, path: &[K]) {
        if let Some(key) = path.get(0) {
            match self {
                NestedTree::Leaf(_) => {}
                NestedTree::Nested(tree) => {
                    tree.modify(key.as_ref(), |child| child.delete(&path[1..]));
                }
            }
        } else {
            *self = NestedTree::default();
        }
    }

    pub fn witness(&self, path: &[K]) -> HashTree {
        if let Some(key) = path.get(0) {
            match self {
                NestedTree::Leaf(value) => value.as_hash_tree(),
                NestedTree::Nested(tree) => {
                    tree.nested_witness(key.as_ref(), |tree| tree.witness(&path[1..]))
                }
            }
        } else {
            self.as_hash_tree()
        }
    }
}

pub fn merge_hash_trees<'a>(lhs: HashTree<'a>, rhs: HashTree<'a>) -> HashTree<'a> {
    use HashTree::{Empty, Fork, Labeled, Leaf, Pruned};

    match (lhs, rhs) {
        (Pruned(l), Pruned(r)) => {
            if l != r {
                panic!("merge_hash_trees: inconsistent hashes");
            }
            Pruned(l)
        }
        (Pruned(_), r) => r,
        (l, Pruned(_)) => l,
        (Fork(l), Fork(r)) => Fork(Box::new((
            merge_hash_trees(l.0, r.0),
            merge_hash_trees(l.1, r.1),
        ))),
        (Labeled(l_label, l), Labeled(r_label, r)) => {
            if l_label != r_label {
                panic!("merge_hash_trees: inconsistent hash tree labels");
            }
            Labeled(l_label, Box::new(merge_hash_trees(*l, *r)))
        }
        (Empty, Empty) => Empty,
        (Leaf(l), Leaf(r)) => {
            if l != r {
                panic!("merge_hash_trees: inconsistent leaves");
            }
            Leaf(l)
        }
        (_l, _r) => {
            panic!("merge_hash_trees: inconsistent tree structure");
        }
    }
}

#[test]
fn nested_tree_operation() {
    let mut tree: NestedTree<&str, Vec<u8>> = NestedTree::default();
    // insertion
    tree.insert(&["one", "two"], vec![2]);
    tree.insert(&["one", "three"], vec![3]);
    assert_eq!(tree.get(&["one", "two"]), Some(&vec![2]));
    assert_eq!(tree.get(&["one", "two", "three"]), None);
    assert_eq!(tree.get(&["one"]), None);
    assert!(tree.contains_leaf(&["one", "two"]));
    assert!(tree.contains_path(&["one"]));
    assert!(!tree.contains_leaf(&["one", "two", "three"]));
    assert!(!tree.contains_path(&["one", "two", "three"]));
    assert!(!tree.contains_leaf(&["one"]));

    // deleting non-existent key doesn't do anything
    tree.delete(&["one", "two", "three"]);
    assert_eq!(tree.get(&["one", "two"]), Some(&vec![2]));
    assert!(tree.contains_leaf(&["one", "two"]));

    // deleting existing key works
    tree.delete(&["one", "three"]);
    assert_eq!(tree.get(&["one", "two"]), Some(&vec![2]));
    assert_eq!(tree.get(&["one", "three"]), None);
    assert!(tree.contains_leaf(&["one", "two"]));
    assert!(!tree.contains_leaf(&["one", "three"]));

    // deleting subtree works
    tree.delete(&["one"]);
    assert_eq!(tree.get(&["one", "two"]), None);
    assert_eq!(tree.get(&["one"]), None);
    assert!(!tree.contains_leaf(&["one", "two"]));
    assert!(!tree.contains_leaf(&["one"]));
}
