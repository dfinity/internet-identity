#[cfg(test)]
mod test;

use hashtree::{
    fork, fork_hash, labeled, labeled_hash, leaf_hash, Hash,
    HashTree::{self, Empty, Leaf, Pruned},
};
use std::cmp::Ordering::{Equal, Greater, Less};
use std::fmt;

#[derive(Clone, Copy, PartialEq, Eq)]
enum Color {
    Red,
    Black,
}

impl Color {
    fn flip(self) -> Self {
        match self {
            Self::Red => Self::Black,
            Self::Black => Self::Red,
        }
    }
}

pub trait AsHashTree {
    fn root_hash(&self) -> Hash;
    fn as_hash_tree(&self) -> HashTree<'_>;
}

impl AsHashTree for Vec<u8> {
    fn root_hash(&self) -> Hash {
        leaf_hash(&self[..])
    }

    fn as_hash_tree(&self) -> HashTree<'_> {
        Leaf(&self[..])
    }
}

impl<T: AsHashTree + 'static> AsHashTree for RbTree<T> {
    fn root_hash(&self) -> Hash {
        if self.root.is_null() {
            Empty.reconstruct()
        } else {
            unsafe { (*self.root).subtree_hash.clone() }
        }
    }
    fn as_hash_tree(&self) -> HashTree<'_> {
        unsafe { Node::full_data_tree(self.root) }
    }
}

// 1. All leaves are black.
// 2. Children of a red node are black.
// 3. Every path from a node goes through the same number of black
//    nodes.
pub struct Node<T> {
    key: Vec<u8>,
    value: T,
    left: *mut Node<T>,
    right: *mut Node<T>,
    color: Color,

    /// Hash of the data hash subtree.  It depends only on the key and
    /// value and doesn't have to be recomputed on rotations.
    data_hash: Hash,

    /// Hash of the full hash tree built from this node and its
    /// children. It needs to be recomputed after every rotation.
    subtree_hash: Hash,
}

impl<T: AsHashTree + 'static> Node<T> {
    fn new(key: Vec<u8>, value: T) -> *mut Self {
        let value_hash = value.root_hash();
        let data_hash = labeled_hash(key.as_slice(), &value_hash);
        Box::into_raw(Box::new(Self {
            key,
            value,
            left: Node::null(),
            right: Node::null(),
            color: Color::Red,
            subtree_hash: data_hash.clone(),
            data_hash,
        }))
    }

    unsafe fn left_hash_tree<'a>(n: *mut Self) -> HashTree<'a> {
        debug_assert!(!n.is_null());
        if (*n).left.is_null() {
            Empty
        } else {
            Pruned((*(*n).left).subtree_hash.clone())
        }
    }

    unsafe fn right_hash_tree<'a>(n: *mut Self) -> HashTree<'a> {
        debug_assert!(!n.is_null());
        if (*n).right.is_null() {
            Empty
        } else {
            Pruned((*(*n).right).subtree_hash.clone())
        }
    }

    fn null() -> *mut Self {
        std::ptr::null::<Self>() as *mut Node<T>
    }

    unsafe fn data_tree<'a>(n: *mut Self) -> HashTree<'a> {
        debug_assert!(!n.is_null());

        labeled((*n).key.as_slice(), (*n).value.as_hash_tree())
    }

    unsafe fn subtree_with<'a>(
        n: *mut Self,
        f: impl FnOnce(&'a T) -> HashTree<'a>,
    ) -> HashTree<'a> {
        debug_assert!(!n.is_null());

        labeled((*n).key.as_slice(), f(&(*n).value))
    }

    unsafe fn full_data_tree<'a>(n: *mut Self) -> HashTree<'a> {
        if n.is_null() {
            return Empty;
        }
        three_way_fork(
            Self::full_data_tree((*n).left),
            Self::data_tree(n),
            Self::full_data_tree((*n).right),
        )
    }

    unsafe fn witness_tree<'a>(n: *mut Self) -> HashTree<'a> {
        debug_assert!(!n.is_null());
        let value_hash = (*n).value.root_hash();
        labeled((*n).key.as_slice(), Pruned(value_hash))
    }

    unsafe fn full_witness_tree<'a>(n: *mut Self) -> HashTree<'a> {
        if n.is_null() {
            return Empty;
        }
        three_way_fork(
            Self::full_witness_tree((*n).left),
            Self::witness_tree(n),
            Self::full_witness_tree((*n).right),
        )
    }

    unsafe fn delete(n: *mut Self) {
        if n.is_null() {
            return;
        }
        Self::delete((*n).left);
        Self::delete((*n).right);
        let _ = Box::from_raw(n);
    }

    unsafe fn subtree_hash(n: *mut Self) -> Hash {
        if n.is_null() {
            return Empty.reconstruct();
        }

        let h = &(*n).data_hash;

        match ((*n).left.is_null(), (*n).right.is_null()) {
            (true, true) => h.clone(),
            (false, true) => fork_hash(&(*(*n).left).subtree_hash, &h),
            (true, false) => fork_hash(&h, &(*(*n).right).subtree_hash),
            (false, false) => fork_hash(
                &(*(*n).left).subtree_hash,
                &fork_hash(&h, &(*(*n).right).subtree_hash),
            ),
        }
    }
}

/// Implements mutable Leaf-leaning red-black trees as defined in
/// https://www.cs.princeton.edu/~rs/talks/LLRB/LLRB.pdf
pub struct RbTree<T: AsHashTree + 'static> {
    root: *mut Node<T>,
}

impl<T: AsHashTree + 'static> Drop for RbTree<T> {
    fn drop(&mut self) {
        unsafe { Node::delete(self.root) }
    }
}

impl<T: AsHashTree + 'static> Default for RbTree<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: AsHashTree + 'static> RbTree<T> {
    pub fn new() -> Self {
        Self { root: Node::null() }
    }

    pub fn get(&self, key: &[u8]) -> Option<&T> {
        unsafe {
            let mut root = self.root;
            while !root.is_null() {
                match key.cmp((*root).key.as_slice()) {
                    Equal => return Some(&(*root).value),
                    Less => root = (*root).left,
                    Greater => root = (*root).right,
                }
            }
            None
        }
    }

    pub fn modify(&mut self, key: &[u8], f: impl FnOnce(&mut T)) {
        unsafe {
            let mut root = self.root;
            while !root.is_null() {
                match key.cmp((*root).key.as_slice()) {
                    Equal => {
                        f(&mut (*root).value);
                        (*root).subtree_hash = Node::subtree_hash(root);
                        return;
                    }
                    Less => root = (*root).left,
                    Greater => root = (*root).right,
                }
            }
        }
    }

    fn range_witness<'a>(
        &'a self,
        left: Option<&'a [u8]>,
        right: Option<&'a [u8]>,
    ) -> HashTree<'a> {
        match (left, right) {
            (None, None) => Empty,
            (Some(l), None) => self.witness_range_above(l),
            (None, Some(r)) => self.witness_range_below(r),
            (Some(l), Some(r)) => self.witness_range_exclusive(l, r),
        }
    }

    pub fn witness<'a>(&'a self, key: &[u8]) -> HashTree<'a> {
        if let Some(t) = self.lookup_and_build_witness(key, |v| v.as_hash_tree()) {
            return t;
        }
        self.range_witness(self.left_neighbor(key), self.right_neighbor(key))
    }

    pub fn nested_witness<'a>(
        &'a self,
        key: &[u8],
        f: impl FnOnce(&'a T) -> HashTree<'a>,
    ) -> HashTree<'a> {
        if let Some(t) = self.lookup_and_build_witness(key, f) {
            return t;
        }
        self.range_witness(self.left_neighbor(key), self.right_neighbor(key))
    }

    pub fn keys(&self) -> HashTree<'_> {
        unsafe { Node::full_witness_tree(self.root) }
    }

    pub fn key_range(&self, first: &[u8], last: &[u8]) -> HashTree<'_> {
        self.range_witness(self.left_neighbor(first), self.right_neighbor(last))
    }

    pub fn keys_with_prefix(&self, prefix: &[u8]) -> HashTree<'_> {
        self.range_witness(
            self.left_neighbor(prefix),
            self.right_prefix_neighbor(prefix),
        )
    }

    pub fn for_each<'a, F>(&'a self, mut f: F)
    where
        F: 'a + FnMut(&'a [u8], &'a T),
    {
        unsafe fn visit<'a, T, F>(n: *mut Node<T>, f: &mut F)
        where
            F: 'a + FnMut(&'a [u8], &'a T),
            T: 'a + AsHashTree,
        {
            debug_assert!(!n.is_null());
            if !(*n).left.is_null() {
                visit((*n).left, f)
            }
            (*f)((*n).key.as_slice(), &(*n).value);
            if !(*n).right.is_null() {
                visit((*n).right, f)
            }
        }
        if self.root.is_null() {
            return;
        }
        unsafe { visit(self.root, &mut f) }
    }

    fn witness_range_above<'a>(&'a self, lo: &[u8]) -> HashTree<'a> {
        unsafe fn go<'a, T: AsHashTree + 'static>(n: *mut Node<T>, lo: &[u8]) -> HashTree<'a> {
            if n.is_null() {
                return Empty;
            }
            match (*n).key.as_slice().cmp(lo) {
                Equal => three_way_fork(
                    Node::left_hash_tree(n),
                    Node::witness_tree(n),
                    Node::full_witness_tree((*n).right),
                ),
                Less => three_way_fork(
                    Node::left_hash_tree(n),
                    Pruned((*n).data_hash.clone()),
                    go((*n).right, lo),
                ),
                Greater => three_way_fork(
                    go((*n).left, lo),
                    Node::witness_tree(n),
                    Node::full_witness_tree((*n).right),
                ),
            }
        }
        unsafe { go(self.root, lo) }
    }

    fn witness_range_below<'a>(&'a self, hi: &[u8]) -> HashTree<'a> {
        unsafe fn go<'a, T: AsHashTree + 'static>(n: *mut Node<T>, hi: &[u8]) -> HashTree<'a> {
            if n.is_null() {
                return Empty;
            }
            match (*n).key.as_slice().cmp(hi) {
                Equal => three_way_fork(
                    Node::full_witness_tree((*n).left),
                    Node::witness_tree(n),
                    Node::right_hash_tree(n),
                ),
                Greater => three_way_fork(
                    go((*n).left, hi),
                    Pruned((*n).data_hash),
                    Node::right_hash_tree(n),
                ),
                Less => three_way_fork(
                    Node::full_witness_tree((*n).left),
                    Node::witness_tree(n),
                    go((*n).right, hi),
                ),
            }
        }
        unsafe { go(self.root, hi) }
    }

    fn witness_range_exclusive<'a>(&'a self, lo: &[u8], hi: &[u8]) -> HashTree<'a> {
        debug_assert!(lo <= hi, "lo = {:?} > hi = {:?}", lo, hi);
        unsafe fn go<'a, T: AsHashTree + 'static>(
            n: *mut Node<T>,
            lo: &[u8],
            hi: &[u8],
        ) -> HashTree<'a> {
            if n.is_null() {
                return Empty;
            }
            let k = (*n).key.as_slice();
            match (lo.cmp(k), k.cmp(hi)) {
                (Less, Less) => three_way_fork(
                    go((*n).left, lo, hi),
                    Node::witness_tree(n),
                    go((*n).right, lo, hi),
                ),
                (Equal, _) => three_way_fork(
                    Node::left_hash_tree(n),
                    Node::witness_tree(n),
                    go((*n).right, lo, hi),
                ),
                (_, Equal) => three_way_fork(
                    go((*n).left, lo, hi),
                    Node::witness_tree(n),
                    Node::right_hash_tree(n),
                ),
                (Less, Greater) => three_way_fork(
                    go((*n).left, lo, hi),
                    Pruned((*n).data_hash.clone()),
                    Node::right_hash_tree(n),
                ),
                (Greater, Less) => three_way_fork(
                    Node::left_hash_tree(n),
                    Pruned((*n).data_hash.clone()),
                    go((*n).right, lo, hi),
                ),
                _ => Pruned((*n).subtree_hash.clone()),
            }
        }
        unsafe { go(self.root, lo, hi) }
    }

    fn left_neighbor<'a>(&'a self, key: &[u8]) -> Option<&'a [u8]> {
        unsafe fn go<'a, T>(n: *mut Node<T>, key: &[u8]) -> Option<&'a [u8]> {
            if n.is_null() {
                return None;
            }
            match (*n).key.as_slice().cmp(key) {
                Less => go((*n).right, key).or_else(|| Some((*n).key.as_slice())),
                Greater | Equal => go((*n).left, key),
            }
        }
        unsafe { go(self.root, key) }
    }

    fn right_neighbor<'a>(&'a self, key: &[u8]) -> Option<&'a [u8]> {
        unsafe fn go<'a, T>(n: *mut Node<T>, key: &[u8]) -> Option<&'a [u8]> {
            if n.is_null() {
                return None;
            }
            match (*n).key.as_slice().cmp(key) {
                Greater => go((*n).left, key).or_else(|| Some((*n).key.as_slice())),
                Less | Equal => go((*n).right, key),
            }
        }
        unsafe { go(self.root, key) }
    }

    fn right_prefix_neighbor<'a>(&'a self, prefix: &[u8]) -> Option<&'a [u8]> {
        fn is_prefix_of(p: &[u8], x: &[u8]) -> bool {
            if p.len() > x.len() {
                return false;
            }
            &x[0..p.len()] == &p[..]
        }
        unsafe fn go<'a, T>(n: *mut Node<T>, prefix: &[u8]) -> Option<&'a [u8]> {
            if n.is_null() {
                return None;
            }
            let node_key = (*n).key.as_slice();
            match node_key.cmp(prefix) {
                Greater if is_prefix_of(prefix, node_key) => go((*n).right, prefix),
                Greater => go((*n).left, prefix).or_else(|| Some(node_key)),
                Less | Equal => go((*n).right, prefix),
            }
        }
        unsafe { go(self.root, prefix) }
    }

    fn lookup_and_build_witness<'a>(
        &'a self,
        key: &[u8],
        f: impl FnOnce(&'a T) -> HashTree<'a>,
    ) -> Option<HashTree<'a>> {
        unsafe fn go<'a, T: AsHashTree + 'static>(
            n: *mut Node<T>,
            key: &[u8],
            f: impl FnOnce(&'a T) -> HashTree<'a>,
        ) -> Option<HashTree<'a>> {
            if n.is_null() {
                return None;
            }
            match key.cmp((*n).key.as_slice()) {
                Equal => Some(three_way_fork(
                    Node::left_hash_tree(n),
                    Node::subtree_with(n, f),
                    Node::right_hash_tree(n),
                )),
                Less => {
                    let subtree = go((*n).left, key, f)?;
                    Some(three_way_fork(
                        subtree,
                        Pruned((*n).data_hash.clone()),
                        Node::right_hash_tree(n),
                    ))
                }
                Greater => {
                    let subtree = go((*n).right, key, f)?;
                    Some(three_way_fork(
                        Node::left_hash_tree(n),
                        Pruned((*n).data_hash.clone()),
                        subtree,
                    ))
                }
            }
        }
        unsafe { go(self.root, key, f) }
    }

    pub fn insert(&mut self, key: Vec<u8>, value: T) {
        unsafe fn go<T: AsHashTree + 'static>(
            mut h: *mut Node<T>,
            k: Vec<u8>,
            v: T,
        ) -> *mut Node<T> {
            if h.is_null() {
                return Node::new(k, v);
            }

            match k.as_slice().cmp((*h).key.as_slice()) {
                Equal => {
                    let value_hash = v.root_hash();
                    (*h).value = v;
                    (*h).data_hash = labeled_hash(k.as_slice(), &value_hash);
                    (*h).subtree_hash = Node::subtree_hash(h);
                }
                Less => {
                    (*h).left = go((*h).left, k, v);
                    (*h).subtree_hash = Node::subtree_hash(h);
                }
                Greater => {
                    (*h).right = go((*h).right, k, v);
                    (*h).subtree_hash = Node::subtree_hash(h);
                }
            }
            balance(h)
        }
        unsafe {
            let mut root = go(self.root, key, value);
            (*root).color = Color::Black;
            debug_assert!(
                is_balanced(root),
                "the tree is not balanced:\n{:?}",
                DebugView(root)
            );
            self.root = root;
        }
    }

    pub fn delete(&mut self, key: &[u8]) {
        unsafe fn move_red_left<T: AsHashTree + 'static>(mut h: *mut Node<T>) -> *mut Node<T> {
            flip_colors(h);
            if is_red((*(*h).right).left) {
                (*h).right = rotate_right((*h).right);
                h = rotate_left(h);
                flip_colors(h);
            }
            h
        }

        unsafe fn move_red_right<T: AsHashTree + 'static>(mut h: *mut Node<T>) -> *mut Node<T> {
            flip_colors(h);
            if is_red((*(*h).left).left) {
                h = rotate_right(h);
                flip_colors(h);
            }
            h
        }

        #[inline]
        unsafe fn min<T: AsHashTree + 'static>(mut h: *mut Node<T>) -> *mut Node<T> {
            while !(*h).left.is_null() {
                h = (*h).left;
            }
            h
        }

        unsafe fn delete_min<T: AsHashTree + 'static>(mut h: *mut Node<T>) -> *mut Node<T> {
            if (*h).left.is_null() {
                debug_assert!((*h).right.is_null());
                Node::delete(h);
                return Node::null();
            }
            if !is_red((*h).left) && !is_red((*(*h).left).left) {
                h = move_red_left(h);
            }
            (*h).left = delete_min((*h).left);
            (*h).subtree_hash = Node::subtree_hash(h);
            balance(h)
        }

        unsafe fn go<T: AsHashTree + 'static>(mut h: *mut Node<T>, key: &[u8]) -> *mut Node<T> {
            if key < (*h).key.as_slice() {
                if !is_red((*h).left) && !is_red((*(*h).left).left) {
                    h = move_red_left(h);
                }
                (*h).left = go((*h).left, key);
                (*h).subtree_hash = Node::subtree_hash(h);
            } else {
                if is_red((*h).left) {
                    h = rotate_right(h);
                }
                if key == (*h).key && (*h).right.is_null() {
                    debug_assert!((*h).left.is_null());
                    Node::delete(h);
                    return Node::null();
                }

                if !is_red((*h).right) && !is_red((*(*h).right).left) {
                    h = move_red_right(h);
                }

                if key == (*h).key.as_slice() {
                    let m = min((*h).right);
                    std::mem::swap(&mut (*h).key, &mut (*m).key);
                    std::mem::swap(&mut (*h).value, &mut (*m).value);
                    (*h).right = delete_min((*h).right);
                    (*h).subtree_hash = Node::subtree_hash(h);
                } else {
                    (*h).right = go((*h).right, key);
                }
            }
            balance(h)
        }

        unsafe {
            if self.get(key).is_none() {
                return;
            }
            if !is_red((*self.root).left) && !is_red((*self.root).right) {
                (*self.root).color = Color::Red;
            }
            self.root = go(self.root, key);
            if !self.root.is_null() {
                (*self.root).color = Color::Black;
            }
            debug_assert!(
                is_balanced(self.root),
                "unbalanced map: {:?}",
                DebugView(self.root)
            );
            debug_assert!(self.get(key).is_none());
        }
    }
}

fn three_way_fork<'a>(l: HashTree<'a>, m: HashTree<'a>, r: HashTree<'a>) -> HashTree<'a> {
    match (l, m, r) {
        (Empty, m, Empty) => m,
        (l, m, Empty) => fork(l, m),
        (Empty, m, r) => fork(m, r),
        (Pruned(lhash), Pruned(mhash), Pruned(rhash)) => {
            Pruned(fork_hash(&lhash, &fork_hash(&mhash, &rhash)))
        }
        (l, Pruned(mhash), Pruned(rhash)) => fork(l, Pruned(fork_hash(&mhash, &rhash))),
        (l, m, r) => fork(l, fork(m, r)),
    }
}

// helper functions
unsafe fn is_red<T>(x: *const Node<T>) -> bool {
    if x.is_null() {
        false
    } else {
        (*x).color == Color::Red
    }
}

unsafe fn balance<T: AsHashTree + 'static>(mut h: *mut Node<T>) -> *mut Node<T> {
    assert!(!h.is_null());

    if is_red((*h).right) && !is_red((*h).left) {
        h = rotate_left(h);
    }
    if is_red((*h).left) && is_red((*(*h).left).left) {
        h = rotate_right(h);
    }
    if is_red((*h).left) && is_red((*h).right) {
        flip_colors(h)
    }
    h
}
/// Make a left-leaning link lean to the right.
unsafe fn rotate_right<T: AsHashTree + 'static>(h: *mut Node<T>) -> *mut Node<T> {
    debug_assert!(!h.is_null());
    debug_assert!(is_red((*h).left));

    let mut x = (*h).left;
    (*h).left = (*x).right;
    (*x).right = h;
    (*x).color = (*(*x).right).color;
    (*(*x).right).color = Color::Red;

    (*h).subtree_hash = Node::subtree_hash(h);
    (*x).subtree_hash = Node::subtree_hash(x);

    x
}

unsafe fn rotate_left<T: AsHashTree + 'static>(h: *mut Node<T>) -> *mut Node<T> {
    debug_assert!(!h.is_null());
    debug_assert!(is_red((*h).right));

    let mut x = (*h).right;
    (*h).right = (*x).left;
    (*x).left = h;
    (*x).color = (*(*x).left).color;
    (*(*x).left).color = Color::Red;

    (*h).subtree_hash = Node::subtree_hash(h);
    (*x).subtree_hash = Node::subtree_hash(x);

    x
}

unsafe fn flip_colors<T>(h: *mut Node<T>) {
    (*h).color = (*h).color.flip();
    (*(*h).left).color = (*(*h).left).color.flip();
    (*(*h).right).color = (*(*h).right).color.flip();
}

unsafe fn is_balanced<T>(root: *mut Node<T>) -> bool {
    unsafe fn go<T>(node: *mut Node<T>, mut num_black: usize) -> bool {
        if node.is_null() {
            return num_black == 0;
        }
        if !is_red(node) {
            debug_assert!(num_black > 0);
            num_black -= 1;
        }
        go((*node).left, num_black) && go((*node).right, num_black)
    }

    let mut num_black = 0;
    let mut x = root;
    while !x.is_null() {
        if !is_red(x) {
            num_black += 1;
        }
        x = (*x).left;
    }
    go(root, num_black)
}

struct DebugView<T>(*const Node<T>);

impl<T> fmt::Debug for DebugView<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        unsafe fn go<T>(
            f: &mut fmt::Formatter<'_>,
            h: *const Node<T>,
            offset: usize,
        ) -> fmt::Result {
            if h.is_null() {
                writeln!(f, "{:width$}[B] <null>", "", width = offset)
            } else {
                writeln!(
                    f,
                    "{:width$}[{}] {:?}",
                    "",
                    if is_red(h) { "R" } else { "B" },
                    (*h).key,
                    width = offset
                )?;
                go(f, (*h).left, offset + 2)?;
                go(f, (*h).right, offset + 2)
            }
        }
        unsafe { go(f, self.0, 0) }
    }
}
