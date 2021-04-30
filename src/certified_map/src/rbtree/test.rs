use super::*;
use std::convert::AsRef;

fn insert(t: &mut RbTree<Vec<u8>, Vec<u8>>, k: impl AsRef<[u8]>, v: impl AsRef<[u8]>) {
    t.insert(k.as_ref().to_vec(), v.as_ref().to_vec())
}

#[test]
fn test_witness() {
    let mut t = RbTree::<Vec<u8>, Vec<u8>>::new();
    for i in 0u64..10 {
        let key = (1 + 2 * i).to_be_bytes();
        let val = (1 + 2 * i).to_le_bytes();
        insert(&mut t, key, val);
        assert_eq!(t.get(&key[..]).map(|v| &v[..]), Some(&val[..]));
    }

    for i in 0u64..10 {
        let key = (1 + 2 * i).to_be_bytes();
        let ht = t.witness(&key[..]);
        assert_eq!(
            ht.reconstruct(),
            t.root_hash(),
            "key: {}, witness {:?}",
            hex::encode(key),
            ht
        );

        let ht = t.keys_with_prefix(&key[..]);
        assert_eq!(
            ht.reconstruct(),
            t.root_hash(),
            "key: {}, left neighbor: {:?}, right neighbor: {:?}, witness {:?}",
            hex::encode(key),
            t.left_neighbor(&key[..]).map(hex::encode),
            t.right_prefix_neighbor(&key[..]).map(hex::encode),
            ht
        );
    }

    for i in 0u64..10 {
        for j in i..10 {
            let start = (2 * i).to_be_bytes();
            let end = (2 * j).to_be_bytes();
            let ht = t.key_range(&start[..], &end[..]);
            assert_eq!(
                ht.reconstruct(),
                t.root_hash(),
                "key range: [{}, {}], witness {:?}",
                hex::encode(&start[..]),
                hex::encode(&end[..]),
                ht
            );
        }
    }

    for i in 0u64..11 {
        let key = (2 * i).to_be_bytes();
        let ht = t.witness(&key[..]);
        assert_eq!(
            ht.reconstruct(),
            t.root_hash(),
            "key: {}, witness {:?}",
            hex::encode(&key[..]),
            ht
        );
    }

    for i in 0u64..10 {
        let key = (1 + 2 * i).to_be_bytes();
        let val = (1 + 2 * i).to_le_bytes();

        assert_eq!(t.get(&key[..]).map(|v| &v[..]), Some(&val[..]));

        t.delete(&key[..]);
        for j in 0u64..10 {
            let witness_key = (1 + 2 * j).to_be_bytes();
            let ht = t.witness(&witness_key[..]);
            assert_eq!(
                ht.reconstruct(),
                t.root_hash(),
                "key: {}, witness {:?}",
                hex::encode(&key[..]),
                ht
            );
        }
        assert_eq!(t.get(&key[..]), None);
    }
}

#[test]
fn test_key_neighbors() {
    let mut t = RbTree::<Vec<u8>, Vec<u8>>::new();
    t.insert(vec![1], vec![10]);
    t.insert(vec![3], vec![30]);

    assert_eq!(t.left_neighbor(&[0u8][..]), None);
    assert_eq!(t.left_neighbor(&[1u8][..]), None);
    assert_eq!(t.left_neighbor(&[2u8][..]), Some(&[1u8][..]));
    assert_eq!(t.left_neighbor(&[3u8][..]), Some(&[1u8][..]));
    assert_eq!(t.left_neighbor(&[4u8][..]), Some(&[3u8][..]));

    assert_eq!(t.right_neighbor(&[0u8][..]), Some(&[1u8][..]));
    assert_eq!(t.right_neighbor(&[1u8][..]), Some(&[3u8][..]));
    assert_eq!(t.right_neighbor(&[2u8][..]), Some(&[3u8][..]));
    assert_eq!(t.right_neighbor(&[3u8][..]), None);
    assert_eq!(t.right_neighbor(&[4u8][..]), None);
}

#[test]
fn test_prefix_neighbor() {
    let mut t = RbTree::<Vec<u8>, Vec<u8>>::new();
    insert(&mut t, b"a/b", vec![0]);
    insert(&mut t, b"a/b/c", vec![1]);
    insert(&mut t, b"a/b/d", vec![2]);
    insert(&mut t, b"a/c/d", vec![3]);

    assert_eq!(t.right_prefix_neighbor(b"a/b/c"), Some(&b"a/b/d"[..]));
    assert_eq!(t.right_prefix_neighbor(b"a/b"), Some(&b"a/c/d"[..]));
    assert_eq!(t.right_prefix_neighbor(b"a/c/d"), None);
    assert_eq!(t.right_prefix_neighbor(b"a"), None);
}

#[test]
fn simple_delete_test() {
    let mut t = RbTree::<Vec<u8>, Vec<u8>>::new();
    insert(&mut t, b"x", b"a");
    insert(&mut t, b"y", b"b");
    insert(&mut t, b"z", b"c");

    t.delete(b"x");
    assert_eq!(t.get(b"x"), None);
    assert_eq!(t.get(b"y").map(|v| &v[..]), Some(&b"b"[..]));
    assert_eq!(t.get(b"z").map(|v| &v[..]), Some(&b"c"[..]));

    t.delete(b"y");
    assert_eq!(t.get(b"y").map(|v| &v[..]), None);
    assert_eq!(t.get(b"z").map(|v| &v[..]), Some(&b"c"[..]));

    t.delete(b"z");
    assert_eq!(t.get(b"z").map(|v| &v[..]), None);
}

#[test]
fn simple_delete_test_2() {
    let mut t = RbTree::<Vec<u8>, Vec<u8>>::new();
    insert(&mut t, b"x", b"y");
    insert(&mut t, b"z", b"w");

    t.delete(b"z");
    assert_eq!(t.get(b"z"), None);
    assert_eq!(t.get(b"x").map(|v| &v[..]), Some(&b"y"[..]));
}

#[test]
fn map_model_test() {
    use std::collections::HashMap;

    let mut hm: HashMap<Vec<u8>, Vec<u8>> = HashMap::new();
    let mut rb = RbTree::<Vec<u8>, Vec<u8>>::new();

    for i in 0..100u64 {
        hm.insert(i.to_be_bytes().to_vec(), i.to_be_bytes().to_vec());
        insert(&mut rb, i.to_be_bytes(), i.to_be_bytes());

        for k in hm.keys() {
            assert_eq!(hm.get(k), rb.get(k));
        }
    }
    let keys: Vec<_> = hm.keys().cloned().collect();

    for k in keys {
        hm.remove(&k);

        assert!(rb.get(&k).is_some());
        rb.delete(&k);
        assert!(rb.get(&k).is_none());

        for k in hm.keys() {
            assert_eq!(hm.get(k), rb.get(k));
        }
    }
    assert_eq!(super::debug_alloc::count_allocated_pointers(), 0);
}

#[test]
fn test_nested_witness() {
    let mut rb: RbTree<Vec<u8>, RbTree<Vec<u8>, Vec<u8>>> = RbTree::new();
    let mut nested = RbTree::new();
    nested.insert(b"bottom".to_vec(), b"data".to_vec());
    rb.insert(b"top".to_vec(), nested);

    let ht = rb.nested_witness(&b"top"[..], |v| v.witness(&b"bottom"[..]));

    assert_eq!(ht.reconstruct(), rb.root_hash());
    match ht {
        HashTree::Labeled(lt, tt) => {
            assert_eq!(lt, b"top");
            match &(*tt) {
                HashTree::Labeled(lb, _) => {
                    assert_eq!(lb, b"bottom");
                }
                other => panic!("unexpected nested tree: {:?}", other),
            }
        }
        other => panic!("expected a labeled tree, got {:?}", other),
    }

    rb.modify(b"top", |m| m.delete(b"bottom"));
    let ht = rb.nested_witness(&b"top"[..], |v| v.witness(&b"bottom"[..]));
    assert_eq!(ht.reconstruct(), rb.root_hash());
}
