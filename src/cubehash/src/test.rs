use super::*;

fn hash64(data: &[u8]) -> String {
    let mut h = CubeHash::new(64);
    h.update(data);
    hex::encode(&h.finalize())
}

#[test]
fn test_hello() {
    assert_eq!(hash64(b"hello"), "f7acd519f51a6caa5387ae730ed999c4c31766d8477e4e1eef4275e9df07dc4c08adc4b64c9dc8359d711020f78627a4d1bcfecadd28a5263c05faf75e96555a".to_string());
    assert_eq!(hash64(b"Hello"), "dcc0503aae279a3c8c95fa1181d37c418783204e2e3048a081392fd61bace883a1f7c4c96b16b4060c42104f1ce45a622f1a9abaeb994beb107fed53a78f588c".to_string());
}
