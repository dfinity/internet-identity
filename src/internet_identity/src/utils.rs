use sha2::{Digest, Sha256};

/// Safely converts unbounded slice to a fixed-size slice.
pub fn slice_to_bounded_32(slice: &[u8]) -> [u8; 32] {
    let mut bounded = [0u8; 32];
    // Don't copy more than 32 bytes
    let copy_len = slice.len().min(32);
    bounded[..copy_len].copy_from_slice(&slice[..copy_len]);
    bounded
}

pub fn sha256sum(slice: &[u8]) -> [u8; 32] {
    let mut hasher = Sha256::new();
    hasher.update(slice);
    let sha256sum = hasher.finalize();
    slice_to_bounded_32(&sha256sum)
}

/// True if `host` (host or `host:port`) is loopback.
///
/// Names under `.localhost` count: RFC 6761 reserves the whole domain for the
/// loopback address, and a canister served by a local gateway is reached at one.
pub fn is_loopback_host(host: &str) -> bool {
    let bare = host.split(':').next().unwrap_or(host).to_ascii_lowercase();
    matches!(bare.as_str(), "localhost" | "127.0.0.1") || bare.ends_with(".localhost")
}

#[cfg(test)]
mod tests {
    use super::is_loopback_host;

    #[test]
    fn a_name_under_localhost_is_loopback_too() {
        assert!(is_loopback_host("localhost"));
        assert!(is_loopback_host("127.0.0.1:8000"));
        assert!(is_loopback_host(
            "t63gs-up777-77776-aaaba-cai.localhost:8000"
        ));

        assert!(!is_loopback_host("nice-name.com"));
        assert!(!is_loopback_host("localhost.example.com"));
        assert!(!is_loopback_host("notlocalhost"));
    }
}
