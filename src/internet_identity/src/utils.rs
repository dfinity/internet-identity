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
