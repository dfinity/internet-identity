//! This module is a port of Bernstein's CubeHash1632 "simple"
//! implementation in C:
//! https://github.com/floodyberry/supercop/blob/master/crypto_hash/cubehash1632/simple/cubehash.c
use std::num::Wrapping;

const CUBEHASH_INITIAL_ROUNDS: u16 = 10;
const CUBEHASH_FINAL_ROUNDS: u16 = 10;
const CUBEHASH_ROUNDS: u16 = 16;
const CUBEHASH_BLOCKBYTES: u16 = 32;

// CubeHash 10+16/10+32-512

#[inline]
fn rotate(a: Wrapping<u32>, b: usize) -> Wrapping<u32> {
    (a << b) | (a >> (32 - b))
}

fn transform(state: &mut [Wrapping<u32>; 32]) {
    let mut y: [Wrapping<u32>; 16] = [Wrapping(0); 16];

    for _round in 0..CUBEHASH_ROUNDS {
        for i in 0..16 {
            state[i + 16] += state[i];
        }
        for i in 0..16 {
            y[i ^ 8] = state[i];
        }
        for i in 0..16 {
            state[i] = rotate(y[i], 7);
        }
        for i in 0..16 {
            state[i] ^= state[i + 16];
        }
        for i in 0..16 {
            y[i ^ 2] = state[i + 16];
        }
        for i in 0..16 {
            state[i + 16] = y[i];
        }
        for i in 0..16 {
            state[i + 16] += state[i];
        }
        for i in 0..16 {
            y[i ^ 4] = state[i];
        }
        for i in 0..16 {
            state[i] = rotate(y[i], 11);
        }
        for i in 0..16 {
            state[i] ^= state[i + 16];
        }
        for i in 0..16 {
            y[i ^ 1] = state[i + 16];
        }
        for i in 0..16 {
            state[i + 16] = y[i];
        }
    }
}

pub struct CubeHash {
    state: [Wrapping<u32>; 32],
    hash_bytes: u16,
    pos: u16,
}

impl CubeHash {
    /// Constructs a new CubeHash state that produces a hash of the
    /// specified length.
    ///
    /// Panics if hash_bytes > 64
    pub fn new(hash_bytes: u16) -> Self {
        assert!(hash_bytes <= 64);

        let mut state = [Wrapping(0); 32];
        state[0] = Wrapping(hash_bytes as u32);
        state[1] = Wrapping(CUBEHASH_BLOCKBYTES as u32);
        state[2] = Wrapping(CUBEHASH_ROUNDS as u32);

        for _i in 0..CUBEHASH_INITIAL_ROUNDS {
            transform(&mut state);
        }

        Self {
            state,
            hash_bytes,
            pos: 0,
        }
    }

    pub fn update(&mut self, data: &[u8]) {
        for b in data.iter() {
            let u = Wrapping(*b as u32) << (8 * (self.pos % 4) as usize);
            self.state[(self.pos / 4) as usize] ^= u;
            self.pos += 1;
            if self.pos == CUBEHASH_BLOCKBYTES {
                transform(&mut self.state);
                self.pos = 0;
            }
        }
    }

    pub fn finalize(mut self) -> Vec<u8> {
        let mut buf = vec![0; self.hash_bytes as usize];

        let u = Wrapping(128u32) << (8 * (self.pos % 4) as usize);
        self.state[(self.pos / 4) as usize] ^= u;
        transform(&mut self.state);
        self.state[31] ^= Wrapping(1);
        for _ in 0..CUBEHASH_FINAL_ROUNDS {
            transform(&mut self.state);
        }
        for i in 0..self.hash_bytes as usize {
            buf[i] = (self.state[i / 4] >> (8 * (i % 4))).0 as u8;
        }
        buf
    }
}

#[cfg(test)]
mod test;
