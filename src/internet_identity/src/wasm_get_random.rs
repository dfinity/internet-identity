//! This module exists to work around a problem with `getrandom` 0.2, which is a dependency
//! of `rand` 0.8
//!
//! For the `wasm32-unknown-unknown` target, `getrandom` 0.2 will refuse to compile. This is an
//! intentional policy decision on the part of the getrandom developers. As a consequence, it
//! would not be possible to compile anything which depends on `rand` 0.8 to wasm for use in
//! canister code.
//!
//! Depending on this crate converts the compile time error into a runtime error, by
//! registering a custom `getrandom` implementation which always fails. This matches the
//! behavior of `getrandom` 0.1.
//!
//! See the [getrandom
//! documentation](https://docs.rs/getrandom/latest/getrandom/macro.register_custom_getrandom.html)
//! for more details on custom implementations.

/// A getrandom implementation that always fails
pub fn always_fail(_buf: &mut [u8]) -> Result<(), getrandom::Error> {
    Err(getrandom::Error::UNSUPPORTED)
}
getrandom::register_custom_getrandom!(always_fail);
