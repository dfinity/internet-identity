/// A small module that makes get_random work on wasm32-unknown-unknown.
/// The dependency on get_random comes from the captcha library.
#[cfg(all(
    target_arch = "wasm32",
    target_vendor = "unknown",
    target_os = "unknown"
))]
mod wasm_get_random;
