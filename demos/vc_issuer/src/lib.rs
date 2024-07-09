/// A small module that makes get_random work on wasm32-unknown-unknown.
#[cfg(all(
    target_arch = "wasm32",
    target_vendor = "unknown",
    target_os = "unknown"
))]
mod wasm_get_random;
