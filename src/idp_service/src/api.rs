//! Direct ic0 bindings.

pub mod ic0 {
    #[link(wasm_import_module = "ic0")]
    extern "C" {
        pub fn msg_arg_data_copy(dst: u32, offset: u32, size: u32);
        pub fn msg_arg_data_size() -> u32;
    }
}

pub fn arg_data() -> Vec<u8> {
    let arg_size = unsafe { ic0::msg_arg_data_size() };
    let mut buf = vec![0; arg_size as usize];
    unsafe { ic0::msg_arg_data_copy(buf.as_mut_ptr() as u32, 0, arg_size) };
    buf
}
