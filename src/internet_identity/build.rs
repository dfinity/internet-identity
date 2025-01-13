use std::path::Path;
use std::{env, fs};

// OpenID Google client id used by tests
const TEST_OPENID_GOOGLE_CLIENT_ID: &str =
    "45431994619-cbbfgtn7o0pp0dpfcg2l66bc4rcg7qbu.apps.googleusercontent.com";

// Write environment variables to constants during build time
fn main() {
    let openid_google_client_id =
        env::var("II_OPENID_GOOGLE_CLIENT_ID").unwrap_or(TEST_OPENID_GOOGLE_CLIENT_ID.into());
    fs::write(
        Path::new(&env::var("OUT_DIR").unwrap()).join("constants.rs"),
        format!("pub const OPENID_GOOGLE_CLIENT_ID: &str = \"{openid_google_client_id}\";"),
    )
    .unwrap();
}
