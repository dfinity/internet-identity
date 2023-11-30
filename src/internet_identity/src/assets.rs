// All assets
//
// This file describes which assets are used and how (content, content type and content encoding).
use crate::http::security_headers;
use crate::state;
use asset_util::{collect_assets_recursive, ContentEncoding, ContentType};
use include_dir::{include_dir, Dir};

// used both in init and post_upgrade
pub fn init_assets() {
    state::assets_mut(|certified_assets| {
        *certified_assets = asset_util::certify_assets(get_static_assets(), &security_headers());
    });
}

static ASSET_DIR: Dir<'_> = include_dir!("$CARGO_MANIFEST_DIR/../../dist");

// Gets the static assets. All static assets are prepared only once (like injecting the canister ID).
fn get_static_assets() -> Vec<(String, Vec<u8>, ContentEncoding, ContentType)> {
    let mut assets = collect_assets_recursive(&ASSET_DIR);

    // Required to make II available on the identity.internetcomputer.org domain.
    // See https://internetcomputer.org/docs/current/developer-docs/production/custom-domain/#custom-domains-on-the-boundary-nodes
    assets.push((
        "/.well-known/ic-domains".to_string(),
        b"identity.internetcomputer.org".to_vec(),
        ContentEncoding::Identity,
        ContentType::OCTETSTREAM,
    ));

    assets
}
