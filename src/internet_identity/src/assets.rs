use ic_cdk::api;
// All assets
//
// This file describes which assets are used and how (content, content type and content encoding).
use crate::http::security_headers;
use crate::state;
use asset_util::{collect_assets_recursive, Asset, ContentEncoding, ContentType};
use base64::engine::general_purpose::STANDARD as BASE64;
use base64::Engine;
use include_dir::{include_dir, Dir};
use lazy_static::lazy_static;
use sha2::Digest;

// used both in init and post_upgrade
pub fn init_assets() {
    state::assets_mut(|certified_assets| {
        *certified_assets = asset_util::certify_assets(get_static_assets(), &security_headers());
    });
}

// The <script> tag that loads the 'index.js'
const JS_SETUP_SCRIPT: &str = "let s = document.createElement('script');s.type = 'module';s.src = '/index.js';document.head.appendChild(s);";

// Fix up HTML pages, by injecting canister ID & script tag
fn fixup_html(html: &str) -> String {
    let canister_id = api::id();
    let setup_js: String = JS_SETUP_SCRIPT.to_string();
    html.replace(
        r#"<script type="module" crossorigin src="/index.js"></script>"#,
        &format!(r#"<script data-canister-id="{canister_id}" type="module">{setup_js}</script>"#),
    )
}

lazy_static! {
    // The SRI sha256 hash of the script tag, used by the CSP policy.
    // https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Security-Policy/script-src
    pub static ref JS_SETUP_SCRIPT_SRI_HASH: String = {
        let hash = &sha2::Sha256::digest(JS_SETUP_SCRIPT.as_bytes());
        let hash = BASE64.encode(hash);
        format!("sha256-{hash}")
    };
}

static ASSET_DIR: Dir<'_> = include_dir!("$CARGO_MANIFEST_DIR/../../dist");

// Gets the static assets. All static assets are prepared only once (like injecting the canister ID).
fn get_static_assets() -> Vec<Asset> {
    let mut assets = collect_assets_recursive(&ASSET_DIR, fixup_html);

    // Required to make II available on the identity.internetcomputer.org domain.
    // See https://internetcomputer.org/docs/current/developer-docs/production/custom-domain/#custom-domains-on-the-boundary-nodes
    assets.push(Asset {
        url_path: "/.well-known/ic-domains".to_string(),
        content: b"identity.internetcomputer.org".to_vec(),
        encoding: ContentEncoding::Identity,
        content_type: ContentType::OCTETSTREAM,
    });
    assets
}
