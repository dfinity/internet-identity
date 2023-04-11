// All assets
//
// This file describes which assets are used and how (content, content type and content encoding).

use crate::{http, state};
use base64::engine::general_purpose::STANDARD as BASE64;
use base64::Engine;
use ic_cdk::api;
use include_dir::{include_dir, Dir};
use lazy_static::lazy_static;
use sha2::Digest;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ContentEncoding {
    Identity,
    GZip,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[allow(clippy::upper_case_acronyms)]
pub enum ContentType {
    HTML,
    JS,
    ICO,
    WEBP,
    CSS,
    OCTETSTREAM,
    PNG,
}

// The <script> tag that loads the 'index.js'
const JS_SETUP_SCRIPT: &str = "let s = document.createElement('script');s.async = true;s.src = 'index.js';document.head.appendChild(s);";

// Fix up HTML pages, by injecting canister ID, script tag and CSP
fn fixup_html(html: &str) -> String {
    let canister_id = api::id();
    let setup_js: String = JS_SETUP_SCRIPT.to_string();
    let html = html.replace(
        r#"<script id="setupJs"></script>"#,
        &format!(r#"<script data-canister-id="{canister_id}" id="setupJs">{setup_js}</script>"#),
    );
    html.replace(
        "<meta replaceme-with-csp/>",
        &format!(
            r#"<meta http-equiv="Content-Security-Policy" content="{}" />"#,
            &http::content_security_policy_meta()
        ),
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

    /// Map of path aliases
    /// This creates a copy of the asset at the mapped path with the same content as the asset at the
    /// original path.
    static ref PATH_ALIASES: HashMap<String, String> = vec![
        ("/index.html".to_string(), "/".to_string()),
        ("/about.html".to_string(), "/about".to_string()),
    ].into_iter().collect();
}

// used both in init and post_upgrade
pub fn init_assets() {
    state::assets_and_hashes_mut(|assets, asset_hashes| {
        for (path, content, content_encoding, content_type) in get_static_assets() {
            asset_hashes.insert(path.clone(), sha2::Sha256::digest(&content).into());
            let mut headers = match content_encoding {
                ContentEncoding::Identity => vec![],
                ContentEncoding::GZip => {
                    vec![("Content-Encoding".to_string(), "gzip".to_string())]
                }
            };
            headers.push((
                "Content-Type".to_string(),
                content_type.to_mime_type_string(),
            ));
            assets.insert(path, (headers, content));
        }
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

fn collect_assets_recursive(dir: &Dir) -> Vec<(String, Vec<u8>, ContentEncoding, ContentType)> {
    let mut assets = collect_assets_from_dir(dir);
    for subdir in dir.dirs() {
        assets.extend(collect_assets_recursive(subdir).into_iter());
    }
    assets
}

fn collect_assets_from_dir(dir: &Dir) -> Vec<(String, Vec<u8>, ContentEncoding, ContentType)> {
    let mut assets: Vec<(String, Vec<u8>, ContentEncoding, ContentType)> = vec![];
    for asset in dir.files() {
        let file_bytes = asset.contents().to_vec();
        let file_path = "/".to_string() + asset.path().to_str().unwrap();
        let (asset_path, content, encoding, content_type) = match asset
            .path()
            .file_name()
            .unwrap()
            .to_str()
            .unwrap()
            .split_once('.')
            .unwrap()
            .1
        {
            "css" => (
                file_path,
                file_bytes,
                ContentEncoding::Identity,
                ContentType::CSS,
            ),
            "html" => (
                file_path,
                fixup_html(String::from_utf8_lossy(&file_bytes).as_ref())
                    .as_bytes()
                    .to_vec(),
                ContentEncoding::Identity,
                ContentType::HTML,
            ),
            "ico" => (
                file_path,
                file_bytes,
                ContentEncoding::Identity,
                ContentType::ICO,
            ),
            "js.gz" => (
                file_path.chars().take(file_path.len() - 3).collect(), // drop ".gz"
                file_bytes,
                ContentEncoding::GZip,
                ContentType::JS,
            ),
            "png" => (
                file_path,
                file_bytes,
                ContentEncoding::Identity,
                ContentType::PNG,
            ),
            "webp" => (
                file_path,
                file_bytes,
                ContentEncoding::Identity,
                ContentType::WEBP,
            ),
            _ => panic!("Unknown asset type: {}", asset.path().display()),
        };

        // Create a copy if an alias exists
        ic_cdk::println!("Adding asset: {}", asset_path);
        if let Some(alias) = PATH_ALIASES.get(&asset_path) {
            assets.push((alias.clone(), content.clone(), encoding, content_type));
        }

        assets.push((asset_path, content, encoding, content_type));
    }
    assets
}
