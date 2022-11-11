// All assets
//
// This file describes which assets are used and how (content, content type and content encoding).

use crate::{http, state};
use ic_cdk::api;
use lazy_static::lazy_static;
use sha2::Digest;

#[derive(Debug, PartialEq, Eq)]
pub enum ContentEncoding {
    Identity,
    GZip,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ContentType {
    HTML,
    JS,
    ICO,
    WEBP,
    CSS,
}

// The <script> tag that loads the 'index.js'
const INDEX_HTML_SETUP_JS: &str = "let s = document.createElement('script');s.async = true;s.src = 'index.js';document.head.appendChild(s);";

lazy_static! {
    // The SRI sha256 hash of the script tag, used by the CSP policy.
    // https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Security-Policy/script-src
    pub static ref INDEX_HTML_SETUP_JS_SRI_HASH: String = {
        let hash = &sha2::Sha256::digest(INDEX_HTML_SETUP_JS.as_bytes());
        let hash = base64::encode(hash);
        format!("sha256-{hash}")
    };

    // The full content of the index.html, after the canister ID, the script tag and the CSP have been
    // injected
    static ref INDEX_HTML_STR: String = {
        let index_html = include_str!("../../../dist/index.html");
        let canister_id = api::id();
        let setup_js: String = INDEX_HTML_SETUP_JS.to_string();
        let index_html = index_html.replace(
            r#"<script id="setupJs"></script>"#,
            &format!(r#"<script data-canister-id="{canister_id}" id="setupJs">{setup_js}</script>"#).to_string()
        );
        let index_html = index_html.replace("<meta replaceme-with-csp/>", &format!(r#"<meta http-equiv="Content-Security-Policy" content="{}" />"#,&http::content_security_policy_meta() ));
        index_html
    };
}

// used both in init and post_upgrade
pub fn init_assets() {
    state::assets_and_hashes_mut(|assets, asset_hashes| {
        for (path, content, content_encoding, content_type) in get_assets() {
            asset_hashes.insert(path, sha2::Sha256::digest(content).into());
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

// Get all the assets. Duplicated assets like index.html are shared and generally all assets are
// prepared only once (like injecting the canister ID).
fn get_assets() -> [(&'static str, &'static [u8], ContentEncoding, ContentType); 8] {
    let index_html: &[u8] = INDEX_HTML_STR.as_bytes();
    [
        (
            "/",
            index_html,
            ContentEncoding::Identity,
            ContentType::HTML,
        ),
        // The FAQ and about pages are the same webapp, but the webapp routes to the correct page
        (
            "/faq",
            index_html,
            ContentEncoding::Identity,
            ContentType::HTML,
        ),
        (
            "/about",
            index_html,
            ContentEncoding::Identity,
            ContentType::HTML,
        ),
        (
            "/index.html",
            index_html,
            ContentEncoding::Identity,
            ContentType::HTML,
        ),
        (
            "/index.js",
            include_bytes!("../../../dist/index.js.gz"),
            ContentEncoding::GZip,
            ContentType::JS,
        ),
        (
            "/index.css",
            include_bytes!("../../../dist/index.css"),
            ContentEncoding::Identity,
            ContentType::CSS,
        ),
        (
            "/loader.webp",
            include_bytes!("../../../dist/loader.webp"),
            ContentEncoding::Identity,
            ContentType::WEBP,
        ),
        (
            "/favicon.ico",
            include_bytes!("../../../dist/favicon.ico"),
            ContentEncoding::Identity,
            ContentType::ICO,
        ),
    ]
}
