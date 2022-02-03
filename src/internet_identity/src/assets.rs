// All assets
//
// This file describes which assets are used and how (content, content type and content encoding).

use sha2::Digest;
use lazy_static::lazy_static;
use ic_cdk::api;


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
    SVG
}

lazy_static! {
    static ref INDEX_HTML_SETUP_JS: String = {
        let canister_id = api::id();
        format!(r#"var canisterId = '{canister_id}';let s = document.createElement('script');s.async = false;s.src = 'index.js';document.head.appendChild(s);"#)
    };

    static ref INDEX_HTML_SETUP_JS_SRI_HASH: String = {

        let js: String = INDEX_HTML_SETUP_JS.to_string();
        let js: &[u8] = js.as_bytes();
        let hash: &[u8] = &sha2::Sha256::digest(js);
        let hash = base64::encode(hash);
        format!("sha256-{hash}")
        //"sha256-9u6qNwkySNEgpTe1e3v0q2oIK9b+b3tB6nTAUew/VuQ=".to_string()
    };
}

pub fn setup_hash() -> String {
    INDEX_HTML_SETUP_JS_SRI_HASH.to_string()
}

lazy_static! {

    /*
    Note: we cannot use a normal script tag like this
        <script src="index.js" integrity="sha256-QKc0t+gyMRWWDNty0lxQKWpPz18K4pD8q3S0YoeQMdo=" defer></script>
    because Firefox does not support SRI with CSP: https://bugzilla.mozilla.org/show_bug.cgi?id=1409200
    */
    static ref INDEX_HTML_STR: String = {
        let index_html = include_str!("../../../dist/index.html");
        let foo: String = INDEX_HTML_SETUP_JS.to_string();
        let index_html = index_html.replace(
            "<script id='setupJs'></script>",
            &format!("<script id='setupJs'>{foo}</script>").to_string()
        );
        index_html
    };

    static ref INDEX_HTML: &'static [u8] = {
        INDEX_HTML_STR.as_bytes()
    };
}


pub fn for_each_asset(mut f: impl FnMut(&'static str, ContentEncoding, ContentType, &'static [u8], &[u8; 32])) {

    let assets: [ (&str, &[u8], ContentEncoding, ContentType); 8] = [
         ("/",
            &INDEX_HTML,
            ContentEncoding::Identity,
            ContentType::HTML,
         ),
         // The FAQ and about pages are the same webapp, but the webapp routes to the correct page
         (
             "/faq",
             &INDEX_HTML,
             ContentEncoding::Identity,
             ContentType::HTML,
         ),
         (
             "/about",
             &INDEX_HTML,
             ContentEncoding::Identity,
             ContentType::HTML,
         ),
         (
             "/index.html",
             &INDEX_HTML,
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
         (
             "/ic-badge.svg",
             include_bytes!("../../../dist/ic-badge.svg"),
             ContentEncoding::Identity,
             ContentType::SVG,
         ),
    ];

    for (name, content, encoding, content_type) in assets {
        let hash = hash_content(content);
        f(name, encoding, content_type, content, &hash);
    }
}


// Hash the content of an asset in an `ic_certified_map` friendly way
fn hash_content(bytes: &[u8]) -> [u8; 32] {
    sha2::Sha256::digest(bytes).into()
}
