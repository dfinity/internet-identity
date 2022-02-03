// All assets
//
// This file describes which assets are used and how (content, content type and content encoding).

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
    SVG
}

pub fn for_each_asset(mut f: impl FnMut(&'static str, ContentEncoding, ContentType, &'static [u8], &[u8; 32])) {

    let index_html = include_bytes!("../../../dist/index.html");

    let assets: [ (&str, &[u8], ContentEncoding, ContentType); 8] = [
         ("/",
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
