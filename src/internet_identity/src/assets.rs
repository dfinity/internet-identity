// All assets
//
// This file describes which assets are used and how (content, content type and content encoding).
use crate::http::security_headers;
use crate::state;
use asset_util::{Asset, CertifiedAssets, ContentEncoding, ContentType};
use base64::engine::general_purpose::STANDARD as BASE64;
use base64::Engine;
use candid::Encode;
use internet_identity_interface::internet_identity::types::{
    InternetIdentityInit, InternetIdentitySynchronizedConfig,
};
use sha2::Digest;

// used both in init and post_upgrade
pub fn init_assets(config: &InternetIdentityInit) {
    state::assets_mut(|certified_assets| {
        let assets = get_static_assets(config);

        // Extract integrity hashes for all inlined scripts, from all the HTML files.
        let integrity_hashes = assets
            .iter()
            .filter(|asset| asset.content_type == ContentType::HTML)
            .fold(vec![], |mut acc: Vec<String>, e| {
                let content = std::str::from_utf8(&e.content).unwrap().to_string();
                for inlined in extract_inline_scripts(content).iter() {
                    let hash = sha2::Sha384::digest(inlined.as_bytes());
                    let hash = BASE64.encode(hash);
                    let hash = format!("sha384-{hash}");
                    acc.push(hash);
                }
                acc
            });

        *certified_assets = CertifiedAssets::certify_assets(
            assets,
            &security_headers(integrity_hashes, config.related_origins.clone()),
        );
    });
}

// Gets the static assets. All static assets are prepared only once (like injecting the canister ID).
pub fn get_static_assets(config: &InternetIdentityInit) -> Vec<Asset> {
    // Required to make the synchronized config available to the II frontend canister.
    // The synchronized config is the part of `InternetIdentityInit` that is needed for both the II backend and frontend.
    vec![Asset {
        url_path: "/.config.did.bin".to_string(),
        content: Encode!(&InternetIdentitySynchronizedConfig::from(config)).unwrap(),
        encoding: ContentEncoding::Identity,
        content_type: ContentType::OCTETSTREAM,
    }]
}

/// Extract all integrity hashes from the given HTML
fn extract_inline_scripts(content: String) -> Vec<String> {
    // Extract content (C to D~ below) of <script> tags; simpler & more efficient
    // than parsing the HTML and works well enough
    //
    //  <script foo=bar>var foo = 42;</script>
    //  ^              ^^            ^
    //  A              BC            D
    content
        .match_indices("<script")
        .map(|(tag_open_start /* A */, _)| {
            let tag_open_len = content[tag_open_start..].find('>').unwrap(); /* B */
            let inline_start /* C */ = tag_open_start + tag_open_len + 1;
            let tag_close_start /* D */ = content[inline_start..].find("</script>").unwrap();
            content[(inline_start)..(inline_start + tag_close_start)].to_string()
        })
        .collect()
}

#[test]
fn test_extract_inline_scripts() {
    let expected: Vec<String> = vec![];
    let actual = extract_inline_scripts(r#"<head></head>"#.to_string());
    assert_eq!(expected, actual);

    let expected: Vec<String> = vec!["this is content".to_string()];
    let actual = extract_inline_scripts(
        r#"<script integrity="hello-world">this is content</script>"#.to_string(),
    );
    assert_eq!(expected, actual);
}
