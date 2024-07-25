// All assets
//
// This file describes which assets are used and how (content, content type and content encoding).
use crate::http::security_headers;
use crate::state;
use asset_util::{collect_assets, Asset, CertifiedAssets, ContentEncoding, ContentType};
use base64::engine::general_purpose::STANDARD as BASE64;
use base64::Engine;
use ic_cdk::api;
use include_dir::{include_dir, Dir};
use sha2::Digest;

// used both in init and post_upgrade
pub fn init_assets() {
    state::assets_mut(|certified_assets| {
        let assets = get_static_assets();

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

        *certified_assets =
            CertifiedAssets::certify_assets(assets, &security_headers(integrity_hashes));
    });
}

// Fix up HTML pages, by injecting canister ID
fn fixup_html(html: &str) -> String {
    let canister_id = api::id();
    html.replace(
        r#"<script "#,
        &format!(r#"<script data-canister-id="{canister_id}" "#),
    )
}

static ASSET_DIR: Dir<'_> = include_dir!("$CARGO_MANIFEST_DIR/../../dist");

// Gets the static assets. All static assets are prepared only once (like injecting the canister ID).
pub fn get_static_assets() -> Vec<Asset> {
    let mut assets = collect_assets(&ASSET_DIR, Some(fixup_html));

    // Required to make II available on the identity.internetcomputer.org domain.
    // See https://internetcomputer.org/docs/current/developer-docs/production/custom-domain/#custom-domains-on-the-boundary-nodes
    assets.push(Asset {
        url_path: "/.well-known/ic-domains".to_string(),
        content: b"identity.internetcomputer.org\nbeta.identity.ic0.app\nbeta.identity.internetcomputer.org".to_vec(),
        encoding: ContentEncoding::Identity,
        content_type: ContentType::OCTETSTREAM,
    });
    assets
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
