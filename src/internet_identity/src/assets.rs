// All assets
//
// This file describes which assets are used and how (content, content type and content encoding).
use crate::http::security_headers;
use crate::state;
use asset_util::{collect_assets, Asset, CertifiedAssets, ContentEncoding, ContentType};
use ic_cdk::api;
use include_dir::{include_dir, Dir};

// used both in init and post_upgrade
pub fn init_assets() {
    state::assets_mut(|certified_assets| {
        let assets = get_static_assets();

        // Extract the integrity hashes from all the HTML files
        // i.e. go through the HTML and grab INTEGRITY from all the <script integrity="INTEGRITY">
        let integrity_hashes = assets
            .iter()
            .filter(|asset| asset.content_type == ContentType::HTML)
            .fold(vec![], |mut acc: Vec<String>, e| {
                let content = std::str::from_utf8(&e.content).unwrap().to_string();
                acc.append(&mut extract_integrity_hashes(content));
                acc
            });

        *certified_assets =
            CertifiedAssets::certify_assets(assets, &security_headers(integrity_hashes));
    });
}

// Fix up HTML pages, by injecting canister ID
fn fixup_html(html: &str) -> String {
    let canister_id = api::id();
    // XXX: to match, we rely on the fact that our bundle injects an 'integrity' attribute
    // before all other attributes
    html.replace(
        r#"<script integrity="#,
        &format!(r#"<script data-canister-id="{canister_id}" integrity="#),
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
        content: b"identity.internetcomputer.org".to_vec(),
        encoding: ContentEncoding::Identity,
        content_type: ContentType::OCTETSTREAM,
    });
    assets
}

/// Extract all integrity hashes from the given HTML
fn extract_integrity_hashes(content: String) -> Vec<String> {
    // Extract anything between 'integrity="' and '"'; simpler & more efficient
    // than parsing the HTML and works well enough
    let hash_prefix = r#"integrity=""#;
    let hash_suffix = r#"""#;
    content
        .match_indices(hash_prefix)
        .map(|(size, _)| {
            let offset = size + hash_prefix.len();
            let len = content[offset..].find(hash_suffix).unwrap();
            content[offset..(offset + len)].to_string()
        })
        .collect()
}

#[test]
fn test_extract_integrity_hashes() {
    let expected: Vec<String> = vec![];
    let actual = extract_integrity_hashes(r#"<head></head>"#.to_string());
    assert_eq!(expected, actual);

    let expected: Vec<String> = vec!["hello-world".to_string()];
    let actual =
        extract_integrity_hashes(r#"<script integrity="hello-world"></script>"#.to_string());
    assert_eq!(expected, actual);

    let expected: Vec<String> = vec!["one".to_string(), "two".to_string()];
    let actual = extract_integrity_hashes(
        r#"<script integrity="one"></script><script foo=bar integrity="two" broken"#.to_string(),
    );
    assert_eq!(expected, actual);
}
