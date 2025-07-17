// All assets
//
// This file describes which assets are used and how (content, content type and content encoding).
use crate::http::security_headers;
use crate::state;
use asset_util::{collect_assets, Asset, CertifiedAssets, ContentEncoding, ContentType};
use base64::engine::general_purpose::STANDARD as BASE64;
use base64::Engine;
use candid::Encode;
use ic_cdk::api;
use include_dir::{include_dir, Dir};
use internet_identity_interface::internet_identity::types::InternetIdentityInit;
use serde_json::json;
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

// Fix up HTML pages, by injecting canister ID and canister config
fn fixup_html(html: &str, config: &InternetIdentityInit) -> String {
    let canister_id = api::id();
    // Encoding to JSON might cause issues with html character escaping,
    // base64 avoids all the potential issues around that.
    //
    // Also, the serde json implementation will likely generate a different output than
    // @dfinity/candid (e.g. bigint), which means it won't match the types anymore.
    //
    // So to avoid all these issues, we send the raw candid to the frontend,
    // then the frontend decodes it just like a canister call response.
    let encoded_config = BASE64.encode(Encode!(&config).unwrap());
    html.replace(
        r#"<body "#,
        &format!(
            r#"<body data-canister-id="{canister_id}" data-canister-config="{encoded_config}" "#
        ),
    )
}

static ASSET_DIR: Dir<'_> = include_dir!("$CARGO_MANIFEST_DIR/../../dist");

// Gets the static assets. All static assets are prepared only once (like injecting the canister ID).
pub fn get_static_assets(config: &InternetIdentityInit) -> Vec<Asset> {
    // Instead of passing a `html_transformer`, iter over assets and use `fixup_html`
    // directly so that it has access to the `config` reference within scope.
    let mut assets: Vec<Asset> = collect_assets(&ASSET_DIR, None)
        .into_iter()
        .map(|mut asset| {
            if asset.content_type == ContentType::HTML {
                asset.content = fixup_html(std::str::from_utf8(&asset.content).unwrap(), config)
                    .as_bytes()
                    .to_vec();
            }
            asset
        })
        .collect();

    // Required to make II available on the identity.internetcomputer.org domain.
    // See https://internetcomputer.org/docs/current/developer-docs/production/custom-domain/#custom-domains-on-the-boundary-nodes
    assets.push(Asset {
        url_path: "/.well-known/ic-domains".to_string(),
        content: b"identity.internetcomputer.org\nbeta.identity.ic0.app\nbeta.identity.internetcomputer.org\nid.ai\nbeta.id.ai\nwww.id.ai".to_vec(),
        encoding: ContentEncoding::Identity,
        content_type: ContentType::OCTETSTREAM,
    });

    if let Some(related_origins) = &config.related_origins {
        // Required to share passkeys with the different domains. Maximum of 5 labels.
        // See https://web.dev/articles/webauthn-related-origin-requests#step_2_set_up_your_well-knownwebauthn_json_file_in_site-1
        let content = json!({
            "origins": related_origins,
        })
        .to_string()
        .into_bytes();
        assets.push(Asset {
            url_path: "/.well-known/webauthn".to_string(),
            content,
            encoding: ContentEncoding::Identity,
            content_type: ContentType::JSON,
        });
    }
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
