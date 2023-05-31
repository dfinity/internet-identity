// All assets
//
// This file describes which assets are used and how (content, content type and content encoding).

use crate::hash::{hash_of_map, Value};
use crate::http::{security_headers, IC_CERTIFICATE_EXPRESSION_HEADER};
use crate::nested_tree::NestedTree;
use crate::{http, state};
use base64::engine::general_purpose::STANDARD as BASE64;
use base64::Engine;
use ic_cdk::api;
use ic_certified_map::{
    fork, fork_hash, labeled, labeled_hash, AsHashTree, Hash, HashTree, RbTree,
};
use include_dir::{include_dir, Dir, File};
use internet_identity_interface::http_gateway::HeaderField;
use lazy_static::lazy_static;
use sha2::Digest;
use std::collections::HashMap;

const LABEL_ASSETS_V1: &[u8] = b"http_assets";
const LABEL_ASSETS_V2: &[u8] = b"http_expr";
const STATUS_CODE_PSEUDO_HEADER: &str = ":ic-cert-status";
pub const EXACT_MATCH_TERMINATOR: &str = "<$>";
pub const IC_CERTIFICATE_EXPRESSION: &str =
    "default_certification(ValidationArgs{certification:Certification{no_request_certification: Empty{},\
    response_certification:ResponseCertification{response_header_exclusions:ResponseHeaderList{headers:[]}}}})";

#[derive(Debug, Default, Clone)]
pub struct CertifiedAssets {
    pub assets: HashMap<String, (Vec<HeaderField>, Vec<u8>)>,
    pub certification_v1: RbTree<String, Hash>,
    pub certification_v2: NestedTree<Vec<u8>, Vec<u8>>,
}

impl CertifiedAssets {
    /// Returns the root_hash of the asset certification tree.
    pub fn root_hash(&self) -> Hash {
        fork_hash(
            // NB: Labels added in lexicographic order.
            &labeled_hash(LABEL_ASSETS_V1, &self.certification_v1.root_hash()),
            &labeled_hash(LABEL_ASSETS_V2, &self.certification_v2.root_hash()),
        )
    }

    pub fn witness_v1(&self, path: &str) -> HashTree {
        let witness = self.certification_v1.witness(path.as_bytes());
        fork(
            labeled(LABEL_ASSETS_V1, witness),
            HashTree::Pruned(labeled_hash(
                LABEL_ASSETS_V2,
                &self.certification_v2.root_hash(),
            )),
        )
    }

    pub fn witness_v2(&self, absolute_path: &str) -> HashTree {
        assert!(absolute_path.starts_with("/"));

        let mut path: Vec<String> = absolute_path.split('/').map(str::to_string).collect();
        path.remove(0); // remove leading empty string due to absolute path
        path.push(EXACT_MATCH_TERMINATOR.to_string());
        let path_bytes: Vec<Vec<u8>> = path.iter().map(String::as_bytes).map(Vec::from).collect();
        let witness = self.certification_v2.witness(&path_bytes);

        fork(
            HashTree::Pruned(labeled_hash(
                LABEL_ASSETS_V1,
                &self.certification_v1.root_hash(),
            )),
            labeled(LABEL_ASSETS_V2, witness),
        )
    }
}

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
    JSON,
    ICO,
    WEBP,
    CSS,
    OCTETSTREAM,
    PNG,
    SVG,
    WOFF2,
}

// The <script> tag that loads the 'index.js'
const JS_SETUP_SCRIPT: &str = "let s = document.createElement('script');s.type = 'module';s.src = 'index.js';document.head.appendChild(s);";

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

     pub static ref EXPR_HASH: Hash = sha2::Sha256::digest(IC_CERTIFICATE_EXPRESSION).into();
}

// used both in init and post_upgrade
pub fn init_assets() {
    state::assets_mut(|certified_assets| {
        for (path, content, content_encoding, content_type) in get_static_assets() {
            let body_hash = sha2::Sha256::digest(&content).into();
            add_certification_v1(certified_assets, &path, body_hash);

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

            add_certification_v2(
                certified_assets,
                &path,
                &security_headers()
                    .iter()
                    .chain(headers.iter())
                    .cloned()
                    .collect::<Vec<_>>(),
                body_hash,
            );

            certified_assets.assets.insert(path, (headers, content));
        }
    });
}

fn add_certification_v1(certified_assets: &mut CertifiedAssets, path: &str, body_hash: Hash) {
    certified_assets
        .certification_v1
        .insert(path.to_string(), body_hash)
}

fn add_certification_v2(
    certified_assets: &mut CertifiedAssets,
    absolute_path: &str,
    headers: &[HeaderField],
    body_hash: Hash,
) {
    assert!(absolute_path.starts_with("/"));

    let mut segments: Vec<Vec<u8>> = absolute_path
        .split('/')
        .map(str::as_bytes)
        .map(Vec::from)
        .collect();
    segments.remove(0); // remove leading empty string due to absolute path
    segments.push(EXACT_MATCH_TERMINATOR.as_bytes().to_vec());
    segments.push(Vec::from(EXPR_HASH.as_slice()));
    segments.push(vec![]);
    segments.push(Vec::from(response_hash(headers, &body_hash)));

    certified_assets.certification_v2.insert(&segments, vec![])
}

fn response_hash(headers: &[HeaderField], body_hash: &Hash) -> Hash {
    let mut response_metadata = HashMap::from_iter(
        headers
            .iter()
            .map(|(header, value)| (header.to_ascii_lowercase(), Value::String(value)))
            .collect::<Vec<_>>(),
    );
    response_metadata.insert(
        IC_CERTIFICATE_EXPRESSION_HEADER.to_ascii_lowercase(),
        Value::String(IC_CERTIFICATE_EXPRESSION),
    );
    response_metadata.insert(STATUS_CODE_PSEUDO_HEADER.to_string(), Value::U64(200));
    let mut response_metadata_hash: Vec<u8> = hash_of_map(response_metadata).into();
    response_metadata_hash.extend_from_slice(body_hash);
    let response_hash: Hash = sha2::Sha256::digest(&response_metadata_hash).into();
    response_hash
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
        let (content, encoding, content_type) = match file_extension(asset) {
            "css" => (file_bytes, ContentEncoding::Identity, ContentType::CSS),
            "html" => (
                fixup_html(String::from_utf8_lossy(&file_bytes).as_ref())
                    .as_bytes()
                    .to_vec(),
                ContentEncoding::Identity,
                ContentType::HTML,
            ),
            "ico" => (file_bytes, ContentEncoding::Identity, ContentType::ICO),
            "json" => (file_bytes, ContentEncoding::Identity, ContentType::JSON),
            "js.gz" => (file_bytes, ContentEncoding::GZip, ContentType::JS),
            "png" => (file_bytes, ContentEncoding::Identity, ContentType::PNG),
            "svg" => (file_bytes, ContentEncoding::Identity, ContentType::SVG),
            "webp" => (file_bytes, ContentEncoding::Identity, ContentType::WEBP),
            "woff2.gz" => (file_bytes, ContentEncoding::GZip, ContentType::WOFF2),
            _ => panic!("Unknown asset type: {}", asset.path().display()),
        };

        assets.push((file_to_asset_path(asset), content, encoding, content_type));
    }
    assets
}

/// Returns the portion of the filename after the first dot.
/// This corresponds to the file extension for the assets handled by this canister.
///
/// The builtin `extension` method on `Path` does not work for file extensions with multiple dots
/// such as `.js.gz`.
fn file_extension<'a>(asset: &'a File) -> &'a str {
    asset
        .path()
        .file_name()
        .unwrap()
        .to_str()
        .unwrap()
        .split_once('.')
        .unwrap()
        .1
}

/// Returns the asset path for a given file:
/// * make relative path absolute
/// * map **/index.html to **/
/// * map **/<foo>.html to **/foo
/// * map **/<foo>.js.gz to **/<foo>.js
fn file_to_asset_path(asset: &File) -> String {
    // make path absolute
    let mut file_path = "/".to_string() + asset.path().to_str().unwrap();

    if file_path.ends_with("index.html") {
        // drop index.html filename (i.e. maps **/index.html to **/)
        file_path = file_path
            .chars()
            .take(file_path.len() - "index.html".len())
            .collect()
    } else if file_path.ends_with(".html") {
        // drop .html file endings (i.e. maps **/<foo>.html to **/foo)
        file_path = file_path
            .chars()
            .take(file_path.len() - ".html".len())
            .collect()
    } else if file_path.ends_with(".gz") {
        // drop .gz for .foo.gz files (i.e. maps **/<foo>.js.gz to **/<foo>.js)
        file_path = file_path
            .chars()
            .take(file_path.len() - ".gz".len())
            .collect()
    }
    file_path
}
