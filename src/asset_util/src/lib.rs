use base64::engine::general_purpose::STANDARD as BASE64;
use base64::Engine;
use ic_cdk::api::data_certificate;
use ic_cdk::trap;
use ic_certification::{
    fork, fork_hash, labeled, labeled_hash, pruned, AsHashTree, Hash, HashTree, NestedTree, RbTree,
};
use ic_representation_independent_hash::{representation_independent_hash, Value};
use include_dir::{Dir, File};
use internet_identity_interface::http_gateway::HeaderField;
use lazy_static::lazy_static;
use serde::Serialize;
use sha2::Digest;
use std::collections::HashMap;

pub const IC_CERTIFICATE_HEADER: &str = "IC-Certificate";
pub const IC_CERTIFICATE_EXPRESSION_HEADER: &str = "IC-CertificateExpression";
pub const LABEL_ASSETS_V1: &str = "http_assets";
pub const LABEL_ASSETS_V2: &str = "http_expr";
pub const STATUS_CODE_PSEUDO_HEADER: &str = ":ic-cert-status";
pub const EXACT_MATCH_TERMINATOR: &str = "<$>";
pub const IC_CERTIFICATE_EXPRESSION: &str =
    "default_certification(ValidationArgs{certification:Certification{no_request_certification: Empty{},\
    response_certification:ResponseCertification{response_header_exclusions:ResponseHeaderList{headers:[]}}}})";

/// Struct to hold assets together with the necessary certification trees.
/// The [CertifiedAssets::root_hash] must be included in the canisters `certified_data` for the
/// certification to be valid.
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
            &labeled_hash(
                LABEL_ASSETS_V1.as_bytes(),
                &self.certification_v1.root_hash(),
            ),
            &labeled_hash(
                LABEL_ASSETS_V2.as_bytes(),
                &self.certification_v2.root_hash(),
            ),
        )
    }

    fn witness_v1(&self, path: &str) -> HashTree {
        let witness = self.certification_v1.witness(path.as_bytes());
        fork(
            labeled(LABEL_ASSETS_V1, witness),
            pruned(labeled_hash(
                LABEL_ASSETS_V2.as_bytes(),
                &self.certification_v2.root_hash(),
            )),
        )
    }

    fn witness_v2(&self, absolute_path: &str) -> HashTree {
        assert!(absolute_path.starts_with('/'));

        let mut path: Vec<String> = absolute_path.split('/').map(str::to_string).collect();
        path.remove(0); // remove leading empty string due to absolute path
        path.push(EXACT_MATCH_TERMINATOR.to_string());
        let path_bytes: Vec<Vec<u8>> = path.iter().map(String::as_bytes).map(Vec::from).collect();
        let witness = self.certification_v2.witness(&path_bytes);

        fork(
            pruned(labeled_hash(
                LABEL_ASSETS_V1.as_bytes(),
                &self.certification_v1.root_hash(),
            )),
            labeled(LABEL_ASSETS_V2.as_bytes(), witness),
        )
    }

    pub fn certificate_headers_v1(
        &self,
        asset_name: &str,
        sigs_tree: HashTree,
    ) -> Vec<(String, String)> {
        let certificate = data_certificate().unwrap_or_else(|| {
            trap("data certificate is only available in query calls");
        });

        let tree = fork(self.witness_v1(asset_name), sigs_tree);
        let mut serializer = serde_cbor::ser::Serializer::new(vec![]);
        serializer.self_describe().unwrap();
        tree.serialize(&mut serializer)
            .unwrap_or_else(|e| trap(&format!("failed to serialize a hash tree: {e}")));
        vec![(
            IC_CERTIFICATE_HEADER.to_string(),
            format!(
                "certificate=:{}:, tree=:{}:",
                BASE64.encode(certificate),
                BASE64.encode(serializer.into_inner())
            ),
        )]
    }

    pub fn certificate_headers_v2(
        &self,
        absolute_path: &str,
        sigs_tree: HashTree,
    ) -> Vec<(String, String)> {
        assert!(absolute_path.starts_with('/'));
        let certificate = data_certificate().unwrap_or_else(|| {
            trap("data certificate is only available in query calls");
        });

        let mut path: Vec<String> = absolute_path.split('/').map(str::to_string).collect();
        // replace the first empty split segment (due to absolute path) with "http_expr"
        *path.get_mut(0).unwrap() = LABEL_ASSETS_V2.to_string();
        path.push(EXACT_MATCH_TERMINATOR.to_string());

        let tree = fork(self.witness_v2(absolute_path), sigs_tree);

        let mut tree_serializer = serde_cbor::ser::Serializer::new(vec![]);
        tree_serializer.self_describe().unwrap();
        tree.serialize(&mut tree_serializer)
            .unwrap_or_else(|e| trap(&format!("failed to serialize a hash tree: {e}")));

        let mut expr_path_serializer = serde_cbor::ser::Serializer::new(vec![]);
        expr_path_serializer.self_describe().unwrap();
        path.serialize(&mut expr_path_serializer)
            .unwrap_or_else(|e| trap(&format!("failed to serialize a expr_path: {e}")));

        vec![
            (
                IC_CERTIFICATE_HEADER.to_string(),
                format!(
                    "certificate=:{}:, tree=:{}:, expr_path=:{}:, version=2",
                    BASE64.encode(certificate),
                    BASE64.encode(tree_serializer.into_inner()),
                    BASE64.encode(expr_path_serializer.into_inner())
                ),
            ),
            (
                IC_CERTIFICATE_EXPRESSION_HEADER.to_string(),
                IC_CERTIFICATE_EXPRESSION.to_string(),
            ),
        ]
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

impl ContentType {
    pub fn to_mime_type_string(self) -> String {
        match self {
            ContentType::HTML => "text/html".to_string(),
            ContentType::JS => "text/javascript".to_string(),
            ContentType::JSON => "application/json".to_string(),
            ContentType::CSS => "text/css".to_string(),
            ContentType::ICO => "image/vnd.microsoft.icon".to_string(),
            ContentType::WEBP => "image/webp".to_string(),
            ContentType::OCTETSTREAM => "application/octet-stream".to_string(),
            ContentType::PNG => "image/png".to_string(),
            ContentType::SVG => "image/svg+xml".to_string(),
            ContentType::WOFF2 => "application/font-woff2".to_string(),
        }
    }
}

lazy_static! {
    pub static ref EXPR_HASH: Hash = sha2::Sha256::digest(IC_CERTIFICATE_EXPRESSION).into();
}

/// Takes a list of assets and returns a [CertifiedAssets] struct containing the assets and their
/// certification.
pub fn certify_assets(
    assets: Vec<(String, Vec<u8>, ContentEncoding, ContentType)>,
    shared_headers: &[HeaderField],
) -> CertifiedAssets {
    let mut certified_assets = CertifiedAssets::default();
    for (path, content, content_encoding, content_type) in assets {
        let body_hash = sha2::Sha256::digest(&content).into();
        add_certification_v1(&mut certified_assets, &path, body_hash);

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

        // Add caching header for fonts only
        if content_type == ContentType::WOFF2 {
            headers.push((
                "Cache-Control".to_string(),
                "public, max-age=604800".to_string(), // cache for 1 week
            ));
        }

        add_certification_v2(
            &mut certified_assets,
            &path,
            &shared_headers
                .iter()
                .chain(headers.iter())
                .cloned()
                .collect::<Vec<_>>(),
            body_hash,
        );

        certified_assets.assets.insert(path, (headers, content));
    }
    certified_assets
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
    assert!(absolute_path.starts_with('/'));

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
    let mut response_metadata: HashMap<String, Value> = HashMap::from_iter(
        headers
            .iter()
            .map(|(header, value)| (header.to_ascii_lowercase(), Value::String(value.clone())))
            .collect::<Vec<_>>(),
    );
    response_metadata.insert(
        IC_CERTIFICATE_EXPRESSION_HEADER.to_ascii_lowercase(),
        Value::String(IC_CERTIFICATE_EXPRESSION.to_string()),
    );
    response_metadata.insert(STATUS_CODE_PSEUDO_HEADER.to_string(), Value::Number(200));
    let mut response_metadata_hash: Vec<u8> =
        representation_independent_hash(&response_metadata.into_iter().collect::<Vec<_>>()).into();
    response_metadata_hash.extend_from_slice(body_hash);
    let response_hash: Hash = sha2::Sha256::digest(&response_metadata_hash).into();
    response_hash
}

pub fn collect_assets_recursive(
    dir: &Dir,
    html_transformer: fn(&str) -> String,
) -> Vec<(String, Vec<u8>, ContentEncoding, ContentType)> {
    let mut assets = collect_assets_from_dir(dir, html_transformer);
    for subdir in dir.dirs() {
        assets.extend(collect_assets_recursive(subdir, html_transformer).into_iter());
    }
    assets
}

pub fn collect_assets_from_dir(
    dir: &Dir,
    html_transformer: fn(&str) -> String,
) -> Vec<(String, Vec<u8>, ContentEncoding, ContentType)> {
    let mut assets: Vec<(String, Vec<u8>, ContentEncoding, ContentType)> = vec![];
    for asset in dir.files() {
        let file_bytes = asset.contents().to_vec();
        let (content, encoding, content_type) = match file_extension(asset) {
            "css" => (file_bytes, ContentEncoding::Identity, ContentType::CSS),
            "html" => (
                html_transformer(String::from_utf8_lossy(&file_bytes).as_ref())
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
            "woff2" => (file_bytes, ContentEncoding::Identity, ContentType::WOFF2),
            "woff2.gz" => (file_bytes, ContentEncoding::GZip, ContentType::WOFF2),
            ext => panic!(
                "Unknown asset type '{}' for asset '{}'",
                ext,
                asset.path().display()
            ),
        };

        let urlpaths = filepath_to_urlpaths(asset.path().to_str().unwrap().to_string());
        for urlpath in urlpaths {
            // XXX: we clone the content for each asset instead of doing something smarter
            // for simplicity & because the only assets that currently may be duplicated are
            // small HTML files.
            //
            // XXX: the behavior is undefined for assets with overlapping URL paths (e.g. "foo.html" &
            // "foo/index.html"). This assumes that the bundler creating the assets directory
            // creates sensible assets.
            assets.push((urlpath, content.clone(), encoding, content_type));
        }
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

/// Returns the URL paths for a given asset filepath. For instance:
///
/// * "index.html" -> "/", "/index.html"
/// * "foo/bar.html" -> "/foo/bar", "/foo/bar/", "foo/bar/index.html"
///
/// NOTE: The behavior is undefined if the argument is NOT relative, i.e. if
/// the filepath has a leading slash.
///
/// NOTE: The returned paths will always start with a slash.
fn filepath_to_urlpaths(file_path: String) -> Vec<String> {
    // Create paths, WITHOUT leading slash (leading lash is prepended later)
    fn inner(elements: Vec<&str>, last: &str) -> Vec<String> {
        if elements.is_empty() && last == "index.html" {
            // The special case of the root index.html, which we serve
            // on both "/" and "/index.html"
            vec!["".to_string(), "index.html".to_string()]
        } else if last == "index.html" {
            // An index.html in a subpath
            let page = elements.join("/").to_string();
            vec![
                format!("{page}"),
                format!("{page}/"),
                format!("{page}/index.html"),
            ]
        } else if let Some(page) = last.strip_suffix(".html") {
            // A (non-index) HTML page
            let mut elements = elements.to_vec();
            elements.push(page);
            let page = elements.join("/").to_string();
            vec![
                format!("{page}"),
                format!("{page}/"),
                format!("{page}/index.html"),
            ]
        } else if let Some(file) = last.strip_suffix(".gz") {
            // A gzipped asset; remove suffix and retry
            // XXX: this recursion is safe (i.e. not infinite) because
            // we always reduce the argument (remove ".gz")
            inner(elements, file)
        } else {
            // The default cases for any asset
            // XXX: here we could create an iterator and `intersperse`
            // the element but this feature is unstable at the time
            // of writing: https://github.com/rust-lang/rust/issues/79524
            let mut elements = elements.clone();
            elements.push(last);
            let asset = elements.join("/").to_string();
            vec![asset]
        }
    }

    let paths = match file_path.split('/').collect::<Vec<&str>>().split_last() {
        None => {
            // The argument was an empty string
            // We can't really do much about this, so we fail explicitly
            panic!("Expected non-empty filepath for asset");
        }
        Some((last, elements)) => inner(elements.to_vec(), last),
    };

    // Prefix everything with "/"
    paths.into_iter().map(|path| format!("/{path}")).collect()
}

#[test]
fn test_filepath_urlpaths() {
    fn assert_gen_paths(inp: String, mut exp: Vec<String>) {
        exp.sort();

        let mut actual = filepath_to_urlpaths(inp);
        actual.sort();
        assert_eq!(exp, actual);
    }

    assert_gen_paths(
        "index.html".to_string(),
        vec!["/".to_string(), "/index.html".to_string()],
    );

    assert_gen_paths(
        "foo.html".to_string(),
        vec![
            "/foo".to_string(),
            "/foo/".to_string(),
            "/foo/index.html".to_string(),
        ],
    );

    assert_gen_paths(
        "foo/index.html".to_string(),
        vec![
            "/foo".to_string(),
            "/foo/".to_string(),
            "/foo/index.html".to_string(),
        ],
    );

    assert_gen_paths("index.css".to_string(), vec!["/index.css".to_string()]);
    assert_gen_paths("foo.bar.gz".to_string(), vec!["/foo.bar".to_string()]);

    assert_gen_paths(
        "sub/foo.bar.gz".to_string(),
        vec!["/sub/foo.bar".to_string()],
    );

    assert_gen_paths(
        "foo.html.gz".to_string(),
        vec![
            "/foo".to_string(),
            "/foo/".to_string(),
            "/foo/index.html".to_string(),
        ],
    );

    assert_gen_paths(
        "sub/foo.html.gz".to_string(),
        vec![
            "/sub/foo".to_string(),
            "/sub/foo/".to_string(),
            "/sub/foo/index.html".to_string(),
        ],
    );
}
