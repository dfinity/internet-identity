use crate::AlternativeOriginsMode::{CertifiedContent, Redirect};
use base64::engine::general_purpose::STANDARD as BASE64;
use base64::Engine;
use candid::{CandidType, Deserialize, Principal};
use ic_cdk::api;
use ic_cdk_macros::{init, post_upgrade, query, update};
use ic_certified_map::{labeled_hash, AsHashTree, Hash, RbTree};
use lazy_static::lazy_static;
use serde::Serialize;
use serde_bytes::{ByteBuf, Bytes};
use sha2::Digest;
use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::HashMap;

thread_local! {
    static ASSETS: RefCell<HashMap<&'static str, (Vec<(String, String)>, Vec<u8>)>> = RefCell::new(HashMap::default());
    static ASSET_HASHES: RefCell<RbTree<&'static str, [u8; 32]>> = RefCell::new(RbTree::default());
    static ALTERNATIVE_ORIGINS_MODE: RefCell<AlternativeOriginsMode> = RefCell::new(CertifiedContent);
}

#[derive(Debug, PartialEq, Eq)]
pub enum ContentType {
    HTML,
    JS,
    JSON,
}

impl ContentType {
    pub fn to_mime_type_string(&self) -> String {
        match self {
            ContentType::HTML => "text/html".to_string(),
            ContentType::JS => "text/javascript".to_string(),
            ContentType::JSON => "application/json".to_string(),
        }
    }
}

#[query]
fn whoami() -> Principal {
    api::caller()
}

/// Function to update the asset /.well-known/ii-alternative-origins.
/// # Arguments
/// * content: new value of this asset. The content type will always be set to application/json.
/// * mode: enum that allows changing the behaviour of the asset. See [AlternativeOriginsMode].
#[update]
fn update_alternative_origins(content: String, mode: AlternativeOriginsMode) {
    ASSETS.with(|a| {
        ASSET_HASHES.with(|ah| {
            let mut assets = a.borrow_mut();
            let mut asset_hashes = ah.borrow_mut();
            let path = "/.well-known/ii-alternative-origins";
            assets.insert(
                path,
                (
                    vec![(
                        "Content-Type".to_string(),
                        ContentType::JSON.to_mime_type_string(),
                    )],
                    content.as_bytes().to_vec(),
                ),
            );
            asset_hashes.insert(path, sha2::Sha256::digest(content.as_bytes()).into());
            update_root_hash(&asset_hashes);
        });
    });

    ALTERNATIVE_ORIGINS_MODE.with(|m| {
        m.replace(mode);
    })
}

pub type HeaderField = (String, String);

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct HttpRequest {
    pub method: String,
    pub url: String,
    pub headers: Vec<(String, String)>,
    pub body: ByteBuf,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct HttpResponse {
    pub status_code: u16,
    pub headers: Vec<HeaderField>,
    pub body: Cow<'static, Bytes>,
}

/// Enum of the available asset behaviours of /.well-known/ii-alternative-origins:
/// * CertifiedContent: Valid certification on the payload. This mode is required to successfully use one of the listed alternative origins.
/// * UncertifiedContent: No `IC-Certificate` header will be sent back with the response. This mode can be used to validate that II rejects uncertified assets when validating alternative origins.
/// * Redirect: This will set the response status code to 302 and a `Location` header with the value provided. This mode can be used to validate that II rejects redirects when validating alternative origins.
#[derive(Clone, Debug, CandidType, Deserialize)]
pub enum AlternativeOriginsMode {
    CertifiedContent,
    UncertifiedContent,
    Redirect { location: String },
}

#[query]
pub fn http_request(req: HttpRequest) -> HttpResponse {
    let parts: Vec<&str> = req.url.split('?').collect();
    let path = parts[0];
    let mut headers = vec![];
    headers.push(("Access-Control-Allow-Origin".to_string(), "*".to_string()));
    let certificate_header =
        ASSET_HASHES.with(|a| make_asset_certificate_header(&a.borrow(), path));

    match path {
        "/.well-known/ii-alternative-origins" => ALTERNATIVE_ORIGINS_MODE.with(|m| {
            let mode = m.borrow();
            let mut status_code = 200;
            match mode.clone() {
                CertifiedContent => {
                    headers.push(certificate_header);
                }
                Redirect { location } => {
                    // needs to be certified content for the service worker
                    // (which the browser will then ignore and redirect anyway)
                    headers.push(certificate_header);
                    headers.push(("Location".to_string(), location));
                    status_code = 302;
                }
                _ => {}
            };
            ASSETS.with(|a| {
                let assets = a.borrow();
                let (asset_headers, value) = assets.get(path).unwrap();
                headers.append(&mut asset_headers.clone());

                HttpResponse {
                    status_code,
                    headers,
                    body: Cow::Owned(ByteBuf::from(value.clone())),
                }
            })
        }),
        _ => {
            headers.push(certificate_header);
            ASSETS.with(|a| match a.borrow().get(path) {
                Some((asset_headers, value)) => {
                    headers.append(&mut asset_headers.clone());

                    HttpResponse {
                        status_code: 200,
                        headers,
                        body: Cow::Owned(ByteBuf::from(value.clone())),
                    }
                }
                None => HttpResponse {
                    status_code: 404,
                    headers,
                    body: Cow::Owned(ByteBuf::from(format!("Asset {} not found.", path))),
                },
            })
        }
    }
}

fn make_asset_certificate_header(
    asset_hashes: &RbTree<&'static str, Hash>,
    asset_name: &str,
) -> (String, String) {
    let certificate = api::data_certificate().unwrap_or_else(|| {
        api::trap("data certificate is only available in query calls");
    });
    let witness = asset_hashes.witness(asset_name.as_bytes());
    let tree = ic_certified_map::labeled(b"http_assets", witness);
    let mut serializer = serde_cbor::ser::Serializer::new(vec![]);
    serializer.self_describe().unwrap();
    tree.serialize(&mut serializer)
        .unwrap_or_else(|e| api::trap(&format!("failed to serialize a hash tree: {}", e)));
    (
        "IC-Certificate".to_string(),
        format!(
            "certificate=:{}:, tree=:{}:",
            BASE64.encode(&certificate),
            BASE64.encode(&serializer.into_inner())
        ),
    )
}

#[init]
#[post_upgrade]
pub fn init_assets() {
    ASSETS.with(|a| {
        ASSET_HASHES.with(|ah| {
            let mut assets = a.borrow_mut();
            let mut asset_hashes = ah.borrow_mut();
            for (path, content, content_type) in get_assets() {
                asset_hashes.insert(path, sha2::Sha256::digest(content).into());
                let mut headers = vec![];
                headers.push((
                    "Content-Type".to_string(),
                    content_type.to_mime_type_string(),
                ));
                assets.insert(path, (headers, content.to_vec()));
            }
            update_root_hash(&asset_hashes);
        });
    });
}

fn update_root_hash(a: &RbTree<&'static str, [u8; 32]>) {
    let prefixed_root_hash = labeled_hash(b"http_assets", &a.root_hash());
    api::set_certified_data(&prefixed_root_hash[..]);
}

lazy_static! {
    // The full content of the index.html, after the canister ID (and script tag) have been
    // injected
    static ref INDEX_HTML_STR: String = {
        let canister_id = api::id();
        let index_html = include_str!("dist/index.html");

        // the string we are replacing here is inserted by webpack during the front-end build
        let index_html = index_html.replace(
            r#"<script defer src="bundle.js"></script>"#,
            &format!(r#"<script>var canisterId = '{canister_id}';</script><script defer="defer" src="bundle.js"></script>"#).to_string()
        );
        index_html
    };
}

// Get all the assets. Duplicated assets like index.html are shared and generally all assets are
// prepared only once (like injecting the canister ID).
fn get_assets() -> [(&'static str, &'static [u8], ContentType); 5] {
    let index_html: &[u8] = INDEX_HTML_STR.as_bytes();
    [
        ("/", index_html, ContentType::HTML),
        ("/index.html", index_html, ContentType::HTML),
        (
            "/index.js",
            include_bytes!("dist/index.js"),
            ContentType::JS,
        ),
        // initially empty alternative origins, but can be populated using the update_alternative_origins call
        (
            "/.well-known/ii-alternative-origins",
            b"{\"alternativeOrigins\":[]}",
            ContentType::JSON,
        ),
        // convenience asset to have an url to point to when testing with the redirect alternative origins behaviour
        (
            "/.well-known/evil-alternative-origins",
            b"{\"alternativeOrigins\":[\"https://evil.com\"]}",
            ContentType::JSON,
        ),
    ]
}
