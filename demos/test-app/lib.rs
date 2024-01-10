use crate::AlternativeOriginsMode::{CertifiedContent, Redirect};
use asset_util::DirectoryTraversalMode::IncludeSubdirs;
use asset_util::{collect_assets, Asset, CertifiedAssets, ContentEncoding, ContentType};
use candid::{CandidType, Deserialize, Principal};
use ic_cdk::api;
use ic_cdk_macros::{init, post_upgrade, query, update};
use include_dir::{include_dir, Dir};
use serde_bytes::ByteBuf;
use std::cell::RefCell;
use AlternativeOriginsMode::UncertifiedContent;

const ALTERNATIVE_ORIGINS_PATH: &str = "/.well-known/ii-alternative-origins";
const EVIL_ALTERNATIVE_ORIGINS_PATH: &str = "/.well-known/evil-alternative-origins";
const EMPTY_ALTERNATIVE_ORIGINS: &str = r#"{"alternativeOrigins":[]}"#;

thread_local! {
    static ASSETS: RefCell<CertifiedAssets> = RefCell::new(CertifiedAssets::default());
    static ALTERNATIVE_ORIGINS_MODE: RefCell<AlternativeOriginsMode> = RefCell::new(CertifiedContent);
}

#[query]
fn whoami() -> Principal {
    api::caller()
}

/// Function to update the asset /.well-known/ii-alternative-origins.
/// # Arguments
/// * alternative_origins: new value of this asset. The content type will always be set to application/json.
/// * mode: enum that allows changing the behaviour of the asset. See [AlternativeOriginsMode].
#[update]
fn update_alternative_origins(alternative_origins: String, mode: AlternativeOriginsMode) {
    ALTERNATIVE_ORIGINS_MODE.with(|m| {
        m.replace(mode);
    });
    ASSETS
        .with_borrow_mut(|assets| {
            assets.update_asset_content(
                ALTERNATIVE_ORIGINS_PATH,
                alternative_origins.as_bytes().to_vec(),
                &static_headers()
            )
        })
        .expect("Failed to update alternative origins");
    update_root_hash()
}

pub type HeaderField = (String, String);

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct HttpRequest {
    pub method: String,
    pub url: String,
    pub headers: Vec<(String, String)>,
    pub body: ByteBuf,
    pub certificate_version: Option<u16>,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct HttpResponse {
    pub status_code: u16,
    pub headers: Vec<HeaderField>,
    pub body: ByteBuf,
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

    match path {
        ALTERNATIVE_ORIGINS_PATH => ALTERNATIVE_ORIGINS_MODE.with_borrow(|mode| {
            let mut certified_response = certified_ok_response(path, req.certificate_version)
                .expect("/.well-known/ii-alternative-origins must be certified");
            match mode {
                CertifiedContent => {
                    // don't tamper with the certified response
                }
                UncertifiedContent => {
                    // drop the IC-Certificate and IC-Certificate-Expr headers
                    certified_response.headers.retain(|(header_name, _)| {
                        !header_name.to_lowercase().starts_with("ic-certificate")
                    });
                }
                Redirect { location } => {
                    // Add a Location header and modify status code to 302 to indicate redirect
                    // Keep the content and certification headers as then the response remains valid
                    // under certification v1.
                    certified_response
                        .headers
                        .push(("Location".to_string(), location.clone()));
                    certified_response.status_code = 302;
                }
            }
            certified_response
        }),
        path => certified_ok_response(path, req.certificate_version)
            .unwrap_or_else(|| not_found_response(path)),
    }
}

fn certified_ok_response(url: &str, max_certificate_version: Option<u16>) -> Option<HttpResponse> {
    let maybe_asset =
        ASSETS.with_borrow(|assets| assets.certified_asset(url, max_certificate_version, None));
    maybe_asset.map(|asset| {
        let mut headers = asset.headers;
        headers.extend(static_headers());
        HttpResponse {
            status_code: 200,
            headers,
            body: ByteBuf::from(asset.content),
        }
    })
}

fn not_found_response(path: &str) -> HttpResponse {
    HttpResponse {
        status_code: 404,
        headers: static_headers(),
        body: ByteBuf::from(format!("Asset {} not found.", path)),
    }
}

fn static_headers() -> Vec<(String, String)> {
    vec![("Access-Control-Allow-Origin".to_string(), "*".to_string())]
}

// Assets
static ASSET_DIR: Dir<'_> = include_dir!("$CARGO_MANIFEST_DIR/dist");

fn fixup_html(html: &str) -> String {
    let canister_id = api::id();

    // the string we are replacing here is inserted by vite during the front-end build
    html.replace(
        r#"<script type="module" crossorigin src="/index.js"></script>"#,
        &format!(r#"<script data-canister-id="{canister_id}" type="module" crossorigin src="/index.js"></script>"#).to_string(),
    )
}

#[init]
#[post_upgrade]
pub fn init() {
    init_assets(EMPTY_ALTERNATIVE_ORIGINS.to_string());
}

/// Collect all the assets from the dist folder.
fn init_assets(alternative_origins: String) {
    let mut assets = collect_assets(&ASSET_DIR, IncludeSubdirs, Some(fixup_html));
    assets.push(Asset {
        url_path: ALTERNATIVE_ORIGINS_PATH.to_string(),
        content: alternative_origins.as_bytes().to_vec(),
        encoding: ContentEncoding::Identity,
        content_type: ContentType::JSON,
    });

    // convenience asset to have an url to point to when testing with the redirect alternative origins behaviour
    assets.push(Asset {
        url_path: EVIL_ALTERNATIVE_ORIGINS_PATH.to_string(),
        content: b"{\"alternativeOrigins\":[\"https://evil.com\"]}".to_vec(),
        encoding: ContentEncoding::Identity,
        content_type: ContentType::JSON,
    });
    ASSETS.with_borrow_mut(|certified_assets| {
        *certified_assets = CertifiedAssets::certify_assets(assets, &static_headers());
    });
    update_root_hash()
}

fn update_root_hash() {
    ASSETS.with_borrow(|assets| {
        api::set_certified_data(&assets.root_hash()[..]);
    })
}
