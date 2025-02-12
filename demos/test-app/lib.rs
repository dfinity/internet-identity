use crate::AlternativeOriginsMode::{CertifiedContent, Redirect};
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
const OUTDATED_INVALID_CERTIFICATE_HEADER: &str = ":2dn3omR0cmVlgwGDAYMBgwJIY2FuaXN0ZXKDAYMBggRYIF7eYW50QXA1hAANBQ4J616Ekjch0ihDxnNGwvlxxIKDgwGCBFggH4wduBeihx+gd8Oe2KvzyQxp/PEe6ustjHJNlVhLbmaDAkqAAAAAABAAAwEBgwGDAYMCTmNlcnRpZmllZF9kYXRhggNYIIA3JGAjACCVyCTmsRmhhlZDI5oDZZkhGVMbpCIFTEejggRYIIMJ950nCB4emD2uvICtY5WfLhcOzb2BaqH4EvUGTX2xggRYIFfnBG3quMbImRDu81QLZKq0ADXD75bQIoPHA2y4JRQVggRYIETEKmiZ1Lflrx8sIiDUOqBdb7X+mJ5+kEturndxJYzeggRYINPKhi8ZGTDLJJGHdaSlL3lxf8JFGiBHe3FVp4y/myCvggRYIIZ883QyMwhObp/SFU8xtXu8w8xGgwEWfkJYAWqC9dNSgwGCBFgg49iYnFVeAADyzEwGNNe…Bcfct/T4ZWVYbJe/P3gUbLOS8n9uDAklodHRwX2V4cHKDAYMBgwGCBFgggaSHI9J56LbuKjb58O8AWYlQNqTWZBxB58L7Y6u9j2ODAksud2VsbC1rbm93boMBggRYIJY8druSGXKdr/LHH3Kr/F+Vo9VwgluKJZS6HxkTrIeUgwJWaWktYWx0ZXJuYXRpdmUtb3JpZ2luc4MCQzwkPoMCWCBiB64Pds+kxrd7O3KKhS3TAcooPTqycnGLKWuiy3dP6IMCQIMCWCCaryvDtyyZdDWHqiLmkc63lZuPrBF2Tt6ULsG0LUkWcIIDQIIEWCAYA1f5ooQFb7bDDkKE0QhYJLkfsn2j1GCIGJvp8r8ucYIEWCB28Uo/B0pARPP3FnDUBj83i4NpGPehI4IGGI2I2iOQhg==:, expr_path=:2dn3hGlodHRwX2V4cHJrLndlbGwta25vd252aWktYWx0ZXJuYXRpdmUtb3JpZ2luc2M8JD4=:, version=2";

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
    ASSETS.with_borrow_mut(|assets| {
        let asset = Asset {
            url_path: ALTERNATIVE_ORIGINS_PATH.to_string(),
            content: alternative_origins.as_bytes().to_vec(),
            encoding: ContentEncoding::Identity,
            content_type: ContentType::JSON,
        };
        match &mode {
            CertifiedContent | UncertifiedContent => assets.certify_asset(asset, &static_headers()),
            Redirect { location } => assets
                .certify_redirect(
                    ALTERNATIVE_ORIGINS_PATH,
                    location.as_str(),
                    &static_headers(),
                )
                .expect("Failed to certify alternative origins redirect"),
        }
    });

    ALTERNATIVE_ORIGINS_MODE.with(|m| {
        m.replace(mode);
    });
    update_root_hash()
}

pub type HeaderField = (String, String);

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct HttpRequest {
    pub method: String,
    pub url: String,
    pub headers: Vec<HeaderField>,
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
            let mut certified_response = certified_response(path, req.certificate_version)
                .expect("/.well-known/ii-alternative-origins must be certified");
            match mode {
                UncertifiedContent => {
                    certified_response.headers = certified_response
                        .headers
                        .into_iter()
                        .map(|(header_name, header_value)| {
                            // Modify the IC-Certificate header to make certification invalid
                            // Note: we cannot simply drop the header because then the local replica (dfx 0.15 and later)
                            // will skip the certification check altogether.
                            if header_name == "IC-Certificate" {
                                (header_name, OUTDATED_INVALID_CERTIFICATE_HEADER.to_string())
                            } else {
                                (header_name, header_value)
                            }
                        })
                        .collect::<_>()
                }
                Redirect { .. } | CertifiedContent => {
                    // don't tamper with the certified response
                }
            }
            certified_response
        }),
        path => certified_response(path, req.certificate_version)
            .unwrap_or_else(|| not_found_response(path)),
    }
}

fn certified_response(url: &str, max_certificate_version: Option<u16>) -> Option<HttpResponse> {
    let maybe_asset =
        ASSETS.with_borrow(|assets| assets.get_certified_asset(url, max_certificate_version, None));
    maybe_asset.map(|asset| {
        let mut headers = asset.headers;
        headers.extend(static_headers());
        HttpResponse {
            status_code: asset.status_code,
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

fn static_headers() -> Vec<HeaderField> {
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
pub fn init() {
    init_assets(EMPTY_ALTERNATIVE_ORIGINS.to_string());
}
#[post_upgrade]
fn post_upgrade() {
    init()
}

/// Collect all the assets from the dist folder.
fn init_assets(alternative_origins: String) {
    let mut assets = collect_assets(&ASSET_DIR, Some(fixup_html));
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
