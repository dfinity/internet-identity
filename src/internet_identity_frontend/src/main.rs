use ic_asset_certification::{Asset, AssetConfig, AssetEncoding, AssetFallbackConfig, AssetRouter};
use ic_cdk::{init, post_upgrade};
use ic_cdk_macros::query;
use ic_http_certification::{
    HeaderField, HttpCertification, HttpCertificationPath, HttpCertificationTree,
    HttpCertificationTreeEntry, HttpRequest, HttpResponse, StatusCode,
};
use include_dir::{include_dir, Dir};
use std::{cell::RefCell, rc::Rc};

thread_local! {
    static HTTP_TREE: Rc<RefCell<HttpCertificationTree>> = Default::default();

    // initializing the asset router with an HTTP certification tree is optional.
    // if direct access to the HTTP certification tree is not needed for certifying
    // requests and responses outside of the asset router, then this step can be skipped
    // and the asset router can be initialized like so:
    // ```
    // static ASSET_ROUTER: RefCell<AssetRouter<'static>> = Default::default();
    // ```
    static ASSET_ROUTER: RefCell<AssetRouter<'static>> = RefCell::new(AssetRouter::with_tree(HTTP_TREE.with(|tree| tree.clone())));
}

static ASSETS_DIR: Dir<'_> = include_dir!("$CARGO_MANIFEST_DIR/../../dist");
const IMMUTABLE_ASSET_CACHE_CONTROL: &str = "public, max-age=31536000, immutable";
const NO_CACHE_ASSET_CACHE_CONTROL: &str = "public, no-cache, no-store";

// Public methods
#[init]
fn init() {
    certify_all_assets();
}

#[post_upgrade]
fn post_upgrade() {
    certify_all_assets();
}

fn certify_all_assets() {
    // 1. Define the asset certification configurations.
    let encodings = vec![
        AssetEncoding::Brotli.default_config(),
        AssetEncoding::Gzip.default_config(),
    ];

    let asset_configs = vec![
        AssetConfig::File {
            path: "index.html".to_string(),
            content_type: Some("text/html".to_string()),
            headers: get_asset_headers(vec![(
                "cache-control".to_string(),
                NO_CACHE_ASSET_CACHE_CONTROL.to_string(),
            )]),
            fallback_for: vec![AssetFallbackConfig {
                scope: "/".to_string(),
                status_code: Some(StatusCode::OK),
            }],
            aliased_by: vec!["/".to_string()],
            encodings: encodings.clone(),
        },
        // Compressed JavaScript files
        AssetConfig::Pattern {
            pattern: "**/*.js.gz".to_string(),
            content_type: Some("text/javascript".to_string()),
            headers: get_asset_headers(vec![(
                "cache-control".to_string(),
                IMMUTABLE_ASSET_CACHE_CONTROL.to_string(),
            )]),
            encodings: encodings.clone(),
        },
        // CSS files
        AssetConfig::Pattern {
            pattern: "**/*.css".to_string(),
            content_type: Some("text/css".to_string()),
            headers: get_asset_headers(vec![(
                "cache-control".to_string(),
                IMMUTABLE_ASSET_CACHE_CONTROL.to_string(),
            )]),
            encodings: encodings.clone(),
        },
        // Font files
        AssetConfig::Pattern {
            pattern: "**/*.woff2".to_string(),
            content_type: Some("font/woff2".to_string()),
            headers: get_asset_headers(vec![(
                "cache-control".to_string(),
                IMMUTABLE_ASSET_CACHE_CONTROL.to_string(),
            )]),
            encodings: vec![],
        },
        AssetConfig::Pattern {
            pattern: "**/*.woff".to_string(),
            content_type: Some("font/woff".to_string()),
            headers: get_asset_headers(vec![(
                "cache-control".to_string(),
                IMMUTABLE_ASSET_CACHE_CONTROL.to_string(),
            )]),
            encodings: vec![],
        },
        // Image files
        AssetConfig::Pattern {
            pattern: "**/*.png".to_string(),
            content_type: Some("image/png".to_string()),
            headers: get_asset_headers(vec![(
                "cache-control".to_string(),
                IMMUTABLE_ASSET_CACHE_CONTROL.to_string(),
            )]),
            encodings: vec![],
        },
        AssetConfig::Pattern {
            pattern: "**/*.svg".to_string(),
            content_type: Some("image/svg+xml".to_string()),
            headers: get_asset_headers(vec![(
                "cache-control".to_string(),
                IMMUTABLE_ASSET_CACHE_CONTROL.to_string(),
            )]),
            encodings: encodings.clone(),
        },
        AssetConfig::Pattern {
            pattern: "**/*.webp".to_string(),
            content_type: Some("image/webp".to_string()),
            headers: get_asset_headers(vec![(
                "cache-control".to_string(),
                IMMUTABLE_ASSET_CACHE_CONTROL.to_string(),
            )]),
            encodings: vec![],
        },
        AssetConfig::Pattern {
            pattern: "**/*.ico".to_string(),
            content_type: Some("image/x-icon".to_string()),
            headers: get_asset_headers(vec![(
                "cache-control".to_string(),
                IMMUTABLE_ASSET_CACHE_CONTROL.to_string(),
            )]),
            encodings: vec![],
        },
        // WebAssembly files
        AssetConfig::Pattern {
            pattern: "**/*.wasm".to_string(),
            content_type: Some("application/wasm".to_string()),
            headers: get_asset_headers(vec![(
                "cache-control".to_string(),
                IMMUTABLE_ASSET_CACHE_CONTROL.to_string(),
            )]),
            encodings: vec![],
        },
        // JSON files
        AssetConfig::Pattern {
            pattern: "**/*.json".to_string(),
            content_type: Some("application/json".to_string()),
            headers: get_asset_headers(vec![(
                "cache-control".to_string(),
                IMMUTABLE_ASSET_CACHE_CONTROL.to_string(),
            )]),
            encodings: encodings.clone(),
        },
    ];

    // 2. Collect all assets from the frontend build directory.
    let mut assets = Vec::new();
    collect_assets(&ASSETS_DIR, &mut assets);

    // 3. Skip certification for the metrics endpoint.
    HTTP_TREE.with(|tree| {
        let mut tree = tree.borrow_mut();

        let metrics_tree_path = HttpCertificationPath::exact("/metrics");
        let metrics_certification = HttpCertification::skip();
        let metrics_tree_entry =
            HttpCertificationTreeEntry::new(metrics_tree_path, metrics_certification);

        tree.insert(&metrics_tree_entry);
    });

    ASSET_ROUTER.with_borrow_mut(|asset_router| {
        // 4. Certify the assets using the `certify_assets` function from the `ic-asset-certification` crate.
        if let Err(err) = asset_router.certify_assets(assets, asset_configs) {
            ic_cdk::trap(&format!("Failed to certify assets: {}", err));
        }

        // 5. Set the canister's certified data.
        ic_cdk::api::set_certified_data(&asset_router.root_hash());
    });
}

fn get_asset_headers(additional_headers: Vec<HeaderField>) -> Vec<HeaderField> {
    // List of recommended security headers as per https://owasp.org/www-project-secure-headers/
    // These headers enable browser security features (like limit access to platform apis and set
    // iFrame policies, etc.)
    let mut headers = vec![
        // X-Frame-Options: DENY
        // Prevents the page from being displayed in a frame, iframe, embed or object
        // This is a legacy header, also enforced by CSP frame-ancestors directive
        ("X-Frame-Options".to_string(), "DENY".to_string()),
        // X-Content-Type-Options: nosniff
        // Prevents browsers from MIME-sniffing a response away from the declared content-type
        // Reduces risk of drive-by downloads and serves as defense against MIME confusion attacks
        ("X-Content-Type-Options".to_string(), "nosniff".to_string()),
        // Content-Security-Policy (CSP)
        // Comprehensive policy to prevent XSS attacks and data injection
        (
            "Content-Security-Policy".to_string(),
            get_content_security_policy(),
        ),
        // Strict-Transport-Security (HSTS)
        // Forces browsers to use HTTPS for all future requests to this domain
        // max-age=31536000: Valid for 1 year (31,536,000 seconds)
        // includeSubDomains: Also applies to all subdomains of this domain
        (
            "Strict-Transport-Security".to_string(),
            "max-age=31536000 ; includeSubDomains".to_string(),
        ),
        // Referrer-Policy: same-origin
        // Controls how much referrer information is sent with outgoing requests
        // same-origin: Only send referrer to same-origin requests (no cross-origin leakage)
        // Note: "no-referrer" would be more strict but breaks local dev deployment
        ("Referrer-Policy".to_string(), "same-origin".to_string()),
        // Permissions-Policy (formerly Feature-Policy)
        // Controls which browser features and APIs can be used
        // Most permissions are denied by default, with specific exceptions:
        // - clipboard-write=(self): Allow copying to clipboard from same origin
        // - publickey-credentials-get=(self): Allow WebAuthn from same origin
        // - sync-xhr=(self): Allow synchronous XMLHttpRequest from same origin
        (
            "Permissions-Policy".to_string(),
            "accelerometer=(),\
             autoplay=(),\
             camera=(),\
             clipboard-read=(),\
             clipboard-write=(self),\
             display-capture=(),\
             encrypted-media=(),\
             fullscreen=(),\
             gamepad=(),\
             geolocation=(),\
             gyroscope=(),\
             hid=(),\
             idle-detection=(),\
             interest-cohort=(),\
             magnetometer=(),\
             microphone=(),\
             midi=(),\
             payment=(),\
             picture-in-picture=(),\
             publickey-credentials-get=(self),\
             screen-wake-lock=(),\
             serial=(),\
             sync-xhr=(self),\
             usb=(),\
             web-share=(),\
             xr-spatial-tracking=()"
                .to_string(),
        ),
    ];
    headers.extend(additional_headers);

    headers
}

/// Full content security policy delivered via HTTP response header.
///
/// CSP directives explained:
///
/// default-src 'none':
///   Default policy for all resource types - deny everything by default
///
/// connect-src 'self' https:
///   Allow network requests to same origin and any HTTPS endpoint
///   - 'self': fetch JS bundles from same origin
///   - https://icp-api.io: official IC HTTP API domain for canister calls
///   - https://*.icp0.io: HTTP fetches for /.well-known/ii-alternative-origins
///   - https://*.ic0.app: legacy domain support for alternative origins
///
/// img-src 'self' data: https://*.googleusercontent.com:
///   Allow images from same origin, data URIs, and Google profile pictures
///
/// script-src 'unsafe-inline' 'unsafe-eval':
///   - 'unsafe-inline': Allow inline scripts (required for SvelteKit)
///   - 'unsafe-eval': Required for WebAssembly modules used by agent-js for BLS signature validation
///
/// base-uri 'none':
///   Prevents injection of <base> tags that could redirect relative URLs
///
/// form-action 'none':
///   Prevents forms from being submitted anywhere (II doesn't use forms)
///
/// style-src 'self' 'unsafe-inline':
///   Allow stylesheets from same origin and inline styles
///   'unsafe-inline' needed due to how styles are currently handled in the app
///
/// font-src 'self':
///   Allow fonts only from same origin
///
/// frame-ancestors 'self':
///   Control embedding - only allow same origin
///
/// upgrade-insecure-requests (production only):
///   Automatically upgrade HTTP requests to HTTPS (omitted in dev for localhost)
fn get_content_security_policy() -> String {
    let connect_src = "'self' https:";

    // Allow connecting via http for development purposes
    #[cfg(feature = "dev_csp")]
    let connect_src = format!("{connect_src} http:");

    let csp = format!(
        "default-src 'none';\
         connect-src {connect_src};\
         img-src 'self' data: https://*.googleusercontent.com;\
         script-src 'self' 'unsafe-inline' 'unsafe-eval';\
         base-uri 'none';\
         form-action 'none';\
         style-src 'self' 'unsafe-inline';\
         style-src-elem 'self' 'unsafe-inline';\
         font-src 'self';\
         frame-ancestors 'self';\
         frame-src 'self';"
    );

    // For production builds, upgrade all HTTP connections to HTTPS
    // Omitted in dev builds to allow localhost development
    #[cfg(not(feature = "dev_csp"))]
    let csp = format!("{csp}upgrade-insecure-requests;");

    csp
}

/// Recursively collect all assets from the provided directory.
/// For .gz files, strip the extension and treat them as the base file
/// (since the build script deletes uncompressed .js and .woff2 files).
fn collect_assets<'content, 'path>(
    dir: &'content Dir<'path>,
    assets: &mut Vec<Asset<'content, 'path>>,
) {
    for file in dir.files() {
        let path_str = file.path().to_string_lossy().to_string();

        assets.push(Asset::new(path_str, file.contents()));
    }

    for dir in dir.dirs() {
        collect_assets(dir, assets);
    }
}

#[query]
fn http_request(req: HttpRequest) -> HttpResponse {
    serve_asset(&req)
}

fn serve_asset(req: &HttpRequest) -> HttpResponse<'static> {
    ASSET_ROUTER.with_borrow(|asset_router| {
        if let Ok(response) = asset_router.serve_asset(
            &ic_cdk::api::data_certificate().expect("No data certificate available"),
            req,
        ) {
            response
        } else {
            ic_cdk::trap("Failed to serve asset");
        }
    })
}

fn main() {}
