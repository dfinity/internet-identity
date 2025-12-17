use asset_util::{collect_assets, Asset as AssetUtilAsset, ContentEncoding, ContentType};
use base64::engine::general_purpose::STANDARD as BASE64;
use base64::Engine;
use candid::{Encode, Principal};
use flate2::read::GzDecoder;
use ic_asset_certification::{Asset, AssetConfig, AssetEncoding, AssetFallbackConfig, AssetRouter};
use ic_cdk::{init, post_upgrade};
use ic_cdk_macros::query;
use ic_http_certification::{
    HeaderField, HttpCertificationTree, HttpRequest, HttpResponse, StatusCode,
};
use include_dir::{include_dir, Dir};
use internet_identity_interface::internet_identity::types::{
    DummyAuthConfig, InternetIdentityInit,
};
use lazy_static::lazy_static;
use serde_json::json;
use sha2::Digest;
use std::io::Read;

use std::{cell::RefCell, rc::Rc};

thread_local! {
    static HTTP_TREE: Rc<RefCell<HttpCertificationTree>> = Default::default();
    static ASSET_ROUTER: RefCell<AssetRouter<'static>> = RefCell::new(AssetRouter::with_tree(HTTP_TREE.with(|tree| tree.clone())));
}

static ASSETS_DIR: Dir<'_> = include_dir!("$CARGO_MANIFEST_DIR/../../dist");
const IMMUTABLE_ASSET_CACHE_CONTROL: &str = "public, max-age=31536000, immutable";
const NO_CACHE_ASSET_CACHE_CONTROL: &str = "public, no-cache, no-store";

// Default configuration for the frontend canister
lazy_static! {
    static ref DEFAULT_CONFIG: InternetIdentityInit = InternetIdentityInit {
        assigned_user_number_range: None,
        archive_config: None,
        canister_creation_cycles_cost: None,
        register_rate_limit: None,
        captcha_config: Some(internet_identity_interface::internet_identity::types::CaptchaConfig {
            max_unsolved_captchas: 50,
            captcha_trigger: internet_identity_interface::internet_identity::types::CaptchaTrigger::Static(
                internet_identity_interface::internet_identity::types::StaticCaptchaTrigger::CaptchaDisabled
            ),
        }),
        related_origins: Some(vec![
            "https://identity.internetcomputer.org".to_string(),
            "https://identity.ic0.app".to_string(),
        ]),
        new_flow_origins: None,
        openid_configs: Some(vec![
            internet_identity_interface::internet_identity::types::OpenIdConfig {
                name: "Google".to_string(),
                logo: "https://www.google.com/favicon.ico".to_string(),
                issuer: "https://accounts.google.com".to_string(),
                client_id: "775077467414-q1ajffledt8bjj82p2rl5a09co8cf4rf.apps.googleusercontent.com".to_string(),
                jwks_uri: "https://www.googleapis.com/oauth2/v3/certs".to_string(),
                auth_uri: "https://accounts.google.com/o/oauth2/v2/auth".to_string(),
                auth_scope: vec!["openid".to_string(), "email".to_string(), "profile".to_string()],
                fedcm_uri: None,
            },
        ]),
        analytics_config: None,
        fetch_root_key: None,
        enable_dapps_explorer: None,
        is_production: None,
        dummy_auth: Some(Some(
            DummyAuthConfig { prompt_for_index: true }
        )),
    };
}

#[init]
fn init(init_arg: Option<InternetIdentityInit>) {
    let config = init_arg.unwrap_or_else(|| DEFAULT_CONFIG.clone());
    certify_all_assets(config);
}

#[post_upgrade]
fn post_upgrade() {
    init(None);
}

fn certify_all_assets(init: InternetIdentityInit) {
    let static_assets = get_static_assets(&init);

    // 2. Extract integrity hashes for inline scripts from HTML files
    let integrity_hashes = static_assets
        .iter()
        .filter(|asset| asset.content_type == ContentType::HTML)
        .fold(vec![], |mut acc, e| {
            let content = std::str::from_utf8(&e.content).unwrap().to_string();
            // We need to import extract_inline_scripts? It is defined in file (Line 396).
            for inlined in extract_inline_scripts(content).iter() {
                let hash = sha2::Sha384::digest(inlined.as_bytes());
                let hash = BASE64.encode(hash);
                let hash = format!("sha384-{hash}");
                acc.push(hash);
            }
            acc
        });

    let mut router_assets = Vec::new();
    for asset in static_assets {
        let content = if asset.encoding == ContentEncoding::GZip {
            let mut decoder = GzDecoder::new(&asset.content[..]);
            let mut s = Vec::new();
            decoder.read_to_end(&mut s).unwrap();
            s
        } else {
            asset.content
        };
        router_assets.push(Asset::new(asset.url_path, content));
    }

    let encodings = vec![
        AssetEncoding::Brotli.default_config(),
        AssetEncoding::Gzip.default_config(),
    ];

    let asset_configs = vec![
        AssetConfig::File {
            path: "index.html".to_string(),
            content_type: Some("text/html".to_string()),
            headers: get_asset_headers(
                integrity_hashes.clone(),
                vec![(
                    "cache-control".to_string(),
                    NO_CACHE_ASSET_CACHE_CONTROL.to_string(),
                )],
            ),
            fallback_for: vec![AssetFallbackConfig {
                scope: "/".to_string(),
                status_code: Some(StatusCode::OK),
            }],
            aliased_by: vec!["/".to_string()],
            encodings: encodings.clone(),
        },
        AssetConfig::Pattern {
            pattern: "**/*.html".to_string(),
            content_type: Some("text/html".to_string()),
            headers: get_asset_headers(
                integrity_hashes.clone(),
                vec![(
                    "cache-control".to_string(),
                    NO_CACHE_ASSET_CACHE_CONTROL.to_string(),
                )],
            ),
            encodings: vec![AssetEncoding::Identity.default_config()],
        },
        AssetConfig::Pattern {
            pattern: "**/*.css".to_string(),
            content_type: Some("text/css".to_string()),
            headers: get_asset_headers(
                vec![],
                vec![(
                    "cache-control".to_string(),
                    IMMUTABLE_ASSET_CACHE_CONTROL.to_string(),
                )],
            ),
            encodings: encodings.clone(),
        },
        AssetConfig::Pattern {
            pattern: "**/*.js".to_string(),
            content_type: Some("text/javascript".to_string()),
            headers: get_asset_headers(
                vec![],
                vec![(
                    "cache-control".to_string(),
                    IMMUTABLE_ASSET_CACHE_CONTROL.to_string(),
                )],
            ),
            encodings: encodings.clone(),
        },
        AssetConfig::Pattern {
            pattern: "**/*.ico".to_string(),
            content_type: Some("image/x-icon".to_string()),
            headers: get_asset_headers(
                vec![],
                vec![(
                    "cache-control".to_string(),
                    IMMUTABLE_ASSET_CACHE_CONTROL.to_string(),
                )],
            ),
            encodings: vec![],
        },
        AssetConfig::Pattern {
            pattern: "**/*.png".to_string(),
            content_type: Some("image/png".to_string()),
            headers: get_asset_headers(
                vec![],
                vec![(
                    "cache-control".to_string(),
                    IMMUTABLE_ASSET_CACHE_CONTROL.to_string(),
                )],
            ),
            encodings: vec![],
        },
        AssetConfig::Pattern {
            pattern: "**/*.woff2".to_string(),
            content_type: Some("font/woff2".to_string()),
            headers: get_asset_headers(
                vec![],
                vec![(
                    "cache-control".to_string(),
                    IMMUTABLE_ASSET_CACHE_CONTROL.to_string(),
                )],
            ),
            encodings: encodings.clone(),
        },
        AssetConfig::Pattern {
            pattern: "**/*.json".to_string(),
            content_type: Some("application/json".to_string()),
            headers: get_asset_headers(
                vec![],
                vec![(
                    "cache-control".to_string(),
                    IMMUTABLE_ASSET_CACHE_CONTROL.to_string(),
                )],
            ),
            encodings: vec![],
        },
    ];

    ASSET_ROUTER.with(|asset_router| {
        if let Err(err) = asset_router
            .borrow_mut()
            .certify_assets(router_assets, asset_configs)
        {
            ic_cdk::trap(&format!("Failed to certify assets: {}", err));
        }
        ic_cdk::api::set_certified_data(&asset_router.borrow().root_hash());
    });
}

fn get_asset_headers(
    integrity_hashes: Vec<String>,
    additional_headers: Vec<HeaderField>,
) -> Vec<HeaderField> {
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
            get_content_security_policy(integrity_hashes),
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
fn get_content_security_policy(integrity_hashes: Vec<String>) -> String {
    let connect_src = "'self' https:";

    // Allow connecting via http for development purposes
    #[cfg(feature = "dev_csp")]
    let connect_src = format!("{connect_src} http:");

    // Build script-src with integrity hashes if provided
    let script_src = if integrity_hashes.is_empty() {
        "'self' 'unsafe-inline' 'unsafe-eval'".to_string()
    } else {
        format!(
            "'self' 'unsafe-inline' 'unsafe-eval' {}",
            integrity_hashes
                .into_iter()
                .map(|x| format!("'{x}'"))
                .collect::<Vec<String>>()
                .join(" ")
        )
    };

    let csp = format!(
        "default-src 'none';\
         connect-src {connect_src};\
         img-src 'self' data: https://*.googleusercontent.com;\
         script-src {script_src};\
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

/// Gets the static assets with HTML fixup and well-known endpoints
fn get_static_assets(config: &InternetIdentityInit) -> Vec<AssetUtilAsset> {
    // Collect assets and fix up HTML files
    let mut assets: Vec<AssetUtilAsset> = collect_assets(&ASSETS_DIR, None)
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

    // Add .well-known/ic-domains for custom domain support
    let ic_domains_content = b"identity.internetcomputer.org\nbeta.identity.ic0.app\nbeta.identity.internetcomputer.org\nid.ai\nbeta.id.ai\nwww.id.ai".to_vec();
    assets.push(AssetUtilAsset {
        url_path: "/.well-known/ic-domains".to_string(),
        content: ic_domains_content,
        encoding: ContentEncoding::Identity,
        content_type: ContentType::OCTETSTREAM,
    });

    // Add .well-known/webauthn for passkey sharing if related_origins is configured
    if let Some(related_origins) = &config.related_origins {
        let content = json!({
            "origins": related_origins,
        })
        .to_string()
        .into_bytes();
        assets.push(AssetUtilAsset {
            url_path: "/.well-known/webauthn".to_string(),
            content,
            encoding: ContentEncoding::Identity,
            content_type: ContentType::JSON,
        });
    }

    assets
}

/// Fix up HTML pages by injecting canister ID and canister config
fn fixup_html(html: &str, config: &InternetIdentityInit) -> String {
    let canister_id = Principal::from_text("uxrrr-q7777-77774-qaaaq-cai").unwrap();
    // Encode config to base64-encoded Candid to avoid JSON escaping issues
    let encoded_config = BASE64.encode(Encode!(config).unwrap());
    html.replace(
        r#"<body "#,
        &format!(
            r#"<body data-canister-id="{canister_id}" data-canister-config="{encoded_config}" "#
        ),
    )
}

/// Extract all inline scripts from HTML for CSP hash generation
fn extract_inline_scripts(content: String) -> Vec<String> {
    content
        .match_indices("<script")
        .map(|(tag_open_start, _)| {
            let tag_open_len = content[tag_open_start..].find('>').unwrap();
            let inline_start = tag_open_start + tag_open_len + 1;
            let tag_close_start = content[inline_start..].find("</script>").unwrap();
            content[(inline_start)..(inline_start + tag_close_start)].to_string()
        })
        .collect()
}

#[query]
fn http_request(request: HttpRequest) -> HttpResponse {
    ASSET_ROUTER.with(|asset_router| {
        if let Ok(response) = asset_router.borrow().serve_asset(
            &ic_cdk::api::data_certificate().expect("No data certificate available"),
            &request,
        ) {
            response
        } else {
            ic_cdk::trap("Failed to serve asset");
        }
    })
}

fn main() {}
