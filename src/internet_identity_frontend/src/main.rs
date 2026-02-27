use asset_util::{collect_assets, Asset as AssetUtilAsset, ContentEncoding, ContentType};
use base64::engine::general_purpose::STANDARD as BASE64;
use base64::Engine;
use candid::{Encode, IDLValue};
use flate2::read::GzDecoder;
use ic_asset_certification::{Asset, AssetConfig, AssetEncoding, AssetRouter};
use ic_cdk::{init, post_upgrade};
use ic_cdk_macros::query;
use ic_http_certification::{HeaderField, HttpCertificationTree, HttpRequest, HttpResponse};
use include_dir::{include_dir, Dir};
use internet_identity_interface::internet_identity::types::{
    InternetIdentityFrontendArgs, InternetIdentityInit,
};
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

#[init]
fn init(args: InternetIdentityFrontendArgs) {
    certify_all_assets(args);
}

#[post_upgrade]
fn post_upgrade(args: InternetIdentityFrontendArgs) {
    certify_all_assets(args);
}

fn certify_all_assets(args: InternetIdentityFrontendArgs) {
    let static_assets = get_static_assets(&args);

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
    for asset in &static_assets {
        let content = if asset.encoding == ContentEncoding::GZip {
            let mut decoder = GzDecoder::new(&asset.content[..]);
            let mut s = Vec::new();
            decoder.read_to_end(&mut s).unwrap();
            s
        } else {
            asset.content.clone()
        };
        router_assets.push(Asset::new(asset.url_path.clone(), content));
    }

    let asset_configs: Vec<_> = static_assets
        .into_iter()
        .map(
            |asset_util::Asset {
                 url_path: path,
                 encoding,
                 content_type,
                 content: _,
             }| {
                if content_type == ContentType::HTML {
                    AssetConfig::File {
                        path,
                        content_type: Some("text/html".to_string()),
                        headers: get_asset_headers(
                            integrity_hashes.clone(),
                            vec![(
                                "cache-control".to_string(),
                                NO_CACHE_ASSET_CACHE_CONTROL.to_string(),
                            )],
                        ),
                        encodings: vec![AssetEncoding::Identity.default_config()],
                        // Fallbacks and aliases are already handled in `get_static_assets()`
                        fallback_for: vec![],
                        aliased_by: vec![],
                    }
                } else {
                    let encodings = if encoding == ContentEncoding::GZip {
                        vec![AssetEncoding::Gzip.default_config()]
                    } else {
                        vec![AssetEncoding::Identity.default_config()]
                    };

                    let headers = if path.starts_with("/_app/immutable") {
                        (
                            "cache-control".to_string(),
                            IMMUTABLE_ASSET_CACHE_CONTROL.to_string(),
                        )
                    } else {
                        (
                            "cache-control".to_string(),
                            NO_CACHE_ASSET_CACHE_CONTROL.to_string(),
                        )
                    };

                    AssetConfig::File {
                        path,
                        content_type: Some(content_type.to_mime_type_string()),
                        encodings,
                        headers: get_asset_headers(integrity_hashes.clone(), vec![headers]),
                        fallback_for: vec![],
                        aliased_by: vec![],
                    }
                }
            },
        )
        .collect();

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
fn get_static_assets(config: &InternetIdentityFrontendArgs) -> Vec<AssetUtilAsset> {
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

    // Serve the initialization argument of this canister as a Candid file
    assets.push(AssetUtilAsset {
        url_path: "/.config".to_string(),
        content: IDLValue::try_from_candid_type(config)
            .unwrap()
            .to_string()
            .as_bytes()
            .to_vec(),
        encoding: ContentEncoding::Identity,
        content_type: ContentType::TXT,
    });

    // Add .well-known/ic-domains for custom domain support
    assets.push(AssetUtilAsset {
        url_path: "/.well-known/ic-domains".to_string(),
        content: config
            .related_origins
            .clone()
            .unwrap_or_default()
            .into_iter()
            .map(|origin| origin.replace("https://", ""))
            .collect::<Vec<_>>()
            .join("\n")
            .into_bytes(),
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
fn fixup_html(html: &str, config: &InternetIdentityFrontendArgs) -> String {
    let backend_canister_id = config.backend_canister_id;
    let backend_origin = config.backend_origin.clone();

    let html = html.replace(
        "</head>",
        &format!(
            r#"<link rel="preload" href="{backend_origin}/.config.did.bin" crossorigin="anonymous" fetchpriority="high" as="fetch"></head>"#,
        ),
    );

    // Encode config to base64-encoded Candid to avoid JSON escaping issues.
    // For backward compatibility, we use the same struct as before the II canister split.
    let config = InternetIdentityInit::from(config.clone());
    let encoded_config = BASE64.encode(Encode!(&config).unwrap());

    // The backend canister ID is now included in the config, but we also set data-canister-id for backward compatibility.
    let html = html.replace(
        r#"<body "#,
        &format!(
            r#"<body data-canister-id="{backend_canister_id}" data-canister-config="{encoded_config}" "#,
        ),
    );

    html
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
    ASSET_ROUTER.with_borrow(|asset_router| {
        if let Ok(response) = asset_router.serve_asset(
            &ic_cdk::api::data_certificate().expect("No data certificate available"),
            &request,
        ) {
            response
        } else {
            ic_cdk::trap("Failed to serve asset");
        }
    })
}

// Order dependent: do not move above any exposed canister method!
candid::export_service!();

fn main() {}

#[cfg(test)]
mod test {
    use crate::__export_service;
    use candid_parser::utils::{service_equal, CandidSource};
    use std::path::Path;

    /// Checks candid interface type equality by making sure that the service in the did file is
    /// a subtype of the generated interface and vice versa.
    #[test]
    fn check_candid_interface_compatibility() {
        let canister_interface = __export_service();
        service_equal(
            CandidSource::Text(&canister_interface),
            CandidSource::File(Path::new("internet_identity_frontend.did")),
        )
        .unwrap_or_else(|e| {
            panic!("the canister code interface is not equal to the did file: {e:?}")
        });
    }
}
