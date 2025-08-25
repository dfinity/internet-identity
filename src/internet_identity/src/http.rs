use crate::http::metrics::metrics;
use crate::state;
use ic_canister_sig_creation::signature_map::LABEL_SIG;
use ic_certification::{labeled_hash, pruned};
use internet_identity_interface::http_gateway::{HeaderField, HttpRequest, HttpResponse};
use serde_bytes::ByteBuf;

mod metrics;

/// CORS-safe security headers for OPTIONS requests
/// These headers provide security without interfering with CORS preflight requests
fn cors_safe_security_headers() -> Vec<HeaderField> {
    vec![
        // X-Frame-Options: DENY
        // Prevents the page from being displayed in a frame, iframe, embed or object
        // This helps prevent clickjacking attacks
        ("X-Frame-Options".to_string(), "DENY".to_string()),
        // X-Content-Type-Options: nosniff
        // Prevents browsers from MIME-sniffing the content type
        // Forces browsers to respect the declared Content-Type header
        ("X-Content-Type-Options".to_string(), "nosniff".to_string()),
        // Strict-Transport-Security (HSTS)
        // Forces browsers to use HTTPS for all future requests to this domain
        // max-age=31536000: Valid for 1 year (365 days)
        // includeSubDomains: Also applies to all subdomains
        (
            "Strict-Transport-Security".to_string(),
            "max-age=31536000 ; includeSubDomains".to_string(),
        ),
        // Referrer-Policy: same-origin
        // Controls how much referrer information is sent with requests
        // same-origin: Only send referrer to same-origin requests
        ("Referrer-Policy".to_string(), "same-origin".to_string()),
        // Content-Security-Policy: default-src 'none'
        // Minimal CSP for OPTIONS - blocks all content since no scripts should execute
        (
            "Content-Security-Policy".to_string(),
            "default-src 'none'".to_string(),
        ),
    ]
}

fn http_options_request() -> HttpResponse {
    let mut headers = vec![
        ("Access-Control-Allow-Origin".to_string(), "*".to_string()),
        (
            "Access-Control-Allow-Methods".to_string(),
            "GET, POST, OPTIONS".to_string(),
        ),
        (
            "Access-Control-Allow-Headers".to_string(),
            "Content-Type".to_string(),
        ),
        ("Content-Length".to_string(), "0".to_string()),
    ];

    headers.append(&mut cors_safe_security_headers());

    HttpResponse {
        // Indicates success without any additional content to be sent in the response content.
        status_code: 204,
        headers,
        body: ByteBuf::from(vec![]),
        upgrade: None,
        streaming_strategy: None,
    }
}

fn http_get_request(url: String, certificate_version: Option<u16>) -> HttpResponse {
    let parts: Vec<&str> = url.split('?').collect();

    match parts[0] {
        "/metrics" => match metrics() {
            Ok(body) => {
                let mut headers = vec![
                    (
                        "Content-Type".to_string(),
                        "text/plain; version=0.0.4".to_string(),
                    ),
                    ("Content-Length".to_string(), body.len().to_string()),
                ];
                headers.append(&mut security_headers(vec![], None));
                HttpResponse {
                    status_code: 200,
                    headers,
                    body: ByteBuf::from(body),
                    upgrade: None,
                    streaming_strategy: None,
                }
            }
            Err(err) => HttpResponse {
                status_code: 500,
                headers: security_headers(vec![], None),
                body: ByteBuf::from(format!("Failed to encode metrics: {err}")),
                upgrade: None,
                streaming_strategy: None,
            },
        },
        probably_an_asset => match get_asset(probably_an_asset, certificate_version) {
            Some((status_code, content, headers)) => HttpResponse {
                status_code,
                headers,
                body: ByteBuf::from(content),
                upgrade: None,
                streaming_strategy: None,
            },
            None => HttpResponse {
                status_code: 404,
                headers: security_headers(vec![], None),
                body: ByteBuf::from(format!("Asset {probably_an_asset} not found.")),
                upgrade: None,
                streaming_strategy: None,
            },
        },
    }
}

fn http_head_request(url: String, certificate_version: Option<u16>) -> HttpResponse {
    let mut resp = http_get_request(url, certificate_version);
    resp.body.clear(); // HEAD has no body
    resp
}

fn method_not_allowed(unsupported_method: &str) -> HttpResponse {
    HttpResponse {
        status_code: 405,
        headers: vec![("Allow".into(), "GET, HEAD, OPTIONS".into())],
        body: ByteBuf::from(format!("Method {unsupported_method} not allowed.")),
        upgrade: None,
        streaming_strategy: None,
    }
}

pub fn http_request(req: HttpRequest) -> HttpResponse {
    let HttpRequest {
        method,
        url,
        certificate_version,
        headers: _,
        body: _,
    } = req;

    match method.as_str() {
        "OPTIONS" => http_options_request(),
        "GET" => http_get_request(url, certificate_version),
        "HEAD" => http_head_request(url, certificate_version),
        unsupported_method => method_not_allowed(unsupported_method),
    }
}

/// List of recommended security headers as per https://owasp.org/www-project-secure-headers/
/// These headers enable browser security features (like limit access to platform apis and set
/// iFrame policies, etc.).
///
/// Integrity hashes for scripts must be specified.
pub fn security_headers(
    integrity_hashes: Vec<String>,
    maybe_related_origins: Option<Vec<String>>,
) -> Vec<HeaderField> {
    // Allow related origins to get WebAuthn credentials from one another
    let public_key_credentials_get = maybe_related_origins
        .clone()
        .unwrap_or_default()
        .iter()
        .fold("self".to_string(), |acc, origin| {
            acc + " \"" + origin + "\""
        });

    vec![
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
        // See content_security_policy_header() function for detailed explanation
        (
            "Content-Security-Policy".to_string(),
            content_security_policy_header(integrity_hashes, maybe_related_origins),
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
        // - publickey-credentials-get: Allow WebAuthn from self and related origins
        // - sync-xhr=(self): Allow synchronous XMLHttpRequest from same origin
        (
            "Permissions-Policy".to_string(),
            format!(
                "accelerometer=(),\
                 ambient-light-sensor=(),\
                 autoplay=(),\
                 battery=(),\
                 camera=(),\
                 clipboard-read=(),\
                 clipboard-write=(self),\
                 conversion-measurement=(),\
                 cross-origin-isolated=(),\
                 display-capture=(),\
                 document-domain=(),\
                 encrypted-media=(),\
                 execution-while-not-rendered=(),\
                 execution-while-out-of-viewport=(),\
                 focus-without-user-activation=(),\
                 fullscreen=(),\
                 gamepad=(),\
                 geolocation=(),\
                 gyroscope=(),\
                 hid=(),\
                 idle-detection=(),\
                 interest-cohort=(),\
                 keyboard-map=(),\
                 magnetometer=(),\
                 microphone=(),\
                 midi=(),\
                 navigation-override=(),\
                 payment=(),\
                 picture-in-picture=(),\
                 publickey-credentials-get=({public_key_credentials_get}),\
                 screen-wake-lock=(),\
                 serial=(),\
                 speaker-selection=(),\
                 sync-script=(),\
                 sync-xhr=(self),\
                 trust-token-redemption=(),\
                 usb=(),\
                 vertical-scroll=(),\
                 web-share=(),\
                 window-placement=(),\
                 xr-spatial-tracking=()"
            )
            .to_string(),
        ),
    ]
}

/// Full content security policy delivered via HTTP response header.
///
/// CSP directives explained:
///
/// default-src 'none':
///   Default policy for all resource types - deny everything by default
///
/// connect-src 'self' https::
///   Allow network requests to same origin and any HTTPS endpoint
///   - 'self': fetch JS bundles from same origin
///   - https://icp-api.io: official IC HTTP API domain for canister calls
///   - https://*.icp0.io: HTTP fetches for /.well-known/ii-alternative-origins
///   - https://*.ic0.app: legacy domain support for alternative origins
///
/// img-src 'self' data: https://*.googleusercontent.com:
///   Allow images from same origin, data URIs, and Google profile pictures
///
/// script-src with 'strict-dynamic':
///   - 'strict-dynamic': Only scripts with listed hashes can load, transitively loaded scripts inherit permission
///   - 'unsafe-eval': Required for WebAssembly modules used by agent-js for BLS signature validation
///   - 'unsafe-inline' https:: Backwards compatibility fallback (ignored by modern browsers)
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
/// frame-ancestors and frame-src:
///   Control embedding - allow self and related origins for cross-domain WebAuthn
///
/// upgrade-insecure-requests (production only):
///   Automatically upgrade HTTP requests to HTTPS (omitted in dev for localhost)
fn content_security_policy_header(
    integrity_hashes: Vec<String>,
    maybe_related_origins: Option<Vec<String>>,
) -> String {
    // Always include 'strict-dynamic', but only include integrity hashes if there are some
    // (i.e. by default deny scripts and only allow whitelist)
    let strict_dynamic = if integrity_hashes.is_empty() {
        "'strict-dynamic'".to_string()
    } else {
        format!(
            "'strict-dynamic' {}",
            integrity_hashes
                .into_iter()
                .map(|x| format!("'{x}'"))
                .collect::<Vec<String>>()
                .join(" ")
        )
    };

    let connect_src = "'self' https:";

    // Allow connecting via http for development purposes
    #[cfg(feature = "dev_csp")]
    let connect_src = format!("{connect_src} http:");

    // Allow related origins to embed one another for cross-domain WebAuthn
    let frame_src = maybe_related_origins
        .unwrap_or_default()
        .iter()
        .fold("'self'".to_string(), |acc, origin| acc + " " + origin);

    let csp = format!(
        "default-src 'none';\
         connect-src {connect_src};\
         img-src 'self' data: https://*.googleusercontent.com;\
         script-src {strict_dynamic} 'unsafe-inline' 'unsafe-eval' https:;\
         base-uri 'none';\
         form-action 'none';\
         style-src 'self' 'unsafe-inline';\
         style-src-elem 'self' 'unsafe-inline';\
         font-src 'self';\
         frame-ancestors {frame_src};\
         frame-src {frame_src};"
    );
    // For production builds, upgrade all HTTP connections to HTTPS
    // Omitted in dev builds to allow localhost development
    #[cfg(not(feature = "dev_csp"))]
    let csp = format!("{csp}upgrade-insecure-requests;");
    csp
}

/// Read an asset from memory, returning the associated HTTP code, content and full list of
/// headers that were certified with the asset.
fn get_asset(
    asset_name: &str,
    certificate_version: Option<u16>,
) -> Option<(u16, Vec<u8>, Vec<HeaderField>)> {
    state::assets_and_signatures(|assets, sigs| {
        let asset = assets.get_certified_asset(
            asset_name,
            certificate_version,
            Some(pruned(labeled_hash(LABEL_SIG, &sigs.root_hash()))),
        )?;
        let shared_headers = assets.shared_headers.clone();

        let mut headers = asset.headers.clone();
        headers.append(&mut shared_headers.to_vec());

        Some((asset.status_code, asset.content, headers))
    })
}
