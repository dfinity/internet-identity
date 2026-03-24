use crate::http::metrics::metrics;
use crate::state;
use ic_canister_sig_creation::signature_map::LABEL_SIG;
use ic_certification::{labeled_hash, pruned};
use internet_identity_interface::http_gateway::{HeaderField, HttpRequest, HttpResponse};
use serde_bytes::ByteBuf;

mod metrics;

fn http_options_request() -> HttpResponse {
    // TODO: Restrict origin to just the II-specific origins.
    let headers = vec![("Access-Control-Allow-Origin".to_string(), "*".to_string())];

    HttpResponse {
        // Indicates success without any additional content to be sent in the response content.
        status_code: 204,
        headers,
        body: ByteBuf::from(vec![]),
        upgrade: None,
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
                headers.append(&mut security_headers(None));
                HttpResponse {
                    status_code: 200,
                    headers,
                    body: ByteBuf::from(body),
                    upgrade: None,
                }
            }
            Err(err) => HttpResponse {
                status_code: 500,
                headers: security_headers(None),
                body: ByteBuf::from(format!("Failed to encode metrics: {err}")),
                upgrade: None,
            },
        },
        probably_an_asset => match get_asset(probably_an_asset, certificate_version) {
            Some((status_code, content, headers)) => HttpResponse {
                status_code,
                headers,
                body: ByteBuf::from(content),
                upgrade: None,
            },
            None => HttpResponse {
                status_code: 404,
                headers: security_headers(None),
                body: ByteBuf::from(format!("Asset {probably_an_asset} not found.")),
                upgrade: None,
            },
        },
    }
}

fn method_not_allowed(unsupported_method: &str) -> HttpResponse {
    HttpResponse {
        status_code: 405,
        headers: vec![("Allow".into(), "GET, OPTIONS".into())],
        body: ByteBuf::from(format!("Method {unsupported_method} not allowed.")),
        upgrade: None,
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
        unsupported_method => method_not_allowed(unsupported_method),
    }
}

/// List of recommended security headers as per https://owasp.org/www-project-secure-headers/
/// These headers enable browser security features (like limit access to platform apis and set
/// iFrame policies, etc.).
///
/// Integrity hashes for scripts must be specified.
pub fn security_headers(maybe_related_origins: Option<Vec<String>>) -> Vec<HeaderField> {
    // Allow related origins to create/get WebAuthn credentials from one another
    let public_key_credentials_create_get = maybe_related_origins
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
        // Blocks all resource loading (scripts, styles, images, fonts, frames, etc.)
        // If any HTML is ever rendered, nothing executes
        // Effectively neutralizes most XSS risks
        (
            "Content-Security-Policy".to_string(),
            "default-src 'none';".to_string(),
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
                 publickey-credentials-create=({public_key_credentials_create_get}),\
                 publickey-credentials-get=({public_key_credentials_create_get}),\
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
