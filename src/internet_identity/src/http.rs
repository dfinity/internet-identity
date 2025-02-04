use crate::http::metrics::metrics;
use crate::state;
use ic_canister_sig_creation::signature_map::LABEL_SIG;
use ic_certification::{labeled_hash, pruned};
use internet_identity_interface::http_gateway::{HeaderField, HttpRequest, HttpResponse};
use serde_bytes::ByteBuf;

mod metrics;

pub fn http_request(req: HttpRequest) -> HttpResponse {
    let parts: Vec<&str> = req.url.split('?').collect();
    match parts[0] {
        // The FAQ used to live in '/faq' but we now use an external website. We redirect in order to not
        // break existing links in the wild.
        "/faq" => HttpResponse {
            status_code: 301,
            headers: vec![(
                "location".to_string(),
                "https://identitysupport.dfinity.org/hc/en-us".to_string(),
            )],
            body: ByteBuf::new(),
            // Redirects are not allowed as query because certification V1 does not cover headers.
            // Upgrading to update fixes this. This flag can be removed when switching to
            // certification V2.
            upgrade: Some(true),
            streaming_strategy: None,
        },
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
        probably_an_asset => match get_asset(probably_an_asset, req.certificate_version) {
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

/// List of recommended security headers as per https://owasp.org/www-project-secure-headers/
/// These headers enable browser security features (like limit access to platform apis and set
/// iFrame policies, etc.).
///
/// Integrity hashes for scripts must be speficied.
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
        ("X-Frame-Options".to_string(), "DENY".to_string()),
        ("X-Content-Type-Options".to_string(), "nosniff".to_string()),
        (
            "Content-Security-Policy".to_string(),
            content_security_policy_header(integrity_hashes, maybe_related_origins),
        ),
        (
            "Strict-Transport-Security".to_string(),
            "max-age=31536000 ; includeSubDomains".to_string(),
        ),
        // "Referrer-Policy: no-referrer" would be more strict, but breaks local dev deployment
        // same-origin is still ok from a security perspective
        ("Referrer-Policy".to_string(), "same-origin".to_string()),
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
/// script-src 'strict-dynamic' ensures that only scripts with hashes listed here (through
/// 'integrity_hashes') can be loaded. Transitively loaded scripts don't need their hashes
/// listed.
///
/// script-src 'unsafe-eval' is required because agent-js uses a WebAssembly module for the
/// validation of bls signatures.
/// There is currently no other way to allow execution of WebAssembly modules with CSP.
/// See https://github.com/WebAssembly/content-security-policy/blob/main/proposals/CSP.md.
///
/// script-src 'unsafe-inline' https: are only there for backwards compatibility and ignored
/// by modern browsers.
///
/// connect-src is used to ensure fetch requests can only be made against known domains:
///     * 'self': used fetch the JS bundles
///     * https://icp-api.io: the official IC HTTP API domain for canister calls to the canister
///     * https://*.icp0.io: HTTP fetches for checking /.well-known/ii-alternative-origins on
///     other canisters (authenticating canisters setting a derivationOrigin)
///     * https://*.ic0.app: same as above, but legacy
///
/// style-src 'unsafe-inline' is currently required due to the way styles are handled by the
/// application. Adding hashes would require a big restructuring of the application and build
/// infrastructure.
///
/// upgrade-insecure-requests is omitted when building in dev mode to allow loading II on localhost
/// with Safari.
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

    // Allow related origins to embed one another
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
    // for the dev build skip upgrading all connections to II to https
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
