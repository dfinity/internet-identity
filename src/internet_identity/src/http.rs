use crate::assets::JS_SETUP_SCRIPT_SRI_HASH;
use crate::http::metrics::metrics;
use crate::state;
use canister_sig_util::signature_map::LABEL_SIG;
use ic_cdk::trap;
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
                headers.append(&mut security_headers());
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
                headers: security_headers(),
                body: ByteBuf::from(format!("Failed to encode metrics: {err}")),
                upgrade: None,
                streaming_strategy: None,
            },
        },
        probably_an_asset => {
            state::assets(
                |certified_assets| match certified_assets.assets.get(probably_an_asset) {
                    Some((asset_headers, data)) => {
                        let mut headers = security_headers();
                        let mut certificate_headers = match req.certificate_version {
                            None | Some(1) => asset_certificate_headers_v1(probably_an_asset),
                            Some(2) => asset_certificate_headers_v2(probably_an_asset),
                            _ => trap("Unsupported certificate version."),
                        };
                        headers.append(&mut certificate_headers);
                        headers.append(&mut asset_headers.clone());

                        HttpResponse {
                            status_code: 200,
                            headers,
                            body: ByteBuf::from(data.clone()),
                            upgrade: None,
                            streaming_strategy: None,
                        }
                    }
                    None => HttpResponse {
                        status_code: 404,
                        headers: security_headers(),
                        body: ByteBuf::from(format!("Asset {probably_an_asset} not found.")),
                        upgrade: None,
                        streaming_strategy: None,
                    },
                },
            )
        }
    }
}

/// List of recommended security headers as per https://owasp.org/www-project-secure-headers/
/// These headers enable browser security features (like limit access to platform apis and set
/// iFrame policies, etc.).
pub fn security_headers() -> Vec<HeaderField> {
    vec![
        ("X-Frame-Options".to_string(), "DENY".to_string()),
        ("X-Content-Type-Options".to_string(), "nosniff".to_string()),
        (
            "Content-Security-Policy".to_string(),
            content_security_policy_header(),
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
             publickey-credentials-get=(self),\
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
                .to_string(),
        ),
    ]
}

/// Full content security policy delivered via HTTP response header.
///
/// The sha256 hash matches the inline script in index.html. This inline script is a workaround
/// for Firefox not supporting SRI (recommended here https://csp.withgoogle.com/docs/faq.html#static-content).
/// This also prevents use of trusted-types. See https://bugzilla.mozilla.org/show_bug.cgi?id=1409200.
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
/// NOTE about `script-src`: we cannot use a normal script tag like this
///   <script src="index.js" integrity="sha256-..." defer></script>
/// because Firefox does not support SRI with CSP: https://bugzilla.mozilla.org/show_bug.cgi?id=1409200
/// Instead, we add the hash of the inline script to the CSP policy.
///
/// upgrade-insecure-requests is omitted when building in dev mode to allow loading II on localhost
/// with Safari.
pub fn content_security_policy_header() -> String {
    let hash = JS_SETUP_SCRIPT_SRI_HASH.to_string();
    let csp = format!(
        "default-src 'none';\
         connect-src 'self' https://identity.internetcomputer.org https://icp-api.io https://*.icp0.io https://*.ic0.app;\
         img-src 'self' data:;\
         script-src '{hash}' 'unsafe-inline' 'unsafe-eval' 'strict-dynamic' https:;\
         base-uri 'none';\
         form-action 'none';\
         style-src 'self' 'unsafe-inline';\
         style-src-elem 'self' 'unsafe-inline';\
         font-src 'self';\
         frame-ancestors 'none';"
    );
    #[cfg(not(feature = "insecure_requests"))]
    let csp = format!("{csp}upgrade-insecure-requests;");
    csp
}

fn asset_certificate_headers_v1(asset_name: &str) -> Vec<(String, String)> {
    state::assets_and_signatures(|assets, sigs| {
        assets.certificate_headers_v1(
            asset_name,
            pruned(labeled_hash(LABEL_SIG, &sigs.root_hash())),
        )
    })
}

fn asset_certificate_headers_v2(absolute_path: &str) -> Vec<(String, String)> {
    state::assets_and_signatures(|assets, sigs| {
        assets.certificate_headers_v2(
            absolute_path,
            pruned(labeled_hash(LABEL_SIG, &sigs.root_hash())),
        )
    })
}
