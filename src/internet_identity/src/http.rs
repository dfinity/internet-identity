use crate::assets::{ContentType, EXACT_MATCH_TERMINATOR, IC_CERTIFICATE_EXPRESSION};
use crate::http::metrics::metrics;
use crate::{assets, state, LABEL_SIG};
use base64::engine::general_purpose::STANDARD as BASE64;
use base64::Engine;
use ic_cdk::api::data_certificate;
use ic_cdk::trap;
use ic_certified_map::HashTree;
use internet_identity_interface::http_gateway::{HeaderField, HttpRequest, HttpResponse};
use serde::Serialize;
use serde_bytes::ByteBuf;

mod metrics;

pub const IC_CERTIFICATE_HEADER: &str = "IC-Certificate";
pub const IC_CERTIFICATE_EXPRESSION_HEADER: &str = "IC-CertificateExpression";
const LABEL_HTTP_EXPR: &str = "http_expr";

impl ContentType {
    pub fn to_mime_type_string(self) -> String {
        match self {
            ContentType::HTML => "text/html".to_string(),
            ContentType::JS => "text/javascript".to_string(),
            ContentType::JSON => "application/json".to_string(),
            ContentType::CSS => "text/css".to_string(),
            ContentType::ICO => "image/vnd.microsoft.icon".to_string(),
            ContentType::WEBP => "image/webp".to_string(),
            ContentType::OCTETSTREAM => "application/octet-stream".to_string(),
            ContentType::PNG => "image/png".to_string(),
            ContentType::SVG => "image/svg+xml".to_string(),
            ContentType::WOFF2 => "application/font-woff2".to_string(),
        }
    }
}

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
/// This policy also includes the `frame-ancestors` directive in addition to the policies included in the HTML `meta` tag.
/// We deliver the CSP by header
fn content_security_policy_header() -> String {
    let meta_policy = content_security_policy_meta();
    format!("{meta_policy}frame-ancestors 'none';")
}

/// Stripped down content security policy for the HTML `meta` tag, where not all directives are supported.
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
pub fn content_security_policy_meta() -> String {
    let hash = assets::JS_SETUP_SCRIPT_SRI_HASH.to_string();
    let csp = format!(
        "default-src 'none';\
         connect-src 'self' https://identity.internetcomputer.org https://icp-api.io https://*.icp0.io https://*.ic0.app;\
         img-src 'self' data:;\
         script-src '{hash}' 'unsafe-inline' 'unsafe-eval' 'strict-dynamic' https:;\
         base-uri 'none';\
         form-action 'none';\
         style-src 'self' 'unsafe-inline';\
         style-src-elem 'self' 'unsafe-inline';\
         font-src 'self';"
    );
    #[cfg(not(feature = "insecure_requests"))]
    let csp = format!("{csp}upgrade-insecure-requests;");
    csp
}

fn asset_certificate_headers_v1(asset_name: &str) -> Vec<(String, String)> {
    let certificate = data_certificate().unwrap_or_else(|| {
        trap("data certificate is only available in query calls");
    });
    state::assets_and_signatures(|assets, sigs| {
        let tree = ic_certified_map::fork(
            assets.witness_v1(asset_name),
            HashTree::Pruned(ic_certified_map::labeled_hash(LABEL_SIG, &sigs.root_hash())),
        );
        let mut serializer = serde_cbor::ser::Serializer::new(vec![]);
        serializer.self_describe().unwrap();
        tree.serialize(&mut serializer)
            .unwrap_or_else(|e| trap(&format!("failed to serialize a hash tree: {e}")));
        vec![(
            IC_CERTIFICATE_HEADER.to_string(),
            format!(
                "certificate=:{}:, tree=:{}:",
                BASE64.encode(&certificate),
                BASE64.encode(serializer.into_inner())
            ),
        )]
    })
}

fn asset_certificate_headers_v2(absolute_path: &str) -> Vec<(String, String)> {
    assert!(absolute_path.starts_with('/'));

    let certificate = data_certificate().unwrap_or_else(|| {
        trap("data certificate is only available in query calls");
    });

    let mut path: Vec<String> = absolute_path.split('/').map(str::to_string).collect();
    // replace the first empty split segment (due to absolute path) with "http_expr"
    *path.get_mut(0).unwrap() = LABEL_HTTP_EXPR.to_string();
    path.push(EXACT_MATCH_TERMINATOR.to_string());

    state::assets_and_signatures(|assets, sigs| {
        let tree = ic_certified_map::fork(
            assets.witness_v2(absolute_path),
            HashTree::Pruned(ic_certified_map::labeled_hash(LABEL_SIG, &sigs.root_hash())),
        );

        let mut tree_serializer = serde_cbor::ser::Serializer::new(vec![]);
        tree_serializer.self_describe().unwrap();
        tree.serialize(&mut tree_serializer)
            .unwrap_or_else(|e| trap(&format!("failed to serialize a hash tree: {e}")));

        let mut expr_path_serializer = serde_cbor::ser::Serializer::new(vec![]);
        expr_path_serializer.self_describe().unwrap();
        path.serialize(&mut expr_path_serializer)
            .unwrap_or_else(|e| trap(&format!("failed to serialize a expr_path: {e}")));

        vec![
            (
                IC_CERTIFICATE_HEADER.to_string(),
                format!(
                    "certificate=:{}:, tree=:{}:, expr_path=:{}:, version=2",
                    BASE64.encode(&certificate),
                    BASE64.encode(tree_serializer.into_inner()),
                    BASE64.encode(expr_path_serializer.into_inner())
                ),
            ),
            (
                IC_CERTIFICATE_EXPRESSION_HEADER.to_string(),
                IC_CERTIFICATE_EXPRESSION.to_string(),
            ),
        ]
    })
}
