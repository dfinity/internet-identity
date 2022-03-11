use crate::{assets, AssetHashes, ContentType, ASSETS, LABEL_ASSETS, LABEL_SIG, STATE};
use ic_cdk::api::stable::stable64_size;
use ic_cdk::api::{data_certificate, time};
use ic_cdk::export::candid::{CandidType, Deserialize, Func};
use ic_cdk::trap;
use ic_certified_map::HashTree;
use internet_identity::metrics_encoder::MetricsEncoder;
use internet_identity::signature_map::SignatureMap;
use serde::Serialize;
use serde_bytes::{ByteBuf, Bytes};
use std::borrow::Cow;

pub type HeaderField = (String, String);

#[derive(Clone, Debug, CandidType, Deserialize)]
struct Token {}

#[derive(Clone, Debug, CandidType, Deserialize)]
enum StreamingStrategy {
    Callback { callback: Func, token: Token },
}

#[derive(Clone, Debug, CandidType, Deserialize)]
struct StreamingCallbackHttpResponse {
    body: ByteBuf,
    token: Option<Token>,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct HttpRequest {
    method: String,
    url: String,
    headers: Vec<(String, String)>,
    body: ByteBuf,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct HttpResponse {
    status_code: u16,
    headers: Vec<HeaderField>,
    body: Cow<'static, Bytes>,
    streaming_strategy: Option<StreamingStrategy>,
}

impl ContentType {
    pub fn to_mime_type_string(&self) -> String {
        match self {
            ContentType::HTML => "text/html".to_string(),
            ContentType::JS => "text/javascript".to_string(),
            ContentType::ICO => "image/vnd.microsoft.icon".to_string(),
            ContentType::WEBP => "image/webp".to_string(),
            ContentType::SVG => "image/svg+xml".to_string(),
        }
    }
}

pub fn http_request(req: HttpRequest) -> HttpResponse {
    let parts: Vec<&str> = req.url.split('?').collect();
    match parts[0] {
        "/metrics" => {
            let mut writer = MetricsEncoder::new(vec![], time() as i64 / 1_000_000);
            match encode_metrics(&mut writer) {
                Ok(()) => {
                    let body = writer.into_inner();
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
                        body: Cow::Owned(ByteBuf::from(body)),
                        streaming_strategy: None,
                    }
                }
                Err(err) => HttpResponse {
                    status_code: 500,
                    headers: security_headers(),
                    body: Cow::Owned(ByteBuf::from(format!("Failed to encode metrics: {}", err))),
                    streaming_strategy: None,
                },
            }
        }
        probably_an_asset => {
            let certificate_header = STATE.with(|s| {
                make_asset_certificate_header(
                    &s.asset_hashes.borrow(),
                    &s.sigs.borrow(),
                    probably_an_asset,
                )
            });
            let mut headers = security_headers();
            headers.push(certificate_header);

            ASSETS.with(|a| match a.borrow().get(probably_an_asset) {
                Some((asset_headers, value)) => {
                    headers.append(&mut asset_headers.clone());

                    HttpResponse {
                        status_code: 200,
                        headers,
                        body: Cow::Borrowed(Bytes::new(value)),
                        streaming_strategy: None,
                    }
                }
                None => HttpResponse {
                    status_code: 404,
                    headers,
                    body: Cow::Owned(ByteBuf::from(format!(
                        "Asset {} not found.",
                        probably_an_asset
                    ))),
                    streaming_strategy: None,
                },
            })
        }
    }
}

fn encode_metrics(w: &mut MetricsEncoder<Vec<u8>>) -> std::io::Result<()> {
    STATE.with(|s| {
        w.encode_gauge(
            "internet_identity_user_count",
            s.storage.borrow().user_count() as f64,
            "Number of users registered in this canister.",
        )?;
        let (lo, hi) = s.storage.borrow().assigned_user_number_range();
        w.encode_gauge(
            "internet_identity_min_user_number",
            lo as f64,
            "The lowest Identity Anchor served by this canister.",
        )?;
        w.encode_gauge(
            "internet_identity_max_user_number",
            (hi - 1) as f64,
            "The highest Identity Anchor that can be served by this canister.",
        )?;
        w.encode_gauge(
            "internet_identity_signature_count",
            s.sigs.borrow().len() as f64,
            "Number of active signatures issued by this canister.",
        )?;
        w.encode_gauge(
            "internet_identity_stable_memory_pages",
            stable64_size() as f64,
            "Number of stable memory pages used by this canister.",
        )?;
        w.encode_gauge(
            "internet_identity_last_upgrade_timestamp",
            s.last_upgrade_timestamp.get() as f64,
            "The most recent IC time (in nanos) when this canister was successfully upgraded.",
        )?;
        w.encode_gauge(
            "internet_identity_inflight_challenges",
            s.inflight_challenges.borrow().len() as f64,
            "The number of inflight CAPTCHA challenges",
        )?;
        w.encode_gauge(
            "internet_identity_users_in_registration_mode",
            s.tentative_device_registrations.borrow().len() as f64,
            "The number of users in registration mode",
        )?;
        Ok(())
    })
}

/// List of recommended security headers as per https://owasp.org/www-project-secure-headers/
/// These headers enable browser security features (like limit access to platform apis and set
/// iFrame policies, etc.).
fn security_headers() -> Vec<HeaderField> {
    let hash = assets::INDEX_HTML_SETUP_JS_SRI_HASH.to_string();
    vec![
        ("X-Frame-Options".to_string(), "DENY".to_string()),
        ("X-Content-Type-Options".to_string(), "nosniff".to_string()),
        // Content Security Policy
        //
        // The sha256 hash matches the inline script in index.html. This inline script is a workaround
        // for Firefox not supporting SRI (recommended here https://csp.withgoogle.com/docs/faq.html#static-content).
        // This also prevents use of trusted-types. See https://bugzilla.mozilla.org/show_bug.cgi?id=1409200.
        //
        // script-src 'unsafe-eval' is required because agent-js uses a WebAssembly module for the
        // validation of bls signatures.
        // There is currently no other way to allow execution of WebAssembly modules with CSP.
        // See https://github.com/WebAssembly/content-security-policy/blob/main/proposals/CSP.md.
        //
        // script-src 'unsafe-inline' https: are only there for backwards compatibility and ignored
        // by modern browsers.
        //
        // style-src 'unsafe-inline' is currently required due to the way styles are handled by the
        // application. Adding hashes would require a big restructuring of the application and build
        // infrastructure.
        //
        // NOTE about `script-src`: we cannot use a normal script tag like this
        //   <script src="index.js" integrity="sha256-..." defer></script>
        // because Firefox does not support SRI with CSP: https://bugzilla.mozilla.org/show_bug.cgi?id=1409200
        // Instead, we add it to the CSP policy
        (
            "Content-Security-Policy".to_string(),
            format!(
                "default-src 'none';\
             connect-src 'self' https://ic0.app;\
             img-src 'self' data:;\
             script-src '{hash}' 'unsafe-inline' 'unsafe-eval' 'strict-dynamic' https:;\
             base-uri 'none';\
             frame-ancestors 'none';\
             form-action 'none';\
             style-src 'self' 'unsafe-inline' https://fonts.googleapis.com;\
             style-src-elem 'unsafe-inline' https://fonts.googleapis.com;\
             font-src https://fonts.gstatic.com;\
             upgrade-insecure-requests;"
            )
            .to_string(),
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

fn make_asset_certificate_header(
    asset_hashes: &AssetHashes,
    sigs: &SignatureMap,
    asset_name: &str,
) -> (String, String) {
    let certificate = data_certificate().unwrap_or_else(|| {
        trap("data certificate is only available in query calls");
    });
    let witness = asset_hashes.witness(asset_name.as_bytes());
    let tree = ic_certified_map::fork(
        ic_certified_map::labeled(LABEL_ASSETS, witness),
        HashTree::Pruned(ic_certified_map::labeled_hash(LABEL_SIG, &sigs.root_hash())),
    );
    let mut serializer = serde_cbor::ser::Serializer::new(vec![]);
    serializer.self_describe().unwrap();
    tree.serialize(&mut serializer)
        .unwrap_or_else(|e| trap(&format!("failed to serialize a hash tree: {}", e)));
    (
        "IC-Certificate".to_string(),
        format!(
            "certificate=:{}:, tree=:{}:",
            base64::encode(&certificate),
            base64::encode(&serializer.into_inner())
        ),
    )
}
