use crate::archive::ArchiveState;
use crate::assets::ContentType;
use crate::{assets, state, LABEL_ASSETS, LABEL_SIG};
use ic_cdk::api::stable::stable64_size;
use ic_cdk::api::{data_certificate, time};
use ic_cdk::trap;
use ic_certified_map::HashTree;
use ic_metrics_encoder::MetricsEncoder;
use internet_identity_interface::{HeaderField, HttpRequest, HttpResponse};
use serde::Serialize;
use serde_bytes::{ByteBuf, Bytes};
use std::borrow::Cow;

impl ContentType {
    pub fn to_mime_type_string(&self) -> String {
        match self {
            ContentType::HTML => "text/html".to_string(),
            ContentType::JS => "text/javascript".to_string(),
            ContentType::CSS => "text/css".to_string(),
            ContentType::ICO => "image/vnd.microsoft.icon".to_string(),
            ContentType::WEBP => "image/webp".to_string(),
            ContentType::OCTETSTREAM => "application/octet-stream".to_string(),
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
                "https://support.dfinity.org/hc/en-us/sections/8730568843412-Internet-Identity"
                    .to_string(),
            )],
            body: Cow::Owned(ByteBuf::new()),
            streaming_strategy: None,
        },
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
                    body: Cow::Owned(ByteBuf::from(format!("Failed to encode metrics: {err}"))),
                    streaming_strategy: None,
                },
            }
        }
        probably_an_asset => {
            let certificate_header = make_asset_certificate_header(probably_an_asset);
            let mut headers = security_headers();
            headers.push(certificate_header);

            state::assets(|a| match a.get(probably_an_asset) {
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
                        "Asset {probably_an_asset} not found."
                    ))),
                    streaming_strategy: None,
                },
            })
        }
    }
}

fn encode_metrics(w: &mut MetricsEncoder<Vec<u8>>) -> std::io::Result<()> {
    state::storage(|storage| {
        w.encode_gauge(
            "internet_identity_user_count",
            storage.anchor_count() as f64,
            "Number of users registered in this canister.",
        )?;
        let (lo, hi) = storage.assigned_anchor_number_range();
        w.encode_gauge(
            "internet_identity_min_user_number",
            lo as f64,
            "The lowest Identity Anchor served by this canister.",
        )?;
        w.encode_gauge(
            "internet_identity_max_user_number",
            (hi - 1) as f64,
            "The highest Identity Anchor that can be served by this canister.",
        )
    })?;
    state::signature_map(|sigs| {
        w.encode_gauge(
            "internet_identity_signature_count",
            sigs.len() as f64,
            "Number of active signatures issued by this canister.",
        )
    })?;
    w.encode_gauge(
        "internet_identity_stable_memory_pages",
        stable64_size() as f64,
        "Number of stable memory pages used by this canister.",
    )?;
    w.encode_gauge(
        "internet_identity_last_upgrade_timestamp",
        state::last_upgrade_timestamp() as f64,
        "The most recent IC time (in nanos) when this canister was successfully upgraded.",
    )?;
    state::inflight_challenges(|inflight_challenges| {
        w.encode_gauge(
            "internet_identity_inflight_challenges",
            inflight_challenges.len() as f64,
            "The number of inflight CAPTCHA challenges",
        )
    })?;
    state::tentative_device_registrations(|tentative_device_registrations| {
        w.encode_gauge(
            "internet_identity_users_in_registration_mode",
            tentative_device_registrations.len() as f64,
            "The number of users in registration mode",
        )
    })?;
    state::usage_metrics(|usage_metrics| {
        w.encode_gauge(
            "internet_identity_delegation_counter",
            usage_metrics.delegation_counter as f64,
            "The number of delegations created since last upgrade",
        )?;
        w.encode_gauge(
            "internet_identity_anchor_operations_counter",
            usage_metrics.anchor_operation_counter as f64,
            "The number of anchor operations since last upgrade",
        )
    })?;
    if let ArchiveState::Created { ref data, .. } = state::archive_state() {
        w.encode_gauge(
            "internet_identity_archive_sequence_number",
            data.sequence_number as f64,
            "The number of entries written to the archive.",
        )?;
        w.encode_gauge(
            "internet_identity_buffered_archive_entries",
            data.entries_buffer.len() as f64,
            "The number of buffered archive entries.",
        )?;
    }
    Ok(())
}

/// List of recommended security headers as per https://owasp.org/www-project-secure-headers/
/// These headers enable browser security features (like limit access to platform apis and set
/// iFrame policies, etc.).
fn security_headers() -> Vec<HeaderField> {
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
/// We deliver the CSP by header _and_ meta tag because the headers are not yet certified.
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
/// connect-src https://*.ic0.app is required in order for II to be able to fetch the
/// /.well-known/ii-alternative-origins path of authenticating canisters setting a derivationOrigin.
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
         connect-src 'self' https://ic0.app https://*.ic0.app;\
         img-src 'self' data:;\
         script-src '{hash}' 'unsafe-inline' 'unsafe-eval' 'strict-dynamic' https:;\
         base-uri 'none';\
         form-action 'none';\
         style-src 'self' 'unsafe-inline' https://fonts.googleapis.com;\
         style-src-elem 'self' 'unsafe-inline' https://fonts.googleapis.com;\
         font-src https://fonts.gstatic.com;"
    );
    #[cfg(not(feature = "insecure_requests"))]
    let csp = format!("{csp}upgrade-insecure-requests;");
    csp
}

fn make_asset_certificate_header(asset_name: &str) -> (String, String) {
    let certificate = data_certificate().unwrap_or_else(|| {
        trap("data certificate is only available in query calls");
    });
    state::asset_hashes_and_sigs(|asset_hashes, sigs| {
        let witness = asset_hashes.witness(asset_name.as_bytes());
        let tree = ic_certified_map::fork(
            ic_certified_map::labeled(LABEL_ASSETS, witness),
            HashTree::Pruned(ic_certified_map::labeled_hash(LABEL_SIG, &sigs.root_hash())),
        );
        let mut serializer = serde_cbor::ser::Serializer::new(vec![]);
        serializer.self_describe().unwrap();
        tree.serialize(&mut serializer)
            .unwrap_or_else(|e| trap(&format!("failed to serialize a hash tree: {e}")));
        (
            "IC-Certificate".to_string(),
            format!(
                "certificate=:{}:, tree=:{}:",
                base64::encode(&certificate),
                base64::encode(serializer.into_inner())
            ),
        )
    })
}
