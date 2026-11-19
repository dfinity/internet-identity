//! Golden "no behaviour change" corpus for the email-verification pipeline.
//!
//! Drives the checked-in fixtures through the public entry points and pins,
//! per fixture and step, the entrypoint response and the polled
//! `(status, reason_code)` in a JSON file next to this module. The type
//! ladder refactoring must keep every file identical unless a PR names the
//! change; then
//! `UPDATE_GOLDEN=1 cargo test -p internet_identity --test integration golden`
//! regenerates the files and the diff is reviewed with the PR.
//!
//! Recorded values are variant names, public SMTP codes and the public
//! `EmailVerificationFailed` text: that text is the FE-facing payload and
//! the only value that tells the failing stages apart, so a PR that rewords
//! it names the change and regenerates. Never recorded: key
//! material, nonces, addresses or any other payload.
//!
//! Each record is a `Serialize` struct, so key order is declaration order
//! whatever features `serde_json` is built with.
//!
//! Coverage:
//! - `dkim-*.json`: `test_vectors/dkim/*.eml` on the DoH route, clock pinned
//!   shortly after the fixtures' signing time: `smtp_request_validate`, the
//!   SMTP rejection codes for envelope and bound violations derived from the
//!   same message, `email_recovery_prepare_delegation`, `smtp_request` with
//!   the message as captured (its `Subject:` carries no challenge nonce, so
//!   the message is dropped and the challenge stays `Pending`), and again
//!   with the `Subject:` replaced by the issued nonce, polled right after
//!   delivery (the DoH route's `ResolvingDoh`) and after the mocked DoH
//!   resolution with the fixture's own TXT record. The signature does not
//!   cover the rewritten subject, so this pins the failure mapping of each
//!   stage, not a success.
//! - `leaf-*.json`: the same `.eml` fixtures on the DNSSEC route with the
//!   deterministic in-test DNSSEC signer (fixed Ed25519 secrets, validity
//!   windows derived from the pinned clock): `email_recovery_prepare_delegation`
//!   with the signed skeleton and DMARC leaf, `smtp_request` with the nonce
//!   subject (`NeedDkimLeaf`), then `email_challenge_submit_dkim_leaf` with
//!   the signed DKIM leaf carrying the fixture's key and the polled verdict.
//! - `dnssec-*.json`: `test_vectors/dnssec/*.json` on the DNSSEC route with
//!   the IANA root anchors, clock pinned at each capture's
//!   `captured_at_unix`: `email_recovery_credential_prepare_add` with the
//!   bundle as captured and with its skeleton (chains only).
//!
//! Not covered: a successful end-to-end verification. The in-test DKIM
//! signer draws a fresh RSA key on every run, so its output is not golden
//! material.

use crate::email_recovery::{dnssec_signer, drive_doh_resolution, fresh_identity};
use candid::Principal;
use canister_tests::{api::internet_identity as api, framework::*};
use internet_identity_interface::internet_identity::types::dnssec::{
    DelegationChain, DelegationLink, DnsProofBundle, DnssecRootAnchor, Rrsig, SignedRRset,
};
use internet_identity_interface::internet_identity::types::email_challenge::{
    EmailChallengeDiagnostics, EmailChallengeDnsInput, EmailChallengeError, EmailChallengeStatus,
    EmailChallengeSubmitDkimLeafArg,
};
use internet_identity_interface::internet_identity::types::smtp::{
    SmtpAddress, SmtpEnvelope, SmtpHeader, SmtpMessage, SmtpRequest, SmtpResponse,
    MAX_HEADER_VALUE_BYTES,
};
use internet_identity_interface::internet_identity::types::{
    DnssecConfig, DohConfig, InternetIdentityInit,
};
use pocket_ic::{query_candid, PocketIc, Time};
use serde::{Deserialize, Serialize};
use serde_bytes::ByteBuf;
use serde_json::Value;
use std::path::{Path, PathBuf};

/// The one DoH-allowlisted domain; the DKIM fixtures are signed for it.
const DOH_DOMAIN: &str = "test.example.com";
/// The `s=` of the DKIM fixtures.
const DKIM_FIXTURES_SELECTOR: &str = "test1";
/// Makes `recover@id.ai` a recognised mailbox.
const MAILBOX_ORIGIN: &str = "https://id.ai";
/// 600 s after the `t=` of the synthetic DKIM fixtures (2026-05-05).
const DKIM_FIXTURES_NOW_SECS: u64 = 1_777_972_889;
/// The DMARC policy the DNSSEC signer publishes for `DOH_DOMAIN`.
const DMARC_TXT: &[u8] = b"v=DMARC1; p=none;";
const UPDATE_ENV: &str = "UPDATE_GOLDEN";
const REGENERATE_HINT: &str =
    "UPDATE_GOLDEN=1 cargo test -p internet_identity --test integration golden";

fn repo_path(relative: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join(relative)
}

fn golden_dir() -> PathBuf {
    repo_path("tests/integration/golden")
}

fn pinned_env(now_secs: u64) -> PocketIc {
    let env = env();
    env.set_time(Time::from_nanos_since_unix_epoch(now_secs * 1_000_000_000));
    env
}

/// DoH route when `anchors` is `None`; DNSSEC route (flag on, anchors
/// configured) otherwise. Mirrors `email_recovery::setup_canister*`.
fn install(env: &PocketIc, anchors: Option<Vec<DnssecRootAnchor>>) -> Principal {
    let args = InternetIdentityInit {
        doh_config: Some(Some(DohConfig {
            allowed_domains: vec![DOH_DOMAIN.into()],
            max_cache_age_secs: Some(3600),
        })),
        related_origins: Some(vec![MAILBOX_ORIGIN.into()]),
        canister_creation_cycles_cost: Some(0),
        enable_dnssec_email_recovery: Some(anchors.is_some()),
        dnssec_config: anchors.map(|root_anchors| Some(DnssecConfig { root_anchors })),
        ..Default::default()
    };
    install_ii_canister_with_arg_and_cycles(env, II_WASM.clone(), Some(args), 10_000_000_000_000)
}

// ---------------------------------------------------------------------------
// Naming responses: variant names and public codes only.
// ---------------------------------------------------------------------------

/// The variant name of a `Serialize`-derived enum value: serde's external
/// tagging renders a unit variant as its name and any other variant as a
/// single-key object.
fn variant_name<T: Serialize>(value: &T) -> String {
    match serde_json::to_value(value).expect("enum serialises") {
        Value::String(name) => name,
        Value::Object(map) => map.keys().next().cloned().expect("tagged variant"),
        other => panic!("not an externally tagged enum: {other}"),
    }
}

/// `EmailVerificationFailed` carries the pipeline's public failure text, the
/// only payload that tells the failing stages apart; every other payload is
/// dropped.
fn error_name(error: &EmailChallengeError) -> String {
    match error {
        EmailChallengeError::EmailVerificationFailed(detail) => {
            format!("EmailVerificationFailed: {detail}")
        }
        other => variant_name(other),
    }
}

fn status_name(status: &EmailChallengeStatus) -> String {
    match status {
        EmailChallengeStatus::Failed(error) => format!("Failed({})", error_name(error)),
        other => variant_name(other),
    }
}

fn result_name<T>(result: &Result<T, EmailChallengeError>) -> String {
    match result {
        Ok(_) => "Ok".into(),
        Err(error) => format!("Err({})", error_name(error)),
    }
}

fn smtp_response_name(response: &SmtpResponse) -> String {
    match response {
        SmtpResponse::Ok {} => "Ok".into(),
        SmtpResponse::Err(error) => format!("Err({})", error.code),
    }
}

#[derive(Serialize)]
struct Diagnostics {
    message_id: Option<String>,
    reason_code: String,
    verification_path: String,
}

impl Diagnostics {
    fn of(diagnostics: EmailChallengeDiagnostics) -> Self {
        Self {
            message_id: diagnostics.message_id,
            reason_code: diagnostics.reason_code,
            verification_path: variant_name(&diagnostics.verification_path),
        }
    }
}

/// The polled `(status, diagnostics)` pair of a challenge.
#[derive(Serialize)]
struct Polled {
    status: String,
    diagnostics: Option<Diagnostics>,
}

fn poll(env: &PocketIc, canister_id: Principal, nonce: &str) -> Polled {
    let status = api::email_challenge_status(env, canister_id, nonce).expect("status query");
    let diagnostics =
        api::email_challenge_diagnostics(env, canister_id, nonce).expect("diagnostics query");
    Polled {
        status: status_name(&status),
        diagnostics: diagnostics.map(Diagnostics::of),
    }
}

/// One `smtp_request` delivery: the gateway response and the status the FE
/// polls right after it.
#[derive(Serialize)]
struct Delivery {
    response: String,
    polled: Polled,
}

fn deliver(env: &PocketIc, canister_id: Principal, request: &SmtpRequest, nonce: &str) -> Delivery {
    let response = api::smtp_request(env, canister_id, request).expect("smtp_request call");
    Delivery {
        response: smtp_response_name(&response),
        polled: poll(env, canister_id, nonce),
    }
}

fn smtp_request_validate(env: &PocketIc, canister_id: Principal, request: &SmtpRequest) -> String {
    let (response,): (SmtpResponse,) = query_candid(
        env,
        canister_id,
        "smtp_request_validate",
        (request.clone(),),
    )
    .expect("smtp_request_validate query");
    smtp_response_name(&response)
}

// ---------------------------------------------------------------------------
// Expectation files.
// ---------------------------------------------------------------------------

fn render<T: Serialize>(record: &T) -> String {
    let mut rendered = serde_json::to_string_pretty(record).expect("record serialises");
    rendered.push('\n');
    rendered
}

/// Compare `record` with the checked-in expectation `name`, or rewrite it
/// when `UPDATE_GOLDEN` is set.
fn check_golden<T: Serialize>(name: &str, record: &T) {
    let path = golden_dir().join(name);
    let rendered = render(record);
    if std::env::var_os(UPDATE_ENV).is_some() {
        std::fs::write(&path, &rendered)
            .unwrap_or_else(|e| panic!("failed to write {}: {e}", path.display()));
        eprintln!("Updated {}", path.display());
        return;
    }
    let expected = std::fs::read_to_string(&path).unwrap_or_else(|e| {
        panic!(
            "failed to read {}: {e}\nIf this file is missing, regenerate with:\n  {REGENERATE_HINT}",
            path.display()
        )
    });
    pretty_assertions::assert_eq!(
        expected,
        rendered,
        "golden record {} changed. If this is intentional, regenerate with:\n  {REGENERATE_HINT}\nand review the diff with the PR that names the change.",
        path.display()
    );
}

fn file_names(dir: &Path, prefix: &str, extension: &str) -> Vec<String> {
    let mut names: Vec<String> = std::fs::read_dir(dir)
        .unwrap_or_else(|e| panic!("failed to read {}: {e}", dir.display()))
        .map(|entry| entry.expect("directory entry").file_name())
        .map(|name| name.to_string_lossy().into_owned())
        .filter(|name| name.starts_with(prefix) && name.ends_with(extension))
        .collect();
    names.sort();
    names
}

fn fixture_names(dir: &Path, extension: &str) -> Vec<String> {
    let names = file_names(dir, "", extension);
    assert!(
        !names.is_empty(),
        "no *{extension} fixtures in {}",
        dir.display()
    );
    names
}

/// The golden files with `prefix` are exactly the `expected` ones, so a
/// renamed or removed fixture cannot leave its record behind.
fn assert_no_orphan_golden(prefix: &str, mut expected: Vec<String>) {
    expected.sort();
    pretty_assertions::assert_eq!(
        file_names(&golden_dir(), prefix, ".json"),
        expected,
        "golden files {}/{prefix}*.json do not match the fixtures; delete the orphans",
        golden_dir().display()
    );
}

fn stem(file_name: &str) -> &str {
    file_name
        .rsplit_once('.')
        .map(|(stem, _)| stem)
        .unwrap_or(file_name)
}

// ---------------------------------------------------------------------------
// DKIM `.eml` fixtures.
// ---------------------------------------------------------------------------

/// Split an RFC 5322 message into headers and body the way the gateway
/// delivers it (and `dkim::test_vectors::parse_eml` reproduces it): folded
/// header values keep their CRLF + WSP boundaries for relaxed
/// canonicalisation, and the single SP after the colon is dropped.
fn parse_eml(raw: &[u8]) -> (Vec<SmtpHeader>, Vec<u8>) {
    let (head, body) = match raw.windows(4).position(|w| w == b"\r\n\r\n") {
        Some(i) => (&raw[..i], raw[i + 4..].to_vec()),
        None => (raw, Vec::new()),
    };
    let mut headers: Vec<SmtpHeader> = Vec::new();
    for line in String::from_utf8_lossy(head).split("\r\n") {
        if line.starts_with([' ', '\t']) {
            if let Some(previous) = headers.last_mut() {
                previous.value.push_str("\r\n");
                previous.value.push_str(line);
            }
        } else if let Some((name, value)) = line.split_once(':') {
            headers.push(SmtpHeader {
                name: name.trim().to_string(),
                value: value.strip_prefix(' ').unwrap_or(value).to_string(),
            });
        }
    }
    (headers, body)
}

fn mailbox(user: &str, domain: &str) -> SmtpAddress {
    SmtpAddress {
        user: user.into(),
        domain: domain.into(),
    }
}

/// The gateway-shaped request for a fixture: envelope sender from `From:`,
/// recipient `recover@id.ai` (the fixtures' `To:`), the file name as the
/// gateway correlation id. `subject` replaces the `Subject:` value.
fn smtp_request_from_eml(file_name: &str, raw: &[u8], subject: Option<&str>) -> SmtpRequest {
    let (mut headers, body) = parse_eml(raw);
    if let Some(subject) = subject {
        for header in headers
            .iter_mut()
            .filter(|h| h.name.eq_ignore_ascii_case("Subject"))
        {
            header.value = subject.to_string();
        }
    }
    let from = headers
        .iter()
        .find(|h| h.name.eq_ignore_ascii_case("From"))
        .and_then(|h| h.value.trim().split_once('@'))
        .map(|(user, domain)| mailbox(user, domain))
        .expect("fixture carries a bare From: mailbox");
    SmtpRequest {
        envelope: Some(SmtpEnvelope {
            from,
            to: vec![mailbox("recover", "id.ai")],
        }),
        message: Some(SmtpMessage {
            headers,
            body: ByteBuf::from(body),
        }),
        gateway_flags: None,
        message_id: Some(file_name.to_string()),
    }
}

/// The sender address of a fixture request.
fn from_address(request: &SmtpRequest) -> String {
    let from = &request.envelope.as_ref().expect("envelope").from;
    format!("{}@{}", from.user, from.domain)
}

/// The responses of both SMTP entry points to one request shape.
#[derive(Serialize)]
struct SmtpVerdict {
    smtp_request_validate: String,
    smtp_request: String,
}

/// Requests derived from the captured message that the SMTP entry points
/// must answer before reading the message: the envelope rule (551 for not
/// exactly one recipient, 550 for an unrecognised mailbox), the shape bound
/// (555) and the other recognised mailbox. Sent before any challenge exists,
/// so a verdict is the response code alone.
#[derive(Serialize)]
struct SmtpVariants {
    missing_envelope: SmtpVerdict,
    two_recipients: SmtpVerdict,
    unknown_mailbox: SmtpVerdict,
    foreign_domain: SmtpVerdict,
    setup_mailbox: SmtpVerdict,
    oversize_header_value: SmtpVerdict,
}

fn smtp_variants(
    env: &PocketIc,
    canister_id: Principal,
    as_captured: &SmtpRequest,
) -> SmtpVariants {
    let with_recipients = |to: Vec<SmtpAddress>| {
        let mut request = as_captured.clone();
        request.envelope.as_mut().expect("envelope").to = to;
        request
    };
    let verdict = |request: SmtpRequest| SmtpVerdict {
        smtp_request_validate: smtp_request_validate(env, canister_id, &request),
        smtp_request: smtp_response_name(
            &api::smtp_request(env, canister_id, &request).expect("smtp_request call"),
        ),
    };

    let mut missing_envelope = as_captured.clone();
    missing_envelope.envelope = None;
    let mut oversize_header_value = as_captured.clone();
    oversize_header_value
        .message
        .as_mut()
        .expect("message")
        .headers
        .push(SmtpHeader {
            name: "X-Golden".into(),
            value: "x".repeat(MAX_HEADER_VALUE_BYTES + 1),
        });

    SmtpVariants {
        missing_envelope: verdict(missing_envelope),
        two_recipients: verdict(with_recipients(vec![
            mailbox("recover", "id.ai"),
            mailbox("register", "id.ai"),
        ])),
        unknown_mailbox: verdict(with_recipients(vec![mailbox("nobody", "id.ai")])),
        foreign_domain: verdict(with_recipients(vec![mailbox("recover", "other.example")])),
        setup_mailbox: verdict(with_recipients(vec![mailbox("register", "id.ai")])),
        oversize_header_value: verdict(oversize_header_value),
    }
}

/// A fixture message on the DoH route.
#[derive(Serialize)]
struct DohRecord {
    fixture: String,
    route: &'static str,
    now_unix: u64,
    smtp_request_validate: String,
    smtp_request_variants: SmtpVariants,
    prepare_delegation: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    smtp_request_as_captured: Option<Delivery>,
    #[serde(skip_serializing_if = "Option::is_none")]
    smtp_request_nonce_subject: Option<Delivery>,
    #[serde(skip_serializing_if = "Option::is_none")]
    polled_after_doh_resolution: Option<Polled>,
}

fn record_doh_fixture(file_name: &str, raw: &[u8], dkim_txt: &[u8]) -> DohRecord {
    let env = pinned_env(DKIM_FIXTURES_NOW_SECS);
    let canister_id = install(&env, None);

    let as_captured = smtp_request_from_eml(file_name, raw, None);
    let mut record = DohRecord {
        fixture: file_name.to_string(),
        route: "Doh",
        now_unix: DKIM_FIXTURES_NOW_SECS,
        smtp_request_validate: smtp_request_validate(&env, canister_id, &as_captured),
        smtp_request_variants: smtp_variants(&env, canister_id, &as_captured),
        prepare_delegation: String::new(),
        smtp_request_as_captured: None,
        smtp_request_nonce_subject: None,
        polled_after_doh_resolution: None,
    };

    let prepared = api::email_recovery_prepare_delegation(
        &env,
        canister_id,
        EmailChallengeDnsInput {
            address: from_address(&as_captured),
            dns_proof: None,
        },
        ByteBuf::from(vec![0x42; 32]),
    )
    .expect("prepare_delegation call");
    record.prepare_delegation = result_name(&prepared);
    let Ok(challenge) = prepared else {
        return record;
    };
    let nonce = challenge.nonce;

    record.smtp_request_as_captured = Some(deliver(&env, canister_id, &as_captured, &nonce));
    let with_nonce = smtp_request_from_eml(file_name, raw, Some(&nonce));
    record.smtp_request_nonce_subject = Some(deliver(&env, canister_id, &with_nonce, &nonce));
    drive_doh_resolution(&env, canister_id, &nonce, dkim_txt);
    record.polled_after_doh_resolution = Some(poll(&env, canister_id, &nonce));
    record
}

/// A fixture message on the DNSSEC route, with the in-test signer
/// publishing the fixture's DKIM key under a chain the canister trusts.
#[derive(Serialize)]
struct LeafRecord {
    fixture: String,
    route: &'static str,
    now_unix: u64,
    prepare_delegation: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    smtp_request_nonce_subject: Option<Delivery>,
    #[serde(skip_serializing_if = "Option::is_none")]
    submit_dkim_leaf: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    polled_after_submit: Option<Polled>,
}

fn record_leaf_fixture(file_name: &str, raw: &[u8], dkim_txt: &[u8]) -> LeafRecord {
    let env = pinned_env(DKIM_FIXTURES_NOW_SECS);
    let chain = dnssec_signer::build_chain(
        DOH_DOMAIN,
        DKIM_FIXTURES_SELECTOR,
        dkim_txt,
        Some(DMARC_TXT),
        u32::try_from(DKIM_FIXTURES_NOW_SECS).expect("pinned clock fits an RRSIG time"),
    );
    let canister_id = install(&env, Some(vec![chain.anchor]));

    let as_captured = smtp_request_from_eml(file_name, raw, None);
    let prepared = api::email_recovery_prepare_delegation(
        &env,
        canister_id,
        EmailChallengeDnsInput {
            address: from_address(&as_captured),
            dns_proof: Some(DnsProofBundle {
                hops: chain.dmarc_leaf.into_iter().collect(),
                ..chain.skeleton
            }),
        },
        ByteBuf::from(vec![0x42; 32]),
    )
    .expect("prepare_delegation call");
    let mut record = LeafRecord {
        fixture: file_name.to_string(),
        route: "Dnssec",
        now_unix: DKIM_FIXTURES_NOW_SECS,
        prepare_delegation: result_name(&prepared),
        smtp_request_nonce_subject: None,
        submit_dkim_leaf: None,
        polled_after_submit: None,
    };
    let Ok(challenge) = prepared else {
        return record;
    };
    let nonce = challenge.nonce;

    let with_nonce = smtp_request_from_eml(file_name, raw, Some(&nonce));
    record.smtp_request_nonce_subject = Some(deliver(&env, canister_id, &with_nonce, &nonce));
    let submitted = api::email_challenge_submit_dkim_leaf(
        &env,
        canister_id,
        EmailChallengeSubmitDkimLeafArg {
            nonce: nonce.clone(),
            hops: vec![chain.dkim_leaf],
            extra_chains: vec![],
        },
    )
    .expect("submit_dkim_leaf call");
    record.submit_dkim_leaf = Some(result_name(&submitted));
    record.polled_after_submit = Some(poll(&env, canister_id, &nonce));
    record
}

/// The `.eml` fixtures with the fixture TXT record (the `p=` of the key
/// that signed them).
fn dkim_fixtures() -> (PathBuf, Vec<String>, Vec<u8>) {
    let dir = repo_path("../../test_vectors/dkim");
    let dkim_txt =
        std::fs::read_to_string(dir.join("synth-rsa-test1._domainkey.test.example.com.txt"))
            .expect("DKIM TXT fixture");
    let names = fixture_names(&dir, ".eml");
    (dir, names, dkim_txt.trim().as_bytes().to_vec())
}

#[test]
fn golden_dkim_eml_fixtures_doh_route() {
    let (dir, names, dkim_txt) = dkim_fixtures();
    for file_name in &names {
        let raw = std::fs::read(dir.join(file_name)).expect("eml fixture");
        let record = record_doh_fixture(file_name, &raw, &dkim_txt);
        check_golden(&format!("dkim-{}.json", stem(file_name)), &record);
    }
    assert_no_orphan_golden(
        "dkim-",
        names
            .iter()
            .map(|n| format!("dkim-{}.json", stem(n)))
            .collect(),
    );
}

#[test]
fn golden_dkim_eml_fixtures_dnssec_route() {
    let (dir, names, dkim_txt) = dkim_fixtures();
    for file_name in &names {
        let raw = std::fs::read(dir.join(file_name)).expect("eml fixture");
        let record = record_leaf_fixture(file_name, &raw, &dkim_txt);
        check_golden(&format!("leaf-{}.json", stem(file_name)), &record);
    }
    assert_no_orphan_golden(
        "leaf-",
        names
            .iter()
            .map(|n| format!("leaf-{}.json", stem(n)))
            .collect(),
    );
}

// ---------------------------------------------------------------------------
// DNSSEC bundle fixtures on the DNSSEC route.
// ---------------------------------------------------------------------------

#[derive(Deserialize)]
struct WireMeta {
    captured_at_unix: u64,
    deepest_zone: String,
    leaf_name: String,
}

#[derive(Deserialize)]
struct WireRrsig {
    type_covered: u16,
    algorithm: u8,
    labels: u8,
    original_ttl: u32,
    expiration: u32,
    inception: u32,
    key_tag: u16,
    signer_name: String,
    signature_hex: String,
}

#[derive(Deserialize)]
struct WireRRset {
    name_hex: String,
    rtype: u16,
    rdata_hex: Vec<String>,
    ttl: u32,
    rrsig: WireRrsig,
}

#[derive(Deserialize)]
struct WireLink {
    child_ds: WireRRset,
    child_dnskey: WireRRset,
}

#[derive(Deserialize)]
struct WireChain {
    links: Vec<WireLink>,
}

#[derive(Deserialize)]
struct WireBundle {
    #[serde(rename = "_meta")]
    meta: WireMeta,
    root_dnskey: WireRRset,
    chains: Vec<WireChain>,
    hops: Vec<WireRRset>,
}

#[derive(Deserialize)]
struct WireAnchor {
    key_tag: u16,
    algorithm: u8,
    digest_type: u8,
    digest_hex: String,
}

#[derive(Deserialize)]
struct WireAnchors {
    anchors: Vec<WireAnchor>,
}

fn bytes(hex_text: &str) -> ByteBuf {
    ByteBuf::from(hex::decode(hex_text).expect("fixture hex"))
}

fn rrset(wire: WireRRset) -> SignedRRset {
    SignedRRset {
        name: bytes(&wire.name_hex),
        rtype: wire.rtype,
        rdata: wire.rdata_hex.iter().map(|h| bytes(h)).collect(),
        ttl: wire.ttl,
        rrsig: Rrsig {
            type_covered: wire.rrsig.type_covered,
            algorithm: wire.rrsig.algorithm,
            labels: wire.rrsig.labels,
            original_ttl: wire.rrsig.original_ttl,
            expiration: wire.rrsig.expiration,
            inception: wire.rrsig.inception,
            key_tag: wire.rrsig.key_tag,
            signer_name: bytes(&wire.rrsig.signer_name),
            signature: bytes(&wire.rrsig.signature_hex),
        },
    }
}

fn bundle(wire: &mut WireBundle) -> DnsProofBundle {
    DnsProofBundle {
        root_dnskey: rrset(std::mem::replace(&mut wire.root_dnskey, empty_rrset())),
        chains: std::mem::take(&mut wire.chains)
            .into_iter()
            .map(|chain| DelegationChain {
                links: chain
                    .links
                    .into_iter()
                    .map(|link| DelegationLink {
                        child_ds: rrset(link.child_ds),
                        child_dnskey: rrset(link.child_dnskey),
                    })
                    .collect(),
            })
            .collect(),
        hops: std::mem::take(&mut wire.hops)
            .into_iter()
            .map(rrset)
            .collect(),
    }
}

fn empty_rrset() -> WireRRset {
    WireRRset {
        name_hex: String::new(),
        rtype: 0,
        rdata_hex: vec![],
        ttl: 0,
        rrsig: WireRrsig {
            type_covered: 0,
            algorithm: 0,
            labels: 0,
            original_ttl: 0,
            expiration: 0,
            inception: 0,
            key_tag: 0,
            signer_name: String::new(),
            signature_hex: String::new(),
        },
    }
}

fn anchors(wire: WireAnchors) -> Vec<DnssecRootAnchor> {
    wire.anchors
        .into_iter()
        .map(|a| DnssecRootAnchor {
            key_tag: a.key_tag,
            algorithm: a.algorithm,
            digest_type: a.digest_type,
            digest: bytes(&a.digest_hex),
        })
        .collect()
}

/// A captured bundle through `prepare_add`, as captured and as its skeleton.
#[derive(Serialize)]
struct DnssecRecord {
    fixture: String,
    route: &'static str,
    now_unix: u64,
    domain: String,
    leaf_name: String,
    prepare_add_as_captured: String,
    prepare_add_skeleton: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    polled: Option<Polled>,
}

fn record_dnssec_fixture(
    file_name: &str,
    mut wire: WireBundle,
    root_anchors: &[DnssecRootAnchor],
) -> DnssecRecord {
    let now_secs = wire.meta.captured_at_unix;
    let env = pinned_env(now_secs);
    let canister_id = install(&env, Some(root_anchors.to_vec()));
    let (identity_number, principal) = fresh_identity(&env, canister_id);

    let as_captured = bundle(&mut wire);
    let skeleton = DnsProofBundle {
        hops: vec![],
        ..as_captured.clone()
    };
    let prepare = |proof: DnsProofBundle| {
        api::email_recovery_credential_prepare_add(
            &env,
            canister_id,
            principal,
            identity_number,
            EmailChallengeDnsInput {
                address: format!("golden@{}", wire.meta.deepest_zone),
                dns_proof: Some(proof),
            },
        )
        .expect("prepare_add call")
    };
    let prepared_as_captured = prepare(as_captured);
    let prepared_skeleton = prepare(skeleton);

    let mut record = DnssecRecord {
        fixture: file_name.to_string(),
        route: "Dnssec",
        now_unix: now_secs,
        domain: wire.meta.deepest_zone.clone(),
        leaf_name: wire.meta.leaf_name.clone(),
        prepare_add_as_captured: result_name(&prepared_as_captured),
        prepare_add_skeleton: result_name(&prepared_skeleton),
        polled: None,
    };
    if let Some(challenge) = prepared_skeleton.ok().or(prepared_as_captured.ok()) {
        record.polled = Some(poll(&env, canister_id, &challenge.nonce));
    }
    record
}

#[test]
fn golden_dnssec_bundle_fixtures() {
    let dir = repo_path("../../test_vectors/dnssec");
    let mut root_anchors: Option<Vec<DnssecRootAnchor>> = None;
    let mut bundles: Vec<(String, WireBundle)> = Vec::new();
    for file_name in fixture_names(&dir, ".json") {
        let text = std::fs::read_to_string(dir.join(&file_name)).expect("json fixture");
        if let Ok(wire) = serde_json::from_str::<WireAnchors>(&text) {
            assert!(root_anchors.is_none(), "one trust-anchor fixture expected");
            root_anchors = Some(anchors(wire));
        } else {
            let wire: WireBundle = serde_json::from_str(&text)
                .unwrap_or_else(|e| panic!("{file_name}: not a bundle fixture: {e}"));
            bundles.push((file_name, wire));
        }
    }
    let root_anchors = root_anchors.expect("trust-anchor fixture");
    assert!(!bundles.is_empty(), "no bundle fixtures");
    let expected = bundles
        .iter()
        .map(|(name, _)| format!("dnssec-{}.json", stem(name)))
        .collect();
    for (file_name, wire) in bundles {
        let record = record_dnssec_fixture(&file_name, wire, &root_anchors);
        check_golden(&format!("dnssec-{}.json", stem(&file_name)), &record);
    }
    assert_no_orphan_golden("dnssec-", expected);
}
