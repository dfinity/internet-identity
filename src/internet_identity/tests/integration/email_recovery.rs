//! Integration tests for the email-recovery setup flow.
//!
//! End-to-end coverage of the prepare → smtp_request → status flow,
//! plus the negative paths (allowlist gate, authz, unknown nonce,
//! TTL expiry). The DKIM-signed email is generated at test time
//! against a fresh RSA keypair, and the canister's DoH outcalls are
//! mocked to return the matching public-key TXT.
//!
//! See `docs/ongoing/email-recovery.md` and the feature module at
//! `crate::email_recovery` for the design.

use crate::v2_api::authn_method_test_helpers::{
    create_identity_with_authn_method, test_authn_method,
};
use canister_tests::{api::internet_identity as api, framework::*};
use internet_identity_interface::internet_identity::types::email_recovery::{
    EmailRecoveryDnsInput, EmailRecoveryError, EmailRecoveryStatus,
};
use internet_identity_interface::internet_identity::types::smtp::{
    SmtpAddress, SmtpEnvelope, SmtpHeader, SmtpMessage, SmtpRequest, SmtpResponse,
};
use internet_identity_interface::internet_identity::types::{DohConfig, InternetIdentityInit};
use pocket_ic::common::rest::{CanisterHttpReply, CanisterHttpResponse, MockCanisterHttpResponse};
use pocket_ic::PocketIc;
use serde_bytes::ByteBuf;
use std::time::Duration;

// ===================================================================
// Fixtures
// ===================================================================

const TEST_DOMAIN: &str = "test.example.com";
const TEST_ADDRESS: &str = "alice@test.example.com";
const TEST_SELECTOR: &str = "test1";
const TEST_BODY: &[u8] = b"Hello world.\r\nThis is a recovery email.\r\n";

fn dns_input() -> EmailRecoveryDnsInput {
    EmailRecoveryDnsInput {
        address: TEST_ADDRESS.into(),
        selector: TEST_SELECTOR.into(),
        dns_proof: None,
    }
}

// ===================================================================
// Setup
// ===================================================================

/// Stand up a canister with an `allowed_domains` list that lets
/// `test.example.com` through to the DoH path. We don't need any
/// other init knobs for these tests.
fn setup_canister(env: &PocketIc) -> candid::Principal {
    let args = InternetIdentityInit {
        doh_config: Some(Some(DohConfig {
            allowed_domains: vec![TEST_DOMAIN.into()],
            max_cache_age_secs: Some(3600),
        })),
        canister_creation_cycles_cost: Some(0),
        ..Default::default()
    };
    install_ii_canister_with_arg_and_cycles(env, II_WASM.clone(), Some(args), 10_000_000_000_000)
}

/// Create an identity and return `(identity_number, principal)`. The
/// principal is needed for any caller-authenticated method calls
/// (`prepare_add`, `credential_remove`).
fn fresh_identity(env: &PocketIc, canister_id: candid::Principal) -> (u64, candid::Principal) {
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(env, canister_id, &authn_method);
    (identity_number, authn_method.principal())
}

// ===================================================================
// Smaller, no-DKIM-needed tests
// ===================================================================

#[test]
fn prepare_add_returns_well_formed_challenge() {
    let env = env();
    let canister_id = setup_canister(&env);
    let (id, p) = fresh_identity(&env, canister_id);

    let challenge =
        api::email_recovery_credential_prepare_add(&env, canister_id, p, id, dns_input())
            .expect("call failed")
            .expect("prepare_add failed");

    // Nonce shape: matches the canister's `II-Recovery-` prefix and
    // the documented suffix length (8 hex bytes → 16 chars).
    assert!(
        challenge.nonce.starts_with("II-Recovery-"),
        "unexpected nonce prefix: {}",
        challenge.nonce
    );
    assert_eq!(
        challenge.nonce.len(),
        "II-Recovery-".len() + 16,
        "unexpected nonce length: {}",
        challenge.nonce
    );
    assert_eq!(challenge.mailbox, "register@id.ai");
    // expires_at is 30 minutes from now in nanoseconds (matches the
    // rest of II's `Timestamp` encoding). Can't pin it precisely
    // because PocketIC's clock advances during the call; check it's
    // within ±5s of now+30min.
    let now_ns = time(&env);
    let half_hour_ns: u64 = 30 * 60 * 1_000_000_000;
    let slack_ns: u64 = 5 * 1_000_000_000;
    assert!(
        challenge.expires_at >= now_ns + half_hour_ns - slack_ns
            && challenge.expires_at <= now_ns + half_hour_ns + slack_ns,
        "expires_at = {} not within ±5s of now+30min ({})",
        challenge.expires_at,
        now_ns + half_hour_ns,
    );
}

#[test]
fn prepare_add_rejects_non_allowlisted_domain() {
    let env = env();
    let canister_id = setup_canister(&env);
    let (id, p) = fresh_identity(&env, canister_id);

    let bad_input = EmailRecoveryDnsInput {
        address: "bob@evil.com".into(),
        selector: "default".into(),
        dns_proof: None,
    };
    let err = api::email_recovery_credential_prepare_add(&env, canister_id, p, id, bad_input)
        .expect("call failed")
        .expect_err("expected failure for non-allowlisted domain");
    match err {
        EmailRecoveryError::DomainNotAllowlisted(d) => assert_eq!(d, "evil.com"),
        other => panic!("expected DomainNotAllowlisted, got {other:?}"),
    }
}

#[test]
fn prepare_add_rejects_unauthorized_caller() {
    let env = env();
    let canister_id = setup_canister(&env);
    let (id, _p) = fresh_identity(&env, canister_id);

    // A different principal should NOT be able to prepare for the
    // anchor — only the anchor's owner can bind a recovery email.
    let stranger = principal_1();
    let err =
        api::email_recovery_credential_prepare_add(&env, canister_id, stranger, id, dns_input())
            .expect("call failed")
            .expect_err("expected Unauthorized");
    match err {
        EmailRecoveryError::Unauthorized(_) => {}
        other => panic!("expected Unauthorized, got {other:?}"),
    }
}

#[test]
fn status_returns_expired_for_unknown_nonce() {
    let env = env();
    let canister_id = setup_canister(&env);
    // Even before any prepare call, an arbitrary nonce should
    // resolve to Expired. This is the FE's signal to show "timed
    // out, please try again" — the canister deliberately doesn't
    // distinguish "never issued" from "evicted".
    let status = api::email_recovery_status(&env, canister_id, "II-Recovery-deadbeefcafe1234")
        .expect("call failed");
    assert!(matches!(status, EmailRecoveryStatus::Expired));
}

#[test]
fn status_returns_pending_after_prepare_add() {
    let env = env();
    let canister_id = setup_canister(&env);
    let (id, p) = fresh_identity(&env, canister_id);

    let challenge =
        api::email_recovery_credential_prepare_add(&env, canister_id, p, id, dns_input())
            .expect("call failed")
            .expect("prepare_add failed");

    let status =
        api::email_recovery_status(&env, canister_id, &challenge.nonce).expect("call failed");
    assert!(matches!(status, EmailRecoveryStatus::Pending));
}

#[test]
fn status_flips_to_expired_after_ttl() {
    let env = env();
    let canister_id = setup_canister(&env);
    let (id, p) = fresh_identity(&env, canister_id);

    let challenge =
        api::email_recovery_credential_prepare_add(&env, canister_id, p, id, dns_input())
            .expect("call failed")
            .expect("prepare_add failed");

    // Advance past the 30-minute TTL.
    env.advance_time(Duration::from_secs(31 * 60));

    let status =
        api::email_recovery_status(&env, canister_id, &challenge.nonce).expect("call failed");
    assert!(matches!(status, EmailRecoveryStatus::Expired));
}

#[test]
fn smtp_request_silently_drops_email_with_no_nonce_in_subject() {
    let env = env();
    let canister_id = setup_canister(&env);

    // Construct an SmtpRequest whose Subject doesn't carry the
    // `II-Recovery-` prefix. The canister should respond with `Ok`
    // (no info leak) and not change any state.
    let request = SmtpRequest {
        envelope: Some(SmtpEnvelope {
            from: SmtpAddress {
                user: "alice".into(),
                domain: TEST_DOMAIN.into(),
            },
            to: SmtpAddress {
                user: "register".into(),
                domain: "id.ai".into(),
            },
        }),
        message: Some(SmtpMessage {
            headers: vec![
                SmtpHeader {
                    name: "From".into(),
                    value: TEST_ADDRESS.into(),
                },
                SmtpHeader {
                    name: "To".into(),
                    value: "register@id.ai".into(),
                },
                SmtpHeader {
                    name: "Subject".into(),
                    value: "Just a friendly email".into(),
                },
            ],
            body: ByteBuf::from(b"hello".to_vec()),
        }),
        gateway_flags: None,
    };
    let resp = api::smtp_request(&env, canister_id, &request).expect("call failed");
    assert!(matches!(resp, SmtpResponse::Ok {}));
}

#[test]
fn smtp_request_silently_drops_email_with_unknown_nonce() {
    let env = env();
    let canister_id = setup_canister(&env);

    let request = SmtpRequest {
        envelope: Some(SmtpEnvelope {
            from: SmtpAddress {
                user: "alice".into(),
                domain: TEST_DOMAIN.into(),
            },
            to: SmtpAddress {
                user: "register".into(),
                domain: "id.ai".into(),
            },
        }),
        message: Some(SmtpMessage {
            headers: vec![
                SmtpHeader {
                    name: "From".into(),
                    value: TEST_ADDRESS.into(),
                },
                SmtpHeader {
                    name: "To".into(),
                    value: "register@id.ai".into(),
                },
                SmtpHeader {
                    name: "Subject".into(),
                    // Looks valid, but no prepare call was made for
                    // this nonce so the canister has no pending entry.
                    value: "II-Recovery-deadbeefcafebabe".into(),
                },
            ],
            body: ByteBuf::from(b"hello".to_vec()),
        }),
        gateway_flags: None,
    };
    let resp = api::smtp_request(&env, canister_id, &request).expect("call failed");
    assert!(matches!(resp, SmtpResponse::Ok {}));
}

#[test]
fn smtp_request_drops_emails_addressed_to_other_recipients() {
    let env = env();
    let canister_id = setup_canister(&env);

    // Recovery flow recipient is reserved (handled in a follow-up
    // PR). Today the canister silently drops any non-`register`
    // recipient — the gateway shouldn't be able to probe the
    // canister for which mailboxes exist.
    let request = SmtpRequest {
        envelope: Some(SmtpEnvelope {
            from: SmtpAddress {
                user: "alice".into(),
                domain: TEST_DOMAIN.into(),
            },
            to: SmtpAddress {
                user: "recover".into(),
                domain: "id.ai".into(),
            },
        }),
        message: Some(SmtpMessage {
            headers: vec![
                SmtpHeader {
                    name: "From".into(),
                    value: TEST_ADDRESS.into(),
                },
                SmtpHeader {
                    name: "To".into(),
                    value: "recover@id.ai".into(),
                },
                SmtpHeader {
                    name: "Subject".into(),
                    value: "II-Recovery-deadbeefcafebabe".into(),
                },
            ],
            body: ByteBuf::from(b"hello".to_vec()),
        }),
        gateway_flags: None,
    };
    let resp = api::smtp_request(&env, canister_id, &request).expect("call failed");
    assert!(matches!(resp, SmtpResponse::Ok {}));
}

#[test]
fn remove_credential_rejects_when_nothing_bound() {
    let env = env();
    let canister_id = setup_canister(&env);
    let (id, p) = fresh_identity(&env, canister_id);

    let err = api::email_recovery_credential_remove(&env, canister_id, p, id, TEST_ADDRESS)
        .expect("call failed")
        .expect_err("expected AddressNotRegistered");
    assert!(matches!(err, EmailRecoveryError::AddressNotRegistered));
}

// ===================================================================
// DNSSEC path tests
// ===================================================================
//
// `prepare_add` accepts an optional `dns_proof: Option<DnsProofBundle>`.
// When supplied, the canister validates the DNSSEC chain synchronously
// and caches the verified DKIM TXT bytes on the pending challenge,
// skipping the DoH outcall fan-out at `smtp_request` time. These tests
// exercise the plumbing through to the verifier; a happy-path DNSSEC
// e2e test (with a `_domainkey`-targeting test vector signed at test
// time) is a follow-up — generating a real DNSSEC chain in Rust is a
// multi-hundred-line signer.

#[test]
fn dnssec_path_rejects_when_no_trust_anchors_configured() {
    use internet_identity_interface::internet_identity::types::dnssec::{
        DelegationLink, DnsProofBundle, Rrsig, SignedRRset,
    };
    use serde_bytes::ByteBuf;

    let env = env();
    let canister_id = setup_canister(&env);
    let (id, p) = fresh_identity(&env, canister_id);

    // A minimal-but-shape-valid bundle. The canister rejects before
    // it even validates because no trust anchors are configured for
    // this canister (we only set `doh_config` in `setup_canister`).
    let stub_rrsig = Rrsig {
        type_covered: 16,
        algorithm: 8,
        labels: 2,
        original_ttl: 3600,
        expiration: 4_000_000_000,
        inception: 0,
        key_tag: 1,
        signer_name: ByteBuf::from(vec![0u8]),
        signature: ByteBuf::from(vec![0u8; 256]),
    };
    let stub_rrset = SignedRRset {
        name: ByteBuf::from(b"\x07example\x03com\x00".to_vec()),
        rtype: 16,
        rdata: vec![ByteBuf::from(b"\x09v=DKIM1;k=rsa;p=AAAA".to_vec())],
        ttl: 3600,
        rrsig: stub_rrsig.clone(),
    };
    let proof = DnsProofBundle {
        leaf: stub_rrset.clone(),
        root_dnskey: stub_rrset,
        chain: vec![DelegationLink {
            child_ds: SignedRRset {
                name: ByteBuf::from(b"\x03com\x00".to_vec()),
                rtype: 43,
                rdata: vec![ByteBuf::from(vec![0u8; 36])],
                ttl: 3600,
                rrsig: stub_rrsig.clone(),
            },
            child_dnskey: SignedRRset {
                name: ByteBuf::from(b"\x03com\x00".to_vec()),
                rtype: 48,
                rdata: vec![ByteBuf::from(vec![0u8; 64])],
                ttl: 3600,
                rrsig: stub_rrsig,
            },
        }],
    };
    let input = EmailRecoveryDnsInput {
        address: TEST_ADDRESS.into(),
        selector: TEST_SELECTOR.into(),
        dns_proof: Some(proof),
    };
    let err = api::email_recovery_credential_prepare_add(&env, canister_id, p, id, input)
        .expect("call failed")
        .expect_err("expected DNSSEC rejection");
    // Without `dnssec_config.root_anchors`, prepare returns the
    // "DNSSEC trust anchors not configured" message via
    // `DomainNotSupported`. The DoH allowlist isn't consulted at all
    // when `dns_proof` is supplied.
    match err {
        EmailRecoveryError::DomainNotSupported(msg) => {
            assert!(
                msg.contains("trust anchors"),
                "expected trust-anchors message, got {msg:?}",
            );
        }
        other => panic!("expected DomainNotSupported, got {other:?}"),
    }
}

#[test]
fn dnssec_path_takes_precedence_over_doh_allowlist() {
    use internet_identity_interface::internet_identity::types::dnssec::{
        DelegationLink, DnsProofBundle, Rrsig, SignedRRset,
    };
    use serde_bytes::ByteBuf;

    // Even when the registered domain *is* on the DoH allowlist,
    // supplying a `dns_proof` puts us on the DNSSEC path. This test
    // confirms that by sending a malformed DNSSEC bundle and
    // checking we get a DNSSEC error rather than the call falling
    // through to the DoH happy path.
    let env = env();
    let canister_id = setup_canister(&env);
    let (id, p) = fresh_identity(&env, canister_id);

    let stub_rrsig = Rrsig {
        type_covered: 16,
        algorithm: 8,
        labels: 2,
        original_ttl: 3600,
        expiration: 4_000_000_000,
        inception: 0,
        key_tag: 1,
        signer_name: ByteBuf::from(vec![0u8]),
        signature: ByteBuf::from(vec![0u8; 256]),
    };
    let stub_rrset = SignedRRset {
        name: ByteBuf::from(b"\x07example\x03com\x00".to_vec()),
        rtype: 16,
        rdata: vec![ByteBuf::from(b"\x09v=DKIM1;k=rsa;p=AAAA".to_vec())],
        ttl: 3600,
        rrsig: stub_rrsig.clone(),
    };
    let proof = DnsProofBundle {
        leaf: stub_rrset.clone(),
        root_dnskey: stub_rrset,
        chain: vec![DelegationLink {
            child_ds: SignedRRset {
                name: ByteBuf::from(b"\x03com\x00".to_vec()),
                rtype: 43,
                rdata: vec![ByteBuf::from(vec![0u8; 36])],
                ttl: 3600,
                rrsig: stub_rrsig.clone(),
            },
            child_dnskey: SignedRRset {
                name: ByteBuf::from(b"\x03com\x00".to_vec()),
                rtype: 48,
                rdata: vec![ByteBuf::from(vec![0u8; 64])],
                ttl: 3600,
                rrsig: stub_rrsig,
            },
        }],
    };
    let input = EmailRecoveryDnsInput {
        // test.example.com IS on the allowlist — but the DNSSEC path
        // takes precedence.
        address: TEST_ADDRESS.into(),
        selector: TEST_SELECTOR.into(),
        dns_proof: Some(proof),
    };
    let err = api::email_recovery_credential_prepare_add(&env, canister_id, p, id, input)
        .expect("call failed")
        .expect_err("expected DNSSEC rejection");
    // Without trust anchors configured, prepare bails with
    // `DomainNotSupported("…trust anchors…")`. Crucially, NOT
    // a `DohFetchFailed` or successful nonce — confirming the
    // canister never fell through to DoH.
    assert!(
        matches!(err, EmailRecoveryError::DomainNotSupported(_)),
        "DNSSEC path should not fall through to DoH when dns_proof \
         is supplied; got {err:?}"
    );
}

// ===================================================================
// Full end-to-end test: prepare → sign → smtp_request → status
// ===================================================================
//
// This one does the real work: a freshly-generated RSA keypair signs
// an email whose Subject contains the canister-issued nonce; the
// canister's DoH outcalls are mocked to return the matching public
// key as a TXT record; the verifier accepts the email; status flips
// to RegistrationSucceeded.
//
// See `dkim_signer` below for the in-test DKIM signing logic.

#[test]
fn full_setup_flow_binds_credential_to_anchor() {
    let env = env();
    let canister_id = setup_canister(&env);
    let (id, p) = fresh_identity(&env, canister_id);

    // 1. prepare_add → get the nonce we'll embed in Subject.
    let challenge =
        api::email_recovery_credential_prepare_add(&env, canister_id, p, id, dns_input())
            .expect("prepare_add call failed")
            .expect("prepare_add failed");

    // 2. Generate a DKIM keypair + sign an email matching the
    //    canister's expectations.
    let signer = dkim_signer::TestSigner::new(TEST_DOMAIN, TEST_SELECTOR);
    let now_secs = time(&env) / 1_000_000_000;
    let signed = signer.sign_email(SignedEmailParams {
        from: TEST_ADDRESS,
        to: "register@id.ai",
        subject: &challenge.nonce,
        body: TEST_BODY,
        timestamp: now_secs,
    });

    // 3. Submit smtp_request. The canister will issue DoH outcalls
    //    for both the DKIM TXT and the DMARC TXT. Mock them in
    //    parallel with `tick`s.
    //
    //    smtp_request is an update call that only returns once the
    //    pipeline has fully run. We therefore submit it
    //    asynchronously, then drive PocketIC forward — fulfilling
    //    each outcall as it appears — and finally retrieve the
    //    result.
    let dkim_txt = signer.public_txt_record();
    let raw_msg_id = env
        .submit_call_with_effective_principal(
            canister_id,
            pocket_ic::common::rest::RawEffectivePrincipal::None,
            candid::Principal::anonymous(),
            "smtp_request",
            candid::encode_one(&signed.request).expect("encode SmtpRequest"),
        )
        .expect("submit_call");

    fulfill_doh_outcalls(&env, &dkim_txt);

    let raw = env
        .await_call_no_ticks(raw_msg_id)
        .expect("await_call_no_ticks");
    let resp: SmtpResponse = candid::decode_one(&raw).expect("decode SmtpResponse");
    assert!(matches!(resp, SmtpResponse::Ok {}));

    // 4. Status flips to RegistrationSucceeded.
    let status = api::email_recovery_status(&env, canister_id, &challenge.nonce)
        .expect("status call failed");
    assert!(
        matches!(status, EmailRecoveryStatus::RegistrationSucceeded),
        "expected RegistrationSucceeded, got {status:?}",
    );

    // 5. The bound credential is removable, end-to-end check that
    //    the binding actually persisted to the anchor (otherwise the
    //    remove would return AddressNotRegistered).
    api::email_recovery_credential_remove(&env, canister_id, p, id, TEST_ADDRESS)
        .expect("remove call failed")
        .expect("remove should succeed");
}

/// Drive PocketIC forward until each of the 5 DoH provider outcalls
/// has been seen, fulfilling them with the supplied DKIM TXT bytes.
/// DMARC outcalls are answered with NXDOMAIN (the verifier's "no
/// DMARC record" path requires DKIM `d=` to equal From: domain — true
/// in this test).
fn fulfill_doh_outcalls(env: &PocketIc, dkim_txt: &[u8]) {
    // The 5 provider URLs the DoH module fans out to. Order doesn't
    // matter because the quorum just needs 3 of them to agree on the
    // same body bytes.
    let providers = [
        "https://cloudflare-dns.com/dns-query",
        "https://dns.google/dns-query",
        "https://dns.quad9.net/dns-query",
        "https://private.canadianshield.cira.ca/dns-query",
        "https://public.dns.iij.jp/dns-query",
    ];
    // We wait for at most this many ticks before assuming the
    // canister isn't going to issue any more outcalls. Each tick is
    // a heartbeat; outcalls land within a few of them.
    const MAX_TICKS: u32 = 60;
    let mut ticks = 0;
    let mut answered = std::collections::HashSet::new();

    while ticks < MAX_TICKS && answered.len() < providers.len() {
        env.tick();
        ticks += 1;
        for req in env.get_canister_http() {
            if answered.contains(&req.request_id) {
                continue;
            }
            let body = if providers.iter().any(|p| req.url.starts_with(p)) {
                // Synthesize a wire-format DNS response carrying the
                // DKIM TXT. The canister's transform reduces the
                // wire response down to TXT bytes, so what we emit
                // here will be what `quorum::decide_quorum` sees.
                fake_dkim_dns_response(dkim_txt)
            } else {
                continue;
            };
            let response = MockCanisterHttpResponse {
                subnet_id: req.subnet_id,
                request_id: req.request_id,
                response: CanisterHttpResponse::CanisterHttpReply(CanisterHttpReply {
                    status: 200,
                    headers: vec![],
                    body,
                }),
                additional_responses: vec![],
            };
            env.mock_canister_http_response(response);
            answered.insert(req.request_id);
        }
    }
    // After fulfilling DKIM outcalls, the canister will also fan out
    // for DMARC. We answer all of those with a 404-equivalent (no
    // record), which the verifier treats as "no DMARC published" —
    // and the strict-alignment fallback (DKIM d= == From: domain) is
    // satisfied by our test setup.
    let dmarc_deadline = ticks + 60;
    while ticks < dmarc_deadline {
        env.tick();
        ticks += 1;
        for req in env.get_canister_http() {
            let response = MockCanisterHttpResponse {
                subnet_id: req.subnet_id,
                request_id: req.request_id,
                response: CanisterHttpResponse::CanisterHttpReply(CanisterHttpReply {
                    status: 404,
                    headers: vec![],
                    body: vec![],
                }),
                additional_responses: vec![],
            };
            env.mock_canister_http_response(response);
        }
    }
}

/// Build a wire-format DNS response carrying a single TXT answer
/// with the given RDATA. Header: ID=0, flags=0x8180 (response, RD,
/// RA, RCODE=0), QDCOUNT=1, ANCOUNT=1.
fn fake_dkim_dns_response(txt: &[u8]) -> Vec<u8> {
    let mut out = Vec::new();
    // Header (12 bytes).
    out.extend_from_slice(&[0, 0, 0x81, 0x80, 0, 1, 0, 1, 0, 0, 0, 0]);
    // Question: a fixed name we don't actually re-parse on the
    // canister side (the verifier's transform only extracts the
    // first TXT answer's RDATA).
    out.extend_from_slice(b"\x07example\x03com\x00\x00\x10\x00\x01");
    // Answer: compression pointer to the question's name + type +
    // class + TTL.
    out.extend_from_slice(&[0xC0, 0x0C]);
    out.extend_from_slice(&16u16.to_be_bytes()); // type=TXT
    out.extend_from_slice(&1u16.to_be_bytes()); // class=IN
    out.extend_from_slice(&300u32.to_be_bytes()); // ttl
                                                  // RDLENGTH and RDATA. TXT RDATA is one or more length-prefixed
                                                  // <character-string>s. Long records get chunked into 255-byte
                                                  // pieces; a fresh-keypair DKIM TXT typically lands around 400
                                                  // bytes so we chunk by 255.
    let chunks: Vec<&[u8]> = txt.chunks(255).collect();
    let rdlength: usize = chunks.iter().map(|c| c.len() + 1).sum();
    out.extend_from_slice(&(rdlength as u16).to_be_bytes());
    for chunk in chunks {
        out.push(chunk.len() as u8);
        out.extend_from_slice(chunk);
    }
    out
}

// ===================================================================
// Args structs
// ===================================================================

struct SignedEmailParams<'a> {
    from: &'a str,
    to: &'a str,
    subject: &'a str,
    body: &'a [u8],
    timestamp: u64,
}

struct SignedEmail {
    request: SmtpRequest,
}

// ===================================================================
// In-test DKIM signer (relaxed/relaxed, RSA-SHA256)
// ===================================================================
//
// Mirrors the canonicalization rules implemented by the verifier in
// `crate::dkim::canonicalize`. We keep this self-contained so the
// integration test isn't fragile against module reorganisation in
// the canister code.

mod dkim_signer {
    use rsa::pkcs8::EncodePublicKey;
    use rsa::{Pkcs1v15Sign, RsaPrivateKey, RsaPublicKey};
    use sha2::{Digest, Sha256};

    pub struct TestSigner {
        domain: String,
        selector: String,
        private_key: RsaPrivateKey,
        public_key: RsaPublicKey,
    }

    impl TestSigner {
        pub fn new(domain: &str, selector: &str) -> Self {
            // 2048-bit keys make for big tests but match real-world
            // DKIM. Smaller keys would also work; the verifier
            // accepts >= 1024 bits per design §5.6.
            let mut rng = rand::rngs::OsRng;
            let private_key = RsaPrivateKey::new(&mut rng, 2048).expect("RSA keygen");
            let public_key = RsaPublicKey::from(&private_key);
            Self {
                domain: domain.into(),
                selector: selector.into(),
                private_key,
                public_key,
            }
        }

        /// `v=DKIM1; k=rsa; p={base64-SPKI-DER}` — the shape the
        /// canister-side verifier expects (X.509 SubjectPublicKeyInfo
        /// per RFC 5280 §4.1, NOT PKCS#1 RSAPublicKey). Real DKIM
        /// records publish keys this way too.
        pub fn public_txt_record(&self) -> Vec<u8> {
            let spki = self.public_key.to_public_key_der().expect("RSA SPKI DER");
            let p_b64 = base64_encode(spki.as_bytes());
            format!("v=DKIM1; k=rsa; p={p_b64}").into_bytes()
        }

        pub fn sign_email(&self, params: super::SignedEmailParams) -> super::SignedEmail {
            use internet_identity_interface::internet_identity::types::smtp::{
                SmtpAddress, SmtpEnvelope, SmtpHeader, SmtpMessage, SmtpRequest,
            };
            use serde_bytes::ByteBuf;

            let date = "Mon, 5 May 2026 12:00:00 +0000";
            let msg_id = format!("<test-{}@{}>", params.timestamp, self.domain);

            // 1. Body hash.
            let canon_body = relaxed_body(params.body);
            let bh: [u8; 32] = Sha256::digest(&canon_body).into();
            let bh_b64 = base64_encode(&bh);

            // 2. The DKIM-Signature header value with `b=` empty.
            //    The signature input includes this (with the empty
            //    `b=`) so the order matters: build the rest first,
            //    leave `b=` last.
            let sig_value_no_b = format!(
                "v=1; a=rsa-sha256; c=relaxed/relaxed; d={d}; s={s}; \
                 t={t}; h=From:To:Subject:Date:Message-ID; \
                 bh={bh}; b=",
                d = self.domain,
                s = self.selector,
                t = params.timestamp,
                bh = bh_b64,
            );

            // 3. Compute the "hash input": canonicalized signed
            //    headers + canonicalized DKIM-Signature header
            //    (with empty b=, no trailing CRLF).
            let mut hash_input = Vec::new();
            hash_input.extend(relaxed_header("From", params.from));
            hash_input.extend(relaxed_header("To", params.to));
            hash_input.extend(relaxed_header("Subject", params.subject));
            hash_input.extend(relaxed_header("Date", date));
            hash_input.extend(relaxed_header("Message-ID", &msg_id));
            let mut canon_dkim = relaxed_header("DKIM-Signature", &sig_value_no_b);
            // Strip trailing CRLF per RFC 6376 §3.7.
            if canon_dkim.ends_with(b"\r\n") {
                canon_dkim.truncate(canon_dkim.len() - 2);
            }
            hash_input.extend(canon_dkim);

            // 4. Sign.
            let digest: [u8; 32] = Sha256::digest(&hash_input).into();
            let sig = self
                .private_key
                .sign(Pkcs1v15Sign::new::<Sha256>(), &digest)
                .expect("RSA sign");
            let sig_b64 = base64_encode(&sig);

            // 5. Final DKIM-Signature header value.
            let sig_value_final = format!("{sig_value_no_b}{sig_b64}");

            // 6. Build the SmtpRequest.
            let from_user = params.from.split('@').next().unwrap_or("alice");
            let from_domain = params
                .from
                .rsplit('@')
                .next()
                .unwrap_or(self.domain.as_str());
            let to_user = params.to.split('@').next().unwrap_or("register");
            let to_domain = params.to.rsplit('@').next().unwrap_or("id.ai");
            let request = SmtpRequest {
                envelope: Some(SmtpEnvelope {
                    from: SmtpAddress {
                        user: from_user.into(),
                        domain: from_domain.into(),
                    },
                    to: SmtpAddress {
                        user: to_user.into(),
                        domain: to_domain.into(),
                    },
                }),
                message: Some(SmtpMessage {
                    headers: vec![
                        SmtpHeader {
                            name: "DKIM-Signature".into(),
                            value: sig_value_final,
                        },
                        SmtpHeader {
                            name: "From".into(),
                            value: params.from.into(),
                        },
                        SmtpHeader {
                            name: "To".into(),
                            value: params.to.into(),
                        },
                        SmtpHeader {
                            name: "Subject".into(),
                            value: params.subject.into(),
                        },
                        SmtpHeader {
                            name: "Date".into(),
                            value: date.into(),
                        },
                        SmtpHeader {
                            name: "Message-ID".into(),
                            value: msg_id,
                        },
                    ],
                    body: ByteBuf::from(params.body.to_vec()),
                }),
                gateway_flags: None,
            };

            super::SignedEmail { request }
        }
    }

    fn base64_encode(bytes: &[u8]) -> String {
        use base64::Engine;
        base64::engine::general_purpose::STANDARD.encode(bytes)
    }

    // -------------------------------------------------------------
    // Relaxed canonicalization — mirrors `crate::dkim::canonicalize`.
    // -------------------------------------------------------------

    /// Apply RFC 6376 §3.4.2 to a single header.
    fn relaxed_header(name: &str, value: &str) -> Vec<u8> {
        let mut out = Vec::with_capacity(name.len() + value.len() + 4);
        for b in name.bytes() {
            out.push(b.to_ascii_lowercase());
        }
        out.push(b':');
        out.extend_from_slice(relaxed_header_value(value).as_bytes());
        out.extend_from_slice(b"\r\n");
        out
    }

    fn relaxed_header_value(value: &str) -> String {
        let bytes = value.as_bytes();
        let mut out: Vec<u8> = Vec::with_capacity(bytes.len());
        let mut i = 0;
        let mut last_was_sp = true;
        while i < bytes.len() {
            let b = bytes[i];
            if b == b'\r' && i + 2 < bytes.len() && bytes[i + 1] == b'\n' && is_wsp(bytes[i + 2]) {
                i += 2;
                continue;
            }
            if is_wsp(b) {
                if !last_was_sp {
                    out.push(b' ');
                    last_was_sp = true;
                }
                i += 1;
                continue;
            }
            out.push(b);
            last_was_sp = false;
            i += 1;
        }
        if out.last() == Some(&b' ') {
            out.pop();
        }
        String::from_utf8(out).unwrap_or_default()
    }

    fn is_wsp(b: u8) -> bool {
        b == b' ' || b == b'\t'
    }

    /// Apply RFC 6376 §3.4.4 to the body.
    fn relaxed_body(body: &[u8]) -> Vec<u8> {
        let mut lines: Vec<Vec<u8>> = Vec::new();
        let mut start = 0;
        while start < body.len() {
            let mut end = start;
            let mut had_crlf = false;
            while end < body.len() {
                if body[end] == b'\r' && end + 1 < body.len() && body[end + 1] == b'\n' {
                    had_crlf = true;
                    break;
                }
                end += 1;
            }
            lines.push(canon_body_line(&body[start..end]));
            start = if had_crlf { end + 2 } else { end };
        }
        while lines.last().is_some_and(|l| l.is_empty()) {
            lines.pop();
        }
        if lines.is_empty() {
            return b"\r\n".to_vec();
        }
        let mut out = Vec::with_capacity(body.len());
        for line in &lines {
            out.extend_from_slice(line);
            out.extend_from_slice(b"\r\n");
        }
        out
    }

    fn canon_body_line(line: &[u8]) -> Vec<u8> {
        let mut out = Vec::with_capacity(line.len());
        let mut last_was_sp = false;
        for &b in line {
            if is_wsp(b) {
                if !last_was_sp {
                    out.push(b' ');
                    last_was_sp = true;
                }
            } else {
                out.push(b);
                last_was_sp = false;
            }
        }
        if out.last() == Some(&b' ') {
            out.pop();
        }
        out
    }
}
