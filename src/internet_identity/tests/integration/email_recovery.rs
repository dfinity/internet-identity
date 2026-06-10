//! Integration tests for the email-recovery setup flow.
//!
//! Covers both verification paths end-to-end (prepare → smtp_request
//! → status):
//!
//! - **DoH path** — `prepare_add` is called without a `dns_proof`, so
//!   `smtp_request` resolves the DKIM (and DMARC) TXT records via
//!   DoH outcalls at email-arrival time. Tests fan out to fulfil
//!   each provider's outcall with a synthesized DNS response built
//!   around the test's RSA keypair.
//! - **DNSSEC path** — `prepare_add` is given a fresh DNSSEC chain
//!   (`dnssec_signer` below) carrying both DKIM and DMARC leaves,
//!   pre-validated synchronously. `smtp_request` then issues *zero*
//!   DoH outcalls — verified by an explicit assertion in the
//!   happy-path test.
//!
//! The DKIM-signed email is generated at test time against a fresh
//! RSA keypair (`dkim_signer` below); both paths use the same email,
//! so the canister-side verification logic is the same — only the
//! TXT-source plumbing differs.
//!
//! Negative paths (allowlist gate, authz, unknown nonce, TTL expiry,
//! malformed DNSSEC bundle, …) are tested separately at the top of
//! the file.
//!
//! See `docs/ongoing/email-recovery.md` and the feature module at
//! `crate::email_recovery` for the design.

use crate::v2_api::authn_method_test_helpers::{
    create_identity_with_authn_method, sample_webauthn_authn_method, test_authn_method,
};
use canister_tests::{api::internet_identity as api, framework::*};
use internet_identity_interface::internet_identity::types::email_recovery::{
    EmailRecoveryDnsInput, EmailRecoveryError, EmailRecoveryStatus, VerificationPath,
};
use internet_identity_interface::internet_identity::types::smtp::{
    SmtpAddress, SmtpEnvelope, SmtpHeader, SmtpMessage, SmtpRequest, SmtpResponse,
};
use internet_identity_interface::internet_identity::types::{
    DnsProofBundle, DohConfig, InternetIdentityInit,
};
use pocket_ic::common::rest::{CanisterHttpReply, CanisterHttpResponse, MockCanisterHttpResponse};
use pocket_ic::PocketIc;
use serde_bytes::ByteBuf;
use std::time::Duration;

// ===================================================================
// Fixtures
// ===================================================================

const TEST_DOMAIN: &str = "test.example.com";
const TEST_ADDRESS: &str = "alice@test.example.com";
// A second address in the *same* domain as `TEST_ADDRESS`, so two
// concurrent verifications resolve the same DKIM FQDN and exercise the
// in-flight cache dedup path. Used by the concurrency reproduction test.
const TEST_ADDRESS_2: &str = "bob@test.example.com";
const TEST_SELECTOR: &str = "test1";
const TEST_BODY: &[u8] = b"Hello world.\r\nThis is a recovery email.\r\n";

fn dns_input() -> EmailRecoveryDnsInput {
    EmailRecoveryDnsInput {
        address: TEST_ADDRESS.into(),
        dns_proof: None,
    }
}

// ===================================================================
// Setup
// ===================================================================

/// Stand up a canister with an `allowed_domains` list that lets
/// `test.example.com` through to the DoH path, and a `related_origins`
/// entry so `mailbox_domains()` accepts `register@id.ai` /
/// `recover@id.ai` envelopes (recipient acceptance reads from
/// `related_origins`; see `email_recovery::mailbox_domains`).
fn setup_canister(env: &PocketIc) -> candid::Principal {
    let args = InternetIdentityInit {
        doh_config: Some(Some(DohConfig {
            allowed_domains: vec![TEST_DOMAIN.into()],
            max_cache_age_secs: Some(3600),
        })),
        related_origins: Some(vec!["https://id.ai".into()]),
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
    // No mailbox field on the challenge — the FE pairs the
    // user-part (`register` / `recover`) with
    // `window.location.hostname` so each tab shows the alias
    // matching the origin the user is on.
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

    // Advance past the 30-minute TTL. `tick()` forces the round to
    // roll over so the subsequent query call reads `time()` after the
    // advance — without it, PocketIC's query path can still see the
    // pre-advance clock.
    env.advance_time(Duration::from_secs(31 * 60));
    env.tick();

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
            to: vec![SmtpAddress {
                user: "register".into(),
                domain: "id.ai".into(),
            }],
        }),
        message: Some(SmtpMessage {
            headers: vec![
                SmtpHeader {
                    name: "From".into(),
                    value: TEST_ADDRESS.into(),
                },
                SmtpHeader {
                    name: "Date".into(),
                    value: "Mon, 5 May 2026 12:00:00 +0000".into(),
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
        message_id: None,
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
            to: vec![SmtpAddress {
                user: "register".into(),
                domain: "id.ai".into(),
            }],
        }),
        message: Some(SmtpMessage {
            headers: vec![
                SmtpHeader {
                    name: "From".into(),
                    value: TEST_ADDRESS.into(),
                },
                SmtpHeader {
                    name: "Date".into(),
                    value: "Mon, 5 May 2026 12:00:00 +0000".into(),
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
        message_id: None,
    };
    let resp = api::smtp_request(&env, canister_id, &request).expect("call failed");
    assert!(matches!(resp, SmtpResponse::Ok {}));
}

#[test]
fn smtp_request_rejects_emails_addressed_to_unknown_recipients_with_550() {
    let env = env();
    let canister_id = setup_canister(&env);

    // The mailbox set this canister handles (`register@<d>` /
    // `recover@<d>` for any `d` in `related_origins`) is part of the
    // public Candid surface, so there's nothing to leak by surfacing
    // an SMTP-level "no such user" — a sender targeting any other
    // mailbox is a caller error and gets a 550 back so the off-chain
    // gateway can bounce upstream.
    let request = SmtpRequest {
        envelope: Some(SmtpEnvelope {
            from: SmtpAddress {
                user: "alice".into(),
                domain: TEST_DOMAIN.into(),
            },
            to: vec![SmtpAddress {
                user: "marketing".into(),
                domain: "id.ai".into(),
            }],
        }),
        message: Some(SmtpMessage {
            headers: vec![
                SmtpHeader {
                    name: "From".into(),
                    value: TEST_ADDRESS.into(),
                },
                SmtpHeader {
                    name: "Date".into(),
                    value: "Mon, 5 May 2026 12:00:00 +0000".into(),
                },
                SmtpHeader {
                    name: "To".into(),
                    value: "marketing@id.ai".into(),
                },
                SmtpHeader {
                    name: "Subject".into(),
                    value: "II-Recovery-deadbeefcafebabe".into(),
                },
            ],
            body: ByteBuf::from(b"hello".to_vec()),
        }),
        gateway_flags: None,
        message_id: None,
    };
    let resp = api::smtp_request(&env, canister_id, &request).expect("call failed");
    match resp {
        SmtpResponse::Err(e) => {
            assert_eq!(e.code, 550, "expected 550 for unknown recipient, got {e:?}");
        }
        SmtpResponse::Ok {} => panic!("expected 550 Err for unknown recipient, got Ok"),
    }
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
// DNSSEC path: full end-to-end setup flow + protocol-shape checks
// ===================================================================
//
// `prepare_add` is called with a `dns_proof` carrying the signed DKIM
// (and optionally DMARC) TXT records. The canister validates the
// chain synchronously against its configured trust anchors and caches
// the verified bytes on the pending challenge, so `smtp_request`
// issues zero DoH outcalls at email-arrival time. Mirror of the DoH
// e2e flow further down the file.
//
// Coverage:
// - `full_setup_flow_via_dnssec_path` — happy path: freshly-signed
//   chain (Ed25519, see `dnssec_signer` below), DKIM + DMARC bundled,
//   asserts no DoH outcalls are observed.
// - `dnssec_path_rejects_when_no_trust_anchors_configured` — config
//   gate: the canister refuses to validate without anchors set.
// - `dnssec_path_takes_precedence_over_doh_allowlist` — path picker:
//   even when the domain is on the DoH allowlist, supplying a
//   `dns_proof` forces the DNSSEC branch.

#[test]
fn dnssec_path_rejects_when_no_trust_anchors_configured() {
    use internet_identity_interface::internet_identity::types::dnssec::{
        DelegationChain, DelegationLink, DnsProofBundle, Rrsig, SignedRRset,
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
        hops: vec![stub_rrset.clone()],
        root_dnskey: stub_rrset,
        chains: vec![DelegationChain {
            links: vec![DelegationLink {
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
        }],
    };
    let input = EmailRecoveryDnsInput {
        address: TEST_ADDRESS.into(),
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
        DelegationChain, DelegationLink, DnsProofBundle, Rrsig, SignedRRset,
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
        hops: vec![stub_rrset.clone()],
        root_dnskey: stub_rrset,
        chains: vec![DelegationChain {
            links: vec![DelegationLink {
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
        }],
    };
    let input = EmailRecoveryDnsInput {
        // test.example.com IS on the allowlist — but the DNSSEC path
        // takes precedence.
        address: TEST_ADDRESS.into(),
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

#[test]
fn full_setup_flow_via_dnssec_path() {
    use internet_identity_interface::internet_identity::types::DnssecConfig;

    // Generate the DKIM keypair first; we need both the TXT record
    // (to embed in the DNSSEC leaf) and the private key (to sign the
    // inbound email).
    let dkim = dkim_signer::TestSigner::new(TEST_DOMAIN, TEST_SELECTOR);
    let dkim_txt = dkim.public_txt_record();

    // Build the DNSSEC chain. RRSIG validity windows are constructed
    // around PocketIC's default initial clock — it's a wall-clock
    // value (around 2021) so we use a fixed `now_secs` that comfortably
    // brackets it.
    let env = env();
    let now_secs: u32 = (time(&env) / 1_000_000_000)
        .try_into()
        .expect("PocketIC initial time fits in u32");
    // Bundle both DKIM and DMARC so the canister can short-circuit
    // every DoH outcall — even DMARC, which would otherwise still
    // fan out at email-arrival time. `p=none` is the most permissive
    // DMARC policy and still satisfies alignment given DKIM `d=` ==
    // From: domain in this test.
    let dmarc_txt = b"v=DMARC1; p=none;";
    let chain = dnssec_signer::build_chain(
        TEST_DOMAIN,
        TEST_SELECTOR,
        &dkim_txt,
        Some(dmarc_txt),
        now_secs,
    );

    // Stand up the canister with both the DoH allowlist (so the
    // domain check on the DoH path would pass too — proving we're
    // actually exercising the DNSSEC branch), the trust anchor, and
    // a `related_origins` entry for `id.ai` so `smtp_request`'s
    // recipient dispatch accepts `register@id.ai`.
    let args = InternetIdentityInit {
        doh_config: Some(Some(DohConfig {
            allowed_domains: vec![TEST_DOMAIN.into()],
            max_cache_age_secs: Some(3600),
        })),
        dnssec_config: Some(Some(DnssecConfig {
            root_anchors: vec![chain.anchor],
        })),
        related_origins: Some(vec!["https://id.ai".into()]),
        canister_creation_cycles_cost: Some(0),
        ..Default::default()
    };
    let canister_id = install_ii_canister_with_arg_and_cycles(
        &env,
        II_WASM.clone(),
        Some(args),
        10_000_000_000_000,
    );
    let (id, p) = fresh_identity(&env, canister_id);

    // 1. prepare_add with the DNSSEC *skeleton* bundle: chain anchored
    //    at root + the DMARC leaf. The DKIM leaf is *not* in the
    //    prepare bundle — its selector is unknown until the email
    //    arrives, so the FE walks it later via `submit_dkim_leaf`.
    let input = EmailRecoveryDnsInput {
        address: TEST_ADDRESS.into(),
        dns_proof: Some(DnsProofBundle {
            hops: chain.dmarc_leaf.clone().map_or(vec![], |l| vec![l]),
            ..chain.skeleton.clone()
        }),
    };
    let challenge = api::email_recovery_credential_prepare_add(&env, canister_id, p, id, input)
        .expect("prepare_add call failed")
        .expect("DNSSEC happy-path prepare_add should succeed");

    // 2. Sign an email matching the issued nonce.
    let signed = dkim.sign_email(SignedEmailParams {
        from: TEST_ADDRESS,
        to: "register@id.ai",
        subject: &challenge.nonce,
        body: TEST_BODY,
        timestamp: time(&env) / 1_000_000_000,
    });

    // 3. Submit smtp_request. The canister parses `s=` from the
    //    DKIM-Signature header, validates the body hash, drops the
    //    body, caches the partial-verification record, and flips
    //    status to `NeedDkimLeaf { selector }`. No DoH outcalls
    //    happen — the chain was anchored at prepare time and the
    //    DMARC TXT was cached.
    let raw_msg_id = env
        .submit_call_with_effective_principal(
            canister_id,
            pocket_ic::common::rest::RawEffectivePrincipal::None,
            candid::Principal::anonymous(),
            "smtp_request",
            candid::encode_one(&signed.request).expect("encode SmtpRequest"),
        )
        .expect("submit_call");

    assert_no_doh_outcalls(&env);

    let raw = env
        .await_call_no_ticks(raw_msg_id)
        .expect("await_call_no_ticks");
    let resp: SmtpResponse = candid::decode_one(&raw).expect("decode SmtpResponse");
    assert!(matches!(resp, SmtpResponse::Ok {}));

    // 4. Status now reports `NeedDkimLeaf { selector }` — the
    //    canister read the selector from the email and is waiting
    //    for the FE to deliver the leaf.
    let status = api::email_recovery_status(&env, canister_id, &challenge.nonce)
        .expect("status call failed");
    let selector = match status {
        EmailRecoveryStatus::NeedDkimLeaf { ref selector } => selector.clone(),
        other => panic!("expected NeedDkimLeaf, got {other:?}"),
    };
    assert_eq!(selector, TEST_SELECTOR);

    // 5. Submit the DKIM leaf. The canister validates it against
    //    the cached zone DNSKEY, runs the cryptographic signature
    //    check using the cached digest, checks DMARC + From: match,
    //    and binds the credential.
    let submit_status = api::email_recovery_submit_dkim_leaf(
        &env,
        canister_id,
        internet_identity_interface::internet_identity::types::email_recovery::EmailRecoverySubmitDkimLeafArg {
            nonce: challenge.nonce.clone(),
            hops: vec![chain.dkim_leaf.clone()],
            extra_chains: vec![],
        },
    )
    .expect("submit_dkim_leaf call failed")
    .expect("submit_dkim_leaf should succeed");
    assert!(
        matches!(submit_status, EmailRecoveryStatus::RegistrationSucceeded),
        "expected RegistrationSucceeded from submit_dkim_leaf, got {submit_status:?}",
    );

    // 6. Polled status also reflects the success.
    let status = api::email_recovery_status(&env, canister_id, &challenge.nonce)
        .expect("status call failed");
    assert!(
        matches!(status, EmailRecoveryStatus::RegistrationSucceeded),
        "expected RegistrationSucceeded, got {status:?}",
    );

    // 7. Removal works → confirms the credential actually persisted
    //    to the anchor.
    api::email_recovery_credential_remove(&env, canister_id, p, id, TEST_ADDRESS)
        .expect("remove call failed")
        .expect("remove should succeed");
}

// ===================================================================
// DNSSEC path: DoH fallback for an unsigned DKIM leaf
// ===================================================================
//
// The DNSSEC path commits at prepare time based on the *apex* being
// signed. But some providers publish the DKIM record as a CNAME into
// an unsigned zone — `selector1._domainkey.outlook.com` is a signed
// CNAME into the unsigned `outbound.protection.outlook.com`,
// `live.com` likewise. The FE's DNSSEC hop-walk can't authenticate
// that hop, so it submits an EMPTY `hops` set, asking the canister to
// resolve the DKIM key over its own (allowlist-gated) DoH path using
// the cached `partial_verification` crypto material instead of leaving
// the entry stuck in `NeedDkimLeaf` until it expires.
//
// These tests reuse the same DNSSEC-signed `TEST_DOMAIN` fixture to
// reach `NeedDkimLeaf`, then exercise the empty-hops fallback with the
// domain on / off the DoH allowlist.

/// Drive a DNSSEC-path setup flow up to the `NeedDkimLeaf` state.
/// `allowed_domains` sets the DoH allowlist the subsequent empty-hops
/// fallback will be gated on. Returns the canister + identity handles,
/// the issued nonce, and the DKIM TXT the fallback's outcall expects
/// to fetch (so the caller can fulfil the DoH fan-out).
fn dnssec_flow_until_need_dkim_leaf(
    env: &PocketIc,
    allowed_domains: Vec<String>,
) -> (candid::Principal, u64, candid::Principal, String, Vec<u8>) {
    use internet_identity_interface::internet_identity::types::DnssecConfig;

    let dkim = dkim_signer::TestSigner::new(TEST_DOMAIN, TEST_SELECTOR);
    let dkim_txt = dkim.public_txt_record();
    let now_secs: u32 = (time(env) / 1_000_000_000)
        .try_into()
        .expect("PocketIC initial time fits in u32");
    // Skeleton bundle = chain + DMARC leaf (no DKIM leaf; its selector
    // is unknown at prepare). `p=none` keeps DMARC alignment permissive.
    let dmarc_txt = b"v=DMARC1; p=none;";
    let chain = dnssec_signer::build_chain(
        TEST_DOMAIN,
        TEST_SELECTOR,
        &dkim_txt,
        Some(dmarc_txt),
        now_secs,
    );

    let args = InternetIdentityInit {
        doh_config: Some(Some(DohConfig {
            allowed_domains,
            max_cache_age_secs: Some(3600),
        })),
        dnssec_config: Some(Some(DnssecConfig {
            root_anchors: vec![chain.anchor],
        })),
        related_origins: Some(vec!["https://id.ai".into()]),
        canister_creation_cycles_cost: Some(0),
        ..Default::default()
    };
    let canister_id = install_ii_canister_with_arg_and_cycles(
        env,
        II_WASM.clone(),
        Some(args),
        10_000_000_000_000,
    );
    let (id, p) = fresh_identity(env, canister_id);

    let input = EmailRecoveryDnsInput {
        address: TEST_ADDRESS.into(),
        dns_proof: Some(DnsProofBundle {
            hops: chain.dmarc_leaf.clone().map_or(vec![], |l| vec![l]),
            ..chain.skeleton.clone()
        }),
    };
    let challenge = api::email_recovery_credential_prepare_add(env, canister_id, p, id, input)
        .expect("prepare_add call failed")
        .expect("DNSSEC prepare_add should succeed");

    let signed = dkim.sign_email(SignedEmailParams {
        from: TEST_ADDRESS,
        to: "register@id.ai",
        subject: &challenge.nonce,
        body: TEST_BODY,
        timestamp: time(env) / 1_000_000_000,
    });
    let raw_msg_id = env
        .submit_call_with_effective_principal(
            canister_id,
            pocket_ic::common::rest::RawEffectivePrincipal::None,
            candid::Principal::anonymous(),
            "smtp_request",
            candid::encode_one(&signed.request).expect("encode SmtpRequest"),
        )
        .expect("submit_call");
    // The DNSSEC path issues no outcalls at email-arrival time (chain +
    // DMARC were anchored at prepare); a handful of ticks executes the
    // message before we collect the response.
    for _ in 0..30 {
        env.tick();
    }
    let raw = env
        .await_call_no_ticks(raw_msg_id)
        .expect("await_call_no_ticks");
    let resp: SmtpResponse = candid::decode_one(&raw).expect("decode SmtpResponse");
    assert!(matches!(resp, SmtpResponse::Ok {}));

    let status =
        api::email_recovery_status(env, canister_id, &challenge.nonce).expect("status call failed");
    match status {
        EmailRecoveryStatus::NeedDkimLeaf { ref selector } => {
            assert_eq!(selector, TEST_SELECTOR)
        }
        other => panic!("expected NeedDkimLeaf, got {other:?}"),
    };

    (canister_id, id, p, challenge.nonce, dkim_txt)
}

#[test]
fn dnssec_path_falls_back_to_doh_when_leaf_is_unsigned() {
    let env = env();
    // Domain IS on the DoH allowlist, so the fallback can complete.
    let (canister_id, id, p, nonce, dkim_txt) =
        dnssec_flow_until_need_dkim_leaf(&env, vec![TEST_DOMAIN.into()]);

    // The FE could not DNSSEC-resolve the leaf, so it calls the
    // DoH-fallback method (no leaf data). The canister resolves the
    // DKIM key over DoH; this issues outcalls, so drive it
    // asynchronously and fulfil the provider fan-out with the test
    // signer's DKIM TXT.
    let raw_msg_id = env
        .submit_call_with_effective_principal(
            canister_id,
            pocket_ic::common::rest::RawEffectivePrincipal::None,
            candid::Principal::anonymous(),
            "email_recovery_submit_dkim_leaf_via_doh",
            candid::encode_one(
                &internet_identity_interface::internet_identity::types::email_recovery::EmailRecoverySubmitDkimLeafViaDohArg {
                    nonce: nonce.clone(),
                },
            )
            .expect("encode via_doh arg"),
        )
        .expect("submit_call");

    fulfill_doh_outcalls(&env, &dkim_txt);

    let raw = env
        .await_call_no_ticks(raw_msg_id)
        .expect("await_call_no_ticks");
    let result: Result<EmailRecoveryStatus, EmailRecoveryError> =
        candid::decode_one(&raw).expect("decode submit_dkim_leaf_via_doh result");
    let submit_status = result.expect("DoH fallback accept should be Ok");
    // Synchronous accept now: the method returns `Verifying` and resolves the
    // DKIM key in the background. The terminal verdict lands on the polled
    // status below (the fetch + finalize ran during `fulfill_doh_outcalls`).
    assert!(
        matches!(submit_status, EmailRecoveryStatus::Verifying),
        "expected Verifying accept from the DoH fallback, got {submit_status:?}",
    );

    // Polled status is the terminal verdict, and the credential actually
    // persisted to the anchor (otherwise remove would return AddressNotRegistered).
    let status = api::email_recovery_status(&env, canister_id, &nonce).expect("status call failed");
    assert!(
        matches!(status, EmailRecoveryStatus::RegistrationSucceeded),
        "expected RegistrationSucceeded, got {status:?}",
    );
    api::email_recovery_credential_remove(&env, canister_id, p, id, TEST_ADDRESS)
        .expect("remove call failed")
        .expect("remove should succeed");
}

#[test]
fn dnssec_path_doh_fallback_rejects_non_allowlisted_domain() {
    let env = env();
    // TEST_DOMAIN is DNSSEC-signed (so the DNSSEC path + NeedDkimLeaf
    // are reached) but is NOT on the DoH allowlist.
    let (canister_id, _id, _p, nonce, _dkim_txt) =
        dnssec_flow_until_need_dkim_leaf(&env, vec![]);

    // DoH fallback. Synchronous accept: the allowlist gate rejects before
    // any outcall (no detach, no DoH fan-out), so the call returns
    // `Ok(Verifying)` and the rejection is written to the polled status by
    // the same call — surfaced below.
    let result = api::email_recovery_submit_dkim_leaf_via_doh(&env, canister_id, &nonce)
        .expect("submit_dkim_leaf_via_doh call failed");
    assert!(
        matches!(result, Ok(EmailRecoveryStatus::Verifying)),
        "expected Ok(Verifying) accept, got {result:?}",
    );

    // The pending entry is now terminally Failed — the user sees the
    // unsupported-domain view instead of polling until Expired.
    let status = api::email_recovery_status(&env, canister_id, &nonce).expect("status call failed");
    assert!(
        matches!(
            status,
            EmailRecoveryStatus::Failed(EmailRecoveryError::DomainNotAllowlisted(_))
        ),
        "expected Failed(DomainNotAllowlisted), got {status:?}",
    );
}

/// Drive PocketIC forward and panic if any DoH outcall is observed.
/// Used by the DNSSEC happy-path test to prove that bundling DKIM +
/// DMARC into a single proof eliminates the DoH fan-out entirely.
fn assert_no_doh_outcalls(env: &PocketIc) {
    const MAX_TICKS: u32 = 30;
    for _ in 0..MAX_TICKS {
        env.tick();
        let outcalls = env.get_canister_http();
        assert!(
            outcalls.is_empty(),
            "expected zero DoH outcalls on the DNSSEC happy path, got {} ({:?})",
            outcalls.len(),
            outcalls.iter().map(|r| &r.url).collect::<Vec<_>>(),
        );
    }
}

// ===================================================================
// DoH path: full end-to-end setup flow
// ===================================================================
//
// `prepare_add` is called without a `dns_proof`, so the canister
// caches no TXT bytes and `smtp_request` falls through to DoH at
// email-arrival time. The test fans out, fulfilling each of the 5
// provider outcalls with a synthesized DNS response carrying the
// test signer's DKIM TXT (DMARC is answered with 404 → strict
// alignment fallback). Mirror of `full_setup_flow_via_dnssec_path`
// above for the DNSSEC path. See `dkim_signer` below for the
// signing logic shared between both flows.

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

// ===================================================================
// DoH path: concurrent in-flight dedup — regression coverage
// ===================================================================
//
// Guards the concurrency bug fixed in #3987. When two `smtp_request`
// calls for addresses in the *same* domain arrive while the DoH cache
// is cold, they dedup onto a single DKIM outcall fan-out (`doh::cache`):
// the first becomes the fetcher, the rest must reuse its result rather
// than each firing their own five-provider fan-out.
//
// The original dedup parked waiters on a shared future that the fetcher
// woke when it published — on the single-threaded canister executor that
// ran the waiter to completion inside the fetcher's call context, tripping
// `RefCell already borrowed` and then mis-routing replies (`already
// replied` / "did not reply"). The single-flight cache is now
// callback-delivery: `smtp_request` is a synchronous accept (status
// `Verifying`) and the verification is detached, so nobody awaits a reply
// cross-call — the hazard can't arise. Dedup still must hold: a second
// request for the same domain joins the in-flight fetch's callback queue
// rather than firing its own fan-out.
//
// This test pins that public-API behaviour with the interleaving made
// explicit (and asserted) rather than left to a fixed tick count:
//   1. Submit request A and drive rounds — *without answering any
//      outcall* — until its provider outcalls are pending; A now owns
//      the fetch.
//   2. Submit request B and confirm it dedups: no new outcalls appear,
//      and its status stays `Verifying` (not terminal) until A publishes.
//   3. Answer the single, deduped DKIM (+ DMARC) outcall set.
//   4. Both requests' statuses reach `RegistrationSucceeded` and their
//      credentials are bound.
#[test]
fn concurrent_doh_dedup_for_same_domain_does_not_trap() {
    let env = env();
    let canister_id = setup_canister(&env);

    // Two identities with *distinct* passkeys. We can't call the shared
    // `fresh_identity` helper twice here: it registers a fixed public
    // key (`sample_webauthn_authn_method(0)`), and II rejects a second
    // registration that reuses a public key.
    let authn_a = sample_webauthn_authn_method(1);
    let authn_b = sample_webauthn_authn_method(2);
    let id_a = create_identity_with_authn_method(&env, canister_id, &authn_a);
    let id_b = create_identity_with_authn_method(&env, canister_id, &authn_b);
    let p_a = authn_a.principal();
    let p_b = authn_b.principal();

    // Two pending challenges, one per identity, both in TEST_DOMAIN.
    let challenge_a = api::email_recovery_credential_prepare_add(
        &env,
        canister_id,
        p_a,
        id_a,
        EmailRecoveryDnsInput {
            address: TEST_ADDRESS.into(),
            dns_proof: None,
        },
    )
    .expect("prepare_add A call failed")
    .expect("prepare_add A failed");
    let challenge_b = api::email_recovery_credential_prepare_add(
        &env,
        canister_id,
        p_b,
        id_b,
        EmailRecoveryDnsInput {
            address: TEST_ADDRESS_2.into(),
            dns_proof: None,
        },
    )
    .expect("prepare_add B call failed")
    .expect("prepare_add B failed");

    // One signer (one selector + domain) → both emails resolve the
    // *same* DKIM FQDN `test1._domainkey.test.example.com`, which is
    // exactly what makes the two verifications dedup in the cache.
    let signer = dkim_signer::TestSigner::new(TEST_DOMAIN, TEST_SELECTOR);
    let now_secs = time(&env) / 1_000_000_000;
    let signed_a = signer.sign_email(SignedEmailParams {
        from: TEST_ADDRESS,
        to: "register@id.ai",
        subject: &challenge_a.nonce,
        body: TEST_BODY,
        timestamp: now_secs,
    });
    let signed_b = signer.sign_email(SignedEmailParams {
        from: TEST_ADDRESS_2,
        to: "register@id.ai",
        subject: &challenge_b.nonce,
        body: TEST_BODY,
        timestamp: now_secs,
    });

    // 1. Submit request A and drive rounds — *without answering any
    //    outcall* — until its provider outcalls are pending. A now owns
    //    the DKIM fetch. Establishing the fetcher first (rather than
    //    submitting both and relying on a fixed tick count) makes the
    //    dedup interleaving explicit: B is guaranteed to find A's
    //    in-flight entry and dedup onto it.
    let msg_a = env
        .submit_call_with_effective_principal(
            canister_id,
            pocket_ic::common::rest::RawEffectivePrincipal::None,
            candid::Principal::anonymous(),
            "smtp_request",
            candid::encode_one(&signed_a.request).expect("encode SmtpRequest A"),
        )
        .expect("submit_call A");
    tick_until_doh_outcalls(&env, DOH_PROVIDER_URLS.len(), 60);

    // 2. Submit request B and give it a few rounds to run its lookup. It
    //    must dedup onto A's in-flight fetch: no *new* DoH outcalls
    //    appear (still just A's fan-out), and B stays in-flight — no
    //    terminal status — until A publishes. We must not answer an
    //    outcall yet: that would let A publish and warm the cache, and B
    //    would hit it instead of deduping, leaving the path untested.
    let msg_b = env
        .submit_call_with_effective_principal(
            canister_id,
            pocket_ic::common::rest::RawEffectivePrincipal::None,
            candid::Principal::anonymous(),
            "smtp_request",
            candid::encode_one(&signed_b.request).expect("encode SmtpRequest B"),
        )
        .expect("submit_call B");
    for _ in 0..5 {
        env.tick();
    }
    assert_eq!(
        pending_doh_outcalls(&env),
        DOH_PROVIDER_URLS.len(),
        "second request must dedup onto the in-flight fetch, not issue its own DoH fan-out",
    );
    // `smtp_request` is a synchronous accept now: B replies `Ok` immediately
    // and its verification joins A's in-flight fetch as a queued callback.
    // The dedup invariant is that B issues no new fan-out (asserted above)
    // and its pending status stays `Verifying` — not terminal — until A's
    // fetch publishes and drains both callbacks.
    let status_b_inflight =
        api::email_recovery_status(&env, canister_id, &challenge_b.nonce).expect("status B");
    assert!(
        matches!(status_b_inflight, EmailRecoveryStatus::Verifying),
        "the dedup waiter must be Verifying (joined A's fetch, no terminal status) \
         before the fetcher publishes, got {status_b_inflight:?}",
    );

    // 3. Answer the single, deduped DKIM + DMARC outcall set.
    fulfill_doh_outcalls(&env, &signer.public_txt_record());

    // 4. Drive both calls to a terminal status — capped, so a
    //    regression that wedges one of them fails the test instead of
    //    hanging it. Read statuses without blocking: we must never
    //    `await` a dedup waiter, which on the pre-fix cache never
    //    completed. `ingress_status` is `None` until a call is terminal.
    for _ in 0..60 {
        if env.ingress_status(msg_a.clone()).is_some()
            && env.ingress_status(msg_b.clone()).is_some()
        {
            break;
        }
        env.tick();
    }
    let status_a = env.ingress_status(msg_a);
    let status_b = env.ingress_status(msg_b);
    let a_ok = matches!(
        &status_a,
        Some(Ok(raw)) if matches!(candid::decode_one::<SmtpResponse>(raw), Ok(SmtpResponse::Ok {}))
    );
    let b_ok = matches!(
        &status_b,
        Some(Ok(raw)) if matches!(candid::decode_one::<SmtpResponse>(raw), Ok(SmtpResponse::Ok {}))
    );

    // Both concurrent verifications must reply Ok. The DoH dedup bug
    // (#3987) made the publisher trap or mis-route its reply
    // ("RefCell already borrowed" / "already replied" / "did not
    // reply"); a regression would resurface as a non-Ok status here.
    assert!(
        a_ok && b_ok,
        "concurrent DoH dedup for the same domain did not complete both \
         requests (regression of #3987).\n  request A => {status_a:?}\n  \
         request B => {status_b:?}",
    );

    // End-to-end: both bindings landed on their anchors.
    for (label, nonce) in [("A", &challenge_a.nonce), ("B", &challenge_b.nonce)] {
        let status =
            api::email_recovery_status(&env, canister_id, nonce).expect("status call failed");
        assert!(
            matches!(status, EmailRecoveryStatus::RegistrationSucceeded),
            "expected RegistrationSucceeded for request {label}, got {status:?}",
        );
    }
}

// ===================================================================
// Tag-contract umbrella smoke tests.
//
// `dkim::tag_checks` exposes two umbrella functions
// (`enforce_signature_header_tag_contract`,
// `enforce_dns_record_tag_contract`) that both verification pipelines
// route their tag enforcement through. Unit and property tests in
// `dkim::tag_checks::tests` cover the umbrellas' verdict directly; the
// two tests below close the loop by exercising each umbrella through
// the canister's public API on the DoH path — submit a synthetic email
// that triggers exactly the failure the umbrella is supposed to catch,
// then assert the canister surfaces it on the polled status. If a
// future refactor wires `smtp_request` past an umbrella (the bug class
// that motivated the facade), these tests fail loudly at the
// integration boundary.
//
// Both tests reuse the same DoH happy-path harness as
// `full_setup_flow_binds_credential_to_anchor`: PocketIC environment,
// real RSA-signed DKIM email, DoH outcalls fulfilled with a custom TXT.
// Only the failure shape differs — they're deliberately tiny.
// ===================================================================

/// Drive the canister through one full DoH-path `smtp_request` using
/// a caller-supplied builder. The builder gets the test signer, the
/// challenge nonce, and the canister's `now_secs`, and returns the
/// `(signed_email, dkim_txt)` pair: the SMTP request to submit, and
/// the DKIM TXT the DoH mock should serve. Returns the polled
/// `EmailRecoveryStatus` after the pipeline runs.
///
/// Shared helper for the umbrella smoke tests below so each test body
/// stays at "what's different about this scenario" granularity.
fn run_doh_path_smoke(
    build: impl FnOnce(&dkim_signer::TestSigner, &str, u64) -> (SignedEmail, Vec<u8>),
) -> EmailRecoveryStatus {
    let env = env();
    let canister_id = setup_canister(&env);
    let (id, p) = fresh_identity(&env, canister_id);

    let challenge =
        api::email_recovery_credential_prepare_add(&env, canister_id, p, id, dns_input())
            .expect("prepare_add call failed")
            .expect("prepare_add failed");

    let signer = dkim_signer::TestSigner::new(TEST_DOMAIN, TEST_SELECTOR);
    let now_secs = time(&env) / 1_000_000_000;
    let (signed, dkim_txt) = build(&signer, &challenge.nonce, now_secs);

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
    // Verification failures on the DoH path still return `Ok` to the
    // gateway (the per-message error goes on the pending challenge);
    // the gateway would otherwise be able to probe which nonces exist.
    assert!(
        matches!(resp, SmtpResponse::Ok {}),
        "smtp_request must return Ok regardless of verification outcome, got {resp:?}"
    );

    api::email_recovery_status(&env, canister_id, &challenge.nonce).expect("status call failed")
}

#[test]
fn doh_path_signature_header_umbrella_rejects_future_dated_t() {
    // Sign an email with `t=now + 10_000s` — well beyond the 60-second
    // skew window in `dkim::tag_checks::CLOCK_SKEW_SECS`. The signature
    // is otherwise structurally valid (correct `bh=`, correct crypto
    // signature over a real RSA key) so the rejection has to come from
    // `enforce_signature_header_tag_contract` short-circuiting at the
    // `t=` check before body-hash or signature verification runs.
    //
    // Validates the *signature-header-only* umbrella is in the
    // smtp_request → verify_email → verify_dkim call chain.
    let status = run_doh_path_smoke(|signer, nonce, now_secs| {
        let signed = signer.sign_email(SignedEmailParams {
            from: TEST_ADDRESS,
            to: "register@id.ai",
            subject: nonce,
            body: TEST_BODY,
            timestamp: now_secs + 10_000,
        });
        // verify_dkim short-circuits before it parses the record, but
        // the DoH mock still needs to satisfy the provider quorum so
        // the canister can read past the fetch.
        (signed, signer.public_txt_record())
    });

    match status {
        EmailRecoveryStatus::Failed(EmailRecoveryError::EmailVerificationFailed(msg)) => {
            assert!(
                msg.contains("SignatureFutureDated"),
                "expected SignatureFutureDated in failure message, got {msg:?}",
            );
        }
        other => {
            panic!("expected Failed(EmailVerificationFailed(SignatureFutureDated…)), got {other:?}")
        }
    }
}

#[test]
fn doh_path_dns_record_umbrella_rejects_testing_mode_key() {
    // Sign an email with valid timestamps and Subject coverage, then
    // serve a DKIM TXT with `; t=y` appended. The signature-header-only
    // umbrella accepts, `parse_dkim_txt` admits the record, and
    // `enforce_dns_record_tag_contract` rejects on the `t=y`
    // testing-mode check — *before* k= match, body hash, or signature
    // verification. Validates the *DNS-record-aware* umbrella is in
    // the call chain and that the canister-served DKIM TXT actually
    // routes through it.
    let status = run_doh_path_smoke(|signer, nonce, now_secs| {
        let signed = signer.sign_email(SignedEmailParams {
            from: TEST_ADDRESS,
            to: "register@id.ai",
            subject: nonce,
            body: TEST_BODY,
            timestamp: now_secs,
        });
        let mut txt = signer.public_txt_record();
        txt.extend_from_slice(b"; t=y");
        (signed, txt)
    });

    match status {
        EmailRecoveryStatus::Failed(EmailRecoveryError::EmailVerificationFailed(msg)) => {
            assert!(
                msg.contains("TestingMode"),
                "expected TestingMode in failure message, got {msg:?}",
            );
        }
        other => panic!("expected Failed(EmailVerificationFailed(TestingMode…)), got {other:?}"),
    }
}

// ===================================================================
// email_recovery_diagnostics — the gateway message_id reaches the FE.
// ===================================================================

/// Happy path: the gateway sets `message_id` on the inbound
/// `SmtpRequest`; the canister retains it on the pending challenge and
/// surfaces it (verbatim) via `email_recovery_diagnostics`, alongside a
/// public reason code + verification path. Confirms the new query + the
/// `canister_tests` API wrapper end-to-end.
#[test]
fn diagnostics_surface_message_id_from_smtp_request() {
    let env = env();
    let canister_id = setup_canister(&env);
    let (id, p) = fresh_identity(&env, canister_id);

    let challenge =
        api::email_recovery_credential_prepare_add(&env, canister_id, p, id, dns_input())
            .expect("prepare_add call failed")
            .expect("prepare_add failed");

    let signer = dkim_signer::TestSigner::new(TEST_DOMAIN, TEST_SELECTOR);
    let now_secs = time(&env) / 1_000_000_000;
    let mut signed = signer.sign_email(SignedEmailParams {
        from: TEST_ADDRESS,
        to: "register@id.ai",
        subject: &challenge.nonce,
        body: TEST_BODY,
        timestamp: now_secs,
    });
    // The gateway attaches a per-message correlation id.
    const GW_ID: &str = "<gw-test-id@gateway.example>";
    signed.request.message_id = Some(GW_ID.into());

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

    let diag = api::email_recovery_diagnostics(&env, canister_id, &challenge.nonce)
        .expect("diagnostics call failed")
        .expect("diagnostics present for a live challenge");
    assert_eq!(diag.message_id.as_deref(), Some(GW_ID));
    assert_eq!(diag.reason_code, "Succeeded");
    assert!(matches!(diag.verification_path, VerificationPath::Doh));
}

/// The core promise: a *verification failure* still retains the gateway
/// `message_id` (the email reached the canister), so a user who reports
/// "it didn't work" has a correlation id to hand to support. Uses the
/// future-dated `t=` failure shape from the umbrella tests above.
#[test]
fn diagnostics_cover_the_failed_path_with_message_id() {
    let env = env();
    let canister_id = setup_canister(&env);
    let (id, p) = fresh_identity(&env, canister_id);

    let challenge =
        api::email_recovery_credential_prepare_add(&env, canister_id, p, id, dns_input())
            .expect("prepare_add call failed")
            .expect("prepare_add failed");

    let signer = dkim_signer::TestSigner::new(TEST_DOMAIN, TEST_SELECTOR);
    let now_secs = time(&env) / 1_000_000_000;
    let mut signed = signer.sign_email(SignedEmailParams {
        from: TEST_ADDRESS,
        to: "register@id.ai",
        subject: &challenge.nonce,
        body: TEST_BODY,
        timestamp: now_secs + 10_000, // future-dated → verification fails
    });
    const GW_ID: &str = "<gw-failed-id@gateway.example>";
    signed.request.message_id = Some(GW_ID.into());

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
    let _ = env
        .await_call_no_ticks(raw_msg_id)
        .expect("await_call_no_ticks");

    let status = api::email_recovery_status(&env, canister_id, &challenge.nonce)
        .expect("status call failed");
    assert!(
        matches!(status, EmailRecoveryStatus::Failed(_)),
        "expected Failed, got {status:?}",
    );

    let diag = api::email_recovery_diagnostics(&env, canister_id, &challenge.nonce)
        .expect("diagnostics call failed")
        .expect("diagnostics present for a failed challenge");
    assert_eq!(diag.message_id.as_deref(), Some(GW_ID));
    assert_eq!(diag.reason_code, "Failed:EmailVerificationFailed");
}

/// An unknown/expired nonce yields `None`, mirroring how
/// `email_recovery_status` collapses both to `Expired` — no oracle.
#[test]
fn diagnostics_returns_none_for_unknown_nonce() {
    let env = env();
    let canister_id = setup_canister(&env);
    let diag = api::email_recovery_diagnostics(&env, canister_id, "II-Recovery-deadbeefcafe1234")
        .expect("diagnostics call failed");
    assert!(diag.is_none());
}

/// The 5 provider URLs the DoH module fans out to. Order doesn't matter
/// because the quorum just needs 3 of them to agree on the same body
/// bytes. Shared by the outcall-fulfilling and outcall-counting helpers
/// so they agree on what counts as a DoH outcall.
const DOH_PROVIDER_URLS: [&str; 5] = [
    "https://cloudflare-dns.com/dns-query",
    "https://dns.google/dns-query",
    "https://dns.quad9.net/dns-query",
    "https://private.canadianshield.cira.ca/dns-query",
    "https://public.dns.iij.jp/dns-query",
];

/// Count the DoH provider outcalls currently pending (unanswered). A
/// dedup waiter's `yield_round` round-trips the management canister
/// (`raw_rand`), which is not an HTTP outcall, so it never inflates this
/// count — letting a test assert "the second request issued no fan-out
/// of its own".
fn pending_doh_outcalls(env: &PocketIc) -> usize {
    env.get_canister_http()
        .iter()
        .filter(|r| DOH_PROVIDER_URLS.iter().any(|p| r.url.starts_with(p)))
        .count()
}

/// Drive rounds — *without answering anything* — until at least `want`
/// DoH outcalls are pending, or panic after `max_ticks`. Lets a test
/// wait for a fetcher to fan out before acting, instead of guessing a
/// fixed tick count.
fn tick_until_doh_outcalls(env: &PocketIc, want: usize, max_ticks: u32) {
    for _ in 0..max_ticks {
        if pending_doh_outcalls(env) >= want {
            return;
        }
        env.tick();
    }
    panic!(
        "expected >= {want} pending DoH outcalls within {max_ticks} ticks, saw {}",
        pending_doh_outcalls(env)
    );
}

/// Drive PocketIC forward until each of the 5 DoH provider outcalls
/// has been seen, fulfilling them with the supplied DKIM TXT bytes.
/// DMARC outcalls are answered with NXDOMAIN (the verifier's "no
/// DMARC record" path requires DKIM `d=` to equal From: domain — true
/// in this test).
fn fulfill_doh_outcalls(env: &PocketIc, dkim_txt: &[u8]) {
    let providers = DOH_PROVIDER_URLS;
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
    // for DMARC. We answer all of those with an authoritative
    // NXDOMAIN (status=200 + RCODE=3 wire-format body), which the
    // verifier surfaces as `DohError::NoAnswer` and treats as "no
    // DMARC published" — the strict-alignment fallback (DKIM `d=` ==
    // From: domain) is satisfied by our test setup. A bare HTTP 404
    // would NOT work: per `classify_upstream` in `doh::mod`, any
    // upstream non-200 collapses onto the upstream-error sentinel
    // (defending against a counterfeit-NoAnswer attack) and the DMARC
    // handler would then refuse to fall back, treating it as a real
    // outage.
    let dmarc_deadline = ticks + 60;
    while ticks < dmarc_deadline {
        env.tick();
        ticks += 1;
        for req in env.get_canister_http() {
            let response = MockCanisterHttpResponse {
                subnet_id: req.subnet_id,
                request_id: req.request_id,
                response: CanisterHttpResponse::CanisterHttpReply(CanisterHttpReply {
                    status: 200,
                    headers: vec![],
                    body: fake_nxdomain_dns_response(),
                }),
                additional_responses: vec![],
            };
            env.mock_canister_http_response(response);
        }
    }
}

/// Build a wire-format DNS NXDOMAIN response (RCODE=3, ANCOUNT=0).
/// The parser collapses RCODE=3 onto `ParseError::NoAnswer`, which
/// the transform turns into `Outcome::NoAnswer` and the quorum
/// surfaces as `DohError::NoAnswer` — the DMARC handler treats that
/// as "no policy published" and falls back to strict alignment.
fn fake_nxdomain_dns_response() -> Vec<u8> {
    let mut out = Vec::new();
    // Header: flags=0x8183 (QR + RD + RA + RCODE=3), QDCOUNT=1, ANCOUNT=0.
    out.extend_from_slice(&[0, 0, 0x81, 0x83, 0, 1, 0, 0, 0, 0, 0, 0]);
    // Question section — the verifier doesn't re-parse the QNAME, but
    // a well-formed NXDOMAIN still carries the question echoed back.
    out.extend_from_slice(b"\x07example\x03com\x00\x00\x10\x00\x01");
    out
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
                    to: vec![SmtpAddress {
                        user: to_user.into(),
                        domain: to_domain.into(),
                    }],
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
                message_id: None,
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

// ===================================================================
// In-test DNSSEC signer (Ed25519, single-link chain)
// ===================================================================
//
// Builds a fully-signed `DnsProofBundle` (root DNSKEY → leaf-zone
// DS+DNSKEY → leaf TXT) plus the matching `DnssecRootAnchor`. Each
// RRset is signed using `ed25519_dalek` against bytes constructed by
// the same canonicalisation rules the canister-side verifier
// implements in `crate::dnssec::canonical`. We replicate those rules
// here rather than depending on `internet_identity::dnssec` directly
// so the test stays self-contained.
//
// The chain is intentionally minimal — one delegation link (root →
// `<domain>`), one RRSIG per RRset, one Ed25519 key per zone. That
// keeps the signer compact while still exercising every verifier
// branch that matters for happy-path validation.

mod dnssec_signer {
    use ed25519_dalek::{Signer, SigningKey};
    use internet_identity_interface::internet_identity::types::dnssec::{
        DelegationChain, DelegationLink, DnsProofBundle, Rrsig, SignedRRset,
    };
    use internet_identity_interface::internet_identity::types::DnssecRootAnchor;
    use serde_bytes::ByteBuf;
    use sha2::{Digest, Sha256};

    const ALG_ED25519: u8 = 15;
    const PROTOCOL_DNSSEC: u8 = 3;
    /// SEP (bit 0) | ZONE (bit 8). The verifier's KSK match only
    /// requires SEP, but real DNSKEYs always set ZONE alongside.
    const FLAGS_KSK_ZONE: u16 = 0x0001 | 0x0100;
    const TYPE_TXT: u16 = 16;
    const TYPE_DS: u16 = 43;
    const TYPE_DNSKEY: u16 = 48;
    const CLASS_IN: u16 = 1;
    const DIGEST_TYPE_SHA256: u8 = 2;

    pub struct ChainOut {
        pub anchor: DnssecRootAnchor,
        /// Skeleton bundle (chain only, `leaf = None`). Callers
        /// compose a full bundle for prepare/submit by cloning this
        /// and setting `leaf` to one of `dkim_leaf` / `dmarc_leaf`.
        pub skeleton: DnsProofBundle,
        /// Signed DKIM leaf at `<selector>._domainkey.<domain>`,
        /// signed under the chain's deepest zone DNSKEY.
        pub dkim_leaf: SignedRRset,
        /// Signed DMARC leaf at `_dmarc.<domain>`, when `dmarc_txt`
        /// was supplied. Same zone as `dkim_leaf`. Tests compose
        /// the prepare-time bundle as `skeleton + dmarc_leaf` and
        /// the submit-leaf-time bundle as `skeleton + dkim_leaf`.
        pub dmarc_leaf: Option<SignedRRset>,
    }

    /// Build a one-link chain (root → `<domain>`) and the
    /// corresponding signed DKIM (and optional DMARC) TXT leaves.
    /// Returns the components separately — single-leaf
    /// `DnsProofBundle` constraints mean callers compose the
    /// bundle they need (skeleton + DMARC at prepare,
    /// skeleton + DKIM at submit_dkim_leaf, etc).
    /// `now_secs` is the wall-clock baseline; RRSIGs are valid
    /// from 60 s before to 7 d after.
    pub fn build_chain(
        domain: &str,
        selector: &str,
        dkim_txt: &[u8],
        dmarc_txt: Option<&[u8]>,
        now_secs: u32,
    ) -> ChainOut {
        let inception = now_secs.saturating_sub(60);
        let expiration = now_secs.saturating_add(7 * 24 * 3600);

        let root_key = ZoneKey::new(b"\x00".to_vec(), [1u8; 32]);
        let zone_wire = encode_dns_name(domain);
        let zone_key = ZoneKey::new(zone_wire.clone(), [2u8; 32]);

        let root_dnskey = sign_rrset(
            &root_key,
            &root_key.owner_name,
            TYPE_DNSKEY,
            vec![root_key.dnskey_rdata()],
            0,
            inception,
            expiration,
        );

        let child_ds = sign_rrset(
            &root_key,
            &zone_wire,
            TYPE_DS,
            vec![ds_rdata_for(&zone_key)],
            count_labels(&zone_wire),
            inception,
            expiration,
        );

        let child_dnskey = sign_rrset(
            &zone_key,
            &zone_wire,
            TYPE_DNSKEY,
            vec![zone_key.dnskey_rdata()],
            count_labels(&zone_wire),
            inception,
            expiration,
        );

        let dkim_owner = encode_dns_name(&format!("{selector}._domainkey.{domain}"));
        let dkim_leaf = sign_rrset(
            &zone_key,
            &dkim_owner,
            TYPE_TXT,
            vec![pack_txt_rdata(dkim_txt)],
            count_labels(&dkim_owner),
            inception,
            expiration,
        );

        let dmarc_leaf = dmarc_txt.map(|dmarc| {
            let dmarc_owner = encode_dns_name(&format!("_dmarc.{domain}"));
            sign_rrset(
                &zone_key,
                &dmarc_owner,
                TYPE_TXT,
                vec![pack_txt_rdata(dmarc)],
                count_labels(&dmarc_owner),
                inception,
                expiration,
            )
        });

        ChainOut {
            anchor: make_anchor(&root_key),
            skeleton: DnsProofBundle {
                hops: vec![],
                root_dnskey,
                chains: vec![DelegationChain {
                    links: vec![DelegationLink {
                        child_ds,
                        child_dnskey,
                    }],
                }],
            },
            dkim_leaf,
            dmarc_leaf,
        }
    }

    struct ZoneKey {
        signing_key: SigningKey,
        public_key: [u8; 32],
        owner_name: Vec<u8>,
    }

    impl ZoneKey {
        fn new(owner_name: Vec<u8>, secret: [u8; 32]) -> Self {
            let signing_key = SigningKey::from_bytes(&secret);
            let public_key = signing_key.verifying_key().to_bytes();
            Self {
                signing_key,
                public_key,
                owner_name,
            }
        }

        fn dnskey_rdata(&self) -> Vec<u8> {
            let mut out = Vec::with_capacity(36);
            out.extend_from_slice(&FLAGS_KSK_ZONE.to_be_bytes());
            out.push(PROTOCOL_DNSSEC);
            out.push(ALG_ED25519);
            out.extend_from_slice(&self.public_key);
            out
        }

        fn key_tag(&self) -> u16 {
            dnskey_key_tag(&self.dnskey_rdata())
        }
    }

    fn sign_rrset(
        signer: &ZoneKey,
        owner_name: &[u8],
        rtype: u16,
        rdatas: Vec<Vec<u8>>,
        labels: u8,
        inception: u32,
        expiration: u32,
    ) -> SignedRRset {
        let mut rrsig = Rrsig {
            type_covered: rtype,
            algorithm: ALG_ED25519,
            labels,
            original_ttl: 3600,
            expiration,
            inception,
            key_tag: signer.key_tag(),
            signer_name: ByteBuf::from(signer.owner_name.clone()),
            signature: ByteBuf::from(Vec::new()),
        };
        let signed_data = build_signed_data(owner_name, rtype, &rrsig, &rdatas);
        let sig = signer.signing_key.sign(&signed_data);
        rrsig.signature = ByteBuf::from(sig.to_bytes().to_vec());
        SignedRRset {
            name: ByteBuf::from(owner_name.to_vec()),
            rtype,
            rdata: rdatas.into_iter().map(ByteBuf::from).collect(),
            ttl: 3600,
            rrsig,
        }
    }

    fn ds_rdata_for(child: &ZoneKey) -> Vec<u8> {
        let dnskey_rdata = child.dnskey_rdata();
        let canon = canonicalize_name(&child.owner_name);
        let mut digest_input = Vec::with_capacity(canon.len() + dnskey_rdata.len());
        digest_input.extend_from_slice(&canon);
        digest_input.extend_from_slice(&dnskey_rdata);
        let digest = Sha256::digest(&digest_input);

        let mut out = Vec::with_capacity(4 + 32);
        out.extend_from_slice(&child.key_tag().to_be_bytes());
        out.push(ALG_ED25519);
        out.push(DIGEST_TYPE_SHA256);
        out.extend_from_slice(&digest);
        out
    }

    fn make_anchor(root: &ZoneKey) -> DnssecRootAnchor {
        let dnskey_rdata = root.dnskey_rdata();
        let canon = canonicalize_name(&root.owner_name);
        let mut digest_input = Vec::with_capacity(canon.len() + dnskey_rdata.len());
        digest_input.extend_from_slice(&canon);
        digest_input.extend_from_slice(&dnskey_rdata);
        let digest = Sha256::digest(&digest_input);

        DnssecRootAnchor {
            key_tag: dnskey_key_tag(&dnskey_rdata),
            algorithm: ALG_ED25519,
            digest_type: DIGEST_TYPE_SHA256,
            digest: ByteBuf::from(digest.to_vec()),
        }
    }

    // -------- canonicalisation (mirrors crate::dnssec::canonical) --------

    fn build_signed_data(
        owner_name: &[u8],
        rtype: u16,
        rrsig: &Rrsig,
        rdatas: &[Vec<u8>],
    ) -> Vec<u8> {
        let canon_name = canonicalize_name(owner_name);
        let mut sorted: Vec<&Vec<u8>> = rdatas.iter().collect();
        sorted.sort_by(|a, b| a.as_slice().cmp(b.as_slice()));

        let mut out = rrsig_rdata_for_signing(rrsig);
        for rdata in sorted {
            out.extend_from_slice(&rr_canonical(&canon_name, rtype, rrsig.original_ttl, rdata));
        }
        out
    }

    fn rrsig_rdata_for_signing(rrsig: &Rrsig) -> Vec<u8> {
        let mut out = Vec::with_capacity(18 + rrsig.signer_name.len());
        out.extend_from_slice(&rrsig.type_covered.to_be_bytes());
        out.push(rrsig.algorithm);
        out.push(rrsig.labels);
        out.extend_from_slice(&rrsig.original_ttl.to_be_bytes());
        out.extend_from_slice(&rrsig.expiration.to_be_bytes());
        out.extend_from_slice(&rrsig.inception.to_be_bytes());
        out.extend_from_slice(&rrsig.key_tag.to_be_bytes());
        out.extend_from_slice(&canonicalize_name(&rrsig.signer_name));
        out
    }

    fn rr_canonical(name_canonical: &[u8], rtype: u16, original_ttl: u32, rdata: &[u8]) -> Vec<u8> {
        let mut out = Vec::with_capacity(name_canonical.len() + 10 + rdata.len());
        out.extend_from_slice(name_canonical);
        out.extend_from_slice(&rtype.to_be_bytes());
        out.extend_from_slice(&CLASS_IN.to_be_bytes());
        out.extend_from_slice(&original_ttl.to_be_bytes());
        out.extend_from_slice(&(rdata.len() as u16).to_be_bytes());
        out.extend_from_slice(rdata);
        out
    }

    fn canonicalize_name(wire: &[u8]) -> Vec<u8> {
        let mut out = Vec::with_capacity(wire.len());
        let mut i = 0;
        while i < wire.len() {
            let len = wire[i];
            out.push(len);
            if len == 0 {
                return out;
            }
            let label_end = i + 1 + (len as usize);
            for &b in &wire[i + 1..label_end] {
                out.push(b.to_ascii_lowercase());
            }
            i = label_end;
        }
        out
    }

    fn dnskey_key_tag(dnskey_rdata: &[u8]) -> u16 {
        let mut acc: u32 = 0;
        for (i, &b) in dnskey_rdata.iter().enumerate() {
            if i & 1 == 0 {
                acc = acc.wrapping_add((b as u32) << 8);
            } else {
                acc = acc.wrapping_add(b as u32);
            }
        }
        acc = acc.wrapping_add((acc >> 16) & 0xFFFF);
        (acc & 0xFFFF) as u16
    }

    fn count_labels(wire: &[u8]) -> u8 {
        let mut count = 0u8;
        let mut i = 0;
        while i < wire.len() {
            let len = wire[i] as usize;
            if len == 0 {
                break;
            }
            count = count.saturating_add(1);
            i += 1 + len;
        }
        count
    }

    fn encode_dns_name(s: &str) -> Vec<u8> {
        let s = s.trim_end_matches('.');
        let mut out = Vec::new();
        for label in s.split('.') {
            assert!(label.len() <= 63, "DNS label too long");
            out.push(label.len() as u8);
            out.extend_from_slice(label.as_bytes());
        }
        out.push(0);
        out
    }

    fn pack_txt_rdata(text: &[u8]) -> Vec<u8> {
        let mut out = Vec::new();
        for chunk in text.chunks(255) {
            out.push(chunk.len() as u8);
            out.extend_from_slice(chunk);
        }
        out
    }
}
