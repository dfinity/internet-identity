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
    create_identity_with_authn_method, test_authn_method,
};
use canister_tests::{api::internet_identity as api, framework::*};
use internet_identity_interface::internet_identity::types::email_recovery::{
    EmailRecoveryDnsInput, EmailRecoveryError, EmailRecoveryStatus,
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
/// entry for `id.ai` so `smtp_request`'s recipient dispatch accepts
/// `register@id.ai` (the address the tests send `to`).
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
