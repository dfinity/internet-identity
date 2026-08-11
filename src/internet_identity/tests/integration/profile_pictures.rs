//! Integration tests for the profile-picture flow.
//!
//! Covers what unit tests can't: that the picture survives the Candid
//! boundary and a canister upgrade, that it is authorized per identity, that
//! the 100 KiB cap holds against a real ingress message, and that a
//! max-size picture makes it all the way through
//! `list_available_attributes` → `prepare_icrc3_attributes` →
//! `get_icrc3_attributes` — the path where its size could plausibly run into
//! a platform limit.

use crate::v2_api::authn_method_test_helpers::{
    create_identity_with_authn_method, sample_webauthn_authn_method, test_authn_method,
};
use canister_tests::{api::internet_identity as api, framework::*};
use internet_identity_interface::internet_identity::types::attributes::{
    AttributeSpec, GetIcrc3AttributeRequest, ListAvailableAttributesRequest,
    PrepareIcrc3AttributeError, PrepareIcrc3AttributeRequest,
};
use internet_identity_interface::internet_identity::types::profile_picture::{
    ProfilePictureError, ProfilePictureMediaType, PROFILE_PICTURE_MAX_BYTES,
    PROFILE_PICTURE_MIN_BYTES,
};
use pocket_ic::PocketIc;

const ORIGIN: &str = "https://some-dapp.com";

/// Bytes that sniff as PNG, padded to `len` with `fill` so different calls
/// produce different pictures of a chosen size.
fn png(len: usize, fill: u8) -> Vec<u8> {
    let mut bytes = vec![0x89, b'P', b'N', b'G', 0x0d, 0x0a, 0x1a, 0x0a];
    bytes.resize(len.max(bytes.len()), fill);
    bytes
}

fn jpeg(len: usize, fill: u8) -> Vec<u8> {
    let mut bytes = vec![0xff, 0xd8, 0xff, 0xe0];
    bytes.resize(len.max(bytes.len()), fill);
    bytes
}

fn setup() -> (PocketIc, candid::Principal) {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    (env, canister_id)
}

/// The `data:` URL the canister certifies for `bytes` — the same string
/// `ProfilePicture::to_data_url` builds, so a test can assert on the wire
/// value without duplicating the encoder.
fn data_url(media_type: ProfilePictureMediaType, bytes: &[u8]) -> String {
    use base64::engine::general_purpose::STANDARD as BASE64;
    use base64::Engine;
    format!(
        "data:{};base64,{}",
        media_type.as_str(),
        BASE64.encode(bytes)
    )
}

#[test]
fn should_set_get_and_remove_a_picture() {
    let (env, canister_id) = setup();
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);
    let principal = authn_method.principal();

    // Nothing set to begin with, and `identity_info` says so.
    assert_eq!(
        api::profile_picture_get(&env, canister_id, principal, identity_number)
            .expect("failed to call profile_picture_get")
            .expect("profile_picture_get error"),
        None
    );
    let info = api::api_v2::identity_info(&env, canister_id, principal, identity_number)
        .expect("failed to call identity_info")
        .expect("identity_info error");
    assert_eq!(info.profile_picture, None);

    let bytes = png(4096, 0x11);
    api::profile_picture_set(&env, canister_id, principal, identity_number, bytes.clone())
        .expect("failed to call profile_picture_set")
        .expect("profile_picture_set error");

    let stored = api::profile_picture_get(&env, canister_id, principal, identity_number)
        .expect("failed to call profile_picture_get")
        .expect("profile_picture_get error")
        .expect("a picture must be set");
    assert_eq!(stored.bytes.as_ref(), bytes.as_slice());
    // The media type is derived from the bytes, never supplied.
    assert_eq!(stored.media_type, ProfilePictureMediaType::Png);

    // `identity_info` reports the summary, and deliberately not the bytes.
    let info = api::api_v2::identity_info(&env, canister_id, principal, identity_number)
        .expect("failed to call identity_info")
        .expect("identity_info error");
    let metadata = info.profile_picture.expect("metadata must be reported");
    assert_eq!(metadata.size_bytes, bytes.len() as u64);
    assert_eq!(metadata.media_type, ProfilePictureMediaType::Png);
    assert_eq!(metadata.uploaded_at, stored.uploaded_at);

    api::profile_picture_remove(&env, canister_id, principal, identity_number)
        .expect("failed to call profile_picture_remove")
        .expect("profile_picture_remove error");

    assert_eq!(
        api::profile_picture_get(&env, canister_id, principal, identity_number)
            .expect("failed to call profile_picture_get")
            .expect("profile_picture_get error"),
        None
    );
}

/// Zero or one: setting again replaces rather than accumulating, which is
/// what bounds an identity's footprint however often the user changes it.
#[test]
fn should_replace_rather_than_accumulate() {
    let (env, canister_id) = setup();
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);
    let principal = authn_method.principal();

    api::profile_picture_set(
        &env,
        canister_id,
        principal,
        identity_number,
        png(2048, 0x22),
    )
    .expect("failed to call profile_picture_set")
    .expect("first set must succeed");

    let replacement = jpeg(3072, 0x33);
    api::profile_picture_set(
        &env,
        canister_id,
        principal,
        identity_number,
        replacement.clone(),
    )
    .expect("failed to call profile_picture_set")
    .expect("second set must succeed");

    let stored = api::profile_picture_get(&env, canister_id, principal, identity_number)
        .expect("failed to call profile_picture_get")
        .expect("profile_picture_get error")
        .expect("a picture must be set");
    assert_eq!(stored.bytes.as_ref(), replacement.as_slice());
    assert_eq!(stored.media_type, ProfilePictureMediaType::Jpeg);
}

#[test]
fn should_reject_a_picture_over_the_cap() {
    let (env, canister_id) = setup();
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);
    let principal = authn_method.principal();

    // Exactly at the cap is accepted...
    api::profile_picture_set(
        &env,
        canister_id,
        principal,
        identity_number,
        png(PROFILE_PICTURE_MAX_BYTES, 0x44),
    )
    .expect("failed to call profile_picture_set")
    .expect("a picture of exactly the maximum size must be accepted");

    // ...one byte over is not.
    let result = api::profile_picture_set(
        &env,
        canister_id,
        principal,
        identity_number,
        png(PROFILE_PICTURE_MAX_BYTES + 1, 0x44),
    )
    .expect("failed to call profile_picture_set");
    assert_eq!(
        result,
        Err(ProfilePictureError::TooLarge {
            size_bytes: PROFILE_PICTURE_MAX_BYTES as u64 + 1,
            max_bytes: PROFILE_PICTURE_MAX_BYTES as u64,
        })
    );

    // The rejected call must not have replaced the accepted picture.
    let stored = api::profile_picture_get(&env, canister_id, principal, identity_number)
        .expect("failed to call profile_picture_get")
        .expect("profile_picture_get error")
        .expect("the accepted picture must survive a rejected set");
    assert_eq!(stored.bytes.len(), PROFILE_PICTURE_MAX_BYTES);
}

#[test]
fn should_reject_bytes_that_are_not_an_accepted_image() {
    let (env, canister_id) = setup();
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);
    let principal = authn_method.principal();

    // An SVG is a script-execution vector in an `<img>` tag on some
    // consumers, so it must not be storable however the caller labels it.
    for (label, bytes) in [
        (
            "svg",
            b"<svg xmlns=\"http://www.w3.org/2000/svg\"></svg>".to_vec(),
        ),
        (
            "html",
            b"<!DOCTYPE html><html><body>hi</body></html>".to_vec(),
        ),
        ("gif", b"GIF89a\0\0\0\0\0\0\0\0\0\0\0\0".to_vec()),
    ] {
        let result = api::profile_picture_set(&env, canister_id, principal, identity_number, bytes)
            .expect("failed to call profile_picture_set");
        assert_eq!(
            result,
            Err(ProfilePictureError::UnsupportedMediaType),
            "should have rejected {label}"
        );
    }

    let result =
        api::profile_picture_set(&env, canister_id, principal, identity_number, png(8, 0x55))
            .expect("failed to call profile_picture_set");
    assert_eq!(
        result,
        Err(ProfilePictureError::TooSmall {
            size_bytes: 8,
            min_bytes: PROFILE_PICTURE_MIN_BYTES as u64,
        })
    );
}

#[test]
fn should_report_not_set_when_removing_nothing() {
    let (env, canister_id) = setup();
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);
    let principal = authn_method.principal();

    assert_eq!(
        api::profile_picture_remove(&env, canister_id, principal, identity_number)
            .expect("failed to call profile_picture_remove"),
        Err(ProfilePictureError::NotSet)
    );

    api::profile_picture_set(
        &env,
        canister_id,
        principal,
        identity_number,
        png(1024, 0x66),
    )
    .expect("failed to call profile_picture_set")
    .expect("set must succeed");
    api::profile_picture_remove(&env, canister_id, principal, identity_number)
        .expect("failed to call profile_picture_remove")
        .expect("first remove must succeed");
    // A double-remove is reported rather than silently succeeding.
    assert_eq!(
        api::profile_picture_remove(&env, canister_id, principal, identity_number)
            .expect("failed to call profile_picture_remove"),
        Err(ProfilePictureError::NotSet)
    );
}

/// The picture is per identity, and only that identity's devices may read or
/// write it.
#[test]
fn should_require_authorization() {
    let (env, canister_id) = setup();
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);
    let principal = authn_method.principal();

    api::profile_picture_set(
        &env,
        canister_id,
        principal,
        identity_number,
        png(1024, 0x77),
    )
    .expect("failed to call profile_picture_set")
    .expect("set must succeed");

    let stranger = principal_2();
    for (label, result) in [
        (
            "get",
            api::profile_picture_get(&env, canister_id, stranger, identity_number)
                .expect("failed to call profile_picture_get")
                .map(|_| ()),
        ),
        (
            "remove",
            api::profile_picture_remove(&env, canister_id, stranger, identity_number)
                .expect("failed to call profile_picture_remove"),
        ),
        (
            "set",
            api::profile_picture_set(
                &env,
                canister_id,
                stranger,
                identity_number,
                png(1024, 0x88),
            )
            .expect("failed to call profile_picture_set"),
        ),
    ] {
        assert_eq!(
            result,
            Err(ProfilePictureError::Unauthorized(stranger)),
            "{label} should have been rejected for an unauthorized caller"
        );
    }

    // ...and the picture is untouched.
    let stored = api::profile_picture_get(&env, canister_id, principal, identity_number)
        .expect("failed to call profile_picture_get")
        .expect("profile_picture_get error")
        .expect("the picture must survive unauthorized calls");
    assert_eq!(stored.bytes.as_ref(), png(1024, 0x77).as_slice());
}

/// The whole point of the feature: a relying party can obtain the picture as
/// a certified attribute. Uses a max-size picture, because its ~137 KB data
/// URL travels in both the `prepare` response and the `get` request — the
/// place a size limit would bite.
#[test]
fn should_certify_a_max_size_picture_for_a_relying_party() {
    let (env, canister_id) = setup();
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);
    let principal = authn_method.principal();

    let bytes = png(PROFILE_PICTURE_MAX_BYTES, 0x99);
    api::profile_picture_set(&env, canister_id, principal, identity_number, bytes.clone())
        .expect("failed to call profile_picture_set")
        .expect("set must succeed");

    let expected_url = data_url(ProfilePictureMediaType::Png, &bytes);

    // The listing shows the consent screen exactly what will be certified.
    let listed = api::list_available_attributes(
        &env,
        canister_id,
        principal,
        ListAvailableAttributesRequest {
            identity_number,
            attributes: Some(vec!["profile_picture".to_string()]),
        },
    )
    .expect("failed to call list_available_attributes")
    .expect("list_available_attributes error");
    assert_eq!(
        listed,
        vec![(
            "profile_picture".to_string(),
            expected_url.clone().into_bytes()
        )]
    );

    // Certify it, pinning the value the listing returned — the round trip the
    // consent screen performs.
    let prepare_response = api::prepare_icrc3_attributes(
        &env,
        canister_id,
        principal,
        PrepareIcrc3AttributeRequest {
            identity_number,
            origin: ORIGIN.to_string(),
            unmapped_origin: None,
            account_number: None,
            attributes: vec![AttributeSpec {
                key: "profile_picture".into(),
                value: Some(expected_url.clone().into_bytes()),
                omit_scope: true,
            }],
            nonce: vec![0u8; 32],
        },
    )
    .expect("failed to call prepare_icrc3_attributes")
    .expect("prepare_icrc3_attributes error");

    // The certified message carries the picture, so it is at least as large.
    assert!(
        prepare_response.message.len() >= expected_url.len(),
        "the certified message ({} bytes) must carry the {}-byte data URL",
        prepare_response.message.len(),
        expected_url.len()
    );

    let get_response = api::get_icrc3_attributes(
        &env,
        canister_id,
        principal,
        GetIcrc3AttributeRequest {
            identity_number,
            origin: ORIGIN.to_string(),
            account_number: None,
            message: prepare_response.message.clone(),
        },
    )
    .expect("failed to call get_icrc3_attributes")
    .expect("get_icrc3_attributes error");
    assert!(!get_response.signature.is_empty());
}

/// A picture changed between listing and certification must fail the call
/// rather than certify bytes the user never consented to.
#[test]
fn should_reject_a_stale_pinned_picture() {
    let (env, canister_id) = setup();
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);
    let principal = authn_method.principal();

    let consented_to = png(2048, 0xaa);
    api::profile_picture_set(
        &env,
        canister_id,
        principal,
        identity_number,
        consented_to.clone(),
    )
    .expect("failed to call profile_picture_set")
    .expect("set must succeed");

    // The user changes their picture after the consent screen listed it.
    api::profile_picture_set(
        &env,
        canister_id,
        principal,
        identity_number,
        png(2048, 0xbb),
    )
    .expect("failed to call profile_picture_set")
    .expect("replacement must succeed");

    let result = api::prepare_icrc3_attributes(
        &env,
        canister_id,
        principal,
        PrepareIcrc3AttributeRequest {
            identity_number,
            origin: ORIGIN.to_string(),
            unmapped_origin: None,
            account_number: None,
            attributes: vec![AttributeSpec {
                key: "profile_picture".into(),
                value: Some(data_url(ProfilePictureMediaType::Png, &consented_to).into_bytes()),
                omit_scope: true,
            }],
            nonce: vec![0u8; 32],
        },
    )
    .expect("failed to call prepare_icrc3_attributes");

    match result {
        Err(PrepareIcrc3AttributeError::AttributeMismatch { problems }) => assert!(
            problems
                .iter()
                .any(|p| p.contains("Attribute value mismatch for profile_picture")),
            "expected a value-mismatch rejection in {problems:?}"
        ),
        other => panic!("expected AttributeMismatch, got {other:?}"),
    }
}

/// An identity with no picture must not be able to certify one, and the
/// listing must not offer it.
#[test]
fn should_not_certify_a_picture_that_is_not_set() {
    let (env, canister_id) = setup();
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);
    let principal = authn_method.principal();

    let listed = api::list_available_attributes(
        &env,
        canister_id,
        principal,
        ListAvailableAttributesRequest {
            identity_number,
            attributes: Some(vec!["profile_picture".to_string()]),
        },
    )
    .expect("failed to call list_available_attributes")
    .expect("list_available_attributes error");
    assert_eq!(listed, vec![]);

    let result = api::prepare_icrc3_attributes(
        &env,
        canister_id,
        principal,
        PrepareIcrc3AttributeRequest {
            identity_number,
            origin: ORIGIN.to_string(),
            unmapped_origin: None,
            account_number: None,
            attributes: vec![AttributeSpec {
                key: "profile_picture".into(),
                value: None,
                omit_scope: true,
            }],
            nonce: vec![0u8; 32],
        },
    )
    .expect("failed to call prepare_icrc3_attributes");

    match result {
        Err(PrepareIcrc3AttributeError::AttributeMismatch { problems }) => assert!(
            problems
                .iter()
                .any(|p| p.contains("No profile picture is set on this identity")),
            "expected a no-picture rejection in {problems:?}"
        ),
        other => panic!("expected AttributeMismatch, got {other:?}"),
    }
}

/// The picture lives in its own stable map, so it has to survive an upgrade
/// the same way the anchor does.
#[test]
fn should_survive_an_upgrade() {
    let (env, canister_id) = setup();
    let authn_method = test_authn_method();
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method);
    let principal = authn_method.principal();

    let bytes = png(8192, 0xcc);
    api::profile_picture_set(&env, canister_id, principal, identity_number, bytes.clone())
        .expect("failed to call profile_picture_set")
        .expect("set must succeed");
    let before = api::profile_picture_get(&env, canister_id, principal, identity_number)
        .expect("failed to call profile_picture_get")
        .expect("profile_picture_get error")
        .expect("a picture must be set");

    upgrade_ii_canister(&env, canister_id, II_WASM.clone());

    let after = api::profile_picture_get(&env, canister_id, principal, identity_number)
        .expect("failed to call profile_picture_get")
        .expect("profile_picture_get error")
        .expect("the picture must survive the upgrade");
    assert_eq!(after, before);
    assert_eq!(after.bytes.as_ref(), bytes.as_slice());
}

/// Two identities' pictures are independent — the map is keyed by identity
/// number, and a bug there would cross-wire avatars between users.
#[test]
fn should_keep_pictures_independent_per_identity() {
    let (env, canister_id) = setup();

    let first_method = test_authn_method();
    let first = create_identity_with_authn_method(&env, canister_id, &first_method);
    let second_method = sample_webauthn_authn_method(1);
    let second = create_identity_with_authn_method(&env, canister_id, &second_method);

    let first_bytes = png(1024, 0x01);
    let second_bytes = jpeg(2048, 0x02);
    api::profile_picture_set(
        &env,
        canister_id,
        first_method.principal(),
        first,
        first_bytes.clone(),
    )
    .expect("failed to call profile_picture_set")
    .expect("set must succeed");
    api::profile_picture_set(
        &env,
        canister_id,
        second_method.principal(),
        second,
        second_bytes.clone(),
    )
    .expect("failed to call profile_picture_set")
    .expect("set must succeed");

    assert_eq!(
        api::profile_picture_get(&env, canister_id, first_method.principal(), first)
            .expect("failed to call profile_picture_get")
            .expect("profile_picture_get error")
            .expect("first identity must have a picture")
            .bytes
            .as_ref(),
        first_bytes.as_slice()
    );
    assert_eq!(
        api::profile_picture_get(&env, canister_id, second_method.principal(), second)
            .expect("failed to call profile_picture_get")
            .expect("profile_picture_get error")
            .expect("second identity must have a picture")
            .bytes
            .as_ref(),
        second_bytes.as_slice()
    );

    // Removing one leaves the other alone.
    api::profile_picture_remove(&env, canister_id, first_method.principal(), first)
        .expect("failed to call profile_picture_remove")
        .expect("remove must succeed");
    assert_eq!(
        api::profile_picture_get(&env, canister_id, first_method.principal(), first)
            .expect("failed to call profile_picture_get")
            .expect("profile_picture_get error"),
        None
    );
    assert!(
        api::profile_picture_get(&env, canister_id, second_method.principal(), second)
            .expect("failed to call profile_picture_get")
            .expect("profile_picture_get error")
            .is_some(),
        "the other identity's picture must be untouched"
    );
}
