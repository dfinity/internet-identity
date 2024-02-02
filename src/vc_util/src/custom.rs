//! Custom utils for handling specific VCs/VPs.

use crate::issuer_api::{ArgumentValue, CredentialSpec};
use crate::{
    inconsistent_jwt_claims, validate_claim, validate_claims_match_spec,
    verify_ii_presentation_jwt_with_canister_ids, CredentialVerificationError,
    PresentationVerificationError, VcFlowSigners,
};

use crate::PresentationVerificationError::InvalidPresentationJwt;
use candid::Principal;
use identity_credential::validator::JwtValidationError;
use std::collections::HashMap;

/// Validates the provided presentation `vp_jwt`, checking the following conditions:
///  - the presentation is a valid presentation of the IC Attribute Sharing flow;
///    in particular the presentation contains IdAlias and a matching VerifiedAdult credentials
///  - both credentials are valid wrt. `vc_flow_signers`, `root_pk_raw`, and `current_time_ns`
///  - `effective_vc_subject` matches the principal to which the presentation applies, i.e.
///    the subject of the IdAlias VC (which is linked via id_alias-id to the VerifiedAdult VC)
pub fn validate_verified_adult_presentation(
    vp_jwt: &str,
    effective_vc_subject: Principal,
    vc_flow_signers: &VcFlowSigners,
    root_pk_raw: &[u8],
    current_time_ns: u128,
) -> Result<(), PresentationVerificationError> {
    let (_alias_tuple, claims) = verify_ii_presentation_jwt_with_canister_ids(
        vp_jwt,
        effective_vc_subject,
        vc_flow_signers,
        root_pk_raw,
        current_time_ns,
    )?;
    validate_claim("iss", &vc_flow_signers.issuer_origin, claims.iss())
        .map_err(invalid_requested_vc)?;
    let vc_claims = claims
        .vc()
        .ok_or(invalid_requested_vc(inconsistent_jwt_claims(
            "missing vc in id_alias JWT claims",
        )))?;
    validate_claims_match_spec(vc_claims, &verified_adult_vc_spec())
        .map_err(invalid_requested_vc)?;
    Ok(())
}

pub fn validate_verified_presentation(
    credential_spec: &CredentialSpec,
    vp_jwt: &str,
    effective_vc_subject: Principal,
    vc_flow_signers: &VcFlowSigners,
    root_pk_raw: &[u8],
    current_time_ns: u128,
) -> Result<(), PresentationVerificationError> {
    let (_alias_tuple, claims) = verify_ii_presentation_jwt_with_canister_ids(
        vp_jwt,
        effective_vc_subject,
        vc_flow_signers,
        root_pk_raw,
        current_time_ns,
    )?;
    validate_claim("iss", &vc_flow_signers.issuer_origin, claims.iss())
        .map_err(invalid_requested_vc)?;
    let vc_claims = claims
        .vc()
        .ok_or(invalid_requested_vc(inconsistent_jwt_claims(
            "missing vc in id_alias JWT claims",
        )))?;

    validate_claims_match_spec(vc_claims, credential_spec)
        .map_err(invalid_requested_vc)?;
    Ok(())
}

fn invalid_requested_vc(e: JwtValidationError) -> PresentationVerificationError {
    PresentationVerificationError::InvalidRequestedCredential(
        CredentialVerificationError::InvalidClaims(e),
    )
}

fn verified_adult_vc_spec() -> CredentialSpec {
    let mut args = HashMap::new();
    args.insert("age_at_least".to_string(), ArgumentValue::Int(18));
    CredentialSpec {
        credential_type: "VerifiedAdult".to_string(),
        arguments: Some(args),
    }
}

#[cfg(test)]
mod tests {
    use super::super::tests::create_verifiable_presentation_jwt_for_test;
    use super::*;
    use crate::II_ISSUER_URL;
    use assert_matches::assert_matches;
    use identity_jose::jwu::decode_b64;

    const TEST_IC_ROOT_RAW_PK_B64URL: &str = "rfZWOKUwVrIiLJG7JFewJ0vKlRmKWsva3-f9chePBpveqNmelHnYCHomhvyBvzxLEf4nVXDUgfFpj3nUaK_g5XrMHimPi2l5jaeokbvsGXCT7F9HWQmSPUi_7WhD2-0f";
    const ID_ALIAS_CREDENTIAL_JWS: &str = "eyJqd2siOnsia3R5Ijoib2N0IiwiYWxnIjoiSWNDcyIsImsiOiJNRHd3REFZS0t3WUJCQUdEdUVNQkFnTXNBQW9BQUFBQUFBQUFBQUVCMGd6TTVJeXFMYUhyMDhtQTRWd2J5SmRxQTFyRVFUX2xNQnVVbmN5UDVVYyJ9LCJraWQiOiJkaWQ6aWNwOnJ3bGd0LWlpYWFhLWFhYWFhLWFhYWFhLWNhaSIsImFsZyI6IkljQ3MifQ.eyJleHAiOjE2MjAzMjk1MzAsImlzcyI6Imh0dHBzOi8vaWRlbnRpdHkuaWMwLmFwcC8iLCJuYmYiOjE2MjAzMjg2MzAsImp0aSI6Imh0dHBzOi8vaWRlbnRpdHkuaWMwLmFwcC9jcmVkZW50aWFsLzE2MjAzMjg2MzAwMDAwMDAwMDAiLCJzdWIiOiJkaWQ6aWNwOnAybmxjLTNzNXVsLWxjdTc0LXQ2cG4yLXVpNWltLWk0YTVmLWE0dGdhLWU2em5mLXRudmxoLXdrbWpzLWRxZSIsInZjIjp7IkBjb250ZXh0IjoiaHR0cHM6Ly93d3cudzMub3JnLzIwMTgvY3JlZGVudGlhbHMvdjEiLCJ0eXBlIjpbIlZlcmlmaWFibGVDcmVkZW50aWFsIiwiSW50ZXJuZXRJZGVudGl0eUlkQWxpYXMiXSwiY3JlZGVudGlhbFN1YmplY3QiOnsiaGFzX2lkX2FsaWFzIjoiZGlkOmljcDpqa2syMi16cWR4Yy1rZ3Blei02c3YybS01cGJ5NC13aTR0Mi1wcm1vcS1nZjJpaC1pMnF0Yy12MzdhYy01YWUifX19.2dn3omtjZXJ0aWZpY2F0ZVkBsdnZ96JkdHJlZYMBgwGDAYMCSGNhbmlzdGVygwGDAkoAAAAAAAAAAAEBgwGDAYMBgwJOY2VydGlmaWVkX2RhdGGCA1gg0zi9m0cg2U6AhUtpoJ7YKvX949iyhX0HssFZavg1DuSCBFgg0sz_P8xdqTDewOhKJUHmWFFrS7FQHnDotBDmmGoFfWCCBFggkzS0s8W3-LMHUd6MNFx8vCZvWiTSFRorIEMWhTwPnnmCBFggb2v6rTj0HcTJloNfmKGknAnxYl3Z7DM2BgMtON2-XyiCBFggMJp7SZcwpVGuz6sLBZpMol2Zn5_ELZ5uqoPUCgWgui2CBFggdfCIVuucl7ykgQOb4p4Cp2hA8C4YKVk3R8YuHCg0Ho6CBFggvJUUsrcvnUQooo4CjuiXSdzpHIMw75qS0VeLqXsfteiDAYIEWCA1U_ZYHVOz3Sdkb2HIsNoLDDiBuFfG3DxH6miIwRPra4MCRHRpbWWCA0mAuK7U3YmkvhZpc2lnbmF0dXJlWDCIiIfdfnkfcdQOsawkjiLy3zZ5vAryQBf6jYl8szyA_oT2sCh8zKNQS6HlIZoegQBkdHJlZYMBggRYIAGrxrjxRQC1-m5yYX0gu_fp5t9vfKWXuij1mbw_X2-8gwJDc2lngwJYIIOQR7wl3Ws9Jb8VP4rhIb37XKLMkkZ2P7WaZ5we60WGgwGCBFggsvWfw_9l6MdknYq3zHvk8WyFan5Rxsb-dfI-p_IA_g6DAlgg_OG2VoT11h2BQfYTnf83Y9blXTdGnmKGRe3SJAvV8h2CA0A";
    const ADULT_CREDENTIAL_JWS: &str = "eyJqd2siOnsia3R5Ijoib2N0IiwiYWxnIjoiSWNDcyIsImsiOiJNRHd3REFZS0t3WUJCQUdEdUVNQkFnTXNBQW9BQUFBQUFBQUFBUUVCMzdLQ29yYjQ2OUVFZ19uYzVvTFI5RHNoSy1SOEhXUUNMN05VMDcwa1R0WSJ9LCJraWQiOiJkaWQ6aWNwOnJya2FoLWZxYWFhLWFhYWFhLWFhYWFxLWNhaSIsImFsZyI6IkljQ3MifQ.eyJleHAiOjE2MjAzMjk1MzAsImlzcyI6Imh0dHBzOi8vYWdlX3ZlcmlmaWVyLmluZm8vIiwibmJmIjoxNjIwMzI4NjMwLCJqdGkiOiJodHRwczovL2FnZV92ZXJpZmllci5pbmZvL2NyZWRlbnRpYWxzLzQyIiwic3ViIjoiZGlkOmljcDpqa2syMi16cWR4Yy1rZ3Blei02c3YybS01cGJ5NC13aTR0Mi1wcm1vcS1nZjJpaC1pMnF0Yy12MzdhYy01YWUiLCJ2YyI6eyJAY29udGV4dCI6Imh0dHBzOi8vd3d3LnczLm9yZy8yMDE4L2NyZWRlbnRpYWxzL3YxIiwidHlwZSI6WyJWZXJpZmlhYmxlQ3JlZGVudGlhbCIsIlZlcmlmaWVkQWR1bHQiXSwiY3JlZGVudGlhbFN1YmplY3QiOnsiYWdlX2F0X2xlYXN0IjoxOH19fQ.2dn3omtjZXJ0aWZpY2F0ZVkBsdnZ96JkdHJlZYMBgwGDAYMCSGNhbmlzdGVygwGCBFggGqA23yasU2u2BherQAbx8y4H6lttXUDVNHAJknxkEcGDAkoAAAAAAAAAAQEBgwGDAYMBgwJOY2VydGlmaWVkX2RhdGGCA1ggAEPaT4CyIbEI6mH0lfgHG1yXPlqUpijL4EYeCJTeI2KCBFgg0sz_P8xdqTDewOhKJUHmWFFrS7FQHnDotBDmmGoFfWCCBFggEKUIjno3uO8-1RpfXTakCG0-J58Sk93bTqYVeU5xlc-CBFgg3Nc5Lp9mVleNY-HHA1ws5XvEGAWHd6Tn45U6R-8dx0WCBFggd8PYYS3mmXQc2roJ34IJMIbDDb4YWB3GZODAFW7AI6yCBFggQ4eqW4cFZ0hQ6C2-tbf4IiKXWNxb_TLP27B23Kgxq8GDAYIEWCA1U_ZYHVOz3Sdkb2HIsNoLDDiBuFfG3DxH6miIwRPra4MCRHRpbWWCA0mAuK7U3YmkvhZpc2lnbmF0dXJlWDCIu9FZjeqZIFpqri2_3JBt2QmnbIsp0Y9O4DGuSF7zhvfyTTegAAhnzJT9E-zoqTdkdHJlZYMBggRYII1hVxROQ2YGU4j3iepO8R7lxxKwSzHdP1lUs1ELgMIngwJDc2lngwJYIPL9DVTa-kaJmcs0Khp5uNdi4aZ5Dnt_kCORxF48UNPjgwJYIK17NMXB0rEaKIbr0yqVwTNJjw85Y53dOxfzqoN8PmgDggNA";
    const EMPLOYEE_CREDENTIAL_JWS: &str = "eyJqd2siOnsia3R5Ijoib2N0IiwiYWxnIjoiSWNDcyIsImsiOiJNRHd3REFZS0t3WUJCQUdEdUVNQkFnTXNBQW9BQUFBQUFBQUFBUUVCMzdLQ29yYjQ2OUVFZ19uYzVvTFI5RHNoSy1SOEhXUUNMN05VMDcwa1R0WSJ9LCJraWQiOiJkaWQ6aWNwOnJya2FoLWZxYWFhLWFhYWFhLWFhYWFxLWNhaSIsImFsZyI6IkljQ3MifQ.eyJleHAiOjE2MjAzMjk1MzAsImlzcyI6Imh0dHBzOi8vZW1wbG95bWVudC5pbmZvLyIsIm5iZiI6MTYyMDMyODYzMCwianRpIjoiaHR0cHM6Ly9lbXBsb3ltZW50LmluZm8vY3JlZGVudGlhbHMvNDIiLCJzdWIiOiJkaWQ6aWNwOmprazIyLXpxZHhjLWtncGV6LTZzdjJtLTVwYnk0LXdpNHQyLXBybW9xLWdmMmloLWkycXRjLXYzN2FjLTVhZSIsInZjIjp7IkBjb250ZXh0IjoiaHR0cHM6Ly93d3cudzMub3JnLzIwMTgvY3JlZGVudGlhbHMvdjEiLCJ0eXBlIjpbIlZlcmlmaWFibGVDcmVkZW50aWFsIiwiVmVyaWZpZWRFbXBsb3llZSJdLCJjcmVkZW50aWFsU3ViamVjdCI6eyJlbXBsb3llZV9vZiI6eyJlbXBsb3llcklkIjoiZGlkOndlYjpkZmluaXR5Lm9yZyIsImVtcGxveWVyTmFtZSI6IkRGSU5JVFkgRm91bmRhdGlvbiJ9fX19.2dn3omtjZXJ0aWZpY2F0ZVkBsdnZ96JkdHJlZYMBgwGDAYMCSGNhbmlzdGVygwGCBFggGqA23yasU2u2BherQAbx8y4H6lttXUDVNHAJknxkEcGDAkoAAAAAAAAAAQEBgwGDAYMBgwJOY2VydGlmaWVkX2RhdGGCA1ggPmrhfVE6NX9-yRvIyijmJ-8ID6iVSxET45GEWm21bw-CBFgg0sz_P8xdqTDewOhKJUHmWFFrS7FQHnDotBDmmGoFfWCCBFggEKUIjno3uO8-1RpfXTakCG0-J58Sk93bTqYVeU5xlc-CBFggq_ssVg2yMUpmx1c3eKj_W104x6P4eK1HYN5PsBcp3ZCCBFggOR-FhmvNHz9A-lDCTrkhgz0ETI7ePYvn8Nh9x1RARDKCBFggwqOwOs93JEnQT97xRsEHERKf6h6IheMH2hBk3O1b_jyDAYIEWCA1U_ZYHVOz3Sdkb2HIsNoLDDiBuFfG3DxH6miIwRPra4MCRHRpbWWCA0mAuK7U3YmkvhZpc2lnbmF0dXJlWDCJRTFzpJm_9HgCb8gYgnsBee-bN2nqxaFQGmRrqI9a4x8YyFN9HmRqt_rF-vZKVCNkdHJlZYMBggRYII1hVxROQ2YGU4j3iepO8R7lxxKwSzHdP1lUs1ELgMIngwJDc2lngwJYIPL9DVTa-kaJmcs0Khp5uNdi4aZ5Dnt_kCORxF48UNPjgwJYINHVvrQeii1pNBIwX-CyraHaGdwOA-brS3SWGayJeKDjggNA";
    const ID_ALIAS_PRINCIPAL: &str =
        "jkk22-zqdxc-kgpez-6sv2m-5pby4-wi4t2-prmoq-gf2ih-i2qtc-v37ac-5ae";
    const RP_PRINCIPAL: &str = "p2nlc-3s5ul-lcu74-t6pn2-ui5im-i4a5f-a4tga-e6znf-tnvlh-wkmjs-dqe";
    const EXPIRY_NS: u128 = 1620329530 * 1_000_000_000; // from ID_ALIAS_CREDENTIAL_JWS
    const MINUTE_NS: u128 = 60 * 1_000_000_000;
    const CURRENT_TIME_BEFORE_EXPIRY_NS: u128 = EXPIRY_NS - MINUTE_NS;
    const CURRENT_TIME_AFTER_EXPIRY_NS: u128 = EXPIRY_NS + MINUTE_NS;
    const II_CANISTER_ID: &str = "rwlgt-iiaaa-aaaaa-aaaaa-cai";
    const ISSUER_CANISTER_ID: &str = "rrkah-fqaaa-aaaaa-aaaaq-cai";

    fn test_ic_root_pk_raw() -> Vec<u8> {
        decode_b64(TEST_IC_ROOT_RAW_PK_B64URL).expect("failure decoding canister pk")
    }

    fn id_alias_principal() -> Principal {
        Principal::from_text(ID_ALIAS_PRINCIPAL).expect("wrong principal")
    }

    fn rp_principal() -> Principal {
        Principal::from_text(RP_PRINCIPAL).expect("wrong principal")
    }

    fn ii_canister_id() -> Principal {
        Principal::from_text(II_CANISTER_ID).expect("wrong principal")
    }

    fn issuer_canister_id() -> Principal {
        Principal::from_text(ISSUER_CANISTER_ID).expect("wrong principal")
    }

    fn default_test_vc_flow_signers() -> VcFlowSigners {
        VcFlowSigners {
            ii_canister_id: ii_canister_id(),
            ii_origin: II_ISSUER_URL.to_string(),
            issuer_canister_id: issuer_canister_id(),
            issuer_origin: "https://age_verifier.info/".to_string(),
        }
    }

    #[test]
    fn should_validate_verified_adult_presentation() {
        let vp_jwt = create_verifiable_presentation_jwt_for_test(
            rp_principal(),
            vec![
                ID_ALIAS_CREDENTIAL_JWS.to_string(),
                ADULT_CREDENTIAL_JWS.to_string(),
            ],
        )
        .expect("vp-creation failed");
        validate_verified_adult_presentation(
            &vp_jwt,
            rp_principal(),
            &default_test_vc_flow_signers(),
            &test_ic_root_pk_raw(),
            CURRENT_TIME_BEFORE_EXPIRY_NS,
        )
        .expect("VP verification failed");
    }

    #[test]
    fn should_fail_validate_verified_adult_presentation_if_wrong_vc_flow_signers() {
        let vp_jwt = create_verifiable_presentation_jwt_for_test(
            rp_principal(),
            vec![
                ID_ALIAS_CREDENTIAL_JWS.to_string(),
                ADULT_CREDENTIAL_JWS.to_string(),
            ],
        )
        .expect("vp-creation failed");

        // wrong ii_canister_id
        let result = validate_verified_adult_presentation(
            &vp_jwt,
            rp_principal(),
            &VcFlowSigners {
                ii_canister_id: issuer_canister_id(),
                ..default_test_vc_flow_signers()
            },
            &test_ic_root_pk_raw(),
            CURRENT_TIME_BEFORE_EXPIRY_NS,
        );
        assert_matches!(result, Err(e) if format!("{:?}", e).to_string().contains("InvalidSignature"));

        // wrong issuer_canister_id
        let result = validate_verified_adult_presentation(
            &vp_jwt,
            rp_principal(),
            &VcFlowSigners {
                issuer_canister_id: ii_canister_id(),
                ..default_test_vc_flow_signers()
            },
            &test_ic_root_pk_raw(),
            CURRENT_TIME_BEFORE_EXPIRY_NS,
        );
        assert_matches!(result, Err(e) if format!("{:?}", e).to_string().contains("InvalidSignature"));

        // wrong issuer_origin
        let result = validate_verified_adult_presentation(
            &vp_jwt,
            rp_principal(),
            &VcFlowSigners {
                issuer_origin: "https://wrong.origin.com".to_string(),
                ..default_test_vc_flow_signers()
            },
            &test_ic_root_pk_raw(),
            CURRENT_TIME_BEFORE_EXPIRY_NS,
        );
        assert_matches!(result, Err(e) if format!("{:?}", e).to_string().contains("InconsistentCredentialJwtClaims"));
    }

    #[test]
    fn should_fail_validate_verified_adult_presentation_if_wrong_effective_subject() {
        let vp_jwt = create_verifiable_presentation_jwt_for_test(
            rp_principal(),
            vec![
                ID_ALIAS_CREDENTIAL_JWS.to_string(),
                ADULT_CREDENTIAL_JWS.to_string(),
            ],
        )
        .expect("vp-creation failed");
        let result = validate_verified_adult_presentation(
            &vp_jwt,
            id_alias_principal(), // wrong effective subject
            &default_test_vc_flow_signers(),
            &test_ic_root_pk_raw(),
            CURRENT_TIME_BEFORE_EXPIRY_NS,
        );
        assert_matches!(result, Err(e) if format!("{:?}", e).to_string().contains("unexpected vc subject"));
    }

    #[test]
    fn should_fail_validate_verified_adult_presentation_if_expired() {
        let vp_jwt = create_verifiable_presentation_jwt_for_test(
            rp_principal(),
            vec![
                ID_ALIAS_CREDENTIAL_JWS.to_string(),
                ADULT_CREDENTIAL_JWS.to_string(),
            ],
        )
        .expect("vp-creation failed");
        let result = validate_verified_adult_presentation(
            &vp_jwt,
            rp_principal(),
            &default_test_vc_flow_signers(),
            &test_ic_root_pk_raw(),
            CURRENT_TIME_AFTER_EXPIRY_NS,
        );
        assert_matches!(result, Err(e) if format!("{:?}", e).to_string().contains("credential expired"));
    }

    #[test]
    fn should_fail_validate_verified_adult_presentation_if_wrong_vcs() {
        let vp_jwt = create_verifiable_presentation_jwt_for_test(
            rp_principal(),
            vec![
                ID_ALIAS_CREDENTIAL_JWS.to_string(),
                EMPLOYEE_CREDENTIAL_JWS.to_string(), // not a VerifiedAdult VC
            ],
        )
        .expect("vp-creation failed");
        let result = validate_verified_adult_presentation(
            &vp_jwt,
            rp_principal(),
            &default_test_vc_flow_signers(),
            &test_ic_root_pk_raw(),
            CURRENT_TIME_BEFORE_EXPIRY_NS,
        );
        assert_matches!(result, Err(e) if format!("{:?}", e).to_string().contains("InconsistentCredentialJwtClaims"));
    }
}
