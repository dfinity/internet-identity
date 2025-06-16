use crate::v2_api::authn_method_test_helpers::{
    create_identity_with_authn_methods, sample_authn_methods,
};
use canister_tests::api::internet_identity::api_v2;
use canister_tests::framework::{env, install_ii_with_archive};
use internet_identity_interface::internet_identity::types::AuthnMethodPurpose;
use pocket_ic::CallError;

#[test]
fn should_get_identity_authn_info() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_with_archive(&env, None, None);
    let authn_methods = sample_authn_methods();
    let identity_number = create_identity_with_authn_methods(&env, canister_id, &authn_methods);

    let identity_authn_info = api_v2::identity_authn_info(&env, canister_id, identity_number)?
        .expect("identity_authn_info failed");

    assert_eq!(
        identity_authn_info.authn_methods,
        authn_methods
            .iter()
            .filter(|x| x.security_settings.purpose == AuthnMethodPurpose::Authentication)
            .map(|x| x.authn_method.clone())
            .collect::<Vec<_>>()
    );
    assert_eq!(
        identity_authn_info.recovery_authn_methods,
        authn_methods
            .iter()
            .filter(|x| x.security_settings.purpose == AuthnMethodPurpose::Recovery)
            .map(|x| x.authn_method.clone())
            .collect::<Vec<_>>()
    );
    Ok(())
}
