use crate::anchor_management::registration::captcha::{
    check_captcha_solution, create_captcha, make_rng,
};
use crate::anchor_management::registration::rate_limit::process_rate_limit;
use crate::anchor_management::registration::Base64;
use crate::anchor_management::{
    activity_bookkeeping, add_openid_credential_skip_checks, check_openid_credential_is_unique,
    post_operation_bookkeeping, set_name,
};
use crate::state::flow_states::RegistrationFlowState;
use crate::storage::anchor::{Anchor, Device};
use crate::storage::Storage;
use crate::{openid, state};
use candid::Principal;
use ic_cdk::api::time;
use ic_cdk::caller;
use ic_stable_structures::Memory;
use internet_identity_interface::archive::types::{DeviceDataWithoutAlias, Operation};
use internet_identity_interface::internet_identity::types::{
    AuthorizationKey, CaptchaTrigger, CheckCaptchaArg, CheckCaptchaError, CreateIdentityData,
    DeviceData, DeviceWithUsage, IdRegFinishArg, IdRegFinishError, IdRegFinishResult,
    IdRegNextStepResult, IdRegStartError, IdentityNumber, OpenIDRegFinishArg,
    RegistrationFlowNextStep, StaticCaptchaTrigger,
};

impl RegistrationFlowState {
    pub fn to_flow_next_step(&self) -> RegistrationFlowNextStep {
        match self {
            RegistrationFlowState::CheckCaptcha {
                captcha_png_base64, ..
            } => RegistrationFlowNextStep::CheckCaptcha {
                captcha_png_base64: captcha_png_base64.0.clone(),
            },
            RegistrationFlowState::FinishRegistration { .. } => RegistrationFlowNextStep::Finish,
        }
    }
}

impl From<RegistrationFlowState> for RegistrationFlowNextStep {
    fn from(value: RegistrationFlowState) -> Self {
        match value {
            RegistrationFlowState::CheckCaptcha {
                captcha_png_base64, ..
            } => RegistrationFlowNextStep::CheckCaptcha {
                captcha_png_base64: captcha_png_base64.0,
            },
            RegistrationFlowState::FinishRegistration { .. } => RegistrationFlowNextStep::Finish,
        }
    }
}

pub async fn identity_registration_start() -> Result<IdRegNextStepResult, IdRegStartError> {
    process_rate_limit().map_err(|_| IdRegStartError::RateLimitExceeded)?;

    let caller = caller();
    if caller == Principal::anonymous() {
        return Err(IdRegStartError::InvalidCaller);
    }

    let now = time();
    let flow_state = match captcha_required() {
        true => captcha_flow_state(now).await.1,
        false => RegistrationFlowState::FinishRegistration {
            flow_created_timestamp_ns: now,
        },
    };

    let next_step_result = IdRegNextStepResult {
        next_step: flow_state.to_flow_next_step(),
    };

    state::with_flow_states_mut(|flow_states| {
        flow_states.new_registration_flow(caller, flow_state)
    })
    .map_err(|_| IdRegStartError::AlreadyInProgress)?;

    Ok(next_step_result)
}

fn captcha_required() -> bool {
    let captcha_config = state::persistent_state(|ps| ps.captcha_config.clone());
    match captcha_config.captcha_trigger {
        CaptchaTrigger::Static(static_trigger) => match static_trigger {
            StaticCaptchaTrigger::CaptchaEnabled => true,
            StaticCaptchaTrigger::CaptchaDisabled => false,
        },
        CaptchaTrigger::Dynamic { .. } => {
            state::storage_borrow(|storage| storage.registration_rates.registration_rates())
                .map(|rates| rates.captcha_required())
                // unreachable because it is only `None` on static captcha
                // default to true nonetheless because requiring a captcha is better than panicking
                .unwrap_or(true)
        }
    }
}

async fn captcha_flow_state(flow_created_timestamp_ns: u64) -> (Base64, RegistrationFlowState) {
    let mut rng = &mut make_rng().await;
    let (captcha_png_base64, captcha_solution) = create_captcha(&mut rng);
    let flow_state = RegistrationFlowState::CheckCaptcha {
        flow_created_timestamp_ns,
        captcha_solution,
        captcha_png_base64: captcha_png_base64.clone(),
    };
    (captcha_png_base64, flow_state)
}

pub async fn check_captcha(arg: CheckCaptchaArg) -> Result<IdRegNextStepResult, CheckCaptchaError> {
    let caller = caller();
    let Some(current_state) = state::with_flow_states(|s| s.registration_flow_state(&caller))
    else {
        return Err(CheckCaptchaError::NoRegistrationFlow);
    };

    let RegistrationFlowState::CheckCaptcha {
        flow_created_timestamp_ns,
        captcha_solution,
        ..
    } = current_state
    else {
        return Err(CheckCaptchaError::UnexpectedCall {
            next_step: RegistrationFlowNextStep::from(current_state),
        });
    };

    if check_captcha_solution(arg.solution, captcha_solution).is_err() {
        let (captcha_png_base64, flow_state) = captcha_flow_state(flow_created_timestamp_ns).await;
        state::with_flow_states_mut(|flow_states| {
            flow_states.update_registration_flow(caller, flow_state)
        })
        // If we fail to update the flow, then the flow has expired in the time it took to create
        // the captcha.
        .map_err(|_| CheckCaptchaError::NoRegistrationFlow)?;

        return Err(CheckCaptchaError::WrongSolution {
            new_captcha_png_base64: captcha_png_base64.0,
        });
    }

    let flow_state = RegistrationFlowState::FinishRegistration {
        flow_created_timestamp_ns,
    };

    state::with_flow_states_mut(|flow_states| {
        flow_states.update_registration_flow(caller, flow_state)
    })
    // The update_registration_flow triggers a clean-up as well, which could lead to a situation where
    // the flow being processed here is cleaned up. In that case, abort with an error.
    .map_err(|_| CheckCaptchaError::NoRegistrationFlow)?;

    Ok(IdRegNextStepResult {
        next_step: RegistrationFlowNextStep::Finish,
    })
}

pub fn identity_registration_finish(
    arg: CreateIdentityData,
) -> Result<IdRegFinishResult, IdRegFinishError> {
    let caller = caller();
    let Some(current_state) = state::with_flow_states(|s| s.registration_flow_state(&caller))
    else {
        return Err(IdRegFinishError::NoRegistrationFlow);
    };

    let RegistrationFlowState::FinishRegistration { .. } = current_state else {
        return Err(IdRegFinishError::UnexpectedCall {
            next_step: RegistrationFlowNextStep::from(current_state),
        });
    };

    let now = time();

    let identity_number = create_identity(&arg, now)?;

    // flow completed --> remove flow state
    state::with_flow_states_mut(|flow_states| flow_states.remove_registration_flow(&caller));

    match arg {
        CreateIdentityData::PubkeyAuthn(id_reg_finish_arg) => {
            // add temp key so the user can keep using the identity used for the registration flow
            state::with_temp_keys_mut(|temp_keys| {
                temp_keys.add_temp_key(
                    &id_reg_finish_arg.authn_method.public_key(),
                    identity_number,
                    caller,
                )
            });
        }
        // we don't need temp keys for OpenId
        // temp keys are only needed because we cannot create an identity out of the
        // AuthenticatorAttestationResponse we get when we create a WebAuthn credential on the frontend
        // since this problem doesn't exist with OIDC, we don't need a temp key
        CreateIdentityData::OpenID(_) => {}
    }

    Ok(IdRegFinishResult { identity_number })
}

fn create_openid_credential_and_config(
    openid_registration_data: &OpenIDRegFinishArg,
) -> Result<(openid::OpenIdCredential, String), IdRegFinishError> {
    let OpenIDRegFinishArg { jwt, salt, name: _ } = openid_registration_data;

    let (openid_credential, openid_config_iss) = openid::with_provider(jwt, |provider| {
        Ok((provider.verify(jwt, salt)?, provider.issuer()))
    })?;

    Ok((openid_credential, openid_config_iss))
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum ValidatedCreateIdentityData {
    PubkeyAuthn(IdRegFinishArg),
    OpenID(ValidatedOpenIDRegFinishArg),
}

impl ValidatedCreateIdentityData {
    pub fn name(&self) -> Option<String> {
        match self {
            ValidatedCreateIdentityData::PubkeyAuthn(arg) => arg.name.clone(),
            ValidatedCreateIdentityData::OpenID(validated_arg) => Some(validated_arg.name.clone()),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ValidatedOpenIDRegFinishArg {
    pub name: String,
    pub credential: openid::OpenIdCredential,
    pub openid_config_iss: String,
}

fn validate_identity_data<M: Memory + Clone>(
    storage: &Storage<M>,
    arg: &CreateIdentityData,
) -> Result<ValidatedCreateIdentityData, IdRegFinishError> {
    match &arg {
        CreateIdentityData::PubkeyAuthn(arg) => {
            Ok(ValidatedCreateIdentityData::PubkeyAuthn(arg.clone()))
        }
        CreateIdentityData::OpenID(openid_registration_data) => {
            let (credential, openid_config_iss) =
                create_openid_credential_and_config(openid_registration_data)?;

            check_openid_credential_is_unique(storage, &credential.key())
                .map_err(|err| IdRegFinishError::InvalidAuthnMethod(err.to_string()))?;

            let name = openid_registration_data.name.clone();

            Ok(ValidatedCreateIdentityData::OpenID(
                ValidatedOpenIDRegFinishArg {
                    name,
                    credential,
                    openid_config_iss,
                },
            ))
        }
    }
}

/// Precondition for OpenID: `check_openid_credential_is_unique` is `Ok`.
fn apply_identity_data(
    identity: &mut Anchor,
    arg: ValidatedCreateIdentityData,
) -> Result<Operation, IdRegFinishError> {
    let name = arg.name().clone();

    set_name(identity, name).map_err(|err| IdRegFinishError::StorageError(err.to_string()))?;

    let (authorization_key, operation) = match arg {
        ValidatedCreateIdentityData::PubkeyAuthn(IdRegFinishArg {
            authn_method,
            name: _,
        }) => {
            let device = DeviceWithUsage::try_from(authn_method)
                .map(|device| Device::from(DeviceData::from(device)))
                .map_err(|err| IdRegFinishError::InvalidAuthnMethod(err.to_string()))?;

            identity
                .add_device(device.clone())
                .map_err(|err| IdRegFinishError::InvalidAuthnMethod(err.to_string()))?;

            let public_key = device.pubkey.clone();
            let device = DeviceDataWithoutAlias::from(device);

            (
                AuthorizationKey::DeviceKey(public_key),
                Operation::RegisterAnchor { device },
            )
        }
        ValidatedCreateIdentityData::OpenID(ValidatedOpenIDRegFinishArg {
            name: _,
            credential,
            openid_config_iss,
        }) => {
            let key = credential.key();
            let iss = openid_config_iss.clone();

            add_openid_credential_skip_checks(identity, credential)
                .map_err(|err| IdRegFinishError::InvalidAuthnMethod(err.to_string()))?;

            (
                AuthorizationKey::OpenIdCredentialKey((key, Some(openid_config_iss))),
                Operation::RegisterAnchorWithOpenIdCredential { iss },
            )
        }
    };

    activity_bookkeeping(identity, &authorization_key);

    Ok(operation)
}

fn create_identity(arg: &CreateIdentityData, now: u64) -> Result<IdentityNumber, IdRegFinishError> {
    let (operation, identity_number): (Operation, u64) = state::storage_borrow_mut(|storage| {
        let arg = validate_identity_data(storage, arg)?;

        storage.allocate_anchor_safe(now, |identity: &mut Anchor| {
            let operation = apply_identity_data(identity, arg)?;
            let identity_number = identity.anchor_number();
            Ok::<_, IdRegFinishError>((operation, identity_number))
        })
    })?;

    // TODO: propagate the `now` timestamp from above to here to avoid `time()` call.
    post_operation_bookkeeping(identity_number, operation);

    Ok(identity_number)
}

#[cfg(test)]
mod create_identity_tests {
    use super::*;
    use crate::state::{storage_borrow, storage_replace};
    use crate::storage::Storage;
    use ic_stable_structures::VectorMemory;
    use internet_identity_interface::internet_identity::types::{
        AuthnMethodData, DeviceData, DeviceProtection, KeyType, Purpose,
    };

    #[test]
    fn create_identity_failure_does_not_leave_dangling_anchor_allocation() {
        storage_replace(Storage::new((0, 10000), VectorMemory::default()));

        // Record initial anchor count
        let initial_anchor_count = storage_borrow(|storage| storage.anchor_count());

        // Create identity data with an extremely long name that will cause failure
        let very_long_name = "a".repeat(10_000); // Assuming this exceeds the name length limit
        let create_identity_data = CreateIdentityData::PubkeyAuthn(
            internet_identity_interface::internet_identity::types::IdRegFinishArg {
                authn_method: AuthnMethodData::from(DeviceData {
                    pubkey: vec![1, 2, 3].into(),
                    alias: "test_device".to_string(),
                    credential_id: Some(vec![4, 5, 6].into()),
                    purpose: Purpose::Authentication,
                    key_type: KeyType::Unknown,
                    protection: DeviceProtection::Unprotected,
                    origin: None,
                    metadata: None,
                }),
                name: Some(very_long_name),
            },
        );

        // Attempt to create identity - this should fail
        let result = create_identity(&create_identity_data, 111);

        // Verify that the creation failed
        assert!(result.is_err());

        // Verify that the anchor count hasn't changed - no dangling allocation
        let final_anchor_count = storage_borrow(|storage| storage.anchor_count());
        assert_eq!(
            initial_anchor_count, final_anchor_count,
            "Anchor count should remain unchanged after failed identity creation"
        );

        // Verify that no anchor was actually created by trying to read the next anchor number
        let expected_next_anchor = storage_borrow(|storage| {
            let (id_range_lo, _) = storage.assigned_anchor_number_range();
            id_range_lo + final_anchor_count as u64
        });

        let read_result = storage_borrow(|storage| storage.read(expected_next_anchor));
        assert!(
            read_result.is_err(),
            "No anchor should exist at the next anchor number after failed creation"
        );
    }
}
