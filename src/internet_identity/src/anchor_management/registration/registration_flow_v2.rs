use crate::anchor_management::registration::captcha::{
    check_captcha_solution, create_captcha, make_rng,
};
use crate::anchor_management::registration::rate_limit::process_rate_limit;
use crate::anchor_management::registration::Base64;
use crate::anchor_management::{
    activity_bookkeeping, add_openid_credential, post_operation_bookkeeping, set_name,
};
use crate::state::flow_states::RegistrationFlowState;
use crate::storage::anchor::Device;
use crate::{openid, state};
use candid::Principal;
use ic_cdk::api::time;
use ic_cdk::caller;
use internet_identity_interface::archive::types::{DeviceDataWithoutAlias, Operation};
use internet_identity_interface::internet_identity::types::IdRegFinishError::IdentityLimitReached;
use internet_identity_interface::internet_identity::types::{
    AuthorizationKey, CaptchaTrigger, CheckCaptchaArg, CheckCaptchaError, CreateIdentityData,
    DeviceData, DeviceWithUsage, IdRegFinishError, IdRegFinishResult, IdRegNextStepResult,
    IdRegStartError, IdentityNumber, OpenIDRegFinishArg, RegistrationFlowNextStep,
    StaticCaptchaTrigger,
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

    let identity_number = create_identity(&arg)?;

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

fn create_identity(arg: &CreateIdentityData) -> Result<IdentityNumber, IdRegFinishError> {
    let Some(mut identity) = state::storage_borrow_mut(|s| s.allocate_anchor()) else {
        return Err(IdentityLimitReached);
    };

    let operation = match &arg {
        CreateIdentityData::PubkeyAuthn(id_reg_finish_arg) => {
            let name = id_reg_finish_arg.name.clone();
            let device = DeviceWithUsage::try_from(id_reg_finish_arg.authn_method.clone())
                .map(|device| Device::from(DeviceData::from(device)))
                .map_err(|err| IdRegFinishError::InvalidAuthnMethod(err.to_string()))?;

            identity
                .add_device(device.clone())
                .map_err(|err| IdRegFinishError::InvalidAuthnMethod(err.to_string()))?;
            set_name(&mut identity, name)
                .map_err(|err| IdRegFinishError::StorageError(err.to_string()))?;
            activity_bookkeeping(
                &mut identity,
                &AuthorizationKey::DeviceKey(device.pubkey.clone()),
            );

            Operation::RegisterAnchor {
                device: DeviceDataWithoutAlias::from(device),
            }
        }
        CreateIdentityData::OpenID(openid_registration_data) => {
            let OpenIDRegFinishArg { jwt, salt } = openid_registration_data;
            let (openid_credential, name) = openid::with_provider(jwt, |provider| {
                let openid_credential = provider.verify(jwt, salt)?;
                let name = provider.metadata_name(openid_credential.metadata.clone());
                Ok((openid_credential, name))
            })?;
            add_openid_credential(&mut identity, openid_credential.clone())
                .map_err(|err| IdRegFinishError::InvalidAuthnMethod(err.to_string()))?;
            set_name(&mut identity, name)
                .map_err(|err| IdRegFinishError::StorageError(err.to_string()))?;
            activity_bookkeeping(
                &mut identity,
                &AuthorizationKey::OpenIdCredentialKey(openid_credential.key()),
            );

            Operation::RegisterAnchorWithOpenIdCredential {
                iss: openid_credential.iss,
            }
        }
    };

    let identity_number = identity.anchor_number();

    state::storage_borrow_mut(|s| {
        s.registration_rates.new_registration();
        s.create(identity)
    })
    .map_err(|err| IdRegFinishError::StorageError(err.to_string()))?;

    post_operation_bookkeeping(identity_number, operation);

    Ok(identity_number)
}
