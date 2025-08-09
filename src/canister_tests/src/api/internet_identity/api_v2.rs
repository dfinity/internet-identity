use candid::Principal;
use ic_cdk::api::management_canister::main::CanisterId;
use internet_identity_interface::internet_identity::types::*;
use pocket_ic::common::rest::RawEffectivePrincipal;
use pocket_ic::{
    call_candid, call_candid_as, query_candid, query_candid_as, PocketIc, RejectResponse,
};
use std::collections::HashMap;

pub fn identity_registration_start(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
) -> Result<Result<IdRegNextStepResult, IdRegStartError>, RejectResponse> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "identity_registration_start",
        (),
    )
    .map(|(x,)| x)
}

pub fn check_captcha(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    solution: String,
) -> Result<Result<IdRegNextStepResult, CheckCaptchaError>, RejectResponse> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "check_captcha",
        (CheckCaptchaArg { solution },),
    )
    .map(|(x,)| x)
}

pub fn identity_registration_finish(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    authn_method: &AuthnMethodData,
    name: Option<String>,
) -> Result<Result<IdRegFinishResult, IdRegFinishError>, RejectResponse> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "identity_registration_finish",
        (IdRegFinishArg {
            authn_method: authn_method.clone(),
            name,
        },),
    )
    .map(|(x,)| x)
}

pub fn openid_identity_registration_finish(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    openid_arg: &OpenIDRegFinishArg,
) -> Result<Result<IdRegFinishResult, IdRegFinishError>, RejectResponse> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "openid_identity_registration_finish",
        (openid_arg.clone(),),
    )
    .map(|(x,)| x)
}

pub fn identity_authn_info(
    env: &PocketIc,
    canister_id: CanisterId,
    identity_number: IdentityNumber,
) -> Result<Result<IdentityAuthnInfo, ()>, RejectResponse> {
    query_candid(env, canister_id, "identity_authn_info", (identity_number,)).map(|(x,)| x)
}

pub fn identity_info(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    identity_number: IdentityNumber,
) -> Result<Result<IdentityInfo, IdentityInfoError>, RejectResponse> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "identity_info",
        (identity_number,),
    )
    .map(|(x,)| x)
}

pub fn identity_metadata_replace(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    identity_number: IdentityNumber,
    metadata: &HashMap<String, MetadataEntryV2>,
) -> Result<Result<(), IdentityMetadataReplaceError>, RejectResponse> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "identity_metadata_replace",
        (identity_number, metadata),
    )
    .map(|(x,)| x)
}

pub fn identity_properties_replace(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    identity_number: IdentityNumber,
    properties: &IdentityPropertiesReplace,
) -> Result<Result<(), IdentityPropertiesReplaceError>, RejectResponse> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "identity_properties_replace",
        (identity_number, properties),
    )
    .map(|(x,)| x)
}

pub fn authn_method_add(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    identity_number: IdentityNumber,
    authn_method: &AuthnMethodData,
) -> Result<Result<(), AuthnMethodAddError>, RejectResponse> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "authn_method_add",
        (identity_number, authn_method),
    )
    .map(|(x,)| x)
}

pub fn authn_method_replace(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    identity_number: IdentityNumber,
    public_key: &PublicKey,
    authn_method: &AuthnMethodData,
) -> Result<Result<(), AuthnMethodReplaceError>, RejectResponse> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "authn_method_replace",
        (identity_number, public_key, authn_method),
    )
    .map(|(x,)| x)
}

pub fn authn_method_metadata_replace(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    identity_number: IdentityNumber,
    public_key: &PublicKey,
    metadata: &HashMap<String, MetadataEntryV2>,
) -> Result<Result<(), AuthnMethodMetadataReplaceError>, RejectResponse> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "authn_method_metadata_replace",
        (identity_number, public_key, metadata),
    )
    .map(|(x,)| x)
}

pub fn authn_method_security_settings_replace(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    identity_number: IdentityNumber,
    public_key: &PublicKey,
    security_settings: &AuthnMethodSecuritySettings,
) -> Result<Result<(), AuthnMethodSecuritySettingsReplaceError>, RejectResponse> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "authn_method_security_settings_replace",
        (identity_number, public_key, security_settings),
    )
    .map(|(x,)| x)
}

pub fn authn_method_remove(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    identity_number: IdentityNumber,
    public_key: &PublicKey,
) -> Result<Result<(), ()>, RejectResponse> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "authn_method_remove",
        (identity_number, public_key),
    )
    .map(|(x,)| x)
}

pub fn authn_method_registration_mode_enter(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    identity_number: IdentityNumber,
    id: Option<String>,
) -> Result<Result<RegistrationModeInfo, AuthnMethodRegistrationModeEnterError>, RejectResponse> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "authn_method_registration_mode_enter",
        (identity_number, id),
    )
    .map(|(x,)| x)
}

pub fn authn_method_registration_mode_exit(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    identity_number: IdentityNumber,
    authn_method: Option<AuthnMethodData>,
) -> Result<Result<(), AuthnMethodRegistrationModeExitError>, RejectResponse> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "authn_method_registration_mode_exit",
        (identity_number, authn_method),
    )
    .map(|(x,)| x)
}

pub fn authn_method_register(
    env: &PocketIc,
    canister_id: CanisterId,
    identity_number: IdentityNumber,
    authn_method: &AuthnMethodData,
) -> Result<Result<AuthnMethodConfirmationCode, AuthnMethodRegisterError>, RejectResponse> {
    call_candid(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        "authn_method_register",
        (identity_number, authn_method),
    )
    .map(|(x,)| x)
}

pub fn authn_method_session_register(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    identity_number: IdentityNumber,
) -> Result<Result<AuthnMethodConfirmationCode, AuthnMethodRegisterError>, RejectResponse> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "authn_method_session_register",
        (identity_number,),
    )
    .map(|(x,)| x)
}

pub fn authn_method_confirm(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    identity_number: IdentityNumber,
    confirmation_code: &str,
) -> Result<Result<(), AuthnMethodConfirmationError>, RejectResponse> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "authn_method_confirm",
        (identity_number, confirmation_code),
    )
    .map(|(x,)| x)
}

pub fn lookup_by_registration_mode_id(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    id: String,
) -> Result<Option<IdentityNumber>, RejectResponse> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "lookup_by_registration_mode_id",
        (id,),
    )
    .map(|(x,)| x)
}

pub fn create_account(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    identity_number: IdentityNumber,
    origin: FrontendHostname,
    name: String,
) -> Result<Result<AccountInfo, CreateAccountError>, RejectResponse> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "create_account",
        (identity_number, origin, name),
    )
    .map(|(x,)| x)
}

pub fn get_accounts(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    identity_number: IdentityNumber,
    origin: FrontendHostname,
) -> Result<Result<Vec<AccountInfo>, GetAccountsError>, RejectResponse> {
    query_candid_as(
        env,
        canister_id,
        sender,
        "get_accounts",
        (identity_number, origin),
    )
    .map(|(x,)| x)
}

pub fn update_account(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    identity_number: IdentityNumber,
    origin: FrontendHostname,
    account_number: Option<AccountNumber>,
    update: AccountUpdate,
) -> Result<Result<AccountInfo, UpdateAccountError>, RejectResponse> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "update_account",
        (identity_number, origin, account_number, update),
    )
    .map(|(x,)| x)
}

#[derive(Clone)]
pub struct AccountDelegationParams<'a> {
    pub env: &'a PocketIc,
    pub canister_id: CanisterId,
    pub sender: Principal,
    pub identity_number: IdentityNumber,
    pub origin: FrontendHostname,
    pub account_number: Option<AccountNumber>,
    pub session_key: SessionKey,
}

impl<'a> AccountDelegationParams<'a> {
    pub fn new(
        env: &'a PocketIc,
        canister_id: CanisterId,
        sender: Principal,
        identity_number: IdentityNumber,
        origin: FrontendHostname,
        account_number: Option<AccountNumber>,
        session_key: SessionKey,
    ) -> Self {
        Self {
            env,
            canister_id,
            sender,
            identity_number,
            origin,
            account_number,
            session_key,
        }
    }
}

pub fn prepare_account_delegation(
    params: &AccountDelegationParams,
    max_ttl: Option<u64>,
) -> Result<Result<PrepareAccountDelegation, AccountDelegationError>, RejectResponse> {
    call_candid_as(
        params.env,
        params.canister_id,
        RawEffectivePrincipal::None,
        params.sender,
        "prepare_account_delegation",
        (
            params.identity_number,
            params.origin.clone(),
            params.account_number,
            params.session_key.clone(),
            max_ttl,
        ),
    )
    .map(|(x,)| x)
}

pub fn get_account_delegation(
    params: &AccountDelegationParams,
    expiration: u64,
) -> Result<Result<SignedDelegation, AccountDelegationError>, RejectResponse> {
    query_candid_as(
        params.env,
        params.canister_id,
        params.sender,
        "get_account_delegation",
        (
            params.identity_number,
            params.origin.clone(),
            params.account_number,
            params.session_key.clone(),
            expiration,
        ),
    )
    .map(|(x,)| x)
}
