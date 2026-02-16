use crate::anchor_management::post_operation_bookkeeping;
use crate::ii_domain::IIDomain;
use crate::storage::anchor::Anchor;
use crate::storage::StorageError;
use crate::{anchor_management, state};
use candid::Principal;
use ic_cdk::caller;
use internet_identity_interface::archive::types::Operation;
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, AuthorizationKey, IdentityNumber,
};
use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub enum IdentityUpdateError {
    Unauthorized(Principal),
    StorageError(IdentityNumber, StorageError),
}

#[derive(Debug)]
pub struct AuthorizationError {
    pub principal: Principal,
}

impl From<Principal> for AuthorizationError {
    fn from(principal: Principal) -> Self {
        Self { principal }
    }
}

impl From<AuthorizationError> for IdentityUpdateError {
    fn from(err: AuthorizationError) -> Self {
        IdentityUpdateError::Unauthorized(err.principal)
    }
}

impl Display for IdentityUpdateError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            IdentityUpdateError::Unauthorized(principal) => {
                // This error message is used by the legacy API and should not be changed, even though
                // it is confusing authentication with authorization.
                f.write_str(&format!("{principal} could not be authenticated."))
            }
            IdentityUpdateError::StorageError(identity_nr, err) => {
                // This error message is used by the legacy API and should not be changed
                f.write_str(&format!(
                    "unable to update anchor {identity_nr} in stable memory: {err}"
                ))
            }
        }
    }
}

impl From<IdentityUpdateError> for String {
    fn from(err: IdentityUpdateError) -> Self {
        format!("{err}")
    }
}

/// Checks the authorization of the caller for the given anchor and calls the provided function, if successful.
/// Handles all the necessary bookkeeping for anchor operations.
/// Returns an error if the caller is not authorized or the anchor cannot be written to stable memory.
///
/// * anchor_number: indicates the anchor to be provided op should be called on
/// * op: Function that modifies an anchor and returns a [Result] indicating
///   success or failure which determines whether additional bookkeeping (on success) is required.
///
///  On success, the function must also return an [Operation] which is used for archiving purposes.
pub fn anchor_operation_with_authz_check<R, E>(
    anchor_number: AnchorNumber,
    op: impl FnOnce(&mut Anchor) -> Result<(R, Operation), E>,
) -> Result<R, E>
where
    E: From<IdentityUpdateError>,
{
    let (mut anchor, authorization_key) = check_authorization(anchor_number)
        .map_err(|err| E::from(IdentityUpdateError::from(err)))?;

    anchor_management::activity_bookkeeping(&mut anchor, &authorization_key);

    let (ret, operation) = op(&mut anchor)?;

    // write back anchor
    state::storage_borrow_mut(|storage| storage.write(anchor))
        .map_err(|err| E::from(IdentityUpdateError::StorageError(anchor_number, err)))?;

    post_operation_bookkeeping(anchor_number, operation);

    Ok(ret)
}

/// Checks if the caller is authorized to operate on the anchor provided and returns a reference to the public key of the authentication method used (device or openid).
/// Returns an error if the caller is not authorized.
pub fn check_authorization(
    anchor_number: AnchorNumber,
) -> Result<(Anchor, AuthorizationKey), AuthorizationError> {
    let anchor = state::anchor(anchor_number);
    let caller = caller();

    // First check device (passkey or recovery) authorization
    for device in anchor.devices() {
        if caller == Principal::self_authenticating(&device.pubkey)
            || state::with_temp_keys_mut(|temp_keys| {
                temp_keys
                    .check_temp_key(&caller, &device.pubkey, anchor_number)
                    .is_ok()
            })
        {
            return Ok((
                anchor.clone(),
                AuthorizationKey::DeviceKey(device.pubkey.clone()),
            ));
        }
    }
    // Else check OpenID authorization
    for credential in anchor.openid_credentials() {
        if caller == credential.principal(anchor_number) {
            return Ok((
                anchor.clone(),
                AuthorizationKey::OpenIdCredentialKey((
                    credential.key(),
                    credential.config_issuer(),
                )),
            ));
        }
    }

    Err(AuthorizationError::from(caller))
}

/// Checks that the caller is authorized to operate on the given anchor_number and updates the authorization method used to
/// reflect the current activity.
/// Also updates the aggregated stats on daily and monthly active users.
/// Returns an error if the caller is not authorized or the anchor cannot be written to stable memory.
///
/// Note: this function reads / writes the anchor from / to stable memory. It is intended to be used by functions that
/// do not further modify the anchor.
pub fn check_authz_and_record_activity(
    anchor_number: AnchorNumber,
) -> Result<Option<IIDomain>, IdentityUpdateError> {
    let (mut anchor, authorization_key) =
        check_authorization(anchor_number).map_err(IdentityUpdateError::from)?;

    let maybe_domain = match &authorization_key {
        AuthorizationKey::DeviceKey(device_key) => anchor.device(device_key).unwrap().ii_domain(),
        _ => None,
    };
    anchor_management::activity_bookkeeping(&mut anchor, &authorization_key);
    state::storage_borrow_mut(|storage| storage.write(anchor))
        .map_err(|err| IdentityUpdateError::StorageError(anchor_number, err))?;
    Ok(maybe_domain)
}

pub fn is_self_authenticating(principal: Principal) -> bool {
    let principal_bytes = principal.as_slice();
    if principal_bytes.len() != Principal::MAX_LENGTH_IN_BYTES {
        return false;
    }
    // Take some self-authenticating principal, compute its tag, and use that.
    let self_authenticating = Principal::self_authenticating([0xde, 0xad, 0xbe, 0xef]);
    let self_authenticating_bytes = self_authenticating.as_slice();

    principal_bytes.last() == self_authenticating_bytes.last()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_self_authenticating() {
        let test_cases = vec![
            (
                "self-authenticating from public key",
                Principal::self_authenticating([0xde, 0xad, 0xbe, 0xef]),
                true,
            ),
            (
                "self-authenticating from different public key",
                Principal::self_authenticating([0x01, 0x02, 0x03, 0x04]),
                true,
            ),
            (
                "self-authenticating from 28-byte public key",
                Principal::self_authenticating([0xff; 28]),
                true,
            ),
            ("anonymous principal", Principal::anonymous(), false),
            (
                "management canister",
                Principal::management_canister(),
                false,
            ),
            (
                "opaque principal (short)",
                Principal::from_slice(&[0x01, 0x02, 0x03]),
                false,
            ),
            (
                "opaque principal (medium)",
                {
                    let bytes = [0xca; 10];
                    Principal::from_slice(&bytes)
                },
                false,
            ),
            (
                "derived principal",
                Principal::from_text("2vxsx-fae").unwrap(),
                false,
            ),
            (
                "internet identity canister",
                Principal::from_text("rdmx6-jaaaa-aaaaa-aaadq-cai").unwrap(),
                false,
            ),
        ];

        for (label, principal, expected) in test_cases {
            let result = is_self_authenticating(principal);
            assert_eq!(
                result, expected,
                "Failed test case '{}': principal = {}, expected = {}, got = {}",
                label, principal, expected, result
            );
        }
    }
}
