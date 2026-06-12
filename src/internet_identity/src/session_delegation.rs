use crate::authz_utils::{check_authorization, AuthorizationError};
use crate::delegation::{add_delegation_signature, der_encode_canister_sig_key};
use crate::storage::anchor::Anchor;
use crate::{state, update_root_hash};
use candid::Principal;
use ic_canister_sig_creation::{
    delegation_signature_msg, signature_map::CanisterSigInputs, DELEGATION_SIG_DOMAIN,
};
use ic_cdk::{api::time, caller};
use ic_certification::Hash;
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, AuthorizationKey, Delegation, PrepareSessionDelegation, SessionDelegationError,
    SessionKey, SessionScope, SignedDelegation, Timestamp,
};
use serde_bytes::ByteBuf;
use sha2::{Digest, Sha256};

pub const DEFAULT_SESSION_DELEGATION_TTL_NS: u64 = 7 * crate::DAY_NS;
pub const MAX_SESSION_DELEGATION_TTL_NS: u64 = 30 * crate::DAY_NS;

fn scope_tag(scope: SessionScope) -> &'static [u8] {
    match scope {
        SessionScope::AccountManagement => b"account_management",
    }
}

pub(crate) fn session_delegation_seed(
    anchor_number: AnchorNumber,
    scope: SessionScope,
    epoch: u32,
) -> Hash {
    const DOMAIN_SEPARATOR: &[u8] = b"session-delegation";

    let salt = state::salt();
    let tag = scope_tag(scope);
    let anchor_bytes = anchor_number.to_le_bytes();
    let epoch_bytes = epoch.to_le_bytes();

    let mut blob: Vec<u8> = Vec::new();
    blob.push(salt.len() as u8);
    blob.extend_from_slice(&salt);

    blob.push(DOMAIN_SEPARATOR.len() as u8);
    blob.extend_from_slice(DOMAIN_SEPARATOR);

    blob.push(anchor_bytes.len() as u8);
    blob.extend_from_slice(&anchor_bytes);

    blob.push(tag.len() as u8);
    blob.extend_from_slice(tag);

    blob.push(epoch_bytes.len() as u8);
    blob.extend_from_slice(&epoch_bytes);

    let mut hasher = Sha256::new();
    hasher.update(&blob);
    hasher.finalize().into()
}

pub enum CallerCapability {
    FullAuth(Box<Anchor>, AuthorizationKey),
    SessionScoped,
}

pub fn check_authorization_with_scope(
    anchor_number: AnchorNumber,
    required_scope: SessionScope,
) -> Result<CallerCapability, AuthorizationError> {
    if let Ok((anchor, key)) = check_authorization(anchor_number) {
        return Ok(CallerCapability::FullAuth(Box::new(anchor), key));
    }

    let salt_initialised = state::storage_borrow(|storage| storage.salt().is_some());
    if salt_initialised {
        let anchor = state::anchor(anchor_number);
        let epoch = anchor.session_delegation_epoch();
        let seed = session_delegation_seed(anchor_number, required_scope, epoch);
        let public_key = der_encode_canister_sig_key(seed.to_vec());
        if caller() == Principal::self_authenticating(public_key) {
            return Ok(CallerCapability::SessionScoped);
        }
    }

    Err(AuthorizationError::from(caller()))
}

pub async fn prepare_session_delegation(
    anchor_number: AnchorNumber,
    scope: SessionScope,
    session_key: SessionKey,
    max_ttl: Option<u64>,
) -> Result<PrepareSessionDelegation, SessionDelegationError> {
    let (anchor, auth_key) = check_authorization(anchor_number)
        .map_err(|err| SessionDelegationError::Unauthorized(err.principal))?;

    if matches!(auth_key, AuthorizationKey::EmailRecoveryAddress(_)) {
        return Err(SessionDelegationError::Unauthorized(caller()));
    }

    state::ensure_salt_set().await;

    let session_duration_ns = u64::min(
        max_ttl.unwrap_or(DEFAULT_SESSION_DELEGATION_TTL_NS),
        MAX_SESSION_DELEGATION_TTL_NS,
    );
    let expiration = time().saturating_add(session_duration_ns);

    let epoch = anchor.session_delegation_epoch();
    let seed = session_delegation_seed(anchor_number, scope, epoch);

    state::signature_map_mut(|sigs| {
        add_delegation_signature(sigs, session_key, &seed, expiration);
    });
    update_root_hash();

    Ok(PrepareSessionDelegation {
        user_key: ByteBuf::from(der_encode_canister_sig_key(seed.to_vec())),
        expiration,
    })
}

pub fn get_session_delegation(
    anchor_number: AnchorNumber,
    scope: SessionScope,
    session_key: SessionKey,
    expiration: Timestamp,
) -> Result<SignedDelegation, SessionDelegationError> {
    check_authorization(anchor_number)
        .map_err(|err| SessionDelegationError::Unauthorized(err.principal))?;

    let anchor = state::anchor(anchor_number);
    let epoch = anchor.session_delegation_epoch();
    let seed = session_delegation_seed(anchor_number, scope, epoch);

    state::assets_and_signatures(|certified_assets, sigs| {
        let inputs = CanisterSigInputs {
            domain: DELEGATION_SIG_DOMAIN,
            seed: &seed,
            message: &delegation_signature_msg(&session_key, expiration, None),
        };
        match sigs.get_signature_as_cbor(&inputs, Some(certified_assets.root_hash())) {
            Ok(signature) => Ok(SignedDelegation {
                delegation: Delegation {
                    pubkey: session_key,
                    expiration,
                    targets: None,
                },
                signature: ByteBuf::from(signature),
            }),
            Err(_) => Err(SessionDelegationError::NoSuchDelegation),
        }
    })
}

pub fn invalidate_session_delegations(
    anchor_number: AnchorNumber,
) -> Result<(), SessionDelegationError> {
    check_authorization(anchor_number)
        .map_err(|err| SessionDelegationError::Unauthorized(err.principal))?;

    let mut anchor = state::anchor(anchor_number);
    anchor.bump_session_delegation_epoch();
    state::storage_borrow_mut(|storage| storage.write(anchor))
        .map_err(|err| SessionDelegationError::InternalCanisterError(format!("{err:?}")))?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::state::{storage_borrow_mut, storage_replace};
    use crate::storage::Storage;
    use ic_stable_structures::VectorMemory;

    fn new_storage() -> Storage<VectorMemory> {
        Storage::new((0, 10000), VectorMemory::default())
    }

    #[test]
    fn seed_determinism() {
        storage_replace(new_storage());
        storage_borrow_mut(|s| s.update_salt([1u8; 32]));

        let s1 = session_delegation_seed(42, SessionScope::AccountManagement, 0);
        let s2 = session_delegation_seed(42, SessionScope::AccountManagement, 0);
        assert_eq!(s1, s2, "same inputs must produce the same seed");
    }

    #[test]
    fn seed_differs_by_anchor() {
        storage_replace(new_storage());
        storage_borrow_mut(|s| s.update_salt([1u8; 32]));

        let s1 = session_delegation_seed(1, SessionScope::AccountManagement, 0);
        let s2 = session_delegation_seed(2, SessionScope::AccountManagement, 0);
        assert_ne!(s1, s2);
    }

    #[test]
    fn seed_differs_by_epoch() {
        storage_replace(new_storage());
        storage_borrow_mut(|s| s.update_salt([1u8; 32]));

        let s1 = session_delegation_seed(42, SessionScope::AccountManagement, 0);
        let s2 = session_delegation_seed(42, SessionScope::AccountManagement, 1);
        assert_ne!(s1, s2);
    }

    #[test]
    fn seed_differs_by_salt() {
        storage_replace(new_storage());
        storage_borrow_mut(|s| s.update_salt([1u8; 32]));
        let s1 = session_delegation_seed(42, SessionScope::AccountManagement, 0);

        storage_replace(new_storage());
        storage_borrow_mut(|s| s.update_salt([2u8; 32]));
        let s2 = session_delegation_seed(42, SessionScope::AccountManagement, 0);

        assert_ne!(s1, s2);
    }

    fn clamp_ttl(max_ttl: Option<u64>) -> u64 {
        u64::min(
            max_ttl.unwrap_or(DEFAULT_SESSION_DELEGATION_TTL_NS),
            MAX_SESSION_DELEGATION_TTL_NS,
        )
    }

    #[test]
    fn ttl_clamped_to_max() {
        let over_max = MAX_SESSION_DELEGATION_TTL_NS + 1;
        assert_eq!(clamp_ttl(Some(over_max)), MAX_SESSION_DELEGATION_TTL_NS);
    }

    #[test]
    fn ttl_default_applied_when_none() {
        assert_eq!(clamp_ttl(None), DEFAULT_SESSION_DELEGATION_TTL_NS);
    }

    #[test]
    fn ttl_zero_does_not_panic() {
        assert_eq!(clamp_ttl(Some(0)), 0);
    }

    #[test]
    fn epoch_none_reads_as_zero() {
        storage_replace(new_storage());
        let anchor = storage_borrow_mut(|s| s.allocate_anchor(0).unwrap());
        assert_eq!(anchor.session_delegation_epoch(), 0);
    }

    #[test]
    fn epoch_bump_increments() {
        storage_replace(new_storage());
        let mut anchor = storage_borrow_mut(|s| s.allocate_anchor(0).unwrap());
        assert_eq!(anchor.session_delegation_epoch(), 0);
        anchor.bump_session_delegation_epoch();
        assert_eq!(anchor.session_delegation_epoch(), 1);
        anchor.bump_session_delegation_epoch();
        assert_eq!(anchor.session_delegation_epoch(), 2);
    }

    #[test]
    fn epoch_wraps_at_u32_max() {
        storage_replace(new_storage());
        let mut anchor = storage_borrow_mut(|s| s.allocate_anchor(0).unwrap());
        anchor.session_delegation_epoch = Some(u32::MAX);
        anchor.bump_session_delegation_epoch();
        assert_eq!(anchor.session_delegation_epoch(), 0);
    }

    #[test]
    fn epoch_roundtrip_through_storage() {
        storage_replace(new_storage());
        let mut anchor = storage_borrow_mut(|s| s.allocate_anchor(0).unwrap());
        let anchor_number = anchor.anchor_number();
        anchor.bump_session_delegation_epoch();
        anchor.bump_session_delegation_epoch();
        storage_borrow_mut(|s| s.write(anchor)).unwrap();

        let loaded = storage_borrow_mut(|s| s.read(anchor_number)).unwrap();
        assert_eq!(loaded.session_delegation_epoch(), 2);
    }

    #[test]
    fn epoch_additive_decode_defaults_to_zero() {
        use crate::storage::storable::anchor::StorableAnchor;
        use ic_stable_structures::Storable;
        use std::borrow::Cow;

        let old = StorableAnchor {
            name: None,
            openid_credentials: vec![],
            created_at_ns: None,
            passkey_credentials: None,
            recovery_keys: None,
            email_recovery: None,
            session_delegation_epoch: None,
        };
        let bytes = old.to_bytes();
        let decoded: StorableAnchor = StorableAnchor::from_bytes(Cow::Owned(bytes.into_owned()));
        assert_eq!(decoded.session_delegation_epoch, None);

        use crate::storage::anchor::Anchor;
        let anchor = Anchor::from((0u64, decoded));
        assert_eq!(anchor.session_delegation_epoch(), 0);
    }

    #[test]
    fn cross_namespace_seeds_are_distinct() {
        storage_replace(new_storage());
        storage_borrow_mut(|s| s.update_salt([1u8; 32]));

        let anchor_number: u64 = 42;
        let origin = "https://example.com".to_string();

        let session_seed =
            session_delegation_seed(anchor_number, SessionScope::AccountManagement, 0);

        let email_seed = crate::email_recovery::smtp::calculate_email_recovery_seed(
            "user@example.com",
            anchor_number,
        );

        let account_seed =
            crate::storage::account::Account::synthetic(anchor_number, origin).calculate_seed();

        assert_ne!(
            session_seed, email_seed,
            "session-delegation seed must differ from email-recovery seed"
        );
        assert_ne!(
            session_seed, account_seed,
            "session-delegation seed must differ from account seed"
        );
        assert_ne!(
            email_seed, account_seed,
            "email-recovery seed must differ from account seed"
        );
    }
}
