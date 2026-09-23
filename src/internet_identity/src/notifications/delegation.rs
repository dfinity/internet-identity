//! A delegation for pulling notification data from an app.
//!
//! The service worker needs to fetch a notification's content from the app it
//! came from, as the user, without holding a delegation that would let it do
//! everything else that user can do there. So this issues one under a seed of
//! its own, derived from the account's but distinct from it, and signs a
//! `sender_info` under the same seed. The IC verifies that signature before the
//! app's wasm runs, and the app reads the account out of it — the caller
//! principal carries no authority anywhere and the app never looks at it.

// The IC's own domain for the sender_info request field, already defined for
// the attribute flow that uses the same mechanism.
use crate::attributes::ICRC3_ATTRIBUTES_CERTIFICATION_DOMAIN as SENDER_INFO_SIG_DOMAIN;
use crate::delegation::{
    add_delegation_signature, delegation_signature_msg_with_permissions,
    der_encode_canister_sig_key,
};
use crate::notifications::{
    consent_granted_for, ValidatedGetNotificationDelegationRequest,
    ValidatedPrepareNotificationDelegationRequest,
};
use crate::state::{self, storage_borrow};
use crate::storage::account::{Account, AccountKey};
use crate::storage::anchor::Anchor;
use crate::update_root_hash;
use candid::Principal;
use ic_canister_sig_creation::{signature_map::CanisterSigInputs, DELEGATION_SIG_DOMAIN};
use ic_certification::Hash;
use internet_identity_interface::internet_identity::types::{
    AccountNumber, AnchorNumber, BrowserId, Delegation, FrontendHostname,
    GetNotificationDelegationResponse, NotificationDelegationError,
    PrepareNotificationDelegationResponse, SignedDelegation, Timestamp,
};
use serde_bytes::ByteBuf;
use sha2::{Digest, Sha256};

/// Fixed: preparing one costs an update and a query, so it is fetched once and
/// reused, not per notification. Matches the SSO session default.
pub const NOTIFICATION_DELEGATION_TTL_NS: u64 = 8 * crate::HOUR_NS;

const SEED_DOMAIN: &[u8] = b"notification-pull";
const SENDER_INFO_DOMAIN: &[u8] = b"notification-sender-info";

/// Derived from the account's own seed so it inherits how that is built, and
/// domain-separated from it so the principal holds none of the account's
/// authority.
fn notification_pull_seed(account: &Account) -> Hash {
    let mut hasher = Sha256::new();
    hasher.update([SEED_DOMAIN.len() as u8]);
    hasher.update(SEED_DOMAIN);
    hasher.update(account.calculate_seed());
    hasher.finalize().into()
}

fn account_principal(account: &Account) -> Principal {
    Principal::self_authenticating(der_encode_canister_sig_key(
        account.calculate_seed().to_vec(),
    ))
}

/// What the app is told: the origin it is being called for and the account
/// principal to serve. Length-prefixed, the way II builds its seed blobs.
fn sender_info(origin: &FrontendHostname, account_principal: &Principal) -> Vec<u8> {
    let mut blob = Vec::new();
    blob.push(SENDER_INFO_DOMAIN.len() as u8);
    blob.extend_from_slice(SENDER_INFO_DOMAIN);
    blob.push(origin.len() as u8);
    blob.extend_from_slice(origin.as_bytes());
    let principal = account_principal.as_slice();
    blob.push(principal.len() as u8);
    blob.extend_from_slice(principal);
    blob
}

/// The caller may mint for this identity only while it is a browser of it that
/// is registered for Web Push, and only while the identity still allows this
/// app to notify it. Both are what the identity revokes from another device
/// when a browser is lost: signing it out clears its registration, and
/// withdrawing consent closes the app. Neither shortens a delegation already
/// minted, which runs its own lifetime out.
fn check_notification_access(
    anchor: &Anchor,
    browser_id: BrowserId,
    origin: &FrontendHostname,
) -> Result<(), NotificationDelegationError> {
    if anchor.webpush_subscription(browser_id).is_none() {
        return Err(NotificationDelegationError::NoNotificationAccess);
    }
    if !consent_granted_for(anchor.anchor_number(), origin) {
        return Err(NotificationDelegationError::NoNotificationAccess);
    }
    Ok(())
}

fn read_account(
    anchor_number: AnchorNumber,
    origin: &FrontendHostname,
    account_number: Option<AccountNumber>,
) -> Result<Account, NotificationDelegationError> {
    storage_borrow(|storage| {
        storage.read_account(&AccountKey {
            anchor_number,
            origin: origin.clone(),
            account_number,
        })
    })
    .ok_or(NotificationDelegationError::NoSuchDelegation)
}

pub fn prepare(
    ValidatedPrepareNotificationDelegationRequest {
        anchor_number,
        origin,
        account_number,
        session_key,
        ..
    }: ValidatedPrepareNotificationDelegationRequest,
    anchor: &Anchor,
    browser_id: BrowserId,
    now_ns: Timestamp,
) -> Result<PrepareNotificationDelegationResponse, NotificationDelegationError> {
    check_notification_access(anchor, browser_id, &origin)?;

    let account = read_account(anchor_number, &origin, account_number)?;
    let seed = notification_pull_seed(&account);
    let message = sender_info(&origin, &account_principal(&account));
    let expiration = now_ns.saturating_add(NOTIFICATION_DELEGATION_TTL_NS);

    state::signature_map_mut(|sigs| {
        add_delegation_signature(sigs, session_key, seed.as_ref(), expiration, None, None);
        sigs.add_signature(&CanisterSigInputs {
            domain: SENDER_INFO_SIG_DOMAIN,
            seed: seed.as_ref(),
            message: &message,
        });
    });
    update_root_hash();

    Ok(PrepareNotificationDelegationResponse {
        user_key: ByteBuf::from(der_encode_canister_sig_key(seed.to_vec())),
        expiration,
        sender_info: ByteBuf::from(message),
    })
}

pub fn get(
    ValidatedGetNotificationDelegationRequest {
        anchor_number,
        origin,
        account_number,
        session_key,
        expiration,
        ..
    }: ValidatedGetNotificationDelegationRequest,
    anchor: &Anchor,
    browser_id: BrowserId,
) -> Result<GetNotificationDelegationResponse, NotificationDelegationError> {
    check_notification_access(anchor, browser_id, &origin)?;

    // Nothing can have been prepared before the salt was set, and deriving a
    // seed without one traps.
    if storage_borrow(|storage| storage.salt().is_none()) {
        return Err(NotificationDelegationError::NoSuchDelegation);
    }

    let account = read_account(anchor_number, &origin, account_number)?;
    let seed = notification_pull_seed(&account);
    let message = sender_info(&origin, &account_principal(&account));

    state::assets_and_signatures(|certified_assets, sigs| {
        let root_hash = Some(certified_assets.root_hash());
        let delegation_signature = sigs
            .get_signature_as_cbor(
                &CanisterSigInputs {
                    domain: DELEGATION_SIG_DOMAIN,
                    seed: seed.as_ref(),
                    message: &delegation_signature_msg_with_permissions(
                        &session_key,
                        expiration,
                        None,
                        None,
                    ),
                },
                root_hash,
            )
            .map_err(|_| NotificationDelegationError::NoSuchDelegation)?;
        let sender_info_signature = sigs
            .get_signature_as_cbor(
                &CanisterSigInputs {
                    domain: SENDER_INFO_SIG_DOMAIN,
                    seed: seed.as_ref(),
                    message: &message,
                },
                root_hash,
            )
            .map_err(|_| NotificationDelegationError::NoSuchDelegation)?;

        Ok(GetNotificationDelegationResponse {
            delegation: SignedDelegation {
                delegation: Delegation {
                    pubkey: session_key,
                    expiration,
                    targets: None,
                    permissions: None,
                },
                signature: ByteBuf::from(delegation_signature),
            },
            sender_info_signature: ByteBuf::from(sender_info_signature),
        })
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::state::{storage_borrow_mut, storage_replace};
    use crate::storage::Storage;
    use ic_stable_structures::VectorMemory;

    const ORIGIN: &str = "https://some-dapp.com";
    const OTHER_ORIGIN: &str = "https://other-dapp.com";

    fn setup() {
        storage_replace(Storage::new((0, 10_000), VectorMemory::default()));
        storage_borrow_mut(|s| s.update_salt([7u8; 32]));
    }

    fn account(anchor: AnchorNumber, origin: &str) -> Account {
        Account::synthetic(anchor, origin.to_string())
    }

    fn principal(byte: u8) -> Principal {
        Principal::self_authenticating([byte; 32])
    }

    #[test]
    fn seed_is_deterministic() {
        setup();
        assert_eq!(
            notification_pull_seed(&account(42, ORIGIN)),
            notification_pull_seed(&account(42, ORIGIN))
        );
    }

    /// The whole point of the separate seed: it is not the account's, so the
    /// principal derived from it holds none of that account's authority.
    #[test]
    fn the_seed_is_not_the_account_seed() {
        setup();
        let account = account(42, ORIGIN);
        assert_ne!(
            notification_pull_seed(&account).as_slice(),
            account.calculate_seed().as_slice()
        );
    }

    /// Two apps must not see the same caller, or they could link the user.
    #[test]
    fn the_seed_differs_per_origin_and_anchor() {
        setup();
        let base = notification_pull_seed(&account(42, ORIGIN));
        assert_ne!(base, notification_pull_seed(&account(42, OTHER_ORIGIN)));
        assert_ne!(base, notification_pull_seed(&account(43, ORIGIN)));
    }

    /// The bytes the app reads say which app and which account, so a change in
    /// either has to change them.
    #[test]
    fn sender_info_binds_the_origin_and_the_account() {
        let first = sender_info(&ORIGIN.to_string(), &principal(1));

        assert_eq!(first, sender_info(&ORIGIN.to_string(), &principal(1)));
        assert_ne!(first, sender_info(&OTHER_ORIGIN.to_string(), &principal(1)));
        assert_ne!(first, sender_info(&ORIGIN.to_string(), &principal(2)));
    }

    #[test]
    fn sender_info_carries_the_account_principal() {
        let account_principal = principal(1);
        let info = sender_info(&ORIGIN.to_string(), &account_principal);
        let bytes = account_principal.as_slice();

        assert!(
            info.windows(bytes.len()).any(|window| window == bytes),
            "sender_info must carry the account principal"
        );
    }
}
