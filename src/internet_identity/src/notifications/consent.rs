//! Per-app consent and the `recipient principal -> anchor` reverse index the
//! send path uses. Channel-agnostic: deliverability per channel is reported by
//! [`consent_status`].

use super::webpush::seal::{drop_origin_seals, seal_devices_for_origin};
use super::webpush::subscription::has_subscribed_device;
use super::{authorize_query, authorize_update, check_enabled, feature_enabled, validate_origin};
use crate::delegation::der_encode_canister_sig_key;
use crate::state::{storage_borrow, storage_borrow_mut};
use crate::storage::account::ReadAccountParams;
use crate::storage::storable::application::StorableOriginSha256;
use crate::storage::storable::notifications::consent::StorableNotificationConsent;
use candid::Principal;
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, FrontendHostname, Timestamp,
};

/// A delivery channel. Web Push is the only one today.
#[derive(candid::CandidType, serde::Deserialize, Clone, Copy, Debug, PartialEq, Eq)]
pub enum NotificationChannel {
    #[serde(rename = "push")]
    Push,
}

/// Whether an app may notify this identity, and which channels can reach it.
#[derive(candid::CandidType, serde::Deserialize, Clone, Debug, Default)]
pub struct NotificationConsentStatus {
    pub consented: Option<bool>,
    pub deliverable_channels: Option<Vec<NotificationChannel>>,
}

/// `recipient` is passed in, not derived here: `delegation::get_principal`
/// needs `ic_cdk::api::id()`, which only runs on-canister, so keeping it out
/// leaves this testable.
fn set_consent(
    anchor_number: AnchorNumber,
    origin: FrontendHostname,
    recipient: Principal,
    account_number: Option<u64>,
    now_ns: Timestamp,
) -> Result<(), String> {
    validate_origin(&origin)?;
    let origin_hash = StorableOriginSha256::from_origin(&origin);

    storage_borrow_mut(|storage| {
        storage.notifications_consent_memory.insert(
            (anchor_number, origin_hash),
            StorableNotificationConsent {
                origin,
                granted_at_ns: now_ns,
                last_sent_ns: None,
                muted: None,
                account_number,
            },
        );
        storage
            .notifications_recipient_index_memory
            .insert(recipient, anchor_number);
    });
    Ok(())
}

/// See [`set_consent`] on why `recipient` is passed in.
fn clear_consent(
    anchor_number: AnchorNumber,
    origin: FrontendHostname,
    recipient: Principal,
) -> Result<(), String> {
    validate_origin(&origin)?;
    let origin_hash = StorableOriginSha256::from_origin(&origin);

    storage_borrow_mut(|storage| {
        storage
            .notifications_consent_memory
            .remove(&(anchor_number, origin_hash));
        storage
            .notifications_recipient_index_memory
            .remove(&recipient);
    });
    Ok(())
}

fn has_consent(anchor_number: AnchorNumber, origin: FrontendHostname) -> bool {
    if validate_origin(&origin).is_err() {
        return false;
    }
    let origin_hash = StorableOriginSha256::from_origin(&origin);
    storage_borrow(|storage| {
        storage
            .notifications_consent_memory
            .contains_key(&(anchor_number, origin_hash))
    })
}

/// The principal an app addresses this identity by, derived the same way the
/// sign-in delegation was: an added account seeds from its account number, both
/// kinds of default account from the anchor. Deriving it from the anchor alone
/// would record a principal the app never sees, so a send from an added account
/// would find no consent.
fn recipient_seed(
    anchor_number: AnchorNumber,
    origin: &FrontendHostname,
    account_number: Option<u64>,
) -> Result<ic_certification::Hash, String> {
    storage_borrow(|storage| {
        storage.read_account(ReadAccountParams {
            account_number,
            anchor_number,
            origin,
            known_app_num: None,
        })
    })
    .map(|account| account.calculate_seed())
    .ok_or_else(|| "no such account for that origin".to_string())
}

/// Wraps [`recipient_seed`] into the principal itself. Separate because the DER
/// encoding reads the canister's own id, which only exists on-canister.
fn recipient_principal(
    anchor_number: AnchorNumber,
    origin: &FrontendHostname,
    account_number: Option<u64>,
) -> Result<Principal, String> {
    let seed = recipient_seed(anchor_number, origin, account_number)?;
    Ok(Principal::self_authenticating(der_encode_canister_sig_key(
        seed.to_vec(),
    )))
}

// ---- caller-facing entry points (called from main.rs's thin wrappers) ----

/// Grants `origin` permission to notify the caller's anchor.
pub async fn grant_consent(
    anchor_number: AnchorNumber,
    origin: FrontendHostname,
    account_number: Option<u64>,
) -> Result<(), String> {
    check_enabled()?;
    authorize_update(anchor_number)?;
    let now_ns = ic_cdk::api::time();
    let recipient = recipient_principal(anchor_number, &origin, account_number)?;
    set_consent(
        anchor_number,
        origin.clone(),
        recipient,
        account_number,
        now_ns,
    )?;
    seal_devices_for_origin(anchor_number, &origin, now_ns).await;
    Ok(())
}

/// Revokes `origin`'s consent. Device subscriptions stay — they're shared
/// across every consented app.
pub fn revoke_consent(anchor_number: AnchorNumber, origin: FrontendHostname) -> Result<(), String> {
    check_enabled()?;
    authorize_update(anchor_number)?;
    // The account the consent was granted from decides which principal the
    // index holds, so read it back rather than re-deriving from the anchor.
    let account_number = storage_borrow(|storage| {
        storage
            .notifications_consent_memory
            .get(&(anchor_number, StorableOriginSha256::from_origin(&origin)))
            .and_then(|consent| consent.account_number)
    });
    let recipient = recipient_principal(anchor_number, &origin, account_number)?;
    clear_consent(anchor_number, origin.clone(), recipient)?;
    drop_origin_seals(anchor_number, &origin);
    Ok(())
}

/// Whether `origin` may notify this identity, and which channels can reach it.
/// The one place the generic layer queries each channel for deliverability.
pub fn consent_status(
    anchor_number: AnchorNumber,
    origin: FrontendHostname,
) -> NotificationConsentStatus {
    if !feature_enabled() || !authorize_query(anchor_number) {
        return NotificationConsentStatus::default();
    }

    let mut channels = Vec::new();
    if has_subscribed_device(anchor_number) {
        channels.push(NotificationChannel::Push);
    }

    NotificationConsentStatus {
        consented: Some(has_consent(anchor_number, origin)),
        deliverable_channels: Some(channels),
    }
}

/// Every origin the caller's anchor has consented to.
pub fn consented_origins(anchor_number: AnchorNumber) -> Vec<FrontendHostname> {
    if !feature_enabled() || !authorize_query(anchor_number) {
        return Vec::new();
    }
    storage_borrow(|storage| storage.notifications_consented_origins(anchor_number))
}

/// One consented app with its metadata, for the Settings notifications page.
#[derive(candid::CandidType, serde::Deserialize, Clone, Debug)]
pub struct NotificationConsentedApp {
    pub origin: FrontendHostname,
    pub granted_at_ns: Timestamp,
    pub last_sent_ns: Option<Timestamp>,
    pub muted: bool,
    /// The account the consent was granted from; `None` = the default account.
    pub account_number: Option<u64>,
}

/// Every consented app for the caller's anchor, with metadata.
pub fn consented_apps(anchor_number: AnchorNumber) -> Vec<NotificationConsentedApp> {
    if !feature_enabled() || !authorize_query(anchor_number) {
        return Vec::new();
    }
    storage_borrow(|storage| {
        storage
            .notifications_consented_apps(anchor_number)
            .into_iter()
            .map(|c| NotificationConsentedApp {
                origin: c.origin,
                granted_at_ns: c.granted_at_ns,
                last_sent_ns: c.last_sent_ns,
                muted: c.muted.unwrap_or(false),
                account_number: c.account_number,
            })
            .collect()
    })
}

/// Mutes or unmutes an already-consented app. Muting keeps the consent row (and
/// its seals) but the send path skips it; unmuting resumes delivery. Errors if
/// the app isn't consented.
pub fn set_app_muted(
    anchor_number: AnchorNumber,
    origin: FrontendHostname,
    muted: bool,
) -> Result<(), String> {
    check_enabled()?;
    authorize_update(anchor_number)?;
    validate_origin(&origin)?;
    let origin_hash = StorableOriginSha256::from_origin(&origin);
    storage_borrow_mut(|storage| {
        let key = (anchor_number, origin_hash);
        let Some(mut consent) = storage.notifications_consent_memory.get(&key) else {
            return Err("no consent for that origin".to_string());
        };
        consent.muted = Some(muted);
        storage.notifications_consent_memory.insert(key, consent);
        Ok(())
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::delegation::{calculate_account_seed, calculate_anchor_seed};
    use crate::notifications::test_setup as setup;
    use crate::notifications::MAX_ORIGIN_LEN;
    use crate::storage::account::CreateAccountParams;

    #[test]
    fn grant_then_revoke_consent_round_trips() {
        setup();
        let anchor = 1;
        let origin = "https://app.example".to_string();
        // Any principal exercises the same path; the real `get_principal`
        // needs a canister.
        let recipient = Principal::from_slice(&[3u8; 10]);

        set_consent(anchor, origin.clone(), recipient, None, 1_000).unwrap();
        assert!(has_consent(anchor, origin.clone()));
        assert_eq!(
            storage_borrow(|s| s.notifications_consented_origins(anchor)),
            vec![origin.clone()]
        );

        assert_eq!(
            storage_borrow(|s| s.notifications_recipient_index_memory.get(&recipient)),
            Some(anchor)
        );

        clear_consent(anchor, origin.clone(), recipient).unwrap();
        assert!(!has_consent(anchor, origin.clone()));
        assert!(storage_borrow(|s| s.notifications_consented_origins(anchor)).is_empty());
        assert_eq!(
            storage_borrow(|s| s.notifications_recipient_index_memory.get(&recipient)),
            None
        );
    }

    /// The bug this guards: deriving the recipient from the anchor alone
    /// recorded a principal an added account's app never sees, so its sends
    /// found no consent.
    #[test]
    fn an_added_account_is_addressed_by_its_own_principal() {
        setup();
        let anchor = 1;
        let origin = "https://app.example".to_string();
        let added = storage_borrow_mut(|storage| {
            storage
                .create_additional_account(CreateAccountParams {
                    anchor_number: anchor,
                    name: "second".to_string(),
                    origin: origin.clone(),
                })
                .expect("failed to create the added account")
        });
        let account_number = added.account_number.expect("added account has a number");

        // Seeds, not principals: the principal wraps the seed with the
        // canister's own id, which does not exist off-canister.
        let default_seed = recipient_seed(anchor, &origin, None).unwrap();
        let added_seed = recipient_seed(anchor, &origin, Some(account_number)).unwrap();
        assert_ne!(
            default_seed, added_seed,
            "an added account must not share the default account's principal"
        );
        assert_eq!(
            default_seed,
            calculate_anchor_seed(anchor, &origin),
            "a default account is addressed by the anchor's principal"
        );
        assert_eq!(
            added_seed,
            calculate_account_seed(account_number, &origin),
            "an added account is addressed by the principal its sign-in derives"
        );
    }

    #[test]
    fn revoking_unconsented_origin_is_a_harmless_no_op() {
        setup();
        let recipient = Principal::from_slice(&[3u8; 10]);
        assert!(clear_consent(1, "https://app.example".to_string(), recipient).is_ok());
    }

    #[test]
    fn consent_rejects_oversized_origin() {
        setup();
        let recipient = Principal::from_slice(&[3u8; 10]);
        let too_long = "a".repeat(MAX_ORIGIN_LEN + 1);
        assert!(set_consent(1, too_long, recipient, None, 0).is_err());
    }

    #[test]
    fn consent_rejects_non_https_origin() {
        setup();
        let recipient = Principal::from_slice(&[3u8; 10]);
        assert!(set_consent(1, "http://app.example".to_string(), recipient, None, 0).is_err());
    }
}
