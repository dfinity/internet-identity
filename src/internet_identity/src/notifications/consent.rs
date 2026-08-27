//! Per-app consent and the `recipient principal -> anchor` reverse index the
//! send path uses. Channel-agnostic: deliverability per channel is reported by
//! [`consent_status`].

use super::webpush::subscription::has_subscribed_device;
use super::{authorize_query, authorize_update, check_enabled, feature_enabled, validate_origin};
use crate::state::{storage_borrow, storage_borrow_mut};
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

// ---- caller-facing entry points (called from main.rs's thin wrappers) ----

/// Grants `origin` permission to notify the caller's anchor.
pub fn grant_consent(anchor_number: AnchorNumber, origin: FrontendHostname) -> Result<(), String> {
    check_enabled()?;
    authorize_update(anchor_number)?;
    let recipient = crate::delegation::get_principal(anchor_number, origin.clone());
    set_consent(anchor_number, origin, recipient, ic_cdk::api::time())
}

/// Revokes `origin`'s consent. Device subscriptions stay — they're shared
/// across every consented app.
pub fn revoke_consent(anchor_number: AnchorNumber, origin: FrontendHostname) -> Result<(), String> {
    check_enabled()?;
    authorize_update(anchor_number)?;
    let recipient = crate::delegation::get_principal(anchor_number, origin.clone());
    clear_consent(anchor_number, origin, recipient)
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
    use crate::notifications::test_setup as setup;
    use crate::notifications::MAX_ORIGIN_LEN;

    #[test]
    fn grant_then_revoke_consent_round_trips() {
        setup();
        let anchor = 1;
        let origin = "https://app.example".to_string();
        // Any principal exercises the same path; the real `get_principal`
        // needs a canister.
        let recipient = Principal::from_slice(&[3u8; 10]);

        set_consent(anchor, origin.clone(), recipient, 1_000).unwrap();
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
        assert!(set_consent(1, too_long, recipient, 0).is_err());
    }

    #[test]
    fn consent_rejects_non_https_origin() {
        setup();
        let recipient = Principal::from_slice(&[3u8; 10]);
        assert!(set_consent(1, "http://app.example".to_string(), recipient, 0).is_err());
    }
}
