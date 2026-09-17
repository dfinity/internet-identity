//! Per-app consent: whether an app may notify an identity.

use super::{
    authorize_query, authorize_update, check_enabled, consent_origin, feature_enabled,
    NotificationError,
};
use crate::state::{storage_borrow, storage_borrow_mut};
use crate::storage::storable::application::StorableOriginSha256;
use crate::storage::storable::notifications::consent::StorableNotificationConsent;
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, FrontendHostname, Timestamp,
};

/// Overflow evicts the oldest grant rather than rejecting the new one. It fails
/// safe either way: an evicted app loses an authorization, so it stops being
/// able to notify and is offered again on the user's next visit.
pub const MAX_CONSENTS_PER_ANCHOR: u64 = 50;

fn set_consent(
    anchor_number: AnchorNumber,
    origin: FrontendHostname,
    now_ns: Timestamp,
) -> Result<(), NotificationError> {
    let origin = consent_origin(&origin)?;
    let origin_hash = StorableOriginSha256::from_origin(&origin);

    storage_borrow_mut(|storage| {
        let key = (anchor_number, origin_hash);
        let is_new_app = storage.notifications_consent_memory.get(&key).is_none();

        if is_new_app {
            let start = (anchor_number, StorableOriginSha256::MIN);
            let end = (anchor_number, StorableOriginSha256::MAX);
            let existing: Vec<(StorableOriginSha256, Timestamp)> = storage
                .notifications_consent_memory
                .range(start..=end)
                .map(|((_, hash), consent)| (hash, consent.granted_at_ns))
                .collect();

            if existing.len() as u64 >= MAX_CONSENTS_PER_ANCHOR {
                if let Some((oldest_hash, _)) = existing.into_iter().min_by_key(|(_, at)| *at) {
                    // The key came from the scan just above, so the removal must
                    // hit; a miss means the row vanished underneath us.
                    let removed = storage
                        .notifications_consent_memory
                        .remove(&(anchor_number, oldest_hash));
                    debug_assert!(removed.is_some(), "evicted a consent that was not present");
                }
            }
        }

        storage.notifications_consent_memory.insert(
            key,
            StorableNotificationConsent {
                origin,
                granted_at_ns: now_ns,
            },
        );
    });
    Ok(())
}

fn clear_consent(
    anchor_number: AnchorNumber,
    origin: FrontendHostname,
) -> Result<(), NotificationError> {
    let origin = consent_origin(&origin)?;
    let origin_hash = StorableOriginSha256::from_origin(&origin);

    storage_borrow_mut(|storage| {
        storage
            .notifications_consent_memory
            .remove(&(anchor_number, origin_hash));
    });
    Ok(())
}

pub(crate) fn has_consent(anchor_number: AnchorNumber, origin: FrontendHostname) -> bool {
    let Ok(origin) = consent_origin(&origin) else {
        return false;
    };
    let origin_hash = StorableOriginSha256::from_origin(&origin);
    storage_borrow(|storage| {
        storage
            .notifications_consent_memory
            .contains_key(&(anchor_number, origin_hash))
    })
}

// ---- caller-facing entry points (called from main.rs's thin wrappers) ----

/// Grants `origin` permission to notify the caller's anchor.
pub fn grant_consent(
    anchor_number: AnchorNumber,
    origin: FrontendHostname,
) -> Result<(), NotificationError> {
    check_enabled()?;
    authorize_update(anchor_number)?;
    set_consent(anchor_number, origin, ic_cdk::api::time())
}

/// Revokes `origin`'s consent. Device subscriptions stay — they're shared
/// across every consented app.
pub fn revoke_consent(
    anchor_number: AnchorNumber,
    origin: FrontendHostname,
) -> Result<(), NotificationError> {
    check_enabled()?;
    authorize_update(anchor_number)?;
    clear_consent(anchor_number, origin)
}

/// Whether `origin` may notify this identity. Answers `false` rather than an
/// error for an unauthorized or disabled call, so a caller cannot use it to
/// probe either.
pub fn consent_status(anchor_number: AnchorNumber, origin: FrontendHostname) -> bool {
    feature_enabled() && authorize_query(anchor_number) && has_consent(anchor_number, origin)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::delegation::FRONTEND_HOSTNAME_LIMIT;
    use crate::notifications::test_setup as setup;

    fn origins_of(anchor: AnchorNumber) -> Vec<FrontendHostname> {
        storage_borrow(|s| {
            let start = (anchor, StorableOriginSha256::MIN);
            let end = (anchor, StorableOriginSha256::MAX);
            s.notifications_consent_memory
                .range(start..=end)
                .map(|(_, consent)| consent.origin)
                .collect()
        })
    }

    #[test]
    fn grant_then_revoke_consent_round_trips() {
        setup();
        let anchor = 1;
        let origin = "https://app.example".to_string();

        set_consent(anchor, origin.clone(), 1_000).unwrap();
        assert!(has_consent(anchor, origin.clone()));
        assert_eq!(origins_of(anchor), vec![origin.clone()]);

        clear_consent(anchor, origin.clone()).unwrap();
        assert!(!has_consent(anchor, origin));
        assert!(origins_of(anchor).is_empty());
    }

    #[test]
    fn revoking_unconsented_origin_is_a_harmless_no_op() {
        setup();
        assert!(clear_consent(1, "https://app.example".to_string()).is_ok());
    }

    #[test]
    fn consent_rejects_oversized_origin() {
        setup();
        let too_long = "a".repeat(FRONTEND_HOSTNAME_LIMIT + 1);
        assert!(set_consent(1, too_long, 0).is_err());
    }

    #[test]
    fn consent_rejects_non_https_origin() {
        setup();
        assert!(set_consent(1, "http://app.example".to_string(), 0).is_err());
    }

    /// A grant naming a modern gateway must be the same row as one naming the
    /// legacy gateway, or the first grant is unrevocable and never delivers.
    #[test]
    fn a_gateway_twin_is_the_same_consent() {
        setup();
        let anchor = 1;
        set_consent(anchor, "https://abc-cai.icp0.io".to_string(), 1_000).unwrap();

        assert!(has_consent(anchor, "https://abc-cai.ic0.app".to_string()));
        assert!(has_consent(anchor, "https://abc-cai.icp.net".to_string()));
        assert_eq!(
            origins_of(anchor).len(),
            1,
            "one app must not occupy three consent rows"
        );

        clear_consent(anchor, "https://abc-cai.icp.net".to_string()).unwrap();
        assert!(!has_consent(anchor, "https://abc-cai.icp0.io".to_string()));
    }

    #[test]
    fn evicts_the_oldest_grant_past_the_cap() {
        setup();
        let anchor = 1;
        for i in 0..MAX_CONSENTS_PER_ANCHOR {
            set_consent(anchor, format!("https://app{i}.example"), i).unwrap();
        }
        assert_eq!(origins_of(anchor).len() as u64, MAX_CONSENTS_PER_ANCHOR);

        set_consent(anchor, "https://new.example".to_string(), 1_000).unwrap();

        assert_eq!(
            origins_of(anchor).len() as u64,
            MAX_CONSENTS_PER_ANCHOR,
            "the cap must hold past the 51st grant"
        );
        assert!(
            !has_consent(anchor, "https://app0.example".to_string()),
            "the oldest grant should have been evicted"
        );
        assert!(has_consent(anchor, "https://new.example".to_string()));
    }

    #[test]
    fn regranting_a_consented_app_does_not_evict() {
        setup();
        let anchor = 1;
        for i in 0..MAX_CONSENTS_PER_ANCHOR {
            set_consent(anchor, format!("https://app{i}.example"), i).unwrap();
        }
        // A re-grant overwrites in place, so it must not push anyone out.
        set_consent(anchor, "https://app5.example".to_string(), 9_000).unwrap();

        assert_eq!(origins_of(anchor).len() as u64, MAX_CONSENTS_PER_ANCHOR);
        assert!(has_consent(anchor, "https://app0.example".to_string()));
    }
}
