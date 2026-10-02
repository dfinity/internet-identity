//! Turning a caller's consent request into one the consent logic can act on: an origin
//! this deployment notifies for, spelled the one way it is keyed by.

use crate::delegation::frontend_length_within_limit;
use internet_identity_interface::internet_identity::types::attributes::remap_to_legacy_domain;
use internet_identity_interface::internet_identity::types::{
    AccountNumber, AnchorNumber, FrontendHostname, GetNextNotificationError,
    GetNextNotificationRequest, GetNotificationDelegationRequest, Notification,
    NotificationConsentGrantedRequest, NotificationDelegationError, NotificationGrantConsentError,
    NotificationGrantConsentRequest, NotificationRevokeConsentError,
    NotificationRevokeConsentRequest, NotificationToShow, PrepareNotificationDelegationRequest,
    RemoveNotificationError, RemoveNotificationRequest, SendNotificationArg, SendNotificationError,
    SessionKey, Timestamp,
};
use std::collections::HashMap;
use url::Url;

/// Held by every validated request below. Private to this module, so the `TryFrom`
/// impls are the only way to build one and holding a request is proof its origin has
/// been through them.
struct Validated;

pub struct ValidatedNotificationGrantConsentRequest {
    pub anchor_number: AnchorNumber,
    pub origin: FrontendHostname,
    _validated: Validated,
}

pub struct ValidatedNotificationRevokeConsentRequest {
    pub anchor_number: AnchorNumber,
    pub origin: FrontendHostname,
    _validated: Validated,
}

pub struct ValidatedNotificationConsentGrantedRequest {
    pub anchor_number: AnchorNumber,
    pub origin: FrontendHostname,
    _validated: Validated,
}

/// Bounds the work one message asks for, not what it could enqueue: a batch
/// may name one pair many times. Fixed, never a capacity signal.
pub const MAX_NOTIFICATIONS_PER_APP_CALL: usize = 1_000;

pub struct ValidatedSendNotificationArg {
    pub origin: FrontendHostname,
    pub notifications: Vec<Notification>,
    _validated: Validated,
}

pub struct ValidatedGetNextNotificationRequest {
    pub anchor_number: AnchorNumber,
    _validated: Validated,
}

impl TryFrom<GetNextNotificationRequest> for ValidatedGetNextNotificationRequest {
    type Error = GetNextNotificationError;

    fn try_from(
        GetNextNotificationRequest { anchor_number }: GetNextNotificationRequest,
    ) -> Result<Self, Self::Error> {
        if !notifications_enabled() {
            return Err(GetNextNotificationError::InternalCanisterError(
                NOT_ENABLED.to_string(),
            ));
        }
        Ok(Self {
            anchor_number,
            _validated: Validated,
        })
    }
}

pub struct ValidatedRemoveNotificationRequest {
    pub anchor_number: AnchorNumber,
    pub notification: NotificationToShow,
    _validated: Validated,
}

impl TryFrom<RemoveNotificationRequest> for ValidatedRemoveNotificationRequest {
    type Error = RemoveNotificationError;

    fn try_from(
        RemoveNotificationRequest {
            anchor_number,
            notification,
        }: RemoveNotificationRequest,
    ) -> Result<Self, Self::Error> {
        if !notifications_enabled() {
            return Err(RemoveNotificationError::InternalCanisterError(
                NOT_ENABLED.to_string(),
            ));
        }
        Ok(Self {
            anchor_number,
            notification,
            _validated: Validated,
        })
    }
}

const NOT_ENABLED: &str = "notifications are not enabled";

impl TryFrom<NotificationGrantConsentRequest> for ValidatedNotificationGrantConsentRequest {
    type Error = NotificationGrantConsentError;

    fn try_from(
        NotificationGrantConsentRequest {
            anchor_number,
            origin,
        }: NotificationGrantConsentRequest,
    ) -> Result<Self, Self::Error> {
        Ok(Self {
            anchor_number,
            origin: notifying_origin(&origin)
                .map_err(NotificationGrantConsentError::InternalCanisterError)?,
            _validated: Validated,
        })
    }
}

impl TryFrom<NotificationRevokeConsentRequest> for ValidatedNotificationRevokeConsentRequest {
    type Error = NotificationRevokeConsentError;

    fn try_from(
        NotificationRevokeConsentRequest {
            anchor_number,
            origin,
        }: NotificationRevokeConsentRequest,
    ) -> Result<Self, Self::Error> {
        Ok(Self {
            anchor_number,
            origin: notifying_origin(&origin)
                .map_err(NotificationRevokeConsentError::InternalCanisterError)?,
            _validated: Validated,
        })
    }
}

impl TryFrom<SendNotificationArg> for ValidatedSendNotificationArg {
    type Error = SendNotificationError;

    fn try_from(
        SendNotificationArg {
            origin,
            notifications,
        }: SendNotificationArg,
    ) -> Result<Self, Self::Error> {
        if notifications.len() > MAX_NOTIFICATIONS_PER_APP_CALL {
            return Err(SendNotificationError::TooManyNotifications {
                limit: MAX_NOTIFICATIONS_PER_APP_CALL as u32,
            });
        }
        let origin = notifying_origin(&origin)
            .and_then(|origin| fetchable_origin(&origin).map(|()| origin))
            .map_err(SendNotificationError::InternalCanisterError)?;
        Ok(Self {
            origin,
            notifications: dedup_last_wins(notifications),
            _validated: Validated,
        })
    }
}

pub struct ValidatedPrepareNotificationDelegationRequest {
    pub anchor_number: AnchorNumber,
    pub origin: FrontendHostname,
    pub account_number: Option<AccountNumber>,
    pub session_key: SessionKey,
    _validated: Validated,
}

pub struct ValidatedGetNotificationDelegationRequest {
    pub anchor_number: AnchorNumber,
    pub origin: FrontendHostname,
    pub account_number: Option<AccountNumber>,
    pub session_key: SessionKey,
    pub expiration: Timestamp,
    _validated: Validated,
}

impl TryFrom<PrepareNotificationDelegationRequest>
    for ValidatedPrepareNotificationDelegationRequest
{
    type Error = NotificationDelegationError;

    fn try_from(
        PrepareNotificationDelegationRequest {
            anchor_number,
            origin,
            account_number,
            session_key,
        }: PrepareNotificationDelegationRequest,
    ) -> Result<Self, Self::Error> {
        Ok(Self {
            origin: notifying_origin(&origin)
                .map_err(NotificationDelegationError::InternalCanisterError)?,
            anchor_number,
            account_number,
            session_key,
            _validated: Validated,
        })
    }
}

impl TryFrom<GetNotificationDelegationRequest> for ValidatedGetNotificationDelegationRequest {
    type Error = NotificationDelegationError;

    fn try_from(
        GetNotificationDelegationRequest {
            anchor_number,
            origin,
            account_number,
            session_key,
            expiration,
        }: GetNotificationDelegationRequest,
    ) -> Result<Self, Self::Error> {
        Ok(Self {
            origin: notifying_origin(&origin)
                .map_err(NotificationDelegationError::InternalCanisterError)?,
            anchor_number,
            account_number,
            session_key,
            expiration,
            _validated: Validated,
        })
    }
}

impl TryFrom<NotificationConsentGrantedRequest> for ValidatedNotificationConsentGrantedRequest {
    type Error = String;

    fn try_from(
        NotificationConsentGrantedRequest {
            anchor_number,
            origin,
        }: NotificationConsentGrantedRequest,
    ) -> Result<Self, Self::Error> {
        Ok(Self {
            anchor_number,
            origin: notifying_origin(&origin)?,
            _validated: Validated,
        })
    }
}

/// Folds `origin` to the spelling consent is keyed by, and refuses every origin while
/// this deployment does not notify.
fn notifying_origin(origin: &str) -> Result<FrontendHostname, String> {
    let origin = remap_to_legacy_domain(&canonical_origin(origin)?);
    if !notifications_enabled() {
        return Err(NOT_ENABLED.to_string());
    }
    Ok(origin)
}

/// Accepts a length-bounded, bare `https://host[:port]`. Anything else is rejected
/// rather than trimmed, so one app cannot hold two consent rows.
fn canonical_origin(origin: &str) -> Result<FrontendHostname, String> {
    if origin.is_empty() {
        return Err("origin is empty".to_string());
    }
    // Before parsing, so an enormous string is refused without being parsed.
    frontend_length_within_limit(&origin.to_string())?;

    let Ok(url) = Url::parse(origin) else {
        return Err("origin is not a URL".to_string());
    };
    // One comparison against the browser's own serialization, so credentials, path,
    // query, fragment, trailing slash, an explicit default port and uppercase host all
    // fail here. The scheme is not restricted: the push is delivered to II's own origin
    // and this is only the key consent is stored under, so an origin II already signs in
    // at is one it can be asked about.
    if url.origin().ascii_serialization() != origin {
        return Err("origin must be a bare scheme://host[:port]".to_string());
    }
    Ok(origin.to_string())
}

/// Collapses each `(recipient, id)` to its last entry, in the place the first
/// held: a re-send replaces a pending notification rather than queueing behind
/// it.
fn dedup_last_wins(notifications: Vec<Notification>) -> Vec<Notification> {
    let mut placed = HashMap::new();
    let mut kept: Vec<Notification> = Vec::with_capacity(notifications.len());
    for notification in notifications {
        let pair = (notification.recipient, notification.id);
        match placed.get(&pair) {
            Some(&index) => kept[index] = notification,
            None => {
                placed.insert(pair, kept.len());
                kept.push(notification);
            }
        }
    }
    kept
}

/// Refuses an origin II will not fetch a sender list from: `notifying_origin`
/// accepts any scheme, but a list read over plain `http` can be replaced in
/// flight. As in SSO discovery, `http` needs a loopback host and the deploy
/// flag.
fn fetchable_origin(origin: &FrontendHostname) -> Result<(), String> {
    let Ok(url) = Url::parse(origin) else {
        return Err("origin is not a URL".to_string());
    };
    if url.scheme() == "https" {
        return Ok(());
    }
    if url.scheme() == "http"
        && allow_insecure_sender_list()
        && crate::utils::is_loopback_host(url.host_str().unwrap_or_default())
    {
        return Ok(());
    }
    Err("origin must be https".to_string())
}

fn allow_insecure_sender_list() -> bool {
    crate::state::persistent_state(|s| s.notifications_allow_insecure_sender_list).unwrap_or(false)
}

/// Whether this deployment notifies, for every app at once.
pub fn notifications_enabled() -> bool {
    crate::state::persistent_state(|s| s.notifications_enabled).unwrap_or(false)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::delegation::FRONTEND_HOSTNAME_LIMIT;
    use internet_identity_interface::internet_identity::types::Urgency;
    fn set_enabled(enabled: Option<bool>) {
        crate::state::persistent_state_mut(|s| s.notifications_enabled = enabled);
    }

    fn allow_insecure(allow: bool) {
        crate::state::persistent_state_mut(|s| {
            s.notifications_allow_insecure_sender_list = Some(allow);
        });
    }

    fn notification(id: u64, recipient: &str, urgency: Urgency) -> Notification {
        Notification {
            id,
            recipient: candid::Principal::from_text(recipient).expect("a principal"),
            expires_at: None,
            urgency: Some(urgency),
        }
    }

    fn validate(notifications: Vec<Notification>) -> ValidatedSendNotificationArg {
        set_enabled(Some(true));
        SendNotificationArg {
            origin: "https://app.example".to_string(),
            notifications,
        }
        .try_into()
        .expect("a notifiable origin")
    }

    /// A batch applies in order, so a repeated (recipient, id) keeps the last
    /// entry — the one that supersedes — and the pair appears once.
    #[test]
    fn a_repeated_recipient_and_id_keeps_the_last_entry() {
        let request = validate(vec![
            notification(1, "ryjl3-tyaaa-aaaaa-aaaba-cai", Urgency::Low),
            notification(1, "ryjl3-tyaaa-aaaaa-aaaba-cai", Urgency::High),
        ]);

        assert_eq!(request.notifications.len(), 1);
        assert_eq!(request.notifications[0].urgency, Some(Urgency::High));
    }

    /// One id sent to two recipients is two notifications, not a repeat.
    #[test]
    fn one_id_for_two_recipients_survives_as_two() {
        let request = validate(vec![
            notification(1, "ryjl3-tyaaa-aaaaa-aaaba-cai", Urgency::Normal),
            notification(1, "rrkah-fqaaa-aaaaa-aaaaq-cai", Urgency::Normal),
        ]);

        assert_eq!(request.notifications.len(), 2);
    }

    /// Surviving entries keep the order they were submitted in.
    #[test]
    fn deduplication_keeps_the_submitted_order() {
        let request = validate(vec![
            notification(1, "ryjl3-tyaaa-aaaaa-aaaba-cai", Urgency::Normal),
            notification(2, "ryjl3-tyaaa-aaaaa-aaaba-cai", Urgency::Normal),
            notification(1, "ryjl3-tyaaa-aaaaa-aaaba-cai", Urgency::High),
        ]);

        let ids: Vec<u64> = request.notifications.iter().map(|n| n.id).collect();
        assert_eq!(ids, vec![1, 2]);
        assert_eq!(request.notifications[0].urgency, Some(Urgency::High));
    }

    /// The cap counts what was submitted, not what survives deduplication: a
    /// batch naming one pair many times still costs a message that much work.
    #[test]
    fn the_cap_counts_submitted_entries() {
        set_enabled(Some(true));
        let repeated = notification(1, "ryjl3-tyaaa-aaaaa-aaaba-cai", Urgency::Normal);
        let request = SendNotificationArg {
            origin: "https://app.example".to_string(),
            notifications: vec![repeated; MAX_NOTIFICATIONS_PER_APP_CALL + 1],
        };

        assert!(matches!(
            ValidatedSendNotificationArg::try_from(request),
            Err(SendNotificationError::TooManyNotifications { .. })
        ));
    }

    /// The scheme decides where II sends an outcall, so a list that would be
    /// fetched over plain http is refused however the origin is spelled.
    #[test]
    fn refuses_an_origin_whose_list_would_not_be_fetched_over_https() {
        allow_insecure(false);
        for origin in [
            "http://app.example",
            "http://localhost:5173",
            "http://127.0.0.1:8080",
        ] {
            assert!(
                fetchable_origin(&origin.to_string()).is_err(),
                "{origin} must not be fetched"
            );
        }
        assert!(fetchable_origin(&"https://app.example".to_string()).is_ok());
    }

    /// The flag reaches loopback only: a public host over http stays refused,
    /// so a flagged deployment still cannot be pointed at a plaintext app.
    #[test]
    fn the_insecure_flag_reaches_loopback_only() {
        allow_insecure(true);
        assert!(fetchable_origin(&"http://localhost:5173".to_string()).is_ok());
        assert!(fetchable_origin(&"http://127.0.0.1:8080".to_string()).is_ok());
        assert!(fetchable_origin(&"http://localhost".to_string()).is_ok());
        assert!(fetchable_origin(&"http://app.example".to_string()).is_err());
        allow_insecure(false);
    }

    /// And it reaches `http` only. A scheme II cannot fetch would fail at the
    /// outcall anyway, but the rule is what the guard states, not where the
    /// call happens to break.
    #[test]
    fn the_insecure_flag_reaches_http_only() {
        allow_insecure(true);
        for origin in ["ftp://localhost", "ws://localhost:5173", "wss://localhost"] {
            assert!(
                fetchable_origin(&origin.to_string()).is_err(),
                "{origin} must not be fetched"
            );
        }
        allow_insecure(false);
    }

    #[test]
    fn canonicalizes_every_gateway_to_the_legacy_one() {
        set_enabled(Some(true));
        for origin in [
            "https://abc-cai.icp0.io",
            "https://abc-cai.icp.net",
            "https://abc-cai.ic0.app",
        ] {
            assert_eq!(notifying_origin(origin).unwrap(), "https://abc-cai.ic0.app");
        }
    }

    #[test]
    fn leaves_a_custom_domain_alone() {
        set_enabled(Some(true));
        assert_eq!(
            notifying_origin("https://oisy.com").unwrap(),
            "https://oisy.com"
        );
    }

    /// The frontend only remaps a single `[\w-]+(.raw)?` label, so a deeper name is a
    /// different origin to it and must stay a different consent row here.
    #[test]
    fn leaves_a_deeper_subdomain_alone() {
        set_enabled(Some(true));
        assert_eq!(
            notifying_origin("https://foo.bar.icp0.io").unwrap(),
            "https://foo.bar.icp0.io"
        );
    }

    #[test]
    fn keeps_the_raw_label_the_frontend_keeps() {
        set_enabled(Some(true));
        assert_eq!(
            notifying_origin("https://abc-cai.raw.icp0.io").unwrap(),
            "https://abc-cai.raw.ic0.app"
        );
    }

    #[test]
    fn accepts_any_origin_while_enabled() {
        set_enabled(Some(true));
        assert!(notifying_origin("https://any.example").is_ok());
    }

    #[test]
    fn refuses_every_origin_while_disabled() {
        set_enabled(Some(false));
        assert!(notifying_origin("https://any.example").is_err());
    }

    #[test]
    fn a_send_is_refused_while_nothing_is_configured() {
        set_enabled(None);

        let refused = ValidatedSendNotificationArg::try_from(SendNotificationArg {
            origin: "https://app.example".to_string(),
            notifications: vec![notification(
                1,
                "ryjl3-tyaaa-aaaaa-aaaba-cai",
                Urgency::Normal,
            )],
        });

        assert!(matches!(
            refused,
            Err(SendNotificationError::InternalCanisterError(_))
        ));
    }

    #[test]
    fn refuses_every_origin_when_nothing_is_configured() {
        set_enabled(None);
        assert!(notifying_origin("https://allowed.example").is_err());
    }

    /// A browser never serializes userinfo into an origin.
    #[test]
    fn rejects_credentials_in_the_authority() {
        for origin in [
            "https://user@app.example",
            "https://user:pass@app.example",
            "https://@app.example",
        ] {
            assert!(
                canonical_origin(origin).is_err(),
                "{origin} must not be accepted"
            );
        }
    }

    /// Each parses far enough to look like an authority, and none is a spelling a
    /// browser can hand an app.
    #[test]
    fn rejects_an_authority_no_browser_can_serialize() {
        for origin in [
            // Out of range for a port, which is a u16.
            "https://app.example:99999",
            // Not a host: a space cannot appear in one.
            "https://not a host",
            // The default port, which a browser drops, so this keys a second row for one app.
            "https://app.example:443",
            // A browser lowercases the host, for the same reason.
            "https://APP.example",
            // An origin carries no path at all, not even the empty one.
            "https://app.example/",
        ] {
            assert!(
                canonical_origin(origin).is_err(),
                "{origin} must not be accepted"
            );
        }
    }

    #[test]
    fn rejects_an_over_long_origin() {
        let too_long = format!("https://{}", "a".repeat(FRONTEND_HOSTNAME_LIMIT));
        assert!(canonical_origin(&too_long).is_err());
    }

    #[test]
    fn accepts_a_bare_origin_with_an_optional_port() {
        canonical_origin("https://app.example").unwrap();
        canonical_origin("https://app.example:8443").unwrap();
    }

    /// The scheme is the app's, not the channel the push travels on: II signs in at
    /// these and the notification is delivered to II's own origin either way.
    #[test]
    fn accepts_an_origin_that_is_not_https() {
        canonical_origin("http://localhost:5173").unwrap();
        canonical_origin("http://127.0.0.1:8080").unwrap();
    }

    /// Anything past the authority would let one app hold several consent rows.
    #[test]
    fn rejects_anything_past_the_authority() {
        for origin in [
            "https://app.example/",
            "https://app.example/path",
            "https://app.example?q=1",
            "https://app.example#frag",
            "https://",
            "https://:443",
            "https://app.example:",
            "https://app.example:https",
            "http://app.example:80",
        ] {
            assert!(
                canonical_origin(origin).is_err(),
                "{origin} must not be accepted"
            );
        }
    }
}
