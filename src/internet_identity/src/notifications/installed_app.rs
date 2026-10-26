//! The record a Home Screen app has of its own, which browsers point at. One installed
//! app is one record however many identities it serves.

use crate::storage::anchor::WebPushSubscription;
use candid::Principal;
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, NotificationId, Timestamp,
};
use std::collections::BTreeSet;

pub use internet_identity_interface::internet_identity::types::NotificationAppId;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NotificationApp {
    pub id: NotificationAppId,
    /// The principal the app signs as, self-authenticating over the key it generated in
    /// its own partition. What an app-facing caller is matched against.
    pub principal: Principal,
    pub created_at_ns: Timestamp,
    /// Stamped by the app's own update calls only, so a query leaves it where it was.
    pub last_used_ns: Timestamp,
    /// The identities whose browsers point at this app. Also the reference count: a
    /// record with none left is deleted.
    pub anchors: BTreeSet<AnchorNumber>,
    /// One subscription for the whole record, absent until the app has permission.
    pub webpush_subscription: Option<WebPushSubscription>,
    /// The last time an entry asked this app to prove it is still installed.
    pub pending_probe: Option<PendingProbe>,
}

/// How long a record has to answer a probe before it counts as gone.
///
/// Above the six seconds the install page waits, so a page that has given up has not yet
/// made the canister give up, and the user is never offered an install the canister would
/// then refuse.
pub const PROBE_ANSWER_WINDOW_NS: u64 = 10 * 1_000_000_000;

/// A probe an entry sent to this record, and when it sent it.
///
/// The ask is a browser's and the answer is the app's: an app answers by calling, which
/// moves `last_used`. Nothing else here says it answered, so both halves of the question
/// live on the record the answer lands on.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct PendingProbe {
    pub notification_id: NotificationId,
    pub asked_at_ns: Timestamp,
}

impl NotificationApp {
    /// The probe still inside its window, which a second ask is handed back instead of
    /// sending another to the same address.
    pub fn outstanding_probe(&self, now_ns: Timestamp) -> Option<NotificationId> {
        let probe = self.pending_probe?;
        (now_ns.saturating_sub(probe.asked_at_ns) < PROBE_ANSWER_WINDOW_NS)
            .then_some(probe.notification_id)
    }

    /// Whether this record was asked to prove itself, has had its window, and never
    /// called. That is the whole of the evidence a Home Screen app is gone: deleting one
    /// tells the canister nothing, and the push service goes on accepting for it.
    pub fn probe_went_unanswered(&self, now_ns: Timestamp) -> bool {
        self.pending_probe.is_some_and(|probe| {
            now_ns.saturating_sub(probe.asked_at_ns) >= PROBE_ANSWER_WINDOW_NS
                && self.last_used_ns < probe.asked_at_ns
        })
    }
}

/// What a browser holds about a Home Screen app: an install it has handed a key to, or
/// the record it ended up pointing at. Never both.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum NotificationAppRef {
    /// An install this browser started. The principal is the one-shot key it put in the
    /// URL fragment, and the only caller that may complete the link.
    Pending {
        link_principal: Principal,
        expires_at_ns: Timestamp,
    },
    /// The record this browser points at. Outlives signing out and turning delivery off,
    /// so coming back is signing in or flipping the switch rather than installing again.
    Linked {
        app_id: NotificationAppId,
        /// The subscription is the app's and is shared, so turning off never removes it.
        enabled: bool,
    },
}

/// The record `app_id` names, or `None` where the deployment does not notify, so a
/// deployment with notifications off never reports an app however its anchors read.
pub fn record(app_id: NotificationAppId) -> Option<NotificationApp> {
    if !super::notifications_enabled() {
        return None;
    }
    crate::state::storage_borrow(|storage| storage.notification_app(app_id))
}

impl NotificationAppRef {
    /// The record this browser points at, where the install has completed.
    pub fn linked_app(&self) -> Option<NotificationAppId> {
        match self {
            NotificationAppRef::Pending { .. } => None,
            NotificationAppRef::Linked { app_id, .. } => Some(*app_id),
        }
    }

    /// Whether this identity wants the app to deliver for it on this device. An install
    /// that has not completed delivers nothing either.
    pub fn is_delivering(&self) -> bool {
        match self {
            NotificationAppRef::Pending { .. } => false,
            NotificationAppRef::Linked { enabled, .. } => *enabled,
        }
    }
}

#[cfg(test)]
mod probe_tests {
    use super::*;

    fn asked_at(asked_at_ns: Timestamp, last_used: Timestamp) -> NotificationApp {
        NotificationApp {
            id: 1,
            principal: Principal::anonymous(),
            created_at_ns: 0,
            last_used_ns: last_used,
            anchors: BTreeSet::new(),
            webpush_subscription: None,
            pending_probe: Some(PendingProbe {
                notification_id: 7,
                asked_at_ns,
            }),
        }
    }

    #[test]
    fn a_record_never_asked_has_nothing_outstanding_and_nothing_to_answer() {
        let never = NotificationApp {
            pending_probe: None,
            ..asked_at(1_000, 0)
        };

        assert_eq!(never.outstanding_probe(1_000), None);
        assert!(!never.probe_went_unanswered(u64::MAX));
    }

    #[test]
    fn an_ask_inside_its_window_is_handed_back_rather_than_asked_again() {
        let app = asked_at(1_000, 0);

        assert_eq!(app.outstanding_probe(1_000), Some(7));
        assert_eq!(
            app.outstanding_probe(1_000 + PROBE_ANSWER_WINDOW_NS - 1),
            Some(7)
        );
        assert_eq!(app.outstanding_probe(1_000 + PROBE_ANSWER_WINDOW_NS), None);
    }

    /// The window is what makes an ask a question with an end to it; inside it the app
    /// still has time, so the app is not yet gone.
    #[test]
    fn an_ask_is_unanswered_only_once_its_window_is_up() {
        let app = asked_at(1_000, 0);

        assert!(!app.probe_went_unanswered(1_000 + PROBE_ANSWER_WINDOW_NS - 1));
        assert!(app.probe_went_unanswered(1_000 + PROBE_ANSWER_WINDOW_NS));
    }

    /// An app answers by calling, which is the only thing that moves `last_used`. A call
    /// from before the ask answers nothing.
    #[test]
    fn only_a_call_made_after_the_ask_answers_it() {
        let answered = asked_at(1_000, 1_001);
        let stale = asked_at(1_000, 999);

        assert!(!answered.probe_went_unanswered(u64::MAX));
        assert!(stale.probe_went_unanswered(u64::MAX));
    }
}
