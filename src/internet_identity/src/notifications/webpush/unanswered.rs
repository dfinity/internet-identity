//! The pushes posted through each subscription that nothing has fetched since.
//!
//! Nothing tells Internet Identity that a Home Screen app was deleted or a browser's
//! subscription broke: a push service may go on accepting pushes for a device that will
//! never show them. What a working device does is fetch what it was woken for, so each
//! subscription counts the pushes posted through it and is reset by a fetch. A count that
//! grows and ages marks the subscription for a probe; nothing else follows from it.

use super::rotation::{self, RegistrationId};
use crate::storage::anchor::{ReadySubscription, UnansweredPushCount};
use internet_identity_interface::internet_identity::types::Timestamp;

/// Counts one push posted to `endpoint` at `now_ns` against the subscription
/// `registration` names, where it is still the registration made at `registered_at_ns`
/// there: one made since, at the same endpoint or not, was never sent it.
pub(crate) fn count_push(
    registration: RegistrationId,
    endpoint: &str,
    registered_at_ns: Timestamp,
    now_ns: Timestamp,
) -> Result<(), String> {
    let Some(mut registered) = rotation::read(registration) else {
        return Ok(());
    };
    if registered.endpoint.as_str() != endpoint || registered.created_at_ns != registered_at_ns {
        return Ok(());
    }
    registered.unanswered_pushes = Some(UnansweredPushCount::after_push(
        registered.unanswered_pushes,
        now_ns,
    ));
    rotation::write(registration, registered)
}

/// Clears the count and the outstanding probe of the subscription `registration` names: a
/// notification was fetched through it. Bookkeeping beside the removal that calls it, so
/// a failure is logged rather than undoing an answer the caller already acted on.
pub(crate) fn reset(registration: RegistrationId) {
    let Some(mut registered) = rotation::read(registration) else {
        return;
    };
    if registered.unanswered_pushes.is_none() && registered.probe_sent_at_ns.is_none() {
        return;
    }
    registered.unanswered_pushes = None;
    registered.probe_sent_at_ns = None;
    if let Err(err) = rotation::write(registration, registered) {
        ic_cdk::println!("ERROR: Failed to reset a subscription's unanswered pushes: {err}");
    }
}

/// How long a probe through a subscription stands unanswered before another may be sent
/// through it: long enough for a push to arrive and its worker to fetch and remove the
/// entry, short enough that a page asking again after a reload is not left without one.
const PROBE_ANSWER_WINDOW_NS: u64 = 60 * 1_000_000_000;

/// Whether a probe accepted through `registered` is still inside
/// [`PROBE_ANSWER_WINDOW_NS`] at `now_ns` with no fetch answering it.
pub(crate) fn is_probe_outstanding(registered: &ReadySubscription, now_ns: Timestamp) -> bool {
    registered
        .probe_sent_at_ns
        .is_some_and(|sent_at_ns| now_ns.saturating_sub(sent_at_ns) < PROBE_ANSWER_WINDOW_NS)
}

/// Whether a probe accepted through `registered` has had [`PROBE_ANSWER_WINDOW_NS`] by
/// `now_ns` and no fetch answered it: evidence that nothing behind the subscription is
/// left to answer.
pub(crate) fn is_probe_unanswered(registered: &ReadySubscription, now_ns: Timestamp) -> bool {
    registered
        .probe_sent_at_ns
        .is_some_and(|sent_at_ns| now_ns.saturating_sub(sent_at_ns) >= PROBE_ANSWER_WINDOW_NS)
}

/// Records that the push service accepted a probe at `now_ns` through the subscription
/// `registration` names, where it still posts to `endpoint`. The probe is already on its
/// way, so a failure is logged: it costs only a second probe if one is asked for.
pub(crate) fn mark_probed(registration: RegistrationId, endpoint: &str, now_ns: Timestamp) {
    let Some(mut registered) = rotation::read(registration) else {
        return;
    };
    if registered.endpoint.as_str() != endpoint {
        return;
    }
    registered.probe_sent_at_ns = Some(now_ns);
    if let Err(err) = rotation::write(registration, registered) {
        ic_cdk::println!("ERROR: Failed to record a probe on its subscription: {err}");
    }
}
