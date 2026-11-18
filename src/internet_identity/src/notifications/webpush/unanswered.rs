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
/// `registration` names, where it still posts there.
pub(crate) fn count_push(
    registration: RegistrationId,
    endpoint: &str,
    now_ns: Timestamp,
) -> Result<(), String> {
    let Some(mut registered) = rotation::read(registration) else {
        return Ok(());
    };
    if registered.endpoint.as_str() != endpoint {
        return Ok(());
    }
    registered.unanswered_pushes = Some(UnansweredPushCount::after_push(
        registered.unanswered_pushes,
        now_ns,
    ));
    rotation::write(registration, registered)
}

/// Clears the count and the outstanding probe of the subscription `registration` names: a
/// notification was fetched through it.
pub(crate) fn reset(registration: RegistrationId) -> Result<(), String> {
    let Some(mut registered) = rotation::read(registration) else {
        return Ok(());
    };
    if registered.unanswered_pushes.is_none() && registered.probe_sent_at_ns.is_none() {
        return Ok(());
    }
    registered.unanswered_pushes = None;
    registered.probe_sent_at_ns = None;
    rotation::write(registration, registered)
}

/// How long a probe through a subscription stands unanswered before another may be sent
/// through it: long enough for a push to arrive and its worker to fetch and remove the
/// entry, short enough that a page asking again after a reload is not left without one.
const PROBE_ANSWER_WINDOW_NS: u64 = 60 * 1_000_000_000;

/// Whether a probe submitted through `registered` is still inside
/// [`PROBE_ANSWER_WINDOW_NS`] at `now_ns` with no fetch answering it.
pub(crate) fn is_probe_outstanding(registered: &ReadySubscription, now_ns: Timestamp) -> bool {
    registered
        .probe_sent_at_ns
        .is_some_and(|sent_at_ns| now_ns.saturating_sub(sent_at_ns) < PROBE_ANSWER_WINDOW_NS)
}

/// Records that a probe was submitted at `now_ns` through the subscription
/// `registration` names, where it still posts to `endpoint`.
pub(crate) fn mark_probed(
    registration: RegistrationId,
    endpoint: &str,
    now_ns: Timestamp,
) -> Result<(), String> {
    let Some(mut registered) = rotation::read(registration) else {
        return Ok(());
    };
    if registered.endpoint.as_str() != endpoint {
        return Ok(());
    }
    registered.probe_sent_at_ns = Some(now_ns);
    rotation::write(registration, registered)
}
