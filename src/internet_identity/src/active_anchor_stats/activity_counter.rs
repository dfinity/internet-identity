use internet_identity_interface::internet_identity::types::Timestamp;

pub mod active_anchor_counter;
pub mod domain_active_anchor_counter;

/// This trait can be implemented by any counter that is used to track activity over both daily and
/// monthly (30-day) periods.
/// The counting infrastructure will take care of counter maintenance (e.g. pruning expired counters
/// and adding new ones).
///
/// The counter infrastructure piggybacks on update calls to perform maintenance when increasing a
/// counter. This means that this infrastructure should not be used to count events that are
/// expected to happen less frequently than once per day.
///
/// There are currently two implementations of this trait:
///     - ActiveAnchorCounter: used to track unique active anchors
///     - IIDomainCounter: used to track unique active anchors per domain
pub trait ActivityCounter: Clone {
    /// Creates a new counter with the given start timestamp.
    fn new(start_timestamp: Timestamp) -> Self;

    /// Returns the start timestamp of the counter.
    fn start_timestamp(&self) -> Timestamp;
}
