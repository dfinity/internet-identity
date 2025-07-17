/// Coarse-grained activity statistics for the Internet Identity.
/// These stats are deduplicated by identity, based on the last usage timestamp.
pub mod activity_stats;

/// Fine-grained statistics for the Internet Identity.
/// These stats track single events. In order to not keep any privacy-sensitive data in the stats,
/// the individual events cannot be attributed to specific identities. As such they are not
/// deduplicated and multiple actions by the same identity will be counted multiple times.
pub mod event_stats;
