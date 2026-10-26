//! The backlogs: notifications accepted and awaiting dispatch. Each is a
//! configuration of [`crate::admission_queue::AdmissionQueue`] — an item type, its
//! `QueueItem` impl, and the caps and retry policy it runs under.

pub(crate) mod app;
