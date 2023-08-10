use internet_identity_interface::internet_identity::types::Timestamp;

pub mod active_anchor_counter;
pub mod domain_active_anchor_counter;

pub trait ActivityCounter: Clone {
    fn new(start_timestamp: Timestamp) -> Self;
    fn start_timestamp(&self) -> Timestamp;
}
