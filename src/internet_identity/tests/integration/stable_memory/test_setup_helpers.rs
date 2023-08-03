// Helper functions for generating II stable memory backups for testing purposes.

use super::*;
pub(crate) fn sample_unique_device(id: usize) -> DeviceData {
    DeviceData {
        pubkey: ByteBuf::from(format!("public key #{}", id)),
        alias: format!("device #{}", id),
        ..known_devices()[id % 6].clone()
    }
}

pub(crate) fn principal(id: usize) -> Principal {
    Principal::self_authenticating(format!("public key #{}", id))
}
