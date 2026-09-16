//! Storable types specific to the Web Push channel: one row per subscribed
//! browser and the device-signed VAPID JWT pool it carries.

pub mod jwt_pool;
pub mod subscription;
