//! Storable types specific to the Web Push channel: one row per subscribed
//! device, its hashed endpoint key, and the device-signed VAPID JWT pool.

pub mod endpoint_hash;
pub mod jwt_pool;
pub mod seal;
pub mod subscription;
