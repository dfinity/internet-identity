//! Rebuild VAPID JWTs from browser-signed claims; II holds no signing key.
//! Header bytes, claim order, and window duration must match `vapidPool.ts`.

use url::Url;

/// The `aud` the browser signed: `new URL(endpoint).origin`, serialized the same way.
pub(crate) fn relay_origin_of(endpoint: &str) -> Option<String> {
    let origin = Url::parse(endpoint).ok()?.origin();
    origin.is_tuple().then(|| origin.ascii_serialization())
}
