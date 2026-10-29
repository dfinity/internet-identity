//! Turning a caller's Web Push request into one the channel can act on: a deployment
//! that notifies at all, and fields the canister can deliver against.

use std::ops::RangeInclusive;
use url::Url;

/// Relay endpoints run ~200-300 bytes; capped at 512 so the row stays small.
const MAX_ENDPOINT_LEN: usize = 512;
/// An uncompressed SEC1 point on P-256, which is what a subscription's own key is.
const SEC1_P256_LEN: usize = 65;

/// A relay address a push can be posted to: 1..=512 bytes, a URL, and a host to post it
/// at.
///
/// Shape only. Whether this deployment will deliver over the scheme is
/// `deliverable_endpoint`, because that reads a deploy flag: a value minted while it was
/// set would otherwise claim something that stops being true when it is cleared.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PushEndpoint(String);

impl PushEndpoint {
    pub fn as_str(&self) -> &str {
        &self.0
    }

    pub fn as_url(&self) -> Option<Url> {
        Url::parse(&self.0).ok()
    }

    pub fn into_string(self) -> String {
        self.0
    }
}

impl TryFrom<&str> for PushEndpoint {
    type Error = String;

    fn try_from(endpoint: &str) -> Result<Self, Self::Error> {
        validate_param_len(endpoint.len(), 1..=MAX_ENDPOINT_LEN, "endpoint")?;
        let Ok(url) = Url::parse(endpoint) else {
            return Err("endpoint is not a URL".to_string());
        };
        if url.host().is_none() {
            return Err("endpoint has no host".to_string());
        }
        Ok(Self(endpoint.to_string()))
    }
}

/// Refuses an endpoint this deployment will not POST to: plain `http` needs a loopback
/// host and the deploy flag, as the sender list and SSO discovery do.
///
/// Read per call rather than carried on the endpoint, because both answers come from
/// persistent state.
pub fn deliverable_endpoint(endpoint: &PushEndpoint) -> Result<(), String> {
    let Some(url) = endpoint.as_url() else {
        return Err("endpoint is not a URL".to_string());
    };
    if url.scheme() == "https" {
        return Ok(());
    }
    if url.scheme() == "http"
        && allow_insecure_endpoint()
        && crate::utils::is_loopback_host(url.host_str().unwrap_or_default())
    {
        return Ok(());
    }
    Err("endpoint must be an https:// URL".to_string())
}

/// RFC 8291 §3.4 input keying material, derived by the browser from this subscription's
/// own keys and an ephemeral pair of its own, and uploaded so the canister can seal a
/// payload without ever running the ECDH.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PayloadIkm(Vec<u8>);

/// Length of the HKDF output RFC 8291 specifies.
const PAYLOAD_IKM_LEN: usize = 32;

impl PayloadIkm {
    pub fn as_slice(&self) -> &[u8] {
        &self.0
    }

    pub fn into_vec(self) -> Vec<u8> {
        self.0
    }
}

impl TryFrom<Vec<u8>> for PayloadIkm {
    type Error = String;

    fn try_from(payload_ikm: Vec<u8>) -> Result<Self, Self::Error> {
        validate_param_len(
            payload_ikm.len(),
            PAYLOAD_IKM_LEN..=PAYLOAD_IKM_LEN,
            "payload_ikm",
        )?;
        Ok(Self(payload_ikm))
    }
}

/// The browser's ephemeral public key, uncompressed SEC1 P-256. Carried in the payload
/// header as `keyid`, which is what the browser derives its half of the IKM against.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PayloadKeyId(Vec<u8>);

impl PayloadKeyId {
    pub fn as_slice(&self) -> &[u8] {
        &self.0
    }

    pub fn into_vec(self) -> Vec<u8> {
        self.0
    }
}

impl TryFrom<Vec<u8>> for PayloadKeyId {
    type Error = String;

    fn try_from(payload_keyid: Vec<u8>) -> Result<Self, Self::Error> {
        validate_param_len(
            payload_keyid.len(),
            SEC1_P256_LEN..=SEC1_P256_LEN,
            "payload_keyid",
        )?;
        if p256::PublicKey::from_sec1_bytes(&payload_keyid).is_err() {
            return Err("payload_keyid is not a valid SEC1 P-256 point".to_string());
        }
        Ok(Self(payload_keyid))
    }
}

fn validate_param_len(
    observed_len_bytes: usize,
    allowed_range_bytes: RangeInclusive<usize>,
    dbg_name: &str,
) -> Result<(), String> {
    if !allowed_range_bytes.contains(&observed_len_bytes) {
        let bounds = if allowed_range_bytes.start() == allowed_range_bytes.end() {
            format!("expected {}", allowed_range_bytes.start())
        } else {
            format!(
                "{}..={}",
                allowed_range_bytes.start(),
                allowed_range_bytes.end()
            )
        };
        return Err(format!(
            "{dbg_name} length {observed_len_bytes} out of range ({bounds})"
        ));
    }
    Ok(())
}

/// Whether this deployment lets a browser register a plain-http endpoint, which only
/// one whose relay is a local server ever should.
fn allow_insecure_endpoint() -> bool {
    crate::state::persistent_state(|s| s.notifications_allow_insecure_endpoint).unwrap_or(false)
}

#[cfg(test)]
mod tests {
    use super::super::fixtures::*;
    use super::*;
    use internet_identity_interface::internet_identity::types::{
        AnchorNumber, SetWebPushSubscriptionRequest,
    };
    const ANCHOR: AnchorNumber = 10_000;

    /// What `notifications_allow_insecure_endpoint` says, for a test that is about a
    /// deployment whose relay is a local server.
    fn allow_insecure(allow: bool) {
        crate::state::persistent_state_mut(|s| {
            s.notifications_allow_insecure_endpoint = Some(allow);
        });
    }

    /// Every field, the way the endpoint takes them: all three problems rather than the
    /// first.
    fn validate(request: SetWebPushSubscriptionRequest) -> Result<(), String> {
        let SetWebPushSubscriptionRequest { endpoint, .. } = request;
        let problems: Vec<String> = [PushEndpoint::try_from(endpoint.as_str())
            .and_then(|endpoint| deliverable_endpoint(&endpoint).map(|()| endpoint))
            .err()]
        .into_iter()
        .flatten()
        .collect();
        if problems.is_empty() {
            return Ok(());
        }
        Err(problems.join("; "))
    }

    #[test]
    fn accepts_what_a_browser_sends() {
        setup();
        assert!(validate(request(ANCHOR, "https://relay.example/a")).is_ok());
    }

    #[test]
    fn refuses_an_endpoint_nothing_can_be_sent_to_in_confidence() {
        setup();
        assert!(validate(request(ANCHOR, "http://relay.example/a")).is_err());
    }

    /// A deployment whose relay is a local server has no certificate to offer.
    #[test]
    fn takes_a_loopback_one_where_the_deployment_says_it_has_no_relay() {
        setup();
        allow_insecure(true);
        let loopback = validate(request(ANCHOR, "http://127.0.0.1:4711/push"));
        let elsewhere = validate(request(ANCHOR, "http://relay.example/push"));
        allow_insecure(false);

        assert!(loopback.is_ok());
        assert!(elsewhere.is_err(), "only a local relay, not any plain host");
    }

    /// The canister POSTs to this to wake the browser, so a value it could never POST
    /// to is a row nothing can be delivered to.
    #[test]
    fn rejects_an_endpoint_that_is_not_an_https_url() {
        setup();
        for endpoint in [
            "",
            "x",
            "http://relay.example/a",
            "relay.example/a",
            "https://",
        ] {
            assert!(
                validate(request(ANCHOR, endpoint)).is_err(),
                "{endpoint} must not be accepted"
            );
        }
        let too_long = format!("https://relay.example/{}", "x".repeat(MAX_ENDPOINT_LEN));
        assert!(validate(request(ANCHOR, &too_long)).is_err());
    }
}
