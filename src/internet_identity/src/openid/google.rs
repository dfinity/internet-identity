use ic_cdk::api::management_canister::http_request::{
    http_request, CanisterHttpRequestArgument, HttpHeader, HttpMethod, HttpResponse, TransformArgs,
    TransformContext,
};
use ic_cdk::api::time;
use identity_jose::jwk::Jwk;
use serde_json::{json, Value};
use std::collections::BTreeMap;
use std::convert::Into;
use std::string::ToString;

const GOOGLE_CERTS_URL: &str = "https://www.googleapis.com/oauth2/v3/certs";

// The amount of cycles needed to make the HTTP outcall with a large enough margin
const CERTS_CALL_CYCLES: u128 = 30_000_000_000;

// The response size is a little under 1KB, so 10KB should give us large enough margin
const MAX_CERTS_RESPONSE_SIZE: u64 = 10_000;

// All HTTP outcalls are expected to be at least within a 5 minutes window
const CACHE_CONTROL_WINDOW_SIZE: u32 = 5 * 60;

const HTTP_STATUS_OK: u8 = 200;

pub async fn get_certs(transform_method: &str) -> Result<(Vec<Jwk>, u64), String> {
    let recorded_time = time();
    let request = CanisterHttpRequestArgument {
        url: GOOGLE_CERTS_URL.into(),
        method: HttpMethod::GET,
        body: None,
        max_response_bytes: Some(MAX_CERTS_RESPONSE_SIZE),
        transform: Some(TransformContext::from_name(transform_method.into(), vec![])),
        headers: vec![
            HttpHeader {
                name: "Accept".into(),
                value: "application/json".into(),
            },
            HttpHeader {
                name: "User-Agent".into(),
                value: "internet_identity_canister".into(),
            },
        ],
    };
    let (response,) = http_request(request, CERTS_CALL_CYCLES)
        .await
        .map_err(|(_, err)| err)?;

    response_to_keys_and_expiry(recorded_time, &response)
}

pub fn transform_certs(raw: TransformArgs) -> Result<HttpResponse, String> {
    let cache_header = raw
        .response
        .headers
        .iter()
        .find(|header| header.name.to_lowercase() == "cache-control")
        .ok_or("Cache control header is missing")?
        .clone();

    Ok(HttpResponse {
        status: raw.response.status,
        headers: vec![HttpHeader {
            name: "Cache-Control".into(),
            value: sort_and_floor_cache_control(&cache_header.value, CACHE_CONTROL_WINDOW_SIZE)?,
        }],
        body: sort_certs(&raw.response.body)?,
    })
}

fn response_to_keys_and_expiry(
    recorded_time: u64,
    response: &HttpResponse,
) -> Result<(Vec<Jwk>, u64), String> {
    // Check HTTP response code
    if response.status != HTTP_STATUS_OK {
        return Err("Invalid HTTP response code".into());
    }

    // Convert response body into JSON Web Keys
    let json: Value =
        serde_json::from_slice(response.body.as_slice()).map_err(|_| "Invalid JSON".to_string())?;
    let keys: Result<Vec<Jwk>, String> = json
        .get("keys")
        .and_then(Value::as_array)
        .ok_or("Keys not found or not an array")?
        .iter()
        .map(|key| serde_json::from_value(key.clone()).map_err(|_| "Invalid JSON".into()))
        .collect();

    // Calculate expiry based on recorded time + max age in cache control header
    let cache_header = response
        .headers
        .iter()
        .find(|header| header.name == "Cache-Control")
        .ok_or("Cache control header is missing")?
        .clone();
    let directives: Vec<&str> = cache_header.value.split(',').map(str::trim).collect();
    let max_age: u32 = directives
        .iter()
        .find(|directive| directive.starts_with("max-age="))
        .and_then(|directive| directive.strip_prefix("max-age="))
        .and_then(|value| value.parse().ok())
        .ok_or("Max age directive is missing or value is invalid")?;
    let expiry = recorded_time + u64::from(max_age);

    Ok((keys?, expiry))
}

fn sort_and_floor_cache_control(value: &str, window_size: u32) -> Result<String, String> {
    // Round max-age directive value down to start of `window_size`
    let mut directives: Vec<&str> = value.split(',').map(str::trim).collect();
    let max_age_directive = directives
        .iter_mut()
        .find(|directive| directive.starts_with("max-age="))
        .ok_or("Max age directive is missing")?;
    let max_age_value: u32 = max_age_directive
        .split_once('=')
        .and_then(|(_, value)| value.parse().ok())
        .ok_or("Max age value is not valid")?;
    let floored_max_age = max_age_value / window_size * window_size;
    let floored_max_age_directive = format!("max-age={floored_max_age}");
    *max_age_directive = &*floored_max_age_directive;

    // Sort directives and combine them again
    directives.sort_unstable();
    Ok(directives.join(", "))
}

fn sort_certs(body: &[u8]) -> Result<Vec<u8>, String> {
    // Parse the input JSON
    let data: Value = serde_json::from_slice(body).map_err(|_| "Invalid JSON".to_string())?;

    // Extract and validate the "keys" array
    let keys = data
        .get("keys")
        .and_then(Value::as_array)
        .ok_or("Keys not found or not an array")?;

    // Filter and process keys
    let mut sorted_keys: Vec<BTreeMap<String, String>> = keys
        .iter()
        .filter_map(Value::as_object) // Ensure each key is an object
        .filter(|key| key.contains_key("kid")) // Ensure each key has a "kid" property
        .map(|key| {
            // Sort the key's properties by collecting them into a BTreeMap
            key.iter()
                .map(|(k, v)| (k.clone(), v.as_str().unwrap_or_default().into()))
                .collect()
        })
        .collect();

    // Sort the keys by the "kid" property
    sorted_keys.sort_by_key(|key| key.get("kid").unwrap().clone());

    // Wrap sorted keys back in an object with a "keys" property
    let result = json!({ "keys": sorted_keys });

    // Convert the result to JSON and return
    serde_json::to_vec(&result).map_err(|_| "Unable to encode JSON".into())
}

#[test]
fn should_sort_cache_control() {
    let input = "public, max-age=0, no-transform, must-revalidate";
    let expected = "max-age=0, must-revalidate, no-transform, public";
    assert_eq!(
        sort_and_floor_cache_control(input, CACHE_CONTROL_WINDOW_SIZE),
        Ok(expected.to_string())
    );
}

#[test]
fn should_floor_cache_control() {
    let expected = "max-age=0, must-revalidate, no-transform, public";
    for offset in 0..CACHE_CONTROL_WINDOW_SIZE {
        assert_eq!(
            sort_and_floor_cache_control(
                &format!("max-age={offset}, must-revalidate, no-transform, public"),
                CACHE_CONTROL_WINDOW_SIZE
            ),
            Ok(expected.to_string())
        );
    }
}

#[test]
fn should_sort_certs() {
    let input = br#"{"keys":[{"e":"AQAB","alg":"RS256","kty":"RSA","kid":"ab8614ff62893badce5aa79a7703b596665d2478","n":"t9OfDNXi2-_bK3_uZizLHS8j8L-Ef4jHjhFvCBbKHkOPOrHQFVoLTSl2e32lIUtxohODogPoYwJKu9uwzpKsMmMj2L2wUwzLB3nxO8M-gOLhIriDWawHMobj3a2ZbVz2eILpjFShU6Ld5f3mQfTV0oHKA_8QnkVfoHsYnexBApJ5xgijiN5BtuK2VPkDLR95XbSnzq604bufWJ3YPSqy8Qc8Y_cFPNtyElePJk9TD2cbnZVpNRUzE7dW9gUtYHFFRrv0jNSKk3XZ-zzkTpz-HqxoNnnyD1c6QK_Ge0tsfsIKdNurRE6Eyuehq9hw-HrI1qdCz-mIqlObQiGdGWx0tQ","use":"sig"},{"use":"sig","alg":"RS256","kty":"RSA","e":"AQAB","n":"wvLUmyAlRhJkFgok97rojtg0xkqsQ6CPPoqRUSXDIYcjfVWMy1Z4hk_-90Y554KTuADfT_0FA46FWb-pr4Scm00gB3CnM8wGLZiaUeDUOu84_Zjh-YPVAua6hz6VFa7cpOUOQ5ZCxCkEQMjtrmei21a6ijy5LS1n9fdiUsjOuYWZSoIQCUj5ow5j2asqYYLRfp0OeymYf6vnttYwz3jS54Xe7tYHW2ZJ_DLCja6mz-9HzIcJH5Tmv5tQRhAUs3aoPKoCQ8ceDHMblDXNV2hBpkv9B6Pk5QVkoDTyEs7lbPagWQ1uz6bdkxM-DnjcMUJ2nh80R_DcbhyqkK4crNrM1w","kid":"89ce3598c473af1bda4bff95e6c8736450206fba"}]}"#;
    let expected = br#"{"keys":[{"alg":"RS256","e":"AQAB","kid":"89ce3598c473af1bda4bff95e6c8736450206fba","kty":"RSA","n":"wvLUmyAlRhJkFgok97rojtg0xkqsQ6CPPoqRUSXDIYcjfVWMy1Z4hk_-90Y554KTuADfT_0FA46FWb-pr4Scm00gB3CnM8wGLZiaUeDUOu84_Zjh-YPVAua6hz6VFa7cpOUOQ5ZCxCkEQMjtrmei21a6ijy5LS1n9fdiUsjOuYWZSoIQCUj5ow5j2asqYYLRfp0OeymYf6vnttYwz3jS54Xe7tYHW2ZJ_DLCja6mz-9HzIcJH5Tmv5tQRhAUs3aoPKoCQ8ceDHMblDXNV2hBpkv9B6Pk5QVkoDTyEs7lbPagWQ1uz6bdkxM-DnjcMUJ2nh80R_DcbhyqkK4crNrM1w","use":"sig"},{"alg":"RS256","e":"AQAB","kid":"ab8614ff62893badce5aa79a7703b596665d2478","kty":"RSA","n":"t9OfDNXi2-_bK3_uZizLHS8j8L-Ef4jHjhFvCBbKHkOPOrHQFVoLTSl2e32lIUtxohODogPoYwJKu9uwzpKsMmMj2L2wUwzLB3nxO8M-gOLhIriDWawHMobj3a2ZbVz2eILpjFShU6Ld5f3mQfTV0oHKA_8QnkVfoHsYnexBApJ5xgijiN5BtuK2VPkDLR95XbSnzq604bufWJ3YPSqy8Qc8Y_cFPNtyElePJk9TD2cbnZVpNRUzE7dW9gUtYHFFRrv0jNSKk3XZ-zzkTpz-HqxoNnnyD1c6QK_Ge0tsfsIKdNurRE6Eyuehq9hw-HrI1qdCz-mIqlObQiGdGWx0tQ","use":"sig"}]}"#;
    assert_eq!(sort_certs(input), Ok(expected.to_vec()));
}
