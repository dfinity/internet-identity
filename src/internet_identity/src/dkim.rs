use base64::engine::general_purpose::STANDARD as BASE64;
use base64::Engine;
#[cfg(not(test))]
use internet_identity_interface::internet_identity::types::smtp::{
    DkimCheck, DkimCheckName, DkimCheckStatus, DkimVerificationStatus, SmtpHeader,
};
#[cfg(not(test))]
use rsa::Pkcs1v15Sign;
use rsa::RsaPublicKey;
#[cfg(not(test))]
use sha2::{Digest, Sha256};

// --- DKIM-Signature parsing ---

#[derive(Clone, Debug)]
pub enum Canon {
    Simple,
    Relaxed,
}

#[derive(Clone, Debug)]
#[cfg_attr(test, allow(dead_code))]
pub struct DkimSignature {
    pub algorithm: String,
    pub domain: String,
    pub selector: String,
    pub signed_headers: Vec<String>,
    pub body_hash: Vec<u8>,
    pub signature: Vec<u8>,
    pub header_canon: Canon,
    pub body_canon: Canon,
    pub body_length: Option<usize>,
    /// `t=` tag — signature creation time (seconds since epoch). Parsed for
    /// completeness but not currently enforced.
    #[allow(dead_code)]
    pub timestamp: Option<u64>,
    /// `x=` tag — signature expiration time (seconds since epoch). The
    /// signature is invalid once the verifier's clock is past this value.
    pub expiration: Option<u64>,
}

pub fn parse_dkim_signature(value: &str) -> Result<DkimSignature, String> {
    let mut tags: Vec<(String, String)> = Vec::new();
    for part in value.split(';') {
        let part = part.trim();
        if part.is_empty() {
            continue;
        }
        let (key, val) = part
            .split_once('=')
            .ok_or_else(|| format!("Invalid tag: {part}"))?;
        tags.push((key.trim().to_lowercase(), val.trim().to_string()));
    }

    let get = |name: &str| -> Result<String, String> {
        tags.iter()
            .find(|(k, _)| k == name)
            .map(|(_, v)| v.clone())
            .ok_or_else(|| format!("Missing DKIM tag: {name}"))
    };

    let version = get("v")?;
    if version != "1" {
        return Err(format!("Unsupported DKIM version: {version}"));
    }

    let algorithm = get("a")?;
    let domain = get("d")?;
    let selector = get("s")?;

    let signed_headers: Vec<String> = get("h")?
        .split(':')
        .map(|h| h.trim().to_lowercase())
        .collect();

    // Remove whitespace from base64 values before decoding
    let bh_raw = get("bh")?.replace(|c: char| c.is_whitespace(), "");
    let body_hash = BASE64
        .decode(&bh_raw)
        .map_err(|e| format!("Invalid base64 in bh: {e}"))?;

    let b_raw = get("b")?.replace(|c: char| c.is_whitespace(), "");
    let signature = BASE64
        .decode(&b_raw)
        .map_err(|e| format!("Invalid base64 in b: {e}"))?;

    let (header_canon, body_canon) = match tags.iter().find(|(k, _)| k == "c") {
        Some((_, v)) => parse_canonicalization(v)?,
        None => (Canon::Simple, Canon::Simple),
    };

    let body_length = tags
        .iter()
        .find(|(k, _)| k == "l")
        .map(|(_, v)| v.parse::<usize>())
        .transpose()
        .map_err(|e| format!("Invalid body length: {e}"))?;

    let timestamp = tags
        .iter()
        .find(|(k, _)| k == "t")
        .map(|(_, v)| v.parse::<u64>())
        .transpose()
        .map_err(|e| format!("Invalid timestamp t=: {e}"))?;

    let expiration = tags
        .iter()
        .find(|(k, _)| k == "x")
        .map(|(_, v)| v.parse::<u64>())
        .transpose()
        .map_err(|e| format!("Invalid expiration x=: {e}"))?;

    Ok(DkimSignature {
        algorithm,
        domain,
        selector,
        signed_headers,
        body_hash,
        signature,
        header_canon,
        body_canon,
        body_length,
        timestamp,
        expiration,
    })
}

fn parse_canonicalization(value: &str) -> Result<(Canon, Canon), String> {
    let parts: Vec<&str> = value.split('/').collect();
    let header = match parts[0].trim() {
        "simple" => Canon::Simple,
        "relaxed" => Canon::Relaxed,
        other => return Err(format!("Unknown header canonicalization: {other}")),
    };
    let body = if parts.len() > 1 {
        match parts[1].trim() {
            "simple" => Canon::Simple,
            "relaxed" => Canon::Relaxed,
            other => return Err(format!("Unknown body canonicalization: {other}")),
        }
    } else {
        Canon::Simple
    };
    Ok((header, body))
}

// --- Canonicalization (RFC 6376 section 3.4) ---

fn canonicalize_header_relaxed(name: &str, value: &str) -> String {
    let name = name.to_lowercase();
    // Unfold lines and compress whitespace
    let value = value.replace("\r\n", "").replace('\n', "");
    let value: String = value.split_whitespace().collect::<Vec<&str>>().join(" ");
    let value = value.trim_end();
    format!("{name}:{value}")
}

fn canonicalize_body_simple(body: &[u8]) -> Vec<u8> {
    let mut result = body.to_vec();
    // Remove trailing empty lines (CRLF)
    while result.ends_with(b"\r\n") {
        let len = result.len();
        if len >= 4 && result[len - 4..len - 2] == *b"\r\n" {
            result.truncate(len - 2);
        } else {
            break;
        }
    }
    // Ensure body ends with CRLF
    if !result.is_empty() && !result.ends_with(b"\r\n") {
        result.extend_from_slice(b"\r\n");
    }
    // Empty body is canonicalized to a single CRLF
    if result.is_empty() {
        result.extend_from_slice(b"\r\n");
    }
    result
}

#[cfg(not(test))]
fn canonicalize_body_relaxed(body: &[u8]) -> Vec<u8> {
    let text = String::from_utf8_lossy(body);
    let mut lines: Vec<String> = text
        .lines()
        .map(|line| {
            // Replace sequences of WSP with a single space
            let compressed: String = line
                .split([' ', '\t'])
                .filter(|s| !s.is_empty())
                .collect::<Vec<&str>>()
                .join(" ");
            // Remove trailing whitespace
            compressed.trim_end().to_string()
        })
        .collect();

    // Remove trailing empty lines
    while lines.last().is_some_and(|l| l.is_empty()) {
        lines.pop();
    }

    let mut result: Vec<u8> = Vec::new();
    for line in &lines {
        result.extend_from_slice(line.as_bytes());
        result.extend_from_slice(b"\r\n");
    }
    // Empty body is canonicalized to a single CRLF
    if result.is_empty() {
        result.extend_from_slice(b"\r\n");
    }
    result
}

// --- Verification (only used in non-test builds via the async orchestrator) ---

#[cfg(not(test))]
fn verify_body_hash(raw_body: &[u8], sig: &DkimSignature) -> Result<(), String> {
    let canonicalized = match sig.body_canon {
        Canon::Simple => canonicalize_body_simple(raw_body),
        Canon::Relaxed => canonicalize_body_relaxed(raw_body),
    };

    let body_to_hash = match sig.body_length {
        Some(len) => &canonicalized[..len.min(canonicalized.len())],
        None => &canonicalized,
    };

    let computed = Sha256::digest(body_to_hash);
    if computed[..] != sig.body_hash[..] {
        return Err("Body hash does not match".into());
    }
    Ok(())
}

/// Reconstruct the data that was signed per RFC 6376 section 3.7.
#[cfg(not(test))]
fn build_signing_input(
    headers: &[SmtpHeader],
    dkim_header_value: &str,
    sig: &DkimSignature,
) -> Vec<u8> {
    let mut result: Vec<u8> = Vec::new();

    // Add each signed header in order specified by h= tag.
    // For duplicate header names, use headers from bottom to top.
    let mut used_indices: Vec<bool> = vec![false; headers.len()];

    for signed_name in &sig.signed_headers {
        // Find the last unused header with this name (bottom to top per RFC 6376)
        let found = headers
            .iter()
            .enumerate()
            .rev()
            .find(|(i, h)| !used_indices[*i] && h.name.eq_ignore_ascii_case(signed_name));

        if let Some((idx, header)) = found {
            used_indices[idx] = true;
            // RFC 6376 §3.4.2: simple header canonicalization uses the
            // header verbatim as it was received. The SMTP gateway has
            // already separated name from value for us, so the best
            // approximation is `name:value` — do NOT insert extra
            // whitespace, since any space after the colon is part of
            // the signed data and adding one will break verification.
            let line = match sig.header_canon {
                Canon::Relaxed => canonicalize_header_relaxed(&header.name, &header.value),
                Canon::Simple => format!("{}:{}", header.name, header.value),
            };
            result.extend_from_slice(line.as_bytes());
            result.extend_from_slice(b"\r\n");
        }
    }

    // Add the DKIM-Signature header itself, with b= value emptied
    let dkim_header_stripped = strip_b_value(dkim_header_value);
    let line = match sig.header_canon {
        Canon::Relaxed => canonicalize_header_relaxed("dkim-signature", &dkim_header_stripped),
        Canon::Simple => format!("DKIM-Signature:{dkim_header_stripped}"),
    };
    // No trailing CRLF for the DKIM-Signature header itself
    result.extend_from_slice(line.as_bytes());

    result
}

/// Remove the value of the b= tag from the DKIM-Signature header value,
/// keeping the tag name and delimiters.
fn strip_b_value(header_value: &str) -> String {
    let mut result = String::new();
    let mut remaining = header_value;

    // Find "b=" that is not "bh=" — it follows a semicolon or starts the string
    loop {
        if let Some(pos) = remaining.find("b=") {
            // Ensure this is the "b" tag, not "bh"
            if pos > 0
                && remaining.as_bytes()[pos - 1] != b';'
                && !remaining[..pos].trim_end().is_empty()
            {
                // Check if the character before 'b' (ignoring whitespace) is ';' or start
                let before = remaining[..pos].trim_end();
                if !before.is_empty() && !before.ends_with(';') {
                    // Not the b= tag (could be part of another tag value)
                    result.push_str(&remaining[..pos + 2]);
                    remaining = &remaining[pos + 2..];
                    continue;
                }
            }
            // Check it's not bh=
            if remaining.len() > pos + 2 && remaining.as_bytes()[pos + 1] == b'h' {
                result.push_str(&remaining[..pos + 3]);
                remaining = &remaining[pos + 3..];
                continue;
            }
            // Found the b= tag — keep "b=" but remove its value up to the next ";"
            result.push_str(&remaining[..pos + 2]);
            let after_b = &remaining[pos + 2..];
            if let Some(semi) = after_b.find(';') {
                remaining = &after_b[semi..];
            } else {
                remaining = "";
            }
            break;
        } else {
            result.push_str(remaining);
            break;
        }
    }
    result.push_str(remaining);
    result
}

#[cfg(not(test))]
fn verify_rsa_sha256(
    signing_input: &[u8],
    signature: &[u8],
    public_key: &RsaPublicKey,
) -> Result<(), String> {
    let hashed = Sha256::digest(signing_input);
    let scheme = Pkcs1v15Sign::new::<Sha256>();
    public_key
        .verify(scheme, &hashed, signature)
        .map_err(|e| format!("RSA verification failed: {e}"))
}

// --- DKIM DNS public key parsing ---

/// Minimum RSA key size we accept for DKIM public keys. RFC 6376 allows
/// shorter keys but modern deployment guidance (RFC 8301) is ≥1024 bits,
/// and most reputable senders are on 1024 or 2048.
#[cfg(not(test))]
const DKIM_MIN_RSA_KEY_BITS: usize = 1024;

/// Parse a DKIM DNS TXT record value (e.g., "v=DKIM1; k=rsa; p=MIGfMA0G...") into an RSA public key.
pub fn parse_dkim_public_key(txt_record: &str) -> Result<RsaPublicKey, String> {
    let mut p_value: Option<String> = None;

    for part in txt_record.split(';') {
        let part = part.trim();
        if let Some((key, val)) = part.split_once('=') {
            // DKIM tag names are case-insensitive per RFC 6376.
            if key.trim().eq_ignore_ascii_case("p") {
                p_value = Some(val.trim().replace(|c: char| c.is_whitespace(), ""));
            }
        }
    }

    let p_base64 = p_value.ok_or("Missing p= tag in DKIM DNS record")?;
    let der_bytes = BASE64
        .decode(&p_base64)
        .map_err(|e| format!("Invalid base64 in DKIM public key: {e}"))?;

    use rsa::pkcs8::DecodePublicKey;
    let key = RsaPublicKey::from_public_key_der(&der_bytes)
        .map_err(|e| format!("Invalid DKIM public key DER: {e}"))?;

    #[cfg(not(test))]
    {
        use rsa::traits::PublicKeyParts;
        let bits = key.n().bits();
        if bits < DKIM_MIN_RSA_KEY_BITS {
            return Err(format!(
                "DKIM public key too small: {bits} bits (minimum {DKIM_MIN_RSA_KEY_BITS})"
            ));
        }
    }
    Ok(key)
}

// --- DoH fetch ---

#[cfg(not(test))]
const DOH_CALL_CYCLES: u128 = 30_000_000_000;

#[cfg(not(test))]
pub async fn fetch_dkim_public_key(selector: &str, domain: &str) -> Result<RsaPublicKey, String> {
    use ic_cdk::api::management_canister::http_request::{
        http_request_with_closure, CanisterHttpRequestArgument, HttpHeader, HttpMethod,
    };

    let query_name = format!("{selector}._domainkey.{domain}");
    let url = format!("https://dns.google/resolve?name={query_name}&type=TXT");

    let request = CanisterHttpRequestArgument {
        url,
        method: HttpMethod::GET,
        body: None,
        max_response_bytes: Some(4096),
        transform: None,
        headers: vec![
            HttpHeader {
                name: "Accept".into(),
                value: "application/dns-json".into(),
            },
            HttpHeader {
                name: "User-Agent".into(),
                value: "internet_identity_canister".into(),
            },
        ],
    };

    let (response,) = http_request_with_closure(request, DOH_CALL_CYCLES, transform_doh_response)
        .await
        .map_err(|(_, err)| err)?;

    let body: serde_json::Value = serde_json::from_slice(&response.body)
        .map_err(|e| format!("Invalid DoH JSON response: {e}"))?;

    let answers = body["Answer"]
        .as_array()
        .ok_or("No Answer section in DoH response")?;

    // Concatenate all TXT record data fragments
    let mut txt_data = String::new();
    for answer in answers {
        // Google DNS returns TXT type as 16
        if answer["type"].as_u64() == Some(16) {
            if let Some(data) = answer["data"].as_str() {
                // Google DNS wraps TXT data in quotes, strip them
                let unquoted = data.trim_matches('"');
                txt_data.push_str(unquoted);
            }
        }
    }

    if txt_data.is_empty() {
        return Err(format!("No DKIM TXT record found for {query_name}"));
    }

    parse_dkim_public_key(&txt_data)
}

#[cfg(not(test))]
fn transform_doh_response(
    response: ic_cdk::api::management_canister::http_request::HttpResponse,
) -> ic_cdk::api::management_canister::http_request::HttpResponse {
    use candid::Nat;
    use ic_cdk::api::management_canister::http_request::HttpResponse;

    let ok_status = Nat::from(200u32);
    if response.status != ok_status {
        ic_cdk::api::trap("DoH request returned non-200 status");
    }

    let parsed: serde_json::Value = serde_json::from_slice(&response.body)
        .unwrap_or_else(|_| ic_cdk::api::trap("Invalid DoH JSON"));

    // Extract and sort TXT answers for deterministic consensus
    let mut txt_records: Vec<String> = Vec::new();
    if let Some(answers) = parsed["Answer"].as_array() {
        for answer in answers {
            if answer["type"].as_u64() == Some(16) {
                if let Some(data) = answer["data"].as_str() {
                    txt_records.push(data.to_string());
                }
            }
        }
    }
    txt_records.sort();

    let canonical = serde_json::json!({
        "Status": parsed["Status"],
        "Answer": txt_records.iter().map(|d| {
            serde_json::json!({"type": 16, "data": d})
        }).collect::<Vec<_>>()
    });

    let body = serde_json::to_vec(&canonical)
        .unwrap_or_else(|_| ic_cdk::api::trap("Failed to serialize canonical DoH response"));

    HttpResponse {
        status: ok_status,
        headers: vec![],
        body,
    }
}

// --- Orchestrator (only compiled in non-test builds) ---

#[cfg(not(test))]
fn pass(name: DkimCheckName, detail: Option<String>) -> DkimCheck {
    DkimCheck {
        name,
        status: DkimCheckStatus::Pass,
        detail,
    }
}

#[cfg(not(test))]
fn fail(name: DkimCheckName, detail: String) -> DkimCheck {
    DkimCheck {
        name,
        status: DkimCheckStatus::Fail,
        detail: Some(detail),
    }
}

#[cfg(not(test))]
fn skipped(name: DkimCheckName) -> DkimCheck {
    DkimCheck {
        name,
        status: DkimCheckStatus::Skipped,
        detail: None,
    }
}

#[cfg(not(test))]
fn skip_remaining(checks: &mut Vec<DkimCheck>, names: &[DkimCheckName]) {
    for name in names {
        checks.push(skipped(name.clone()));
    }
}

#[cfg(not(test))]
pub async fn verify_email_dkim(headers: &[SmtpHeader], raw_body: &[u8]) -> DkimVerificationStatus {
    // Collect every DKIM-Signature header. Legitimate multi-hop mail
    // commonly carries several — the original signer plus each mailing-list
    // forwarder. We accept the email as verified if ANY of them verify,
    // since the sender only needs to prove authenticity through one chain.
    let dkim_headers: Vec<&SmtpHeader> = headers
        .iter()
        .filter(|h| h.name.eq_ignore_ascii_case("dkim-signature"))
        .collect();

    if dkim_headers.is_empty() {
        let mut checks: Vec<DkimCheck> = Vec::new();
        checks.push(fail(
            DkimCheckName::DkimSignaturePresent,
            "No DKIM-Signature header found".into(),
        ));
        skip_remaining(
            &mut checks,
            &[
                DkimCheckName::SignatureParsed,
                DkimCheckName::AlgorithmSupported,
                DkimCheckName::RequiredHeadersSigned,
                DkimCheckName::BodyHashValid,
                DkimCheckName::PublicKeyFetched,
                DkimCheckName::SignatureValid,
            ],
        );
        return DkimVerificationStatus::Unverified { checks };
    }

    // Try each signature in turn. Keep the last attempt's checks so we have
    // something useful to show the user if nothing verifies.
    let mut last_checks: Vec<DkimCheck> = Vec::new();
    for dkim_header in &dkim_headers {
        let (checks, verified) = verify_single_signature(dkim_header, headers, raw_body).await;
        if verified {
            return DkimVerificationStatus::Verified { checks };
        }
        last_checks = checks;
    }
    DkimVerificationStatus::Unverified {
        checks: last_checks,
    }
}

/// Runs the full check pipeline against a single DKIM-Signature header and
/// returns its checks plus whether the signature verified.
#[cfg(not(test))]
async fn verify_single_signature(
    dkim_header: &SmtpHeader,
    headers: &[SmtpHeader],
    raw_body: &[u8],
) -> (Vec<DkimCheck>, bool) {
    let mut checks: Vec<DkimCheck> = Vec::new();

    // 1. DKIM-Signature header present (always Pass once we reach this
    //    helper — the caller already filtered).
    checks.push(pass(DkimCheckName::DkimSignaturePresent, None));

    // 2. Signature parsed
    let sig = match parse_dkim_signature(&dkim_header.value) {
        Ok(s) => {
            checks.push(pass(
                DkimCheckName::SignatureParsed,
                Some(format!("d={}, s={}", s.domain, s.selector)),
            ));
            s
        }
        Err(e) => {
            checks.push(fail(DkimCheckName::SignatureParsed, e));
            skip_remaining(
                &mut checks,
                &[
                    DkimCheckName::AlgorithmSupported,
                    DkimCheckName::RequiredHeadersSigned,
                    DkimCheckName::BodyHashValid,
                    DkimCheckName::PublicKeyFetched,
                    DkimCheckName::SignatureValid,
                ],
            );
            return (checks, false);
        }
    };

    // 2b. Signature freshness — if `x=` is set, reject once the verifier's
    //     clock is past it. Reported inline with the `SignatureValid`
    //     check at the end (no separate enum variant to stay
    //     backward-compatible on the Candid wire).
    let now_secs = ic_cdk::api::time() / 1_000_000_000;
    let expired = matches!(sig.expiration, Some(x) if now_secs > x);

    // 3. Algorithm supported
    if sig.algorithm == "rsa-sha256" {
        checks.push(pass(
            DkimCheckName::AlgorithmSupported,
            Some("rsa-sha256".into()),
        ));
    } else {
        checks.push(fail(
            DkimCheckName::AlgorithmSupported,
            format!("Unsupported: {}", sig.algorithm),
        ));
        skip_remaining(
            &mut checks,
            &[
                DkimCheckName::RequiredHeadersSigned,
                DkimCheckName::BodyHashValid,
                DkimCheckName::PublicKeyFetched,
                DkimCheckName::SignatureValid,
            ],
        );
        return (checks, false);
    }

    // 4. Required headers signed
    let required = ["from", "to", "subject"];
    let missing: Vec<&str> = required
        .iter()
        .filter(|req| !sig.signed_headers.iter().any(|h| h == **req))
        .copied()
        .collect();

    if missing.is_empty() {
        checks.push(pass(
            DkimCheckName::RequiredHeadersSigned,
            Some(sig.signed_headers.join(", ")),
        ));
    } else {
        checks.push(fail(
            DkimCheckName::RequiredHeadersSigned,
            format!("Missing: {}", missing.join(", ")),
        ));
        skip_remaining(
            &mut checks,
            &[
                DkimCheckName::BodyHashValid,
                DkimCheckName::PublicKeyFetched,
                DkimCheckName::SignatureValid,
            ],
        );
        return (checks, false);
    }

    // 5. Body hash valid
    if let Err(e) = verify_body_hash(raw_body, &sig) {
        checks.push(fail(DkimCheckName::BodyHashValid, e));
        skip_remaining(
            &mut checks,
            &[
                DkimCheckName::PublicKeyFetched,
                DkimCheckName::SignatureValid,
            ],
        );
        return (checks, false);
    }
    checks.push(pass(DkimCheckName::BodyHashValid, None));

    // 6. Public key fetched
    let dns_name = format!("{}._domainkey.{}", sig.selector, sig.domain);
    let public_key = match fetch_dkim_public_key(&sig.selector, &sig.domain).await {
        Ok(pk) => {
            checks.push(pass(DkimCheckName::PublicKeyFetched, Some(dns_name)));
            pk
        }
        Err(e) => {
            checks.push(fail(DkimCheckName::PublicKeyFetched, e));
            skip_remaining(&mut checks, &[DkimCheckName::SignatureValid]);
            return (checks, false);
        }
    };

    // 7. Signature valid. An expired signature is treated as a SignatureValid
    //    failure so the UI surfaces something actionable without needing a
    //    new Candid variant.
    if expired {
        checks.push(fail(
            DkimCheckName::SignatureValid,
            format!(
                "Signature expired (x={}, now={})",
                sig.expiration.unwrap(),
                now_secs
            ),
        ));
        return (checks, false);
    }
    let signing_input = build_signing_input(headers, &dkim_header.value, &sig);
    match verify_rsa_sha256(&signing_input, &sig.signature, &public_key) {
        Ok(()) => {
            checks.push(pass(DkimCheckName::SignatureValid, None));
            (checks, true)
        }
        Err(e) => {
            checks.push(fail(DkimCheckName::SignatureValid, e));
            (checks, false)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_dkim_signature() {
        let value = "v=1; a=rsa-sha256; d=example.com; s=selector1; \
                      h=from:to:subject:date; \
                      bh=MTIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTI=; \
                      b=dGVzdA==";
        let sig = parse_dkim_signature(value).unwrap();
        assert_eq!(sig.algorithm, "rsa-sha256");
        assert_eq!(sig.domain, "example.com");
        assert_eq!(sig.selector, "selector1");
        assert_eq!(sig.signed_headers, vec!["from", "to", "subject", "date"]);
    }

    #[test]
    fn test_parse_dkim_signature_with_canonicalization() {
        let value = "v=1; a=rsa-sha256; c=relaxed/relaxed; d=gmail.com; s=20230601; \
                      h=from:to:subject; bh=dGVzdA==; b=dGVzdA==";
        let sig = parse_dkim_signature(value).unwrap();
        assert!(matches!(sig.header_canon, Canon::Relaxed));
        assert!(matches!(sig.body_canon, Canon::Relaxed));
    }

    #[test]
    fn test_strip_b_value() {
        assert_eq!(
            strip_b_value("v=1; a=rsa-sha256; b=abc123; d=example.com"),
            "v=1; a=rsa-sha256; b=; d=example.com"
        );
        // bh= should not be affected
        assert_eq!(
            strip_b_value("bh=hashvalue; b=sigvalue"),
            "bh=hashvalue; b="
        );
    }

    #[test]
    fn test_canonicalize_header_relaxed() {
        assert_eq!(
            canonicalize_header_relaxed("From", "  John  Doe  <john@example.com>  "),
            "from:John Doe <john@example.com>"
        );
    }

    #[test]
    fn test_canonicalize_body_simple_trailing_empty_lines() {
        let body = b"hello\r\n\r\n\r\n";
        let result = canonicalize_body_simple(body);
        assert_eq!(result, b"hello\r\n");
    }

    #[test]
    fn test_canonicalize_body_simple_empty() {
        let result = canonicalize_body_simple(b"");
        assert_eq!(result, b"\r\n");
    }

    #[test]
    fn test_parse_dkim_public_key_missing_p() {
        let result = parse_dkim_public_key("v=DKIM1; k=rsa");
        assert!(result.is_err());
    }
}
