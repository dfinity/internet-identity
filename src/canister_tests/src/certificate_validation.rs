// adapted from https://github.com/dfinity/icx-proxy/blob/0cd1a22f717b56ac550a3554a25a845878bfb4e8/src/main.rs#L611
// TODO: certificate validation should be its own library

use crate::certificate_validation::ValidationError::{
    AssetHashMismatch, AssetPathLookupFailed, CertificateExpired, MalformedCertificate,
};
use flate2::read::GzDecoder;
use ic_cdk::api::management_canister::main::CanisterId;
use ic_certification::{verify_certified_data, CertificateValidationError};
use ic_crypto_utils_threshold_sig_der::parse_threshold_sig_key_from_der;
use ic_sdk_certification::{HashTree, LookupResult};
use ic_types::Time;
use regex::Regex;
use serde::Deserialize;
use sha2::{Digest, Sha256};
use std::io::Read;
use std::time::{Duration, SystemTime};

// The limit of a buffer we should decompress ~10mb.
const MAX_CHUNK_SIZE_TO_DECOMPRESS: usize = 1024;
const MAX_CHUNKS_TO_DECOMPRESS: u64 = 10_240;

#[derive(Debug)]
pub enum ValidationError {
    MalformedCertificate { message: String },
    CertificateValidationFailed { inner: CertificateValidationError },
    CertificateExpired,
    AssetPathLookupFailed,
    AssetHashMismatch,
}

/// Validates asset certification according to the HTTP gateway specification:
/// https://internetcomputer.org/docs/current/references/ic-interface-spec/#http-gateway-certification
pub fn validate_certification(
    ic_certificate: &str,
    canister_id: CanisterId,
    uri_path: &str,
    body: &[u8],
    encoding: Option<&str>,
    root_key: &[u8],
    current_time: SystemTime,
) -> Result<(), ValidationError> {
    let root_key = parse_threshold_sig_key_from_der(root_key).unwrap();
    // 2. The value of the header must be a structured header according to RFC 8941 with fields certificate and tree, both being byte sequences.
    let (encoded_cert, encoded_tree) = parse_header(ic_certificate)?;
    let cert_blob = base64::decode(encoded_cert).map_err(|err| MalformedCertificate {
        message: format!("failed to decode base64 certificate: {:?}", err),
    })?;

    // 4. The tree must be a hash tree as per Encoding of certificates.
    // (Out of order because verify_certificate also checks certified_data.)
    let tree: HashTree = decode_base64_encoded_cbor(encoded_tree)?;

    // 3. The certificate must be a valid certificate as per Certification, signed by the root key.
    // If the certificate contains a subnet delegation, the delegation must be valid for the given canister.
    // The subnet state tree in the certificate must reveal the canister's certified data.
    // 5. The root hash of that tree must match the canister's certified data.
    // (Out of order because verify_certificate also checks certified_data.)
    let certificate_time = verify_certified_data(
        &cert_blob,
        &ic_types::CanisterId::try_from(canister_id.as_slice()).unwrap(),
        &root_key,
        &tree.digest(),
    )
    .map_err(|err| ValidationError::CertificateValidationFailed { inner: err })?;

    // 3. (cont.) The timestamp in /time must be recent.
    let certificate_validity = Duration::from_secs(300); // 5 min, also used by the service worker and deemed a reasonable interpretation of 'recent'
    let current_time = Time::from_nanos_since_unix_epoch(
        current_time
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap()
            .as_nanos() as u64,
    );
    if (current_time - certificate_time) > certificate_validity {
        return Err(CertificateExpired);
    }

    // 6. The path ["http_assets",<url>], where url is the utf8-encoded url from the HttpRequest must exist and be a leaf.
    // Else, if it does not exist, ["http_assets","/index.html"] must exist and be a leaf.
    let asset_path = ["http_assets".into(), uri_path.into()];
    let tree_sha = match tree.lookup_path(&asset_path) {
        LookupResult::Found(v) => v,
        _ => match tree.lookup_path(&["http_assets".into(), "/index.html".into()]) {
            LookupResult::Found(v) => v,
            _ => {
                return Err(AssetPathLookupFailed);
            }
        },
    };

    // 7. That leaf must contain the SHA-256 hash of the decoded body.
    // This is where Internet Identity breaks spec because it certifies encoded response bodies, see L2-722 for details.
    let body_sha = decode_body_to_sha256(body, encoding).unwrap();
    if body_sha != tree_sha {
        return Err(AssetHashMismatch);
    }
    Ok(())
}

fn parse_header(ic_certificate: &str) -> Result<(&str, &str), ValidationError> {
    let captures = Regex::new("^certificate=:([^:]*):,\\s*tree=:([^:]*):$")
        .unwrap()
        .captures(ic_certificate)
        .ok_or(MalformedCertificate {
            message: "unexpected format".to_string(),
        })?;
    let encoded_cert = captures
        .get(1)
        .ok_or(MalformedCertificate {
            message: "no match for encoded cert".to_string(),
        })?
        .as_str();
    let encoded_tree = captures
        .get(2)
        .ok_or(MalformedCertificate {
            message: "no match for encoded tree".to_string(),
        })?
        .as_str();
    Ok((encoded_cert, encoded_tree))
}

fn decode_base64_encoded_cbor<T>(encoded_value: &str) -> Result<T, ValidationError>
where
    T: for<'a> Deserialize<'a>,
{
    let blob = base64::decode(encoded_value).map_err(|err| MalformedCertificate {
        message: format!("failed to decode base64 value: {:?}", err),
    })?;
    serde_cbor::from_slice(&blob).map_err(|err| MalformedCertificate {
        message: format!("failed to decode cbor value: {:?}", err),
    })
}

fn decode_body_to_sha256(body: &[u8], encoding: Option<&str>) -> Option<[u8; 32]> {
    let mut sha256 = Sha256::new();
    let mut decoded = [0u8; MAX_CHUNK_SIZE_TO_DECOMPRESS];
    match encoding {
        Some("gzip") => {
            let mut decoder = GzDecoder::new(body);
            for _ in 0..MAX_CHUNKS_TO_DECOMPRESS {
                let bytes = decoder.read(&mut decoded).ok()?;
                if bytes == 0 {
                    return Some(sha256.finalize().into());
                }
                sha256.update(&decoded[0..bytes]);
            }
            if decoder.bytes().next().is_some() {
                // return None if payload exceed limits for decompression
                return None;
            }
        }
        _ => sha256.update(body),
    };
    Some(sha256.finalize().into())
}
