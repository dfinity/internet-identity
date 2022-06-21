// adapted from https://github.com/dfinity/icx-proxy/blob/0cd1a22f717b56ac550a3554a25a845878bfb4e8/src/main.rs#L611
// TODO: certificate validation should be its own library

use crate::certificate_validation::ValidationError::{
    AssetHashMismatch, AssetPathLookupFailed, MalformedCertificate, TreeHashCertifiedDataMismatch,
    WitnessLookupFailed,
};
use candid::types::ic_types::hash_tree::LookupResult;
use flate2::read::GzDecoder;
use ic_agent::agent::http_transport::ReqwestHttpReplicaV2Transport;
use ic_agent::hash_tree::HashTree;
use ic_agent::{lookup_value, Agent, AgentError};
use ic_crypto_utils_threshold_sig_der::public_key_to_der;
use ic_state_machine_tests::{CanisterId, ThresholdSigPublicKey};
use regex::Regex;
use serde::Deserialize;
use sha2::{Digest, Sha256};
use std::io::Read;

// The limit of a buffer we should decompress ~10mb.
const MAX_CHUNK_SIZE_TO_DECOMPRESS: usize = 1024;
const MAX_CHUNKS_TO_DECOMPRESS: u64 = 10_240;

#[derive(Debug)]
pub enum ValidationError {
    MalformedCertificate { message: String },
    CertificateValidationFailed { inner: AgentError },
    WitnessLookupFailed { inner: AgentError },
    TreeHashCertifiedDataMismatch,
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
    root_key: ThresholdSigPublicKey,
) -> Result<(), ValidationError> {
    // 2. The value of the header must be a structured header according to RFC 8941 with fields certificate and tree, both being byte sequences.
    let (encoded_cert, encoded_tree) = parse_header(ic_certificate)?;

    // 3. The certificate must be a valid certificate as per Certification, signed by the root key.
    // If the certificate contains a subnet delegation, the delegation must be valid for the given canister.
    // The timestamp in /time must be recent.
    let cert = decode_value(encoded_cert)?;
    let disable_range_check = false; // Agent-rs allows to disable the range check for the canister ids. false -> check enabled
    if let Err(err) = agent(&root_key).verify(&cert, canister_id.get().0, disable_range_check) {
        return Err(ValidationError::CertificateValidationFailed { inner: err });
    }

    // 3. (cont.) The subnet state tree in the certificate must reveal the canister's certified data.
    let certified_data_path = vec![
        "canister".into(),
        canister_id.into(),
        "certified_data".into(),
    ];
    let witness = match lookup_value(&cert, certified_data_path) {
        Ok(witness) => witness,
        Err(err) => {
            return Err(WitnessLookupFailed { inner: err });
        }
    };

    // 4. The tree must be a hash tree as per Encoding of certificates.
    let tree: HashTree = decode_value(encoded_tree)?;
    // 5. The root hash of that tree must match the canister's certified data.
    if *witness != tree.digest() {
        return Err(TreeHashCertifiedDataMismatch);
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

fn agent(root_key: &ThresholdSigPublicKey) -> Agent {
    let agent = Agent::builder()
        .with_transport(
            ReqwestHttpReplicaV2Transport::create("https://irrelevant-but-mandatory.com").unwrap(),
        )
        .build()
        .unwrap();
    agent
        .set_root_key(public_key_to_der(&root_key.into_bytes()).unwrap())
        .expect("setting root key failed");
    agent
}

fn decode_value<T>(encoded_value: &str) -> Result<T, ValidationError>
where
    T: for<'a> Deserialize<'a>,
{
    let cert_blob = base64::decode(encoded_value).map_err(|err| MalformedCertificate {
        message: format!("failed to decode base64 value: {:?}", err),
    })?;
    serde_cbor::from_slice(&cert_blob).map_err(|err| MalformedCertificate {
        message: format!("failed to decode cbor value: {:?}", err),
    })
}

fn decode_body_to_sha256(body: &[u8], encoding: Option<&str>) -> Option<[u8; 32]> {
    let mut sha256 = Sha256::new();
    let mut decoded = [0u8; MAX_CHUNK_SIZE_TO_DECOMPRESS];
    match encoding.as_deref() {
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
