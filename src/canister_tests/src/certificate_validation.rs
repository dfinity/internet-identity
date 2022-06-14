// adapted from https://github.com/dfinity/icx-proxy/blob/0cd1a22f717b56ac550a3554a25a845878bfb4e8/src/main.rs#L611
// TODO: certificate validation should be it's own library

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

const CERTIFICATE_REGEX: &'static str = "^certificate=:([^:]*):, tree=:([^:]*):$";

#[derive(Debug)]
pub enum ValidationError {
    MalformedCertificate { message: String },
    CertificateValidationFailed { inner: AgentError },
    WitnessLookupFailed { inner: AgentError },
    TreeHashCertifiedDataMismatch,
    AssetPathLookupFailed,
    AssetHashMismatch,
}

pub fn validate_certification(
    ic_certificate: &str,
    canister_id: CanisterId,
    uri_path: &str,
    body: &[u8],
    encoding: Option<&str>,
    root_key: ThresholdSigPublicKey,
) -> Result<(), ValidationError> {
    let cert = parse_header_value(ic_certificate, 1)?;
    let tree: HashTree = parse_header_value(ic_certificate, 2)?;
    let agent = initialize_agent(&root_key);

    if let Err(err) = agent.verify(&cert, canister_id.get().0, false) {
        return Err(ValidationError::CertificateValidationFailed { inner: err });
    }

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

    if *witness != tree.digest() {
        return Err(TreeHashCertifiedDataMismatch);
    }

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

    let body_sha = decode_body_to_sha256(body, encoding).unwrap();
    if body_sha != tree_sha {
        return Err(AssetHashMismatch);
    }
    Ok(())
}

fn initialize_agent(root_key: &ThresholdSigPublicKey) -> Agent {
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

fn parse_header_value<T>(ic_certificate: &str, capture_group: usize) -> Result<T, ValidationError>
where
    T: for<'a> Deserialize<'a>,
{
    let captures = Regex::new(CERTIFICATE_REGEX)
        .unwrap()
        .captures(ic_certificate)
        .ok_or(MalformedCertificate {
            message: "unexpected format".to_string(),
        })?;
    let cert_blob = base64::decode(
        captures
            .get(capture_group)
            .ok_or(MalformedCertificate {
                message: format!("no regex match for capture group {}", capture_group),
            })?
            .as_str(),
    )
    .map_err(|err| MalformedCertificate {
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
                return None;
            }
        }
        _ => sha256.update(body),
    };
    Some(sha256.finalize().into())
}
