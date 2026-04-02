use candid::CandidType;
use serde::{Deserialize, Serialize};

/// ICRC-3 Value type.
///
/// See: https://github.com/dfinity/ICRC-1/blob/main/standards/ICRC-3/README.md
#[derive(Clone, Debug, PartialEq, CandidType, Serialize, Deserialize)]
pub enum Icrc3Value {
    Nat(candid::Nat),
    Int(candid::Int),
    Blob(Vec<u8>),
    Text(String),
    Array(Vec<Icrc3Value>),
    Map(Vec<(String, Icrc3Value)>),
}
