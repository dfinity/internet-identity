//! Types as defined by the HTTP gateway spec.
//! See https://internetcomputer.org/docs/current/references/ic-interface-spec/#http-gateway-interface

use candid::{define_function, CandidType, Deserialize};
use serde_bytes::ByteBuf;

pub type HeaderField = (String, String);

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct Token {}

define_function!(pub StreamingCallbackFunction : (Token) -> (StreamingCallbackHttpResponse) query);

#[derive(Clone, Debug, CandidType, Deserialize)]
pub enum StreamingStrategy {
    Callback {
        callback: StreamingCallbackFunction,
        token: Token,
    },
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct StreamingCallbackHttpResponse {
    pub body: ByteBuf,
    pub token: Option<Token>,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct HttpRequest {
    pub method: String,
    pub url: String,
    pub headers: Vec<(String, String)>,
    pub body: ByteBuf,
    pub certificate_version: Option<u16>,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct HttpResponse {
    pub status_code: u16,
    pub headers: Vec<HeaderField>,
    pub body: ByteBuf,
    pub upgrade: Option<bool>,
    pub streaming_strategy: Option<StreamingStrategy>,
}
