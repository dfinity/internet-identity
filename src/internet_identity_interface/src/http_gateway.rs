//! Types as defined by the HTTP gateway spec.
//! See https://internetcomputer.org/docs/current/references/ic-interface-spec/#http-gateway-interface

use std::borrow::Cow;

use candid::{define_function, CandidType, Deserialize};
use ic_http_certification;
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
    pub headers: Vec<HeaderField>,
    pub body: ByteBuf,
    pub certificate_version: Option<u16>,
}

impl TryFrom<HttpRequest> for ic_http_certification::HttpRequest<'static> {
    type Error = String;

    fn try_from(request: HttpRequest) -> Result<Self, Self::Error> {
        let HttpRequest {
            method,
            url,
            headers,
            body,
            certificate_version,
        } = request;

        let method = ic_http_certification::Method::from_bytes(method.as_bytes())
            .map_err(|_| format!("Invalid HTTP method: {}", method))?;

        let body = Cow::Owned(body.into_vec());

        let mut builder = Self::builder()
            .with_method(method)
            .with_url(url)
            .with_headers(headers)
            .with_body(body);

        if let Some(certificate_version) = certificate_version {
            builder = builder.with_certificate_version(certificate_version);
        }

        Ok(builder.build())
    }
}

impl TryFrom<HttpResponse> for ic_http_certification::HttpResponse<'_> {
    type Error = String;

    fn try_from(
        response: HttpResponse,
    ) -> Result<ic_http_certification::HttpResponse<'static>, Self::Error> {
        let HttpResponse {
            status_code,
            headers,
            body,
            upgrade,
        } = response;

        let body = Cow::Owned(body.into_vec());

        let status_code = ic_http_certification::StatusCode::from_u16(status_code)
            .map_err(|_| format!("Invalid HTTP status code: {}", status_code))?;

        let mut builder = ic_http_certification::HttpResponse::builder()
            .with_status_code(status_code)
            .with_headers(headers)
            .with_body(body);

        if let Some(upgrade) = upgrade {
            builder = builder.with_upgrade(upgrade);
        }

        Ok(builder.build())
    }
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct HttpResponse {
    pub status_code: u16,
    pub headers: Vec<HeaderField>,
    pub body: ByteBuf,
    pub upgrade: Option<bool>,
}
