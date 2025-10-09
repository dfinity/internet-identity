//! Types as defined by the HTTP gateway spec.
//! See https://internetcomputer.org/docs/current/references/ic-interface-spec/#http-gateway-interface

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

impl TryFrom<HttpRequest> for ic_http_certification::HttpRequest<'_> {
    type Error = String;

    fn try_from(value: HttpRequest) -> Result<Self, Self::Error> {
        use ic_http_certification::{HttpRequestBuilder, Method};

        let HttpRequest {
            method,
            url,
            headers,
            body,
            certificate_version,
        } = value;

        let method = Method::from_bytes(method.as_bytes())
            .map_err(|_| format!("Unexpected method {}", method))?;

        let mut request = HttpRequestBuilder::new()
            .with_method(method)
            .with_url(url)
            .with_headers(headers)
            .with_body(body.into_vec());

        if let Some(certificate_version) = certificate_version {
            request = request.with_certificate_version(certificate_version);
        }

        Ok(request.build())
    }
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct HttpResponse {
    pub status_code: u16,
    pub headers: Vec<HeaderField>,
    pub body: ByteBuf,
    pub upgrade: Option<bool>,
    pub streaming_strategy: Option<StreamingStrategy>,
}

impl From<ic_http_certification::HttpResponse<'_>> for HttpResponse {
    fn from(value: ic_http_certification::HttpResponse<'_>) -> Self {
        let body = ByteBuf::from(value.body());
        let status_code = value.status_code().into();
        let upgrade = value.upgrade();
        let headers = value.headers().iter().cloned().collect::<Vec<_>>();

        HttpResponse {
            status_code,
            headers,
            body,
            upgrade,
            streaming_strategy: None,
        }
    }
}
