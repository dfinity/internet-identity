use ic_cdk_macros::query;
use internet_identity_interface::http_gateway::{HttpRequest, HttpResponse};

mod http;

#[query]
fn http_request(req: HttpRequest) -> HttpResponse {
    http::http_request(req)
}

fn main() {}
