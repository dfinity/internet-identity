use super::*;

fn new_encoder() -> MetricsEncoder<Vec<u8>> {
    MetricsEncoder::new(Vec::new())
}

fn as_text(e: MetricsEncoder<Vec<u8>>) -> String {
    String::from_utf8(e.into_inner()).unwrap()
}

#[test]
fn test_counter_encoding() {
    let mut w = new_encoder();
    w.encode_counter(
        "http_requests_total",
        1027.0,
        "The total number of HTTP requests.",
    )
    .unwrap();
    assert_eq!(
        &as_text(w),
        r#"# HELP http_requests_total The total number of HTTP requests.
# TYPE http_requests_total counter
http_requests_total 1027

"#
    )
}

#[test]
fn test_histogram_encoding() {
    let mut w = new_encoder();
    w.encode_histogram(
        "http_request_duration_seconds",
        [
            (0.05, 24054.0),
            (0.1, 9390.0),
            (0.2, 66948.0),
            (0.5, 28997.0),
            (1.0, 4599.0),
            (std::f64::INFINITY, 10332.0),
        ]
        .iter()
        .cloned(),
        53423.0,
        "A histogram of the request duration.",
    )
    .unwrap();
    assert_eq!(
        &as_text(w),
        r#"# HELP http_request_duration_seconds A histogram of the request duration.
# TYPE http_request_duration_seconds histogram
http_request_duration_seconds_bucket{le="0.05"} 24054
http_request_duration_seconds_bucket{le="0.1"} 33444
http_request_duration_seconds_bucket{le="0.2"} 100392
http_request_duration_seconds_bucket{le="0.5"} 129389
http_request_duration_seconds_bucket{le="1"} 133988
http_request_duration_seconds_bucket{le="+Inf"} 144320
http_request_duration_seconds_sum 53423
http_request_duration_seconds_count 144320

"#
    )
}
