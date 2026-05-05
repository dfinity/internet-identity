//! DoH (DNS-over-HTTPS) fallback for fetching DKIM/DMARC TXT records
//! when the queried domain isn't DNSSEC-signed.
//!
//! Design constraints (from the team Slack writeup + design doc §7.6):
//!
//! - **Allowlist-gated.** Outcalls happen only for domains explicitly
//!   listed in the deploy/upgrade-arg `DohConfig.allowed_domains`. The
//!   verifier never goes to the internet for an arbitrary domain.
//! - **Lazy.** No background polling. The cache is populated on
//!   demand the first time a real `smtp_request` arrives for a listed
//!   domain.
//! - **Quorum across independent operators.** Every cache miss fires
//!   parallel outcalls to three providers from three jurisdictions
//!   (Cloudflare 🇺🇸, Quad9 🇨🇭, CIRA Canadian Shield 🇨🇦) and accepts
//!   the result iff at least 2-of-3 agree on the TXT bytes. No single
//!   operator is trusted; one provider being down or returning forged
//!   bytes never breaks the verifier.
//! - **In-flight dedup.** Multiple concurrent verification requests
//!   for the same domain share one outcall fan-out, not three each.
//! - **Heap cache.** Fast and cheap. Keys are re-fetchable, so an
//!   upgrade rebuilding the cache from scratch is fine.

#![allow(dead_code)]

mod parser;
mod types;

#[allow(unused_imports)]
pub use parser::{build_txt_query, parse_txt_response, ParseError};
#[allow(unused_imports)]
pub use types::{DohError, DohProvider, PROVIDERS};
