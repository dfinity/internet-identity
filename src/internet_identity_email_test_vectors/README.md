# `internet_identity_email_test_vectors`

Captured input vectors for the email-recovery verifier stack
(`docs/ongoing/email-recovery.md`). These are recorded once from real DoH
responses and DKIM-signed inbound mail and committed alongside the verifier
code so unit tests can exercise byte sequences the canister can't produce on
its own.

## Status

Scaffold only. PR #1 lands the crate and its loader API. Real vectors arrive
incrementally:

- **PR #1b** — DNSSEC chains for the major consumer-mailbox domains we care
  about (gmail.com, icloud.com, outlook.com, fastmail.com, proton.me), plus
  deliberately-tampered negatives.
- **Phase-0 PR #2** — DKIM happy-path + tampering vectors per RFC 6376.
- **Phase-0 PR #3** — DMARC alignment matrix (`adkim=s` / `adkim=r` × equal /
  X-subdomain-of-Y / unrelated).

See `docs/ongoing/email-recovery.md` §9 for the full list of vector
categories the verifier stack is expected to cover.
