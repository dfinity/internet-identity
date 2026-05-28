# DKIM verifier test vectors

Static fixtures consumed by the DKIM verifier's test suite (see
`src/internet_identity/src/dkim/test_vectors.rs`). Each `.eml` is a
real RFC 5322 message with a `DKIM-Signature` header that signed it,
and the `.txt` is the corresponding DKIM DNS TXT record (the public
key the verifier validates against).

## Files

- `synth-rsa-relaxed-relaxed.eml` — synthetic message signed with
  `c=relaxed/relaxed` (the canonicalisation pair we accept on both
  sides).
- `synth-rsa-relaxed-simple.eml` — `c=relaxed/simple`. Header side
  relaxed; body side simple. Verifier accepts both.
- `synth-rsa-simple-simple.eml` — `c=simple/simple`. Verifier rejects
  with `UnsupportedCanonicalization` per design §5.2.
- `synth-rsa-test1._domainkey.test.example.com.txt` — DKIM TXT record
  matching the synthetic key.

## Provenance

Synthetic vectors were generated offline (one-off, in the sandbox
host) using [`dkimpy`](https://launchpad.net/dkimpy) — the canonical
Python DKIM library — against a freshly-generated 2048-bit RSA key.
The private key was used solely to produce these `.eml` files and was
**not committed**: the public key lives in the TXT-record file as the
`p=` tag, the signed bytes live in the `.eml` files, and that's all
the verifier needs.

To regenerate (different private key → different signatures → same
fixtures):

```bash
openssl genrsa -out test.pem 2048
# Then run a small Python signer (see scripts/ in PR history) that
# uses dkimpy.dkim.sign() to produce the .eml + TXT pair.
```

The verifier only consumes the public artifacts and is deterministic
across regenerations — i.e. a regenerated set of fixtures will pass the
existing tests as long as the signer follows the same shape.

## What's *not* here yet

Real-world captured DKIM-signed emails from production senders (e.g.
proton.me, tutanota.com — the consumer providers that *also* sign
their DNS zones with DNSSEC and so are reachable end-to-end through
PR 1's verifier). Those land in a follow-up commit; the synthetic
vectors above already exercise every branch of the verifier.
