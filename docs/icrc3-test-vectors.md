# ICRC-3 Certified Attribute Test Vectors

This document describes the test vectors for ICRC-3 certified attribute messages
produced by the Internet Identity backend canister.

## Background

Internet Identity certifies user attributes (e.g. email, name) as an ICRC-3
Value map. The map is Candid-encoded into a **message** blob, which is then
signed using an IC canister signature with the domain separator
`ic-sender-info`.

## Encoding pipeline

1. Build a `BTreeMap<String, Icrc3Value>` from the requested attributes plus
   three implicit entries (`implicit:nonce`, `implicit:origin`,
   `implicit:issued_at_timestamp_ns`).
2. Convert to `Icrc3Value::Map(Vec<(String, Icrc3Value)>)` (BTreeMap ordering).
3. Candid-encode the value: `message = Encode!(&value)`.
4. Build the domain-separated payload that is signed:
   ```
   signed_message = [0x0e] || "ic-sender-info" || message
   ```
   where `0x0e` is the length of the domain string (14 bytes).
5. Sign with IC canister signature, producing a CBOR certificate.

## Icrc3Value types used

| Map key | Icrc3Value variant | Example |
|---|---|---|
| Attribute values (email, name, verified_email) | `Text` | `Text("user@example.com")` |
| `implicit:origin` | `Text` | `Text("https://some-dapp.com")` |
| `implicit:issued_at_timestamp_ns` | `Nat` | `Nat(1740583730000000000)` |
| `implicit:nonce` | `Blob` | `Blob(hex"00...00")` (32 bytes) |

## Test vector scenarios

The integration test `icrc3_test_vectors` in
`src/internet_identity/tests/integration/attributes.rs` generates 10 vectors
covering the following scenarios. All vectors use a synthetic test identity
backed by a JWT signed in-test by a deterministically-generated RSA key (no
real OpenID provider is contacted). The attribute values are:
- **email:** `alice.example@icrc3-test.invalid`
- **name:** `Alice Example`
- **origin:** `https://some-dapp.com`
- **issuer:** `https://accounts.google.com` (configured, but with a mocked JWKS
  endpoint that returns the test public key)

### 1. Single email attribute with scoped key

Requests `openid:https://accounts.google.com:email` with `omit_scope = false`.

Expected map keys (BTreeMap order):
```
implicit:issued_at_timestamp_ns  -> Nat(...)
implicit:nonce                   -> Blob(hex"00..00")
implicit:origin                  -> Text("https://some-dapp.com")
openid:https://accounts.google.com:email -> Text("alice.example@icrc3-test.invalid")
```

### 2. Single email attribute with unscoped key

Requests the same email with `omit_scope = true`.

Expected map keys:
```
email                            -> Text("alice.example@icrc3-test.invalid")
implicit:issued_at_timestamp_ns  -> Nat(...)
implicit:nonce                   -> Blob(hex"00..00")
implicit:origin                  -> Text("https://some-dapp.com")
```

### 3. Single name attribute with scoped key

Requests `openid:https://accounts.google.com:name` with `omit_scope = false`.

Expected map keys:
```
implicit:issued_at_timestamp_ns  -> Nat(...)
implicit:nonce                   -> Blob(hex"00..00")
implicit:origin                  -> Text("https://some-dapp.com")
openid:https://accounts.google.com:name -> Text("Alice Example")
```

### 4. Email and name with scoped keys

Both attributes requested with `omit_scope = false`.

Expected map keys:
```
implicit:issued_at_timestamp_ns  -> Nat(...)
implicit:nonce                   -> Blob(hex"00..00")
implicit:origin                  -> Text("https://some-dapp.com")
openid:https://accounts.google.com:email -> Text("alice.example@icrc3-test.invalid")
openid:https://accounts.google.com:name  -> Text("Alice Example")
```

### 5. Email and name with unscoped keys

Both attributes requested with `omit_scope = true`.

Expected map keys:
```
email                            -> Text("alice.example@icrc3-test.invalid")
implicit:issued_at_timestamp_ns  -> Nat(...)
implicit:nonce                   -> Blob(hex"00..00")
implicit:origin                  -> Text("https://some-dapp.com")
name                             -> Text("Alice Example")
```

### 6. Email unscoped and name scoped

Email with `omit_scope = true`, name with `omit_scope = false`.

Expected map keys:
```
email                            -> Text("alice.example@icrc3-test.invalid")
implicit:issued_at_timestamp_ns  -> Nat(...)
implicit:nonce                   -> Blob(hex"00..00")
implicit:origin                  -> Text("https://some-dapp.com")
openid:https://accounts.google.com:name -> Text("Alice Example")
```

### 7. Email with value validation

Requests email with `value = Some(b"alice.example@icrc3-test.invalid")`. The canister
verifies the provided value matches the stored credential value before including
it.

Expected map keys: same as vector 1.

### 8. Single email with specific nonce

Same as vector 1 but with nonce
`5f87b8f041d8e1121d5a7d0360a02213e4b7b3b44b25d0c7f070c7e2b694b29c`.

Expected map keys:
```
implicit:issued_at_timestamp_ns  -> Nat(...)
implicit:nonce                   -> Blob(hex"5f87b8f0...b29c")
implicit:origin                  -> Text("https://some-dapp.com")
openid:https://accounts.google.com:email -> Text("alice.example@icrc3-test.invalid")
```

### 9. Email unscoped + name scoped with specific nonce

Combines mixed scoping (vector 6) with nonce `aa` repeated 32 times.

### 10. No user attributes, only implicit entries

Sends an empty attribute list. The message contains only the 3 implicit entries.

Expected map keys:
```
implicit:issued_at_timestamp_ns  -> Nat(...)
implicit:nonce                   -> Blob(hex"00..00")
implicit:origin                  -> Text("https://some-dapp.com")
```

## Regenerating the snapshot

The integration test compares its output against
`docs/icrc3-test-vectors.json` and fails CI if they diverge, so any change that
affects the vectors must also be reflected in the committed snapshot. To
regenerate it after an intentional change, run:

```bash
UPDATE_ICRC3_VECTORS=1 cargo test -p internet_identity --test integration icrc3_test_vectors
```

and commit the resulting update to `docs/icrc3-test-vectors.json`. The snapshot
has the following shape:

```json
{
  "canister_sig_pk_hex": "...",
  "root_key_hex": "...",
  "origin": "https://some-dapp.com",
  "issuer": "https://accounts.google.com",
  "email": "alice.example@icrc3-test.invalid",
  "name": "Alice Example",
  "vectors": [
    {
      "label": "Single email attribute with scoped key",
      "icrc3_value": "Map({ ... })",
      "message_hex": "...",
      "signed_message_hex": "...",
      "certificate_cbor_hex": "..."
    }
  ]
}
```

### Fields

| Field | Description |
|---|---|
| `canister_sig_pk_hex` | DER-encoded canister signature public key (shared by all vectors). |
| `root_key_hex` | IC root key from the test environment (needed for signature verification). |
| `message_hex` | Candid-encoded `Icrc3Value::Map` (the `message` field from `PrepareIcrc3AttributeResponse`). |
| `signed_message_hex` | Domain-separated payload (`[0x0e]` followed by the 14-byte domain `ic-sender-info` followed by `message`). This is the byte string verified against the canister signature. |
| `certificate_cbor_hex` | CBOR-encoded IC certificate (the `signature` field from `GetIcrc3AttributeResponse`). |

## Verifying a vector

To verify a test vector independently:

1. Decode `certificate_cbor_hex` as CBOR to obtain the IC certificate.
2. Verify the IC certificate against `root_key_hex`.
3. Extract the certified data from the certificate's state tree.
4. Verify that the canister signature over `signed_message_hex` is valid for `canister_sig_pk_hex`.
5. Decode `message_hex` as Candid to recover the `Icrc3Value::Map`.
6. Inspect the map entries to confirm attribute keys and values match expectations.
