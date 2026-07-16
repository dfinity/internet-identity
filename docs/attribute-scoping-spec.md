# II Attribute Scoping Spec

This document specifies how attribute **scoping** works across the three
boundaries an attribute request crosses: the relying-party app, the Internet
Identity frontend (FE), and the Internet Identity canister backend (BE). It
pins down the one term that has, until now, only been defined implicitly in
code: what it means for an attribute to be **unscoped**.

Attribute sharing lets a relying-party app request user attributes (`email`,
`name`, `verified_email`) that II certifies with a canister signature. An app
may request an attribute either from **a specific source** (e.g. the user's
Google account) or from **any source** (e.g. "an email, I don't care which").
These are the *scoped* and *unscoped* requests, and the subtlety is that
"unscoped" is represented differently at each of the three boundaries.

This spec covers the ICRC-3 attribute flow (`prepare_icrc3_attributes` /
`get_icrc3_attributes`). The legacy per-attribute flow (`prepare_attributes` /
`get_attributes` / `list_available_attributes`) is deprecated, does not support
`sso:<domain>` scopes, and will not gain new functionality.

## Terminology

- **Attribute name** — one of `email`, `name`, `verified_email`. Defined by
  `AttributeName` in
  [`attributes.rs`](../src/internet_identity_interface/src/internet_identity/types/attributes.rs).
- **Scope** — the source an attribute value comes from. Either an OpenID issuer
  (`openid:<issuer>`) or a discoverable SSO domain (`sso:<domain>`). Defined by
  `AttributeScope`.
- **Wire key** — the colon-delimited string identifying an attribute on the
  wire: `<scope>:<attribute_name>` when scoped (e.g.
  `openid:https://accounts.google.com:email`), or the bare `<attribute_name>`
  when unscoped (e.g. `email`). Parsed by `AttributeKey::try_from`, which splits
  on the **last** colon: everything after it is the attribute name, everything
  before it (if present) is the scope.
- **Scoped request** — the app names the source. It wants the value from that
  source only, and it wants the response key to identify that source.
- **Unscoped request** — the app does not name a source. It wants a value of
  the given attribute name from *any* source the user chooses, and it wants the
  response key to **not** reveal which source was used.

## The canonical definition of "unscoped"

An attribute is **unscoped** when the relying-party app expressed no preference
about the value's source and must not learn it from the response. Concretely
this single idea takes three distinct forms as the request crosses each
boundary:

| Boundary | "Unscoped" is represented as | Example |
| --- | --- | --- |
| App → FE (request) | the **absence of a scope prefix** in the requested wire key | `email` |
| FE → BE (request) | a **per-spec boolean** `omit_scope: true`; the spec's `key` still carries the real (possibly scoped) wire key | `{ key: "openid:…:email", omit_scope: true }` |
| BE → App (response) | the **absence of a scope prefix** in the certified key | `email` in the signed map |

The value's underlying source is *always* concrete (a specific credential or
verified-email entry). "Unscoped" never means "no source" — it means "the
source is hidden from the app". `omit_scope` is the one bit that carries this
intent from FE to BE.

## Layer 1 — App → II Frontend

An app sends a JSON-RPC request over the II channel (`postMessage` or HTTP)
using method `ii-icrc3-attributes`, with params:

```ts
// src/frontend/src/lib/utils/transport/utils.ts
Icrc3AttributesParamsSchema = z.object({
  keys: z.array(z.string()),   // requested attribute wire keys
  nonce: z.base64(),
  icrc95DerivationOrigin: z.optional(OriginSchema),
});
```

Each entry in `keys` is one requested attribute wire key:

- **Scoped**: `openid:<issuer>:<name>` or `sso:<domain>:<name>`, e.g.
  `openid:https://accounts.google.com:email`, `sso:dfinity.org:name`.
- **Unscoped**: the bare `<name>`, e.g. `email`, `name`, `verified_email`.

At this boundary, **unscoped is the shape of the request key**: no colon-scoped
prefix. This is the only form in which the app expresses "any source".

## Layer 2 — II Frontend → II Backend

The FE resolves each requested key against the anchor's available attributes,
renders a consent screen, and (on consent) sends the user's selections to the
canister as `AttributeSpec`s.

```rust
// src/internet_identity_interface/.../attributes.rs
pub struct AttributeSpec {
    pub key: String,             // full wire key of the *selected* source
    pub value: Option<Vec<u8>>,  // selected value, for match-verification
    pub omit_scope: bool,        // true ⇒ strip scope from the certified key
}
```

### Fan-out: one unscoped request → many candidate sources

`resolveKey` in
[`attributes.ts`](../src/frontend/src/lib/stores/channelHandlers/attributes.ts)
translates the app's request against the available rows:

- A **scoped** request (`requestedKey.includes(":")`) matches the one exact wire
  row and is marked `omitScope: false`.
- An **unscoped** request fans out: it collects every available row whose
  attribute-name suffix matches — the bare unscoped rows (verified emails) *and*
  every scoped `…:<name>` row (OIDC/SSO sources) — and marks them all
  `omitScope: true`.

So an unscoped `email` request can surface several candidate options
(`email` = a verified email, `openid:…:email`, `sso:…:email`), each carrying
its real wire key but all flagged `omit_scope: true`. The user picks **one**;
the FE sends that one option's `AttributeSpec`.

The FE never fabricates a scope and never strips one from the `key` field — the
`key` always identifies the true source. The unscoped intent rides entirely on
`omit_scope`, which the FE copies verbatim into the outgoing spec.

### Collapsing to avoid response-key collisions

Because every option in an unscoped fan-out would certify to the **same** bare
key (`email`), selecting two of them would collide in the backend (see Layer 3).
The FE therefore presents an unscoped group as a single choice and collapses
options with identical values, so at most one `omit_scope: true` spec per
attribute name is ever sent. `MergedGroup.omitScope` is the authoritative
per-group flag for this — it must not be re-derived by inspecting option keys,
since a fanned-out unscoped group legitimately contains scoped option keys.

## Layer 3 — II Backend → App (signed response)

`Anchor::prepare_icrc3_attributes` in
[`attributes.rs`](../src/internet_identity/src/attributes.rs) resolves each
spec against stored credentials, builds a map of certified pairs, signs it with
a canister signature, and returns the encoded message. `get_icrc3_attributes`
later returns the matching signature.

For each spec, the backend looks up the value from the source named by
`spec.key.scope`:

- `OpenId { issuer }` / `Sso { domain }` — the matching stored credential.
- `None` (bare key) — a stored verified-email entry matching `spec.value`.

It then computes the **certified key**:

```rust
// insert_certified_attribute
let certified_key = if spec.omit_scope {
    spec.key.attribute_name.to_string()   // "email"
} else {
    spec.key.to_string()                  // "openid:https://…:email"
};
```

This is the single point where scope is stripped. `omit_scope: true` ⇒ the
certified key is the bare attribute name, hiding the source from the app;
`omit_scope: false` ⇒ the certified key is the full scoped wire key.

If two specs resolve to the same certified key (e.g. two `omit_scope: true`
specs for `email`), the backend records a `Duplicate certified attribute key`
problem and rejects the request — which is why the FE collapses unscoped groups
to a single selection.

The signed map also carries an implicit envelope the app can rely on:

```
implicit:nonce                    (blob)  — echoes the request nonce
implicit:origin                   (text)  — the relying party's origin
implicit:issued_at_timestamp_ns   (nat)   — issuance time
```

At this boundary, **unscoped is again a key shape**: the certified key has no
scope prefix. The app receives `email: "user@example.com"` and cannot tell from
the key whether it came from a verified email, Google, or an SSO domain.

## Worked examples

**Scoped request** — app asks for `["openid:https://accounts.google.com:email"]`:

1. App → FE: `keys: ["openid:https://accounts.google.com:email"]`.
2. FE → BE: `{ key: "openid:https://accounts.google.com:email", value: …, omit_scope: false }`.
3. BE → App: `{ "openid:https://accounts.google.com:email": "user@gmail.com", implicit:… }`.

The app knows the value came from Google.

**Unscoped request** — app asks for `["email"]`, user has a verified email and a
Google account:

1. App → FE: `keys: ["email"]`.
2. FE fans out to candidate options `email` (verified) and
   `openid:…:email` (Google), both `omit_scope: true`; the user picks one, say
   Google.
3. FE → BE: `{ key: "openid:https://accounts.google.com:email", value: …, omit_scope: true }`.
4. BE → App: `{ "email": "user@gmail.com", implicit:… }`.

The app receives an email but cannot tell it came from Google.

## Notes and constraints

- **SSO scopes are ICRC-3 only.** The legacy flow rejects any `sso:<domain>`
  key with `LEGACY_SSO_SCOPE_REJECTION`. New work targets the ICRC-3 flow.
- **`verified_email` semantics differ by scope.** An unscoped
  `email`/`verified_email` resolves against stored verified-email entries and
  requires `spec.value` to identify which entry; a scoped key resolves against
  the named credential.
- **Case handling.** SSO domains are normalized to lowercase at parse time so
  `sso:DFINITY.ORG:email` and `sso:dfinity.org:email` address the same
  credential; verified-email matching is case-insensitive on the address.
