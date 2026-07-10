# MCP server guide: connecting to Internet Identity

This documents the protocol an MCP server implements to act on an Internet
Identity user's behalf. At connect, II mints a **short-lived registration
delegation** (5 minutes) that your server redeems to bind its **session
key** to the user's identity (a _grant_, up to 30 days, revocable in II
Settings at any time); the server then signs `mcp_*` canister calls with
that key. The registration delegation is the only delegation chain your
server ever handles, and it is redeemed at connect. Per-app delegations
(up to 1 hour) are minted on demand through `mcp_prepare_delegation` /
`mcp_get_delegation`.

Your server generates **two keypairs per connection**:

- the **registration key `X`** — per connect attempt; its public key rides
  the connect link, and the registration chain's final, browser-signed hop
  targets it. Its private key is what makes the chain redeemable by you
  alone. (The chain's canister-signed hop targets an ephemeral key the II
  frontend holds — see §3b — but you never handle that key.)
- the **session key `S`** — the long-lived key the grant is bound to; you
  sign all subsequent `mcp_*` calls with it.

## Lifecycle at a glance

```mermaid
sequenceDiagram
    autonumber
    actor U as User / browser
    participant M as MCP server
    participant F as II frontend /mcp
    participant C as II canister
    participant App as Target app

    rect rgb(244, 244, 250)
    note over U,C: Phase 1 — connect handshake (once per session)
    M->>U: redirect to /mcp#registration_key, callback, state, ttl
    U->>F: open /mcp, authenticate and consent (pick TTL)
    F->>C: mcp_get_config(anchor)
    C-->>F: enabled + trusted URL (verify callback origin)
    F->>M: GET /.well-known/ii-auth-callbacks
    M-->>F: {"callbacks": [...]} — link's callback must exact-match
    note over F: generate ephemeral key Y (browser-held)
    F->>C: prepare_mcp_registration_delegation(anchor, Y, permissions, ttl)
    C-->>F: {user_key, expiration} — P_reg from a random nonce, consent stored
    F->>C: get_mcp_registration_delegation(anchor, Y, user_key, expiration)
    C-->>F: SignedDelegation — the P_reg to Y hop (inert without Y)
    note over F: sign the second hop Y to X locally — full chain never transits the IC
    F->>U: navigate tab to callback#delegation, state
    U->>M: your connect page reads the fragment, ships it to your backend
    M->>C: mcp_register_v2(S) — signed via the chain; consent recovered server-side
    C-->>M: {expiration, permissions} — grant bound: S's principal to anchor
    note over U,M: your page owns the tab now — finish your flow (e.g. OAuth code redirect)
    end

    rect rgb(240, 248, 244)
    note over M,App: Phase 2 — acting for the user (repeat until expiry)
    M->>C: mcp_get_accounts(target_origin) [signed by session key]
    C-->>M: [AccountInfo]
    M->>C: mcp_prepare_delegation(target, account?, app_key, ttl <= 1h)
    C-->>M: {user_key, expiration, account_number}
    M->>C: mcp_get_delegation(target, account_number, app_key, expiration)
    C-->>M: SignedDelegation
    M->>App: call as user (chain: user_key to app_key)
    App-->>M: response
    end

    rect rgb(252, 244, 244)
    note over U,C: End of life — grant expires, or the user revokes in Settings
    opt user revokes early
        U->>C: mcp_set_config(disable / change URL) — grant deleted
    end
    M->>C: mcp_* (signed by session key)
    C-->>M: Err Unauthorized(principal)
    note over M: session over → prompt a fresh connect with new keys
    end
```

## 1. Declare your callbacks

II delivers the registration delegation only to a callback **you declare**.
Host an allow-list at a fixed well-known path on your origin:

```
GET https://<your-origin>/.well-known/ii-auth-callbacks

200 Content-Type: application/json
{"callbacks": ["https://<your-origin>/mcp/connect"]}
```

Requirements — II's fetch is strict and fails the connect (closed) on any
violation:

- **Same origin only.** Every entry must be an absolute URL on this origin
  (a declared cross-origin entry is rejected, never honoured). Different
  origins are separate trusted-server entries, each with its own file.
- **Exact match, no normalization.** The connect link's `callback` must equal
  a declared entry byte-for-byte — declare the exact string you put in links
  (no trailing-slash or case slack, query strings allowed but must match).
- **No fragments** in entries (II appends its own).
- Served as `application/json`, at most 8 KiB, **without redirects** (II
  refuses to follow any — an open redirect at this path must not let a third
  party serve your list), and with **CORS** headers that let II read it
  (`Access-Control-Allow-Origin: <II origin>` or `*`; the fetch carries no
  credentials). It is fetched with `cache: no-store` at every connect, so
  changes take effect immediately.
- **List only clean endpoints** you fully control — no reflecting routes, no
  user-content paths, nothing that redirects. The list is a security
  boundary: anything on it can receive registration delegations.

The name is deliberately not MCP-specific: it is a general auth-callback
allow-list that other II flows can reuse.

## 2. Connect link

Send the user's browser to:

```
https://<II_ORIGIN>/mcp#registration_key=<base64url DER public key>&callback=<declared URL>&state=<opaque>&ttl=<seconds>
```

- `registration_key` — the public key of a **fresh registration keypair `X`,
  minted per connect attempt** (Ed25519 recommended, e.g. agent-js
  `Ed25519KeyIdentity`), DER-encoded, base64url without padding. Only public
  key material travels in the link; the corresponding private key stays on
  your server and is what lets you — and only you — redeem the delegation
  chain. Mint a fresh `X` per attempt and bind it to that attempt's `state`:
  the chain you receive expires within 5 minutes, and a failed connect is
  restarted with a fresh link, never by re-driving the old one.
- `callback` — one of the URLs you declared in §1, verbatim. The user must
  have set your origin as their trusted MCP server in II Settings (matching
  is by **origin**; the callback is then matched against your declared list).
- `state` — unguessable, single-use, bound server-side to this pending
  connection. It comes back alongside the delegation; treat a mismatch as a
  hard reject.
- `ttl` — optional requested grant lifetime in seconds. Default 3600, clamped
  to [600, 2 592 000] (10 min – 30 days). The user can override it in the
  consent UI, so treat it as a suggestion; the authoritative value is the
  `expiration` returned by `mcp_register_v2`.

## 3. Receive and redeem the registration delegation

**a) Delivery.** After the user consents, II mints a canister-signed
delegation `P_reg -> Y` (where `P_reg` is a registration principal seeded
from a **fresh random nonce** — not from the consent — and `Y` is an
ephemeral key the II frontend holds; you never construct or see either),
extends it browser-side with a second, browser-signed hop `Y -> X` to your
registration key, and navigates the connecting tab to your declared callback
with the full chain in the **URL fragment**:

```
https://<your-origin>/mcp/connect#delegation=<URL-encoded chain JSON>&state=<state>
```

The whole consent — the anchor, the access level, the grant lifetime, and
the trusted server URL — is **recorded canister-side** on the registration
entry keyed by `P_reg` at consent time. You carry none of it: the fragment
holds only the chain and your `state`, and redemption (§3c) passes only your
session key. So the server can't name a different anchor, upgrade the access
level, or stretch the lifetime — it never handles any of those values — and
it **never learns the user's anchor (identity) number**. The fragment is
never sent in the HTTP request, so the chain stays out of your access logs
and any intermediary's. Your callback page reads it client-side and ships it
to your backend over a same-origin request:

```js
const params = new URLSearchParams(location.hash.slice(1));
const delegation = params.get("delegation"); // chain JSON
const state = params.get("state");
// Clear the fragment, keeping any query string your declared callback carries.
history.replaceState(null, "", location.pathname + location.search);
// POST { delegation, state } to your backend, same-origin
```

**b) The chain format** is agent-js `DelegationChain.toJSON()`: an object
`{"delegations": [ ... ], "publicKey": "<hex>"}` with hex-encoded fields,
where `publicKey` is `P_reg`'s DER key and `delegations` carries **two
hops** — the canister-signed `P_reg -> Y` and the browser-signed `Y -> X` to
your registration key. The split is deliberate: what the II canister signs
transits the IC (replicas, boundary nodes) and must be inert on its own, so
it targets the browser-held `Y`; the redeemable chain is assembled only in
the consenting browser and reaches you only via the fragment. You never
handle `Y` — with agent-js, reconstruction is the same two calls:

```js
import { DelegationChain, DelegationIdentity } from "@icp-sdk/core/identity";

const chain = DelegationChain.fromJSON(JSON.parse(delegation));
const identity = DelegationIdentity.fromDelegation(registrationKeyX, chain);
```

**c) Redemption.** Using that identity (so the canister sees `caller() ==
P_reg`), call:

```candid
mcp_register_v2 : (
    session_key : blob            // DER public key of your session key S
  ) -> (variant { Ok : McpRegistrationV2; Err : text });

type McpRegistrationV2 = record {
    expiration : nat64;        // grant expiry, ns since epoch
    permissions : Permissions; // queries = read-only, all = full
};
```

You pass **only** the DER public key of your **session key `S`** — no
anchor, no access level, no lifetime. The canister looks up the registration
entry keyed by `caller() == P_reg` and recovers the entire consent from it
(the anchor to bind, the access level, and the grant lifetime), so you never
handle any of those values and never learn the user's identity number. On
`Ok`, the grant is live: `S`'s self-authenticating principal is bound to the
consenting user's identity until `expiration`, with the access level the user
chose (see [Read-only sessions](#read-only-sessions)). The response echoes
back the effective `permissions` and `expiration` so you know exactly what
was granted. This response is synchronous and authoritative — there is no
separate completion notification to wait for or tolerate missing.

Semantics to build against:

- **Redeem immediately.** The registration delegation lives 5 minutes —
  sized to cover the browser hops plus the IC's permitted clock drift, not
  a queue. Expired delegations get a clean `Err`.
- **The consent is fixed at consent time.** The canister records the whole
  consent (anchor, access level, grant lifetime, trusted-server URL) on the
  registration entry when the user consents, and recovers it at redemption
  from the entry keyed by your `caller()` (`P_reg`). You pass none of it, so
  there is nothing to alter — you cannot upgrade the access level or stretch
  the TTL. Redemption also requires the recorded trusted-server URL to still
  equal the anchor's current one, so a config change between consent and
  redemption (the user switching or disabling the trusted server) invalidates
  an in-flight delegation with a clean `Err`.
- **Retry-safe, replace-not-add.** Within its 5-minute lifetime the
  delegation redeems repeatedly: a retry of the same call (same chain, same
  `S` — e.g. after a network timeout) just re-binds `S`, and the user's
  identity only ever holds **one** session — a redemption with a different
  `S` replaces the previous grant rather than adding a session. A failed
  connect is restarted with a fresh `X`, fresh `state`, and a new link —
  never by re-driving the old one.
- **Check `state` before redeeming.** Only redeem a delegation delivered
  with a `state` you issued and haven't consumed; consume it atomically on
  first use.
- The chain is **inert to anyone without `X`'s private key**, authenticates
  nothing but `mcp_register_v2` for the consented values, and grants no
  access by itself. Discard it after redemption.

**d) Finish on your own page.** After delivery, the tab is on _your_ origin,
on a page you serve — there is no redirect-back channel to ask II for.
Verify the redemption result and continue your flow from there (show a
"connected" state, or redirect onward — e.g. hand an OAuth code back to an
MCP client). A 2xx from your own endpoint is not proof of registration; the
canister's `Ok` is.

### Serving MCP clients over OAuth

Real-world remote MCP clients (claude.ai, Claude Desktop, Cursor, VS Code,
the MCP Inspector) authenticate per the MCP auth spec: **OAuth 2.1
authorization code + PKCE**, discovered via RFC 9728 protected-resource
metadata and RFC 8414 AS metadata, with RFC 7591 dynamic client registration.
The RFC 8628 device grant is **not** part of that profile — no current client
uses it, and it adds a device-code phishing surface with none of the PKCE
binding the rest of the flow relies on, so prefer to omit it. The II connect
slots into the code flow as your "identity provider" leg, with your connect
page closing the loop:

1. `GET /oauth/authorize` — validate `client_id` + exact `redirect_uri` +
   PKCE, create a pending auth `{code_challenge, oauth_state, resource}`,
   mint a **fresh registration keypair `X`** and a fresh, single-use
   `connect_state` for it, set an initiator cookie
   (`sid`, `HttpOnly; Secure; SameSite=Lax`) referencing the pending auth,
   and 302 the browser to the II connect link (§2).
2. The consenting browser arrives at your declared callback (§3a) carrying
   the delegation and `connect_state`; the page ships both to your backend
   (the same-origin request carries the `sid` cookie).
3. Bind before you redeem: accept the delivery only if the request carries
   the `sid` cookie of the pending auth **and** the matching, unconsumed
   `connect_state`. Then redeem (§3c) and, on `Ok`, mint the authorization
   code and 302 to the client's `redirect_uri` with `code` + the client's
   original `state`. If the initiator binding is missing, refuse and redeem
   nothing.
4. `POST /oauth/token` — exchange code + PKCE verifier for a bearer token.
   Bound its lifetime by the grant — never issue a token that outlives the
   grant. Refresh tokens are **optional**: they enable silent renewal but only
   pay off alongside server-side persistence (a restart wipes them too) and add
   no security over the grant, which is the hard, user-revocable ceiling either
   way. Without them the client re-runs the browser flow when the token
   expires; the `error="invalid_token"` challenge lets it prompt inline.

**Consent-Bound Completion is structural in this flow.** The classic
split-browser takeover — an attacker registers a client (open DCR), starts
`/oauth/authorize`, lifts the II connect link, and phishes it to a victim who
already trusts your origin (II's consent screen names only **your server's
origin**, never the OAuth client) — is broken at step 3: the delegation is
delivered only to the consenting browser (the victim's), and that browser
does not hold the attacker's `sid` cookie, so the delivery is refused,
nothing is redeemed, and no code is minted. Conversely the attacker's
browser holds `sid` but never receives the delegation. Only the legitimate
same-browser flow holds both proofs — _initiator_ (`sid`) and _consenter_
(the delegation, short-lived and redeemable only with your `X`) — so a code
can never be minted for an identity other than the one operating the
initiating client. No side-channel secret or "exactly one request, never
retried" discipline is needed; the delegation itself is the consent-bound
credential.

This closes the takeover for every client transport, including loopback. It
does **not** by itself close the _same-browser_ variant (the attacker induces
the victim's own browser to both initiate and consent) toward a **hosted**
`redirect_uri`; that residual needs hosted-redirect allow-listing.
Loopback/native clients — whose redirect resolves on the consenter's own
machine — are safe either way. (II cannot help with any of this: its trust
model only ever identifies your origin, not the OAuth client.)

Advertise what you actually implement (`authorization_endpoint`,
`response_types_supported: ["code"]`, `grant_types_supported` containing
`authorization_code`, plus `refresh_token` only if you issue them). At DCR,
handle requested `grant_types` by **intersecting them with what you support
and returning the resulting set in the registration response** (RFC 7591
makes the response authoritative — a client that asked for `refresh_token`
must be able to see it wasn't granted). Two failure modes to avoid: rejecting
a registration because the requested set isn't exactly yours, or replacing it
with a server default that drops `authorization_code` — clients request
`["authorization_code", "refresh_token"]` optimistically and must come away
able to run the code flow. Never echo a grant type you don't implement; if
the intersection loses `authorization_code` (or is empty), fail with
`invalid_client_metadata` instead of registering a client that can't complete
any flow, and keep the response consistent with `grant_types_supported`.
Persist DCR client registrations across deploys (clients cache their
`client_id`), exempt `/oauth/*` and `/.well-known/*` from bearer-token
middleware, and return proper AS error codes (`invalid_client`,
`invalid_request`) from the authorize endpoint.

### Read-only sessions

At consent the user picks an access level, and **the connect screen defaults to
read-only** — so unless the user deliberately opts out, you get a read-only
session. II fixes the level at consent time and applies it to _every_ per-app
delegation your session mints: a read-only session's delegations carry
`permissions = "queries"`, so the IC rejects update calls made through them at
ingress — enforcement is protocol-level, not up to the target app. This
includes **every management-canister call** (create/install/start/stop/
uninstall/delete, and even `canister_status` and cycles reads are update
calls), so that entire class of tools is inert under the default session. You
don't choose this per call and can't widen it; it's fixed for the session.

You learn the level synchronously at redemption: it is the `permissions`
field of `mcp_register_v2`'s `Ok` response (`queries` = read-only, `all` =
full). The `permissions` field on any `SignedDelegation` you later mint
carries the same value, as a cross-check. If your server needs update access
and the session is read-only, tell the user to reconnect with read-only off
instead of surfacing raw IC rejections. (You never pass the level — it is
recorded canister-side at consent and recovered at redemption, so there is
nothing for the server to choose or alter.)

## 4. Calling Internet Identity

Sign directly with the session key (a plain identity, no `DelegationChain`):

```candid
mcp_get_accounts : (target_origin : text)
  -> (variant { Ok : vec AccountInfo; Err : AccountDelegationError }) query;

mcp_prepare_delegation : (
    target_origin : text,
    account_number : opt nat64,   // from mcp_get_accounts; null = the anchor's default there
    session_key : blob,           // per-app key YOU generate for this target app
    max_ttl : opt nat64           // ns; default and cap: 1 hour
  ) -> (variant { Ok : McpPrepareDelegation; Err : AccountDelegationError });

mcp_get_delegation : (
    target_origin : text,
    account_number : opt nat64,   // echo the value returned by prepare
    session_key : blob,
    expiration : nat64            // echo the value returned by prepare
  ) -> (variant { Ok : SignedDelegation; Err : AccountDelegationError }) query;
```

Per-app delegations are capped at 1 hour and never outlive the grant.

`account_number = null` selects the anchor's **current default account** at the
origin (the user can change which account that is; it is not a fixed identity).
Because the default is mutable, `prepare` returns the `account_number` it
actually resolved — echo that exact value into `get`; re-deriving your own would
risk `NoSuchDelegation` if the default changed in between.

### Account principals and `target_origin`

The principal a per-app delegation acts as is derived from `target_origin` — a
**domain**, not an account you name. Pass a bare `https://<host>` (no path,
port, or trailing slash). II applies one internal remap before deriving:
`*.icp0.io` and `*.icp.net` gateway origins fold to the legacy `*.ic0.app` (so a
dapp reached through either gateway gets one stable principal); every other
origin is used as-is. A non-bare origin (with a path/port/trailing slash)
bypasses the remap and derives a _different_ principal, so normalize to the bare
origin yourself.

Because derivation is domain-based, the resulting principal is **not guaranteed
to equal the one the same user gets signing into the app in a browser.** A dapp
can declare a custom _derivation origin_ via
`/.well-known/ii-alternative-origins` — e.g. `app.example.com` deriving
principals as `<canister>.ic0.app`.
A browser honors that; the `mcp_*` methods don't expose it, so from
`target_origin` alone the server derives from the visible domain and lands on a
different principal — `mcp_get_accounts` then shows only the default/empty set,
and delegations act as an identity the user has never used there. If a user says
"this account/balance isn't what I see in my browser," this is the likely cause:
the app uses a custom derivation origin the server can't discover from the
domain. Surface that explanation, and offer to reconcile it — e.g. look up the
app's `ii-alternative-origins` (a web search or a direct fetch) and retry with
the declared derivation origin as `target_origin`. Resolving it automatically is
a planned enhancement, not current behavior.

## 5. Session lifecycle

- **One active session per user identity.** A new connect (any agent, any
  device) replaces the previous grant immediately; the old key starts getting
  `Unauthorized`.
- **Expiry:** at the grant's `expiration` every call returns
  `Unauthorized(<your principal>)`. Reconnect via a fresh connect link — with
  a fresh registration keypair `X` and a fresh session keypair `S`.
- **Revocation:** the user can revoke at any time in II Settings (toggling MCP
  off or changing the trusted URL). Indistinguishable from expiry on your
  side. Treat any `Unauthorized` as "session over → offer reconnect"; do not
  retry-loop.
