# MCP server guide: connecting to Internet Identity

This documents the protocol an MCP server implements to act on an Internet
Identity user's behalf. The user registers the server's **session key** with
the II canister (a _grant_, up to 30 days, revocable in II Settings at any
time); the server then signs `mcp_*` canister calls with that key — no
delegation chains are delivered or handled. Per-app delegations (up to 1 hour)
are minted on demand through `mcp_prepare_delegation` / `mcp_get_delegation`.

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
    M->>U: redirect to /mcp#callback, state, ttl
    U->>F: open /mcp, authenticate and consent (pick TTL)
    F->>C: mcp_get_config(anchor)
    C-->>F: enabled + trusted URL (verify callback origin)
    F->>M: POST callback {state}
    M-->>F: 200 {public_key} — fresh keypair for this connection
    F->>C: mcp_register(anchor, session_key, ttl) [user-authenticated]
    C-->>F: {expiration} — grant bound: principal to anchor
    F-)M: POST callback {state, expiration} (best effort)
    F-->>U: "You're signed in"
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
    note over M: session over → prompt a fresh connect with a new key
    end
```

## 1. Connect link

Send the user's browser to:

```
https://<II_ORIGIN>/mcp#callback=<https URL on your origin>&state=<opaque>&ttl=<seconds>
```

- `callback` — an https URL on your origin. The user must have set your origin
  as their trusted MCP server in II Settings (matching is by **origin**).
- `state` — unguessable, single-use, bound server-side to this pending
  connection. Treat a mismatch as a hard reject.
- `ttl` — optional requested grant lifetime in seconds. Default 3600, clamped
  to [600, 2 592 000] (10 min – 30 days). The user can override it in the
  consent UI, so treat it as a suggestion; the authoritative value arrives in
  the completion notification.

No key material travels in the link: II never registers anything taken from
the (attacker-craftable) fragment.

## 2. Callback endpoint

Your callback answers JSON POSTs from the II frontend. Enable CORS: answer
`OPTIONS` preflights and set `Access-Control-Allow-Origin: <II origin>` (or
`*`; no credentials are used), `Access-Control-Allow-Headers: content-type`,
`Access-Control-Allow-Methods: POST`.

**a) Key request** — after user consent, II's frontend asks for your session
public key:

```
POST <callback>            Content-Type: application/json
{"state": "<state>"}
```

Respond `200 {"public_key": "<base64url, unpadded, DER-encoded public key>"}`.

Generate a **fresh keypair per user-connection** — Ed25519 recommended (e.g.
agent-js `Ed25519KeyIdentity`). The registered principal is
`self_authenticating(DER)`; one principal serves at most one identity, so a
key reused across users makes the second registration fail. An unknown,
already-used, or expired `state` must get a non-2xx response — the connect
flow then errors out and **nothing is registered**.

**b) Completion notification** — best effort, after II registers the key
(distinguish from (a) by the `expiration` field):

```
POST <callback>
{"state": "<state>", "expiration": "<grant expiry, ns since epoch, decimal string>"}
```

Respond with any 2xx; mark the connection live and store `expiration` (a
string because u64 nanoseconds overflow JSON numbers). You must tolerate never
receiving this call (e.g. network failure after registration succeeded): fall
back to attempting a signed call — success means you are registered.

There is no redirect back to II; the II tab finishes on its own.

## 3. Calling Internet Identity

Sign directly with the session key (a plain identity, no `DelegationChain`):

```candid
mcp_get_accounts : (target_origin : text)
  -> (variant { Ok : vec AccountInfo; Err : AccountDelegationError }) query;

mcp_prepare_delegation : (
    target_origin : text,
    account_number : opt nat64,   // from mcp_get_accounts; null = default account
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

## 4. Session lifecycle

- **One active session per user identity.** A new connect (any agent, any
  device) replaces the previous grant immediately; the old key starts getting
  `Unauthorized`.
- **Expiry:** at the grant's `expiration` every call returns
  `Unauthorized(<your principal>)`. Reconnect via a fresh connect link with a
  fresh keypair.
- **Revocation:** the user can revoke at any time in II Settings (toggling MCP
  off or changing the trusted URL). Indistinguishable from expiry on your
  side. Treat any `Unauthorized` as "session over → offer reconnect"; do not
  retry-loop.
