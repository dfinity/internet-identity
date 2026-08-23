# App sessions in the client library: specification

**Design:** [client-app-sessions.md](client-app-sessions.md) covers what this builds and why. This document assumes it and does not repeat it.

**Depends on:** [revocable-app-sessions-spec.md](revocable-app-sessions-spec.md) for the session record, the chain that authenticates a call as that session, and the account principal a ceremony returns.

## Constants

| Value                   | Where it comes from                               | Setting                    |
| ----------------------- | ------------------------------------------------- | -------------------------- |
| App delegation lifetime | `APP_DELEGATION_TTL_NS`, enforced by the canister | 5 minutes, not requestable |
| Session lifetime        | chosen at consent, clamped by the canister        | 10 minutes to 30 days      |
| Block margin            | this library                                      | 10 seconds                 |
| Pre-mint threshold      | this library                                      | 15 seconds                 |

The block margin covers a request's flight time, because the delegation has to still be valid when the replica verifies the request it was attached to.

The pre-mint threshold covers one mint and nothing else, because a refresh is scheduled for the moment it is needed rather than waiting for a request to arrive inside a window. Minting before a delegation expires discards the rest of its life, so an active session consumes lifetime at `TTL / (TTL - threshold)`: at 15 seconds it refreshes about every four and three quarter minutes against a floor of five. A threshold wide enough to catch a passing request would have to be several times larger, and every second of it becomes update calls and stable writes on every active session for as long as it lives.

## Sequence

```mermaid
sequenceDiagram
    autonumber
    participant App as application code
    participant AC as AuthClient
    participant Id as the identity
    participant IIF as II frontend
    participant IIC as II canister

    App->>AC: signIn()
    AC->>IIF: ii_session_delegation(session public key)
    IIF->>IIC: prepare_account_session / get_account_session
    IIF-->>AC: session chain, targets = II canister
    AC->>AC: store chain and session key
    AC->>Id: hand it the session
    Id->>IIC: app_prepare_delegation(app public key)
    IIC-->>Id: account key, expiration
    Id->>IIC: app_get_delegation(app public key, expiration)
    IIC-->>Id: delegation, five minutes
    AC->>AC: store the account principal, write hint
    AC-->>App: signIn() resolves

    App->>AC: getIdentity()
    AC-->>App: the same identity, no call
    App->>Id: request
    Id-->>App: signed with the delegation it holds
```

`getIdentity()` makes no call and returns the same object every time. The mint at sign-in is step 7 onwards, and after that the identity replaces its own delegation on the schedule and the request paths of the section below. An application that calls `getIdentity()` a thousand times mints nothing; one that makes a thousand requests over an hour mints about twelve times.

## Acquiring a session

**ACQ-1.**
`signIn()` requests a session with `ii_session_delegation`. Nothing checks first whether the provider offers it: this library is for Internet Identity, and a provider that cannot answer is a failed sign-in rather than a case to fall back from.

**ACQ-2.**
The request carries a session public key and, where the application configured one, a derivation origin. It carries no access level and no lifetime, both of which the user decides at consent.

**ACQ-3.**
The returned chain is rejected unless its `targets` name the II canister and nothing else. A chain without that restriction is not a session chain, and treating one as a session would give the library something it could sign arbitrary calls with.

**ACQ-4.**
The session chain is persisted through `DelegationStorage`.

**ACQ-5.**
The app delegation is never persisted, by any path.

**ACQ-6.**
The account principal is persisted alongside the session chain. It is not derivable from that chain, which is rooted at the session's own key, and the transport result carries only the chain, so it is taken from the `user_key` of the first mint, which MINT-9 makes part of signing in.

## Keys

**KEY-1.**
The session key is persisted through `IdentityStorage`, so it is the key that survives a reload and it inherits whatever non-extractable backing the configured store provides.

**KEY-2.**
The app key is generated per `AuthClient` instance and held in memory only. A reload therefore mints against a fresh app key, which costs one round trip and leaves nothing on disk that can sign for the app.

**KEY-3.**
No public export returns the session key, the session chain, or a handle from which either can be recovered.

## Minting an app delegation

A request arriving finds one of three situations:

```mermaid
flowchart LR
    R["a request"] --> Q{"life left in<br/>the delegation"}
    Q -->|"comfortable"| A["serve from it"]
    Q -->|"under the threshold"| B["serve from it, and<br/>mint behind the request"]
    Q -->|"under the margin,<br/>or none held"| C["wait for a mint"]
```

A scheduled mint asks one question before it runs:

```mermaid
flowchart LR
    T["a scheduled mint fires"] --> Q{"active<br/>recently?"}
    Q -->|"yes"| M["mint"]
    Q -->|"no"| X["cancel, and let the<br/>delegation lapse"]
```

Whatever reached it, a mint ends one of five ways:

```mermaid
flowchart LR
    M["a mint"] --> P{"outcome"}
    P -->|"a delegation whose root<br/>is the stored principal"| OK["adopt it, schedule the next"]
    P -->|"a delegation rooted<br/>anywhere else"| F["discard it"]
    P -->|"NoMatchingSession"| E["the session is over"]
    P -->|"any other failure,<br/>serving a request"| FR["fail the request,<br/>keep the session"]
    P -->|"any other failure,<br/>in the background"| S["stay silent,<br/>keep the delegation"]
```

Sign-in and a restored session reach a mint directly, without the questions above. No mint starts at all while the session itself has less than the block margin left, and if one is already running an arrival joins it rather than starting a second.

**MINT-1.**
`getIdentity()` resolves to an identity carrying an app delegation minted from the session.

**MINT-2.**
A request arriving when the held delegation has less than the block margin left, or when none is held, waits for a mint.

**MINT-3.**
While an application is active, one mint is scheduled for the moment the held delegation reaches the pre-mint threshold. It is a single scheduled refresh of a known delegation, not a recurring one.

**MINT-4.**
A scheduled mint checks at the moment it fires whether the application has been active recently, and cancels if it has not. Without that check, a session that went idle seconds after its last request would still be refreshed, stamping it as used and inflating the signal MINT-11 exists to keep honest. An application that stops making requests refreshes at most once more and then lets its delegation lapse.

**MINT-5.**
A request arriving when the held delegation has less than the pre-mint threshold and at least the block margin left is served from the delegation already held and starts a mint in the background. This is the guarantee behind the schedule, which is best effort: browsers throttle timers in hidden tabs and fire them late after a machine has slept.

**MINT-6.**
At most one mint is in flight. Callers arriving while one is running await that one rather than starting another, and a request that has to wait joins a background mint already running rather than starting a second.

**MINT-7.**
`app_get_delegation` is called with the exact `expiration` that `app_prepare_delegation` returned. A mismatch is a failed mint, not a retry with a different value.

**MINT-8.**
No mint is started when the session itself has less than the block margin left. A delegation is minted for `min(5 minutes, what remains of the session)`, so a mint against a nearly finished session returns one already too short to satisfy MINT-2, and refreshing on the delegation's remaining life alone would mint without end. Such a session is over and is treated as ERR-1.

**MINT-9.**
`signIn()` mints before it resolves, so the first request after signing in does not wait. The cost is hidden inside a ceremony the user is already waiting for.

**MINT-10.**
A page load that restores a stored session starts a mint in the background. `getIdentity()` does not wait for it.

**MINT-11.**
No recurring timer or interval triggers a mint, and no mint happens without recent activity. Activity is what arms a refresh and what MINT-4 confirms before one fires, which is what keeps the session's last-refreshed stamp a record of use rather than of an open tab.

**MINT-12.**
A mint that fails leaves the stored session chain and session key exactly as they were, except where ERR-1 applies.

**MINT-13.**
The principal a caller sees does not change when a delegation is replaced. `app_prepare_delegation` roots every delegation at the account's key, so successive mints agree. A mint whose `user_key` does not match the stored account principal is a failed mint, and its delegation is not adopted.

**MINT-14.**
`getPrincipal()` never triggers a mint. It is answered from the account principal persisted by ACQ-6, which is why the background mint of MINT-10 does not have to complete before a restored session can report who is signed in.

## Failure

**ERR-1.**
`NoMatchingSession` is terminal. The library discards the session chain, the session key and the hint, notifies subscribers, and reports the user as not authenticated.

**ERR-2.**
`InternalCanisterError`, a transport failure, and an unreachable boundary node are transient. The library retains the session, propagates the failure to the caller, and does not report a sign-out.

**ERR-3.**
`check_session` returning `false` is treated as ERR-1. It exists so the library can reach that conclusion without minting, for instance when deciding on load whether a stored chain is worth keeping.

**ERR-4.**
No failure path leaves a session chain stored without its key, or a key without its chain.

**ERR-5.**
A background mint that fails transiently is not surfaced to the application and does not report a sign-out. The delegation already held stays in use, and the next request retries, waiting if MINT-2 applies by then.

**ERR-6.**
`NoMatchingSession` from a background mint is terminal exactly as in the foreground. It means the session is gone, and which mint discovered that does not change what is true.

## The cross-subdomain hint

**HINT-1.**
The hint cookie carries the signed-in principal and the **session's** expiry.

**HINT-2.**
The hint is written when a session is acquired and removed when one is discarded, whether by sign-out or by ERR-1.

**HINT-3.**
Minting does not write the hint. An app delegation's expiry is five minutes away and says nothing about whether a sibling has a session to re-issue from.

## Signing out

**END-1.**
`signOut()` calls `app_revoke_session` before clearing local state.

**END-2.**
Local state is cleared whether or not that call succeeded. A user who pressed sign out must not remain signed in on the device in front of them.

**END-3.**
`app_revoke_session` returns nothing and always succeeds, so there is no error to surface and a repeated sign-out needs no special case.

## Reaching the II canister

**AGENT-1.**
The II canister id is taken from the `targets` of the session chain, which ACQ-3 has already established names that canister and nothing else. It is not configured.

**AGENT-2.**
Mint calls go to the origin of the configured identity provider, because the II canister is served by the same gateway that serves the II frontend.

**AGENT-3.**
Where that origin is loopback, the agent fetches the replica's root key. Where it is not, it does not.

**AGENT-4.**
No host, agent or canister-id option is added. Everything above is derived from configuration the library already has, or from the session itself.

## Public surface

**API-1.**
`signIn()`, `getIdentity()`, `signOut()`, `isAuthenticated()` and `subscribe()` keep their current signatures. Sessions add no public type, option or method.

**API-2.**
`isAuthenticated()` reports whether a session is held and unexpired, not whether an app delegation is currently valid. A held session with a lapsed delegation is authenticated, because the next call mints.

**API-3.**
`getIdentity()` performs no canister call and returns the same identity for the life of the session. It is the identity that refreshes, not `AuthClient` that hands out a fresh one, because an application passes the identity to an agent once and that agent keeps the object: an identity that was a snapshot of one delegation would go on signing with it until it expired, and no later `getIdentity()` call would reach the agent holding it.
