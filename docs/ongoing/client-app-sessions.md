# App sessions in the client library

**Depends on:** [revocable-app-sessions.md](revocable-app-sessions.md) for the session this holds and the methods it calls, and [silent-reauth-redirect.md](silent-reauth-redirect.md) for the parameters that let a re-issue happen without rendering anything.

## Summary

An app that signs in through `@icp-sdk/auth` receives a delegation valid for as long as the user agreed to, up to 30 days, and nothing can withdraw it before it expires. II's side of the fix is designed and built: a session the user can see and end, with short-lived delegations minted from it. No client uses those methods, so no app can reach the feature.

This holds the session inside `AuthClient`. An app calls `signIn()` and gets an identity, as it does today. Behind that identity is a session, and the delegations the identity carries last five minutes and are re-minted as they lapse. `signOut()` ends the session at the canister instead of only clearing local storage. Sessions do not appear in the public API at all: an app never handles a session chain, and nothing it can call returns one.

## Context

A delegation is a signed statement that one key may act for an identity, for a stated period. The app holds the key and the delegation together, and a canister receiving a call verifies the pair without asking II anything. That is what makes a delegation cheap to use and impossible to withdraw.

Signing in today calls `icrc34_delegation`. The user picks a duration at the consent screen, II signs a delegation to the key the library generated, and the library stores both. Every call the app makes for the next few hours or weeks is signed by that key and carries that delegation.

II now offers a different arrangement. `prepare_account_session` records a session and signs a chain to a key, and `app_prepare_delegation` with `app_get_delegation` mint a delegation from that session with a ceiling of five minutes that a caller cannot raise. `app_revoke_session` deletes the session, and `check_session` answers whether one is still there. The session itself lives at the canister, so ending it ends what can be minted from it.

## Problem

Three things follow from a delegation that cannot be withdrawn, and the third is what makes this urgent rather than merely desirable.

A delegation that leaks is usable for its whole life. Exfiltrated from storage, copied off a shared machine, or captured from a compromised dependency, it acts as the user until it expires, and no action by the user or by II reaches it.

Signing out is local. `signOut()` clears the library's storage, which stops that browser from using the delegation; it does not stop anyone else who has a copy.

The library cannot call II's session methods at all, so the canister work has no consumer. The guide for sharing a sign-in across sibling subdomains is written against sessions and promises that signing out of one app signs the user out of the others, which is only true once something in the client revokes.

## Out of scope

- Exposing sessions in the public API. An app receives an identity, and the session behind it is the library's business.
- The browser key proof and the browser registry. Those are II's side and are specified in [revocable-app-sessions-spec.md](revocable-app-sessions-spec.md).
- Listing an identity's sessions, or revoking another browser's, from an app. Both belong to II's settings, and an app is authenticated as one session.
- Migrating delegations stored by an earlier version of the library. The storage change that carries this is already a breaking one.

## Approach

### Two keys, one of them private to the library

The session key is what the session chain delegates to. It signs calls to the II canister and nothing else, because the chain carries `targets` naming only that canister. The app key is what an app delegation delegates to, and it signs the calls the app actually makes.

An app is handed an identity built on the second. The first never leaves `AuthClient`.

### Acquiring

`signIn()` asks for a session rather than a long-lived delegation, and stores the chain it gets back. Because the chain is restricted to the II canister, a copy of it is worth nothing against the app's own canisters, and it is only useful to whoever can also reach II and mint.

### Minting

The identity handed to the app holds a five-minute app delegation and replaces it before it lapses, by calling `app_prepare_delegation` and then `app_get_delegation` signed as the session. Minting happens when a caller needs a delegation, and one mint is in flight at a time, so several calls arriving together wait on the same round trip rather than each starting one.

A background timer was the alternative, and it is worse: re-minting on a schedule keeps every open tab calling the canister for as long as it is open, including tabs nobody is looking at. Minting on demand costs one round trip on the first call after a lapse and nothing while the app is idle.

### What is stored

The session chain, and not the app delegation. Keeping an artifact that dies in five minutes buys nothing, and it would leave a stale one to reconcile on the next load.

### The cross-subdomain hint carries the session's expiry

`CookieDelegationStorage` derives its hint from the delegation it is handed. Handed a five-minute app delegation, the hint would announce to a sibling subdomain that the session expires in five minutes, and the sibling would decide there was nothing worth resuming. The hint takes the session's expiry, because what a sibling is deciding is whether a session exists to re-issue from.

### Two kinds of failure, told apart

A mint is a canister call, so it fails for two very different reasons and the library has to distinguish them.

`NoMatchingSession` means the session is not there: revoked, expired, or pruned. The library discards its local state and reports the user as signed out, because that is what has happened.

An `InternalCanisterError`, a network failure, or an unreachable boundary node means the session may well be alive. The library keeps it and lets the caller retry. Signing a user out because their train entered a tunnel is worse than a call that failed.

`check_session` exists for the case where the library wants that answer without minting, such as deciding on page load whether a stored chain is worth keeping.

### Ending

`signOut()` calls `app_revoke_session` before clearing local state, so access ends within one app-delegation lifetime instead of running to the session's expiry. It clears local state even when the call fails, because a user who pressed sign out must not remain signed in on the device in front of them.

### Talking to an II that has no session methods

A library that only knows how to acquire a session would break against a deployment that predates these methods. The scopes returned when a connection is established say whether `ii_session_delegation` is available; where it is not, `signIn()` falls back to `icrc34_delegation` and behaves as it does today. An app therefore gets revocable sessions where the provider supports them and a working sign-in everywhere else, without choosing.

## Specification

[client-app-sessions-spec.md](client-app-sessions-spec.md) states the requirements, the call sequences, and the constants.

## Implementation stages

1. **Mint app delegations from a session chain.**
   The refreshing identity, its failure classification, and the shared in-flight mint. Nothing produces a session chain yet, so this changes no behaviour and is safe to release on its own.
2. **Acquire a session at sign-in, and carry the session's expiry in the hint.**
   This is the stage that turns the feature on, and the two parts belong together: the hint becomes wrong the moment a five-minute delegation is what a sibling reads. The fallback for providers without the method lands here too, since this is the first stage that asks for a session.
3. **Revoke at sign-out.**
   Harmless before stage 2 and only meaningful after it.
