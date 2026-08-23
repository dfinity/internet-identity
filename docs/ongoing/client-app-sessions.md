# App sessions in the client library

**Depends on:** [revocable-app-sessions.md](revocable-app-sessions.md) for the session this holds and the methods it calls, and [silent-reauth-redirect.md](silent-reauth-redirect.md) for the parameters that let a re-issue happen without rendering anything.

## Summary

An app that signs in through `@icp-sdk/auth` receives a delegation valid for as long as the user agreed to, up to 30 days, and nothing can withdraw it before it expires. II's side of the fix is designed and built: a session the user can see and end, with short-lived delegations minted from it. No client uses those methods, so no app can reach the feature.

This holds the session inside `AuthClient`. An app calls `signIn()` and gets an identity, as it does today. Behind that identity is a session, and the delegations it signs calls with last five minutes: it obtains one when a call needs one and replaces it as it ages, rather than always holding one. `signOut()` ends the session at the canister instead of only clearing local storage. Sessions do not appear in the public API at all: an app never handles a session chain, and nothing it can call returns one.

## Context

A delegation is a signed statement that one key may act for an identity, for a stated period. The app holds the key and the delegation together, and a canister receiving a call verifies the pair without asking II anything. That is what makes a delegation cheap to use and impossible to withdraw.

Signing in today calls `icrc34_delegation`. The user picks a duration at the consent screen, II signs a delegation to the key the library generated, and the library stores both. Every call the app makes for the next few hours or weeks is signed by that key and carries that delegation.

II now offers a different arrangement. `prepare_account_session` records a session and signs a chain to a key, and `app_prepare_delegation` with `app_get_delegation` mint a delegation from that session with a ceiling of five minutes that a caller cannot raise. `app_revoke_session` deletes the session. The session itself lives at the canister, so ending it ends what can be minted from it.

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
- Negotiating with a provider that has no session methods. `AuthClient` is for Internet Identity, so `signIn()` asks for a session and expects one. Nothing inspects advertised scopes to decide, and a deployment predating these methods is not a case the library carries code for.
- Telling an application that its session is read-only. A session the user consented to for queries only mints delegations carrying a permissions field, and surfacing that wants an API of its own.

## Approach

### Two keys, one of them private to the library

The session key is what the session chain delegates to. It signs calls to the II canister and nothing else, because the chain carries `targets` naming only that canister. The app key is what an app delegation delegates to, and it signs the calls the app actually makes.

An app is handed an identity built on the second. The first never leaves `AuthClient`.

### Acquiring

`signIn()` asks for a session rather than a long-lived delegation, and stores the chain it gets back.

An application can say how long it is willing for that session to last, and `maxTimeToLive` keeps meaning what it meant for a delegation: the longest the thing being granted may live. It is a ceiling rather than a request, since what the user picks at consent wins over it, an organization's cap narrows it further, and the canister clamps the result. What an application cannot ask for is an access level, which is the user's alone. Because the chain is restricted to the II canister, a copy of it is worth nothing against the app's own canisters, and it is only useful to whoever can also reach II and mint.

### Minting

The identity handed to the app carries a five-minute app delegation, obtained by calling `app_prepare_delegation` and then `app_get_delegation` signed as the session. An agent holds that identity and signs every request with it, possibly for hours, so the identity is what has to notice its delegation ageing: it mints from inside the per-request hook the agent already calls, and one mint is in flight at a time, so several requests arriving together wait on the same round trip.

The identity is one object for the life of the session, and it is the object that refreshes. `getIdentity()` returns it without calling anything. The alternative, having `AuthClient` mint and hand back a fresh identity on each call, fails on how identities are actually used: an application passes one to an agent once, and the agent keeps it. A snapshot of a single delegation would go on signing with that delegation until it expired, and no later call to `AuthClient` would reach the agent still holding the old one.

The principal an app sees is not in the session chain. That chain is rooted at the session's own key, derived from the session seed, while an app delegation is rooted at the account's key. They are different principals, and only the second is what the app's canisters will see. The ceremony computes it and the canister returns it as `account_principal`, but the result the app receives over the transport carries only the chain, so the library learns the account principal from the first mint, where it arrives as `user_key`.

It is therefore recorded alongside the session chain rather than recomputed. A reload can answer for the principal from what it stored, without a mint, and `getPrincipal()` stays synchronous. Every later mint returns the same key, since the account seed does not change, so a mint that returns a different root is a failed mint rather than a new principal.

### Refreshing ahead of use, never on a clock

Waiting until a delegation has expired means one request every five minutes pays for a mint, which an interactive app shows as a stall. Refreshing on a timer avoids that and is worse for two reasons, one of which has nothing to do with cost.

`app_prepare_delegation` stamps the session's last-refreshed time, and that stamp is what II's settings screen shows the user as "this browser used this app 3 minutes ago", and what the session cap reclaims on. A timer refreshes whether or not anyone is looking at the tab, so the column stops meaning "in use" and starts meaning "has a tab open", which is not the reading the user is being offered. Minting only when a request needs one keeps that signal honest at no cost.

So requests are what arm a refresh, and the refresh itself is scheduled for the moment it is needed. A request from an active application schedules one mint for just before its delegation runs out, rather than minting early or waiting for another request to arrive at the right time.

Scheduling it, rather than minting whenever a request happens to arrive with little life left, is what keeps the threshold small. A threshold that has to catch a passing request must be wide enough that one turns up inside it, and everything it discards is paid for: minting before a delegation expires throws away the rest of its life, so an active session consumes lifetime at `TTL / (TTL - threshold)`. Two minutes of threshold turns a five-minute refresh into a three-minute one and adds two thirds again to the update calls and stable writes of every active session, permanently. A scheduled mint only has to cover the mint itself, which is seconds, and an active session then refreshes at nearly the floor the canister sets.

What the application sees is nothing at all. A mint lands between two of its requests, in the gap where the delegation it already holds is still good:

```mermaid
sequenceDiagram
    autonumber
    participant App as application
    participant Id as the identity it holds
    participant IIC as II canister

    App->>Id: request
    Id-->>App: signed with the delegation held
    Note over Id: mint scheduled for<br/>shortly before expiry
    Note over Id: it fires, and this delegation<br/>did serve a request
    Id->>IIC: app_prepare_delegation
    IIC-->>Id: account key, expiration
    Id->>IIC: app_get_delegation
    IIC-->>Id: delegation, five minutes
    Note over Id: replaced, and the next<br/>mint is scheduled
    App->>Id: request, minutes later
    Id-->>App: signed with the new delegation,<br/>having waited for nothing
```

The schedule needs one check, or it becomes the timer this section rejects: an application that goes quiet still has a refresh armed, and firing it would stamp the session as used. So a scheduled mint asks whether the delegation it is about to replace signed a request, and cancels if it did not.

Signing a request is the only thing that counts as use, because it is the only activity the library can see, and the window is that one delegation's lifetime rather than any longer history. Nothing here needs a constant or a window to tune.

Asking it of each delegation separately is what stops one request buying an indefinite chain. A refresh happens only if the delegation being retired was used, and the delegation it produces has to earn the next refresh the same way. An application making a request at least once per delegation lifetime therefore refreshes for as long as that holds, while one that goes quiet refreshes exactly once more and then lets the replacement lapse unused.

One trigger is worth adding beyond requests, because the case it covers is ordinary. A tab in the background has its timers throttled, so its delegation lapses while nobody is looking, and the user's first click after coming back waits for a mint. Coming back to a tab happens a second or two before that click, which is room enough to hide one, so becoming visible or regaining focus starts a mint if one is due. The identity still decides whether it is due, so a glance at a tab with a healthy delegation costs nothing.

That trigger is the one part of this that cannot be assumed to exist. `AuthClient` runs outside a browser as well, so the identity holds no reference to a DOM and the listening lives in a separable piece constructed only where those APIs are present, in the way idle detection already is. It is on by default and can be turned off. Where there is no DOM, the schedule and the request paths are the whole mechanism, and nothing is incorrect without it: a request after a long gap waits for its mint.

Requests remain the guarantee, because a schedule is best effort: browsers throttle timers in tabs nobody is looking at and fire them late after a machine has slept. A request that finds its delegation inside the threshold starts a mint in the background and is served from what it already has, and one that finds it below the block margin waits. The block margin covers a request's flight time, since the delegation has to still be valid when the replica verifies it.

Minting at the end of sign-in is not only about latency: it is where the account principal comes from, so an application that signs in and reads its principal without making a request gets an answer.

Two stalls are worth removing outright rather than absorbing, and both are hidden inside something the user is already waiting for. `signIn()` mints at the end of the ceremony, so the first call after signing in is instant. A page load that finds a stored session mints in the background while the page starts, without making `getIdentity()` wait for it.

A delegation lasts `min(five minutes, what remains of the session)`, so near the end of a session a mint returns something shorter than the margin it was meant to satisfy. Refreshing on remaining life alone would then mint, find the result already too short, and mint again without end, each iteration an update call. A session with less than the block margin left is over, and the library treats it as over rather than minting against it.

### One delegation for every tab of an origin

Tabs of one origin share their storage, so they share the session. What they do not
share is the delegation minted from it: the app key lives in memory, so each tab
mints for a key of its own, and five tabs cost five update calls and five stable
writes every five minutes for one person's one session.

They can share instead. A non-extractable key survives a structured clone, which
is what lets one live in IndexedDB, and the same property lets it cross a
`BroadcastChannel` to another tab as a handle that signs but cannot be exported.
So a tab opening asks on the channel, a tab already running answers with the key
and the delegation it holds, and the new tab adopts both without minting. Nothing
is persisted, so a delegation still never outlives the tabs that hold it and there
is nothing stale to reconcile on a load.

The hard part is not the sharing. It is that the two things one wants pull in
opposite directions: not minting five times wants a single tab responsible for
refreshing, and not failing to mint at all wants no tab to be load-bearing. A
designated refresher satisfies the first and fails the second the moment that tab
is closed.

**So coordination may only ever suppress a mint, never be required for one.** Every
tab schedules its own refresh, as it would alone. What the channel adds is the
chance to notice that the work is already done, or being done, and stand down. If
every message were lost the tabs would each mint, which is the cost of doing
nothing at all rather than a failure; and no tab waits on another to act.

Three things make that suppression usually work:

- **Tabs do not all wake at once.** The delegation's expiry is shared, so an
  unjittered schedule has every tab firing in the same instant, and the channel
  cannot suppress what has already started. Each tab picks a random offset earlier
  than the moment it would otherwise fire, wide enough that the first to wake has
  finished and told the others before the next one wakes.
- **A tab re-reads before it mints.** By the time its own timer fires, a delegation
  another tab minted may already be in hand, in which case there is nothing to do.
- **A tab about to mint says so.** Two tabs that wake close together both announce
  first and one stands down, which narrows the window from the length of a mint to
  the length of a message. A tab that stood down waits for the result, and mints
  itself if it does not arrive, because the tab that claimed it may have been closed
  mid-flight.

A request that needs a delegation now does not take part in any of this. It mints
immediately, because a caller is waiting, and the result reaches the other tabs the
same way.

### Where the mint calls go

This is the first thing in the library that calls a canister at all. Everything until now produced an identity and left the network to the application, which is why nothing in it is configured with a host.

Nothing new has to be configured. The session chain names the II canister in its `targets`, so the canister id arrives with the session. The II canister is served by the same gateway that serves the II frontend, so the origin of the configured identity provider is the host to call it on, and a loopback origin is a local replica whose root key has to be fetched.

### What is stored

A session, and not the app delegation. Keeping an artifact that dies in five minutes buys nothing, and it would leave a stale one to reconcile on the next load.

What the client stores is named for that: a session store rather than a delegation store. The distinction is not cosmetic, because a session is no longer a delegation. It is what a delegation is minted _from_, it outlives any one of them, and it carries the account's key alongside the chain, which is what lets a restored session answer for its principal without minting. A store shaped around a bare chain has nowhere to put that.

### The cross-subdomain hint carries the session's expiry

`CookieSessionStorage` derives its hint from what it is handed. This is why what it is handed is a session rather than a delegation: given a five-minute app delegation, the hint would announce to a sibling subdomain that the session expires in five minutes, and the sibling would decide there was nothing worth resuming. The hint takes the session's expiry, because what a sibling is deciding is whether a session exists to re-issue from.

### Two kinds of failure, told apart

A mint is a canister call, so it fails for two very different reasons and the library has to distinguish them.

`NoMatchingSession` means the session is not there: revoked, expired, or pruned. The library discards its local state and reports the user as signed out, because that is what has happened.

An `InternalCanisterError`, a network failure, or an unreachable boundary node means the session may well be alive. The library keeps it and lets the caller retry. Signing a user out because their train entered a tunnel is worse than a call that failed.

The library never asks whether a session is still alive. It tries to use it, and a mint that comes back with `NoMatchingSession` is the answer. That is why a page load starts a mint in the background rather than probing first: the mint it needs anyway is also the check, so there is one mechanism where there could have been two.

### Ending

`signOut()` calls `app_revoke_session` before clearing local state, so access ends within one app-delegation lifetime instead of running to the session's expiry. It clears local state even when the call fails, because a user who pressed sign out must not remain signed in on the device in front of them.

## Specification

[client-app-sessions-spec.md](client-app-sessions-spec.md) states the requirements, the call sequences, and the constants.

## Implementation stages

1. **Mint app delegations from a session chain.**
   The refreshing identity, its failure classification, and the shared in-flight mint. Nothing produces a session chain yet, so this changes no behaviour and is safe to release on its own.
2. **Acquire a session at sign-in, and carry the session's expiry in the hint.**
   This is the stage that turns the feature on, and the two parts belong together: the hint becomes wrong the moment a five-minute delegation is what a sibling reads.
3. **Revoke at sign-out.**
   Harmless before stage 2 and only meaningful after it.
