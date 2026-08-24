# App sessions in the client library

**Depends on:** [revocable-app-sessions.md](revocable-app-sessions.md) for the session this holds and the methods it calls, and [silent-reauth-redirect.md](silent-reauth-redirect.md) for the parameters that let a re-issue happen without rendering anything.

## Summary

An app that signs in through `@icp-sdk/auth` receives a delegation valid for as long as the user agreed to, up to 30 days, and nothing can withdraw it before it expires. Internet Identity's side of the fix is designed and built: a session the user can see and end, with short-lived delegations minted from it. No client uses those methods, so no app can reach the feature.

This holds the session inside `AuthClient`. An app calls `signIn()` and gets an identity, as it does today. Behind that identity is a session, and the delegations it signs calls with last five minutes. The library replaces them ahead of use and never stores one, so a delegation exists only in the tab holding it and only for as long as it is good for. `signOut()` ends the session at the canister instead of only clearing local storage. Sessions do not appear in the public API at all, so an app never handles a session chain and nothing it can call returns one. Upgrading is not free, though: the stored record changes shape, nothing migrates it, and everyone signed in through an earlier version signs in again once.

## Context

A delegation is a signed statement that one key may act for an identity, for a stated period. The app holds the key and the delegation together, and a canister receiving a call verifies the pair without asking II anything.

Signing in today calls `icrc34_delegation`. The user picks a duration at the consent screen, II signs a delegation to the key the library generated, and the library stores both. Every call the app makes for the next few hours or weeks is signed by that key and carries that delegation.

II now offers a different arrangement. `prepare_account_session` records a session and signs a chain to a key, and `app_prepare_delegation` with `app_get_delegation` mint a delegation from that session with a ceiling of five minutes that a caller cannot raise. `app_revoke_session` deletes the session. The session itself lives at the canister, so ending it ends what can be minted from it.

## Problem

A delegation that leaks is usable for its whole life. Exfiltrated from storage, copied off a shared machine, or captured from a compromised dependency, it acts as the user until it expires, and no action by the user or by II reaches it.

Signing out is local. `signOut()` clears the library's storage, which stops that browser from using the delegation; it does not stop anyone else who has a copy.

The library cannot call II's session methods at all, so the canister work has no consumer. The guide for sharing a sign-in across sibling subdomains is written against sessions and promises that signing out of one app signs the user out of the others, which is only true once something in the client revokes.

## Out of scope

- Exposing sessions in the public API. An app receives an identity, and the session behind it is the library's business.
- The browser key proof and the browser registry. Those are II's side and are specified in [revocable-app-sessions-spec.md](revocable-app-sessions-spec.md).
- Listing an identity's sessions, or revoking another browser's, from an app. Both belong to II's settings, and an app is authenticated as one session.
- Migrating delegations stored by an earlier version of the library. Users signed in through one sign in again, as the Summary says.
- Negotiating with a provider that has no session methods. `AuthClient` is for Internet Identity, so `signIn()` asks for a session and expects one. Nothing inspects advertised scopes to decide, and a deployment predating these methods is not a case the library carries code for.
- Telling an application that its session is read-only. A session the user consented to for queries only mints delegations carrying a permissions field, and surfacing that wants an API of its own.

## Approach

### The durations this rests on

Three nested lifetimes, and two marks near the end of a delegation.

| Duration                | Value                          | Set by                                                                                 | What it bounds                                                      |
| ----------------------- | ------------------------------ | -------------------------------------------------------------------------------------- | ------------------------------------------------------------------- |
| Session lifetime        | 10 minutes to 30 days          | the app requests a ceiling, the user chooses at consent, an SSO cap narrows, II clamps | how long anything can be minted at all                              |
| App delegation lifetime | `min(5 minutes, session left)` | II, and not requestable                                                                | how long one delegation signs an app's calls                        |
| Pre-mint threshold      | 15 seconds before expiry       | this library                                                                           | when a refresh is scheduled, and when a request mints behind itself |
| Block margin            | 10 seconds before expiry       | this library                                                                           | below this a request waits for a mint                               |
| Inherit window          | 200 milliseconds               | this library                                                                           | how long a starting tab waits for another tab's pair                |

### Two keys, one of them private to the library

The session key is what the session chain delegates to. It signs calls to the II canister and nothing else, because the chain carries `targets` naming only that canister. It lasts as long as the session.

The app key is what an app delegation delegates to, and it signs the calls the app actually makes. It lasts as long as that delegation, which is five minutes, because the authority is in the delegation and a key that outlives one carries none. So a mint makes a key and gets a delegation for it in one act, and the pair is replaced as one.

An app is handed an identity built on the second, and never sees either. A third value travels with them and is not a secret. The account key is the public key an app delegation is rooted at, returned by the canister as `user_key`, and the principal an app's canisters see is derived from it. It is stored beside the chain precisely because it is public.

|                   | Session key                          | App key                                  |
| ----------------- | ------------------------------------ | ---------------------------------------- |
| Delegated to by   | the session chain, from the ceremony | an app delegation, from a mint           |
| Signs             | calls to the II canister only        | every call the application makes         |
| Lives for         | the session                          | its delegation, so five minutes          |
| Stored            | IndexedDB, non-extractable           | memory only, never persisted             |
| Leaves the origin | never                                | never; it reaches other tabs as a handle |

### Acquiring

`signIn()` asks for a session rather than a long-lived delegation, and stores the chain it gets back.

An application can say how long it is willing for that session to last, and `maxTimeToLive` keeps meaning what it meant for a delegation, which is the longest the thing being granted may live. It is a ceiling rather than a request, since what the user picks at consent wins over it, an organization's cap narrows it further, and the canister clamps the result. What an application cannot ask for is an access level, which is the user's alone. Because the chain is restricted to the II canister, a copy of it is worth nothing against the app's own canisters, and it is only useful to whoever can also reach II and mint.

### Minting

The identity handed to the app carries a five-minute app delegation, obtained by calling `app_prepare_delegation` and then `app_get_delegation` signed as the session. An agent holds that identity and signs every request with it, possibly for hours, so the identity is what has to notice its delegation ageing: it mints from inside the per-request hook the agent already calls, and one mint is in flight at a time, so several requests arriving together wait on the same round trip.

The identity is one object for the life of the session even though what it signs with is replaced every five minutes, which works because the object reaches the current pair rather than holding one. That is also why an app can hold the identity and never notice a rotation: it never sees the app key, and the principal it does see comes from the account, which does not change.

`getIdentity()` returns the same object without calling anything. Having `AuthClient` mint and hand back a fresh identity on each call would fail on how identities are used, because an application passes one to an agent once and the agent keeps it. A snapshot of a single delegation would go on signing with that delegation until it expired, and no later call to `AuthClient` would reach the agent still holding the old one.

The principal an app sees does not appear in the session chain, which is rooted at the session's own key. An app delegation is rooted at the account's key instead. They are different principals, and only the second is what the app's canisters will see. The ceremony computes it and the canister returns it as `account_principal`, but the result the app receives over the transport carries only the chain, so the library learns the account principal from the first mint, where it arrives as `user_key`.

It is therefore recorded alongside the session chain rather than recomputed. A reload can answer for the principal from what it stored, without a mint, and `getPrincipal()` stays synchronous. Every later mint returns the same key, since the account seed does not change, so a mint that returns a different root is a failed mint rather than a new principal.

### Refreshing ahead of use, never on a clock

Waiting until a delegation has expired means one request every five minutes pays for a mint, which an interactive app shows as a stall. Refreshing on a timer avoids that and is worse for two reasons, one of which has nothing to do with cost.

#### Why not a timer

`app_prepare_delegation` stamps the session's last-refreshed time. II's settings screen shows that stamp to the user as "this browser used this app 3 minutes ago", and the session cap reclaims on it. A timer refreshes whether or not anyone is looking at the tab, so the column would come to mean "has a tab open" instead of "in use". Minting only when a request needs one keeps the timestamp accurate at no cost.

#### How a refresh is scheduled

A request from an active application schedules one mint for just before its delegation runs out, rather than minting early or waiting for another request to arrive at the right time.

Scheduling the mint keeps the threshold at 15 seconds. A threshold wide enough to catch a passing request has to be minutes wide, and every second of it is discarded delegation life, because an active session then mints at `TTL / (TTL - threshold)`. A two-minute threshold turns a five-minute refresh into a three-minute one, adding two thirds to the update calls and stable writes of every active session for as long as it lives. A scheduled mint only has to cover the mint itself.

A mint lands between two of an application's requests, in the gap where the delegation it already holds is still good:

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

#### The check that keeps the schedule honest

The schedule needs one check, or it becomes the timer this section rejects. An application that has gone quiet still has a refresh armed, and firing it would record the session as used. So a scheduled mint asks whether the delegation it is about to replace signed a request, and cancels if it did not.

Signing a request is the only thing that counts as use, because it is the only activity the library can see, and the window is that one delegation's lifetime rather than any longer history.

Asking it of each delegation separately is what stops one request buying an indefinite chain. A refresh happens only if the delegation being retired was used, and the delegation it produces has to earn the next refresh the same way. An application making a request at least once per delegation lifetime therefore refreshes for as long as that holds, while one that goes quiet refreshes exactly once more and then lets the replacement lapse unused.

#### Coming back to a tab

One trigger is added beyond requests. A tab in the background has its timers throttled, so its delegation lapses while nobody is looking and the user's first click after returning waits for a mint. Returning happens a second or two before that click, long enough to cover a mint, so becoming visible or regaining focus starts one if it is due. The identity still decides whether it is due, so focusing a tab with a healthy delegation costs nothing.

That trigger is the one part of this that cannot be assumed to exist. `AuthClient` also runs outside a browser, so the identity holds no DOM reference and the listening lives in a separable piece constructed only where those APIs exist, as idle detection already does. It is on by default and can be turned off. Without a DOM the schedule and the request paths are the whole mechanism, and a request after a long gap simply waits for its mint.

Requests remain the guarantee, because a schedule is best effort. Browsers throttle timers in tabs nobody is looking at, and fire them late after a machine has slept. A request that finds its delegation inside the threshold starts a mint in the background and is served from what it already has, and one that finds it below the block margin waits. The block margin covers a request's flight time, since the delegation has to still be valid when the replica verifies it.

#### Sign-in and page load

`signIn()` mints at the end of the ceremony, so the first call after signing in is instant. A page load that finds a stored session mints while the page starts, without making `getIdentity()` wait for it.

Minting at the end of sign-in is also where the account principal comes from, so an application that signs in and reads its principal without making a request gets an answer.

The page-load mint arrives through the same trigger as returning to a tab, because a page load is the page becoming visible for the first time. Turning that trigger off therefore turns off the page-load mint too, and with it the only thing that discovers a revoked session before the application makes a request of its own.

| Trigger                                  | Condition                                                            | Does the caller wait? |
| ---------------------------------------- | -------------------------------------------------------------------- | --------------------- |
| A request, comfortable life left         | more than 15 seconds remaining                                       | no mint at all        |
| A request inside the threshold           | 10 to 15 seconds remaining                                           | no, it mints behind   |
| A request under the margin, or none held | less than 10 seconds, or nothing held                                | **yes**               |
| The scheduled refresh                    | armed at 15 seconds; cancels unless that delegation signed a request | background            |
| The page becoming visible or focused     | only if a mint is due; covers the page load                          | background            |
| `signIn()`                               | at the end of the ceremony                                           | **yes**               |
| A recurring timer                        | never, and this is the thing the design rejects                      | not applicable        |

A delegation lasts `min(five minutes, what remains of the session)`, so near the end of a session a mint returns something shorter than the margin it was meant to satisfy. Refreshing on remaining life alone would then mint, find the result already too short, and mint again without end, each iteration an update call. A session with less than the block margin left is over, and the library treats it as over rather than minting against it.

### What is shared, and how far

The session is the widest of the three and the one nothing local can see. A derivation origin is the origin an application asks II to derive its principals from, so sibling subdomains configured with the same one resolve to a single application rather than to several. A session record is keyed on the identity, that application, the account and the browser. `chat` and `hr` therefore share one record instead of holding two that behave alike, and either of them signing out ends it for both.

Everything the client holds is narrower. A chain is in `localStorage`, which is per origin, and the app key and its delegation ride a `BroadcastChannel`, which is also per origin. So `chat`'s tabs coordinate with each other and `hr`'s tabs coordinate with each other, and the two sets cannot speak at all.

The hint cookie is the only thing that crosses between siblings, and it carries a principal and the session's expiry, which is all a sibling needs in order to decide whether to try a silent re-auth.

The floor is therefore one mint per active origin, not one per domain. Two siblings open means two mints every five minutes for one session, which is inherent and not a gap to close later.

A delegation is issued to a key and each origin signs with its own, so a delegation minted for `chat` authorises nothing for `hr`. Sharing one between them would mean sharing the key it was issued to. A cookie holds bytes, so passing a key through one hands out the private material itself, to anything on the domain that can read a cookie and to every request that carries it.

Tabs of one origin pass a key handle instead, which signs without being exportable. Between origins there is nothing of the kind to pass.

| Shared thing                      | Where it lives                                                             | How far it reaches          | The consequence                                             |
| --------------------------------- | -------------------------------------------------------------------------- | --------------------------- | ----------------------------------------------------------- |
| The session                       | the II canister, one record per identity, application, account and browser | every sibling of the domain | either sibling signing out ends it for both                 |
| The session chain and session key | `localStorage` and IndexedDB                                               | one origin                  | each sibling holds its own chain to the same session        |
| The app key and its delegation    | memory, offered over a `BroadcastChannel`                                  | the tabs of one origin      | the floor is one mint per active origin                     |
| The hint                          | a cookie scoped to the domain                                              | every sibling of the domain | a sibling decides whether to acquire silently, with no call |

```mermaid
flowchart TB
    subgraph D["example.com: as far as the hint cookie reaches"]
        H[("cookie: the account's principal<br/>and the session's expiry")]
        subgraph O1["chat.example.com"]
            L1[("session chain, account key,<br/>session key")]
            B1["app key and its delegation,<br/>in memory, shared across tabs"]
        end
        subgraph O2["hr.example.com"]
            L2[("session chain, account key,<br/>session key")]
            B2["app key and its delegation,<br/>in memory, shared across tabs"]
        end
    end
    L1 -->|mints from| S
    L2 -->|mints from| S
    S[("one session record,<br/>at the II canister")]
```

### One delegation for every tab of an origin

Tabs of one origin share their storage, so they share the session. What they do not share is the delegation minted from it: the app key lives in memory, so each tab mints for a key of its own, and five tabs cost five update calls and five stable writes every five minutes for one person's one session.

#### Passing the pair between tabs

A non-extractable key can be structured-cloned, which is what lets one live in IndexedDB, and the same property lets it cross a `BroadcastChannel` as a handle that signs but cannot be exported. What crosses is the pair, because that is what a mint produces and what expires together.

A tab opening asks on the channel and a tab already running answers with the pair it holds. The asking tab waits 200 milliseconds for an answer, which is the only waiting in this design: long enough for a tab that is there to reply, and short enough that a tab starting alone is not delayed by tabs that do not exist. Nothing is persisted, so a delegation never outlives the tabs holding it and there is nothing stale to reconcile on a load.

```mermaid
sequenceDiagram
    autonumber
    participant T2 as a tab starting
    participant BC as BroadcastChannel<br/>(this origin only)
    participant T1 as a tab already running
    participant II as II canister

    T2->>BC: ask
    BC->>T1: ask
    alt a tab answers within the inherit window
        T1-->>BC: offer: the key handle and its delegation
        BC-->>T2: offer
        Note over T2: adopts the pair and reschedules<br/>from that delegation's expiry
    else nothing answers in 200 ms
        T2->>II: mints a pair of its own
        T2->>BC: offers it, unasked
    end
    Note over T1,T2: either way both hold the same pair
```

Sharing the whole pair also makes tabs converge with nothing electing a winner. Two tabs that end up with a pair each, because a browser restored them in the same instant with neither able to answer the other, are not stuck that way: the next mint produces one pair, its broadcast reaches both, and both adopt it. So divergence costs an extra mint or two and lasts at most one delegation's lifetime, where sharing a key alone would have left an origin permanently minting once per tab.

#### Why no tab may be in charge

Avoiding five mints wants one tab responsible for refreshing; never missing a mint wants no tab to be essential. A designated refresher gives the first and loses the second the moment that tab is closed.

The rules that follow:

1. Coordination may only ever suppress a mint, never be required for one.
2. Every tab schedules its own refresh, as it would if it were alone.
3. Losing every message costs mints, never correctness.
4. The lock may be relied on for how much this costs, never for whether it works.

#### The lock

A named lock is what makes the suppression work, where the browser has one. A tab about to mint takes it, and tabs that wake in the same moment queue behind it rather than each starting a mint of their own. Whoever holds it looks again before acting, because by then the delegation may already have been replaced, in which case there is nothing left to do.

The lock also settles the case a schedule cannot. A tab holding it can be closed mid-mint, and the browser releases a lock when the tab holding it goes away, so the next tab in the queue simply proceeds. Nothing has to guess how long to wait for a tab that is not coming back, which a timeout would have had to.

Where the browser has no such lock every tab mints, which is the cost of no coordination and not a failure.

```mermaid
flowchart TD
    N["a tab is about to mint"] --> L{"does this browser have<br/>the named-lock API?"}
    L -->|no| M1["mint, and broadcast the pair"]
    L -->|yes| Q["queue on the lock"]
    Q --> H["holds it"]
    H --> R{"has a fresh pair arrived<br/>while queueing?"}
    R -->|yes| S["skip the mint and adopt it"]
    R -->|no| M2["mint, and broadcast the pair"]
    M2 -.->|tab closed mid-mint| RL["the browser releases the lock,<br/>the next in the queue proceeds"]
```

A request that needs a delegation now queues on the same lock rather than jumping it. Waiting costs at most one mint, which is what it would have spent minting anyway, and it often ends with another tab's fresh delegation and no mint at all.

### Signing out is not the same as finding out

Because one session serves every sibling, a client that discovers its own chain is dead must be careful about what it concludes. Signing in again replaces the browser's session at that application, so the sibling that did not sign in is left holding a chain to a session that no longer exists. It finds out on its next mint.

What it must not do then is remove the shared hint. The hint is the domain's, not this origin's, and the sibling that signed in has just written a fresh one. Taking it away would tell that sibling, correctly by its own rules, that the session it just obtained is gone.

The two acts differ in one respect.

|                             | Signing out                  | Finding out the chain is stale        |
| --------------------------- | ---------------------------- | ------------------------------------- |
| What happened               | the user ended the sign-in   | a sibling signed in and replaced it   |
| Discovered by               | `signOut()`                  | a mint returning `NoMatchingSession`  |
| The session at the canister | revoked                      | already gone                          |
| Local chain and session key | removed                      | removed                               |
| The shared hint             | **removed**                  | **left alone**                        |
| What the user sees          | signed out across the domain | nothing, and a silent re-auth follows |

The sibling recovers without the user seeing anything:

```mermaid
sequenceDiagram
    autonumber
    participant chat as chat.example.com
    participant hint as the hint cookie
    participant hr as hr.example.com
    participant IIC as II canister

    chat->>IIC: signs in again
    Note over IIC: the browser's session at this<br/>application is replaced
    chat->>hint: writes the new session's expiry
    hr->>IIC: mints from the chain it holds
    IIC--xhr: no such session
    hr->>hr: drops its chain, leaves the hint alone
    hint-->>hr: a session exists, for this account
    hr->>IIC: asks again, rendering nothing
    IIC-->>hr: a chain to the new session
```

Recovery works because that record is keyed on the application, which for siblings is the derivation origin they share, so `chat`'s ceremony had one record to replace rather than one per origin. The ceremony `chat` ran replaced that one record, so the request `hr` makes finds the new one instead of nothing.

A hint can outlive the session it describes, after a revocation from settings for instance, so a sibling acting on one has to be able to fall back to asking the user; it is a hint and not an authority. And two siblings asking at once is safe, because asking without rendering never creates a session: both are handed a chain from the same record, and neither replaces anything.

### Where the mint calls go

This is the first thing in the library that calls a canister at all, and everything until now left the network to the application. So Internet Identity is configured as two values, the URL a ceremony renders at and the canister that mints. They are not the same address. A custom domain can front the mainnet canister, and a local deployment changes both. Each half defaults to its mainnet value, so an application deploying against mainnet configures neither, and options for the agent making the calls are handed to it as its own.

Deriving any of this from the authorize URL was the alternative, and it reads well until an application needs a deployment of its own: the origin of a URL is not a promise about which canister answers there, and taking the canister id out of the session chain would leave the library reading its own configuration out of a credential.

| Setting       | Default                  | Used for                                              |
| ------------- | ------------------------ | ----------------------------------------------------- |
| Authorize URL | the mainnet ceremony URL | rendering the sign-in ceremony                        |
| Canister id   | the mainnet II canister  | minting, revoking, and checking the chain's `targets` |

The chain's `targets` are a check and not a source. A session chain names that canister and nothing else, so a chain naming anything else, or nothing at all, is refused before the first call. The unrestricted case matters most, because the session key signs with that chain and accepting one would leave the library holding a credential good for any call.

### What is stored

The session is stored and the app delegation is not. Keeping an artifact that dies in five minutes buys nothing and leaves a stale one to reconcile on the next load.

The store is named for that. A session is what delegations are minted _from_, it outlives any one of them, and it carries the account's key beside the chain, so a restored session can return its principal without minting. A store shaped around a bare chain has nowhere to keep that key.

| What                                      | Where                                  | Survives a reload | Removed by                           |
| ----------------------------------------- | -------------------------------------- | ----------------- | ------------------------------------ |
| Session chain and account key, one record | `localStorage`, per origin             | yes               | signing out, and finding out         |
| Session key                               | IndexedDB, non-extractable             | yes               | signing out, and finding out         |
| App key and its delegation                | memory, shared across an origin's tabs | no                | the tab closing, expiry, replacement |
| The hint                                  | a cookie scoped to the domain          | yes               | signing out only                     |

### The cross-subdomain hint carries the session's expiry

`CookieSessionStorage` derives its hint from what it is handed. This is why what it is handed is a session rather than a delegation: given a five-minute app delegation, the hint would announce to a sibling subdomain that the session expires in five minutes, and the sibling would decide there was nothing worth resuming. The hint takes the session's expiry, because what a sibling is deciding is whether a session exists to re-issue from.

### Two kinds of failure, told apart

`NoMatchingSession` means the session is not there: revoked, expired, or pruned. Anything else leaves the session possibly alive, so the library keeps it and lets the caller retry. A transient network fault must not sign a user out.

| Failure                          | What it means            | Local session | What the caller sees         |
| -------------------------------- | ------------------------ | ------------- | ---------------------------- |
| `NoMatchingSession`              | the session is gone      | discarded     | reported signed out          |
| Anything else, serving a request | the session may be alive | kept          | the failure, to retry        |
| Anything else, in the background | the session may be alive | kept          | nothing; the held pair stays |

A mint returning `NoMatchingSession` is how the library finds out, which is why a page load starts one in the background. The mint it needs anyway is also the check.

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
