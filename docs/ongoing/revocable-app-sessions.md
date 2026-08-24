# Revocable app sessions

**Depends on:** `tracked-default-accounts.md`, which supplies the account reference a session is stored on and the principal index the refresh path resolves through.

## Summary

An app delegation cannot be revoked. II signs one at sign-in, the app uses it for up to 30 days, and verification never consults II again, so nothing II or the user does can reach it. Signing out of an app clears that app's own storage and invalidates nothing.

Two things take its place. The canister keeps a **session** for the account the user signed in with. The app holds a delegation chain rooted at that session, restricted so it can only call II, and mints itself a 5-minute delegation whenever it needs one. Deleting the session stops further mints, so access ends within five minutes.

Because the canister knows which browser each session came from, it can also group them: a user can see the browsers they are signed in from and sign one out of every app at once.

Apps do not implement any of it. `AuthClient` holds the session and re-mints on its own schedule, so an app author still writes a sign-in call and gets an identity.

## Context

When a user signs in to an app with Internet Identity, the app does not get a password or a token it can send to a server. It gets a **delegation**: a short signed statement saying that a particular public key is allowed to act for a particular principal, until a stated expiry. The app holds the matching private key and signs its calls with it.

The delegation is signed by the II canister, and anything on the network can check that signature on its own. That is what makes it cheap: an app canister receiving a call verifies the signature and the expiry and nothing else. It never asks II whether the delegation is still good, because there is nothing in the format that would let it ask.

The app decides how long the delegation lasts when it requests one. The current maximum is 30 days.

```mermaid
sequenceDiagram
    autonumber
    actor U as user
    participant A as app
    participant II as II canister
    participant D as app canister
    U->>II: signs in
    II-->>A: delegation, valid up to 30 days
    A->>D: calls, signed with it
    Note over A,D: II is never consulted again
```

## Problem

Because nothing consults II after the fact, a delegation cannot be withdrawn. Once II has signed one it is usable until its expiry, and no action by the user or by II reaches it.

Nor is there an indirect way to invalidate one. II derives an account's principal by hashing a secret salt together with the identity and the origin; rotating that salt would change every future derivation without touching an artifact already signed, so it would break every existing principal and revoke nothing.

The practical effects:

|                     | Today                                                     |
| ------------------- | --------------------------------------------------------- |
| A stolen delegation | Works at that app until it expires, up to 30 days         |
| Signing out         | Clears local state. Invalidates nothing already issued    |
| Visibility          | Neither the user nor II can list or end an active sign-in |

The same gap prevents a user from signing one browser out. II keeps no record that a given browser is signed in anywhere, so there is nothing to show the user and nothing to delete.

## Out of scope

- **Changing the initial ceremony.**  
  Sign-in is unchanged.
- **Changing anything for apps that have not upgraded their client.**  
  No existing method changes shape or behaviour. An app that does nothing keeps what it has today.
- **Listing an identity's individual sessions.**  
  The browser list ships, and signing a browser out with it. Listing the individual sessions behind a browser does not: a flat per-identity list mixes every origin together and wants a surface that lists applications first.
- **Deleting a device record.**  
  Signing a browser out is in scope. Forgetting the browser is a separate operation, not designed here.
- **Long-lived offline access.**  
  An app that cannot reach II cannot re-mint, so an app needing weeks of offline authority is not served by this.

## Approach

1. **Store a session.**  
   Signing in records a session against the account the user chose, on the per-account row that `tracked-default-accounts.md` introduces: when it was created, when it expires, when it was last used, which browser it came from, and the access level the user consented to.

   On that row rather than in a table of its own, because the row is already the thing a settings screen lists and is already capped and swept. A separate table would need its own bound and its own cleanup, and would let a session outlive the record of the app it belongs to. The cost is that the two share a fate: evicting a row for an app the identity stopped using ends any session on it, which is the behaviour `tracked-default-accounts.md` argues for rather than an accident of layout.

2. **Hand the app a chain, not a credential.**  
   The canister signs the session to a key the II frontend generates and cannot export, and the frontend extends that chain to a key the app supplies. The app's hop is restricted to the II canister, so the chain can be used to ask II for delegations and for nothing else.
3. **Give the app a way to say which account it means.**  
   The app names nothing. Its calls are signed by the session chain, so the canister recognises the caller as that session and looks up which account that session belongs to. There is nothing for an app to supply, and so nothing for it to get wrong or lie about.
4. **Mint short delegations on demand.**  
   The app calls `app_prepare_delegation` / `app_get_delegation` with that chain and gets a 5-minute delegation for its account principal. This is a direct canister call: no popup, no iframe, no navigation.
5. **Revoke by deleting the record.**  
   No new delegation can be minted, and the one already out expires within five minutes.
6. **Group sessions by browser.**  
   Each browser keeps a key of its own that never leaves II, proves possession of it at sign-in, and rotates it to a fresh one each time. Every session records which browser created it, so settings can list browsers and end all of one browser's sessions at once. A sign-in performed with a stolen access method cannot present that key, so it shows up as a browser the user does not recognise, and because the key is retired after each use, a copied browser profile cannot keep signing in alongside the real one without surfacing as a second entry.

### Core principles

Creating a session requires an access method: a passkey, or an OpenID sign-in. A session is not an access method, so a session cannot create another one or extend its own life. A stolen chain mints 5-minute delegations until it is revoked, and nothing more.

An app is told nothing about the identity behind the account. Not the identity number, not the account number. The only identifier that crosses the boundary is the account's principal, which the app already holds. Two apps comparing notes must not be able to tell they are talking to the same user, which is why nothing II hands an app, or accepts from one, carries the numbers it uses internally.

Revoking deletes the record rather than marking it, so there is nothing left for a later call to overlook. A call that cannot resolve a usable session gets one error, whatever the cause, so a client has a single failure to handle.

The minting and revocation halves already exist in the canister, scoped to MCP servers. An
MCP grant maps a session principal to an identity, an expiry and an access level;
delegations minted against it are capped at five minutes; and revoking means deleting the
grant. MCP is this mechanism with a single client.

What MCP does not need, and apps do, is many sessions per identity, somewhere to keep them,
and a way for a user to see and end them. MCP holds one grant per identity, reached through
a pointer from its own config, which is why it needs neither an index nor a cap.

## System overview

| Layer        | Trust                   | Role                                                                                                                             |
| ------------ | ----------------------- | -------------------------------------------------------------------------------------------------------------------------------- |
| App frontend | Untrusted (user device) | Requests a session, holds the chain, mints its own delegations. All of this is `AuthClient`, not app code                        |
| II frontend  | Untrusted (user device) | Runs the ceremony, creates the session, holds the non-extractable key the chain is rooted at, extends the chain to the app's key |
| II canister  | Trusted (IC replicas)   | Stores sessions, signs session identities, mints app delegations, enforces caps and revocation                                   |
| App canister | Trusted (IC replicas)   | Sees an ordinary delegation for the account's principal. Unchanged by this design                                                |

A compromised app or II frontend cannot forge a session, because sessions exist only in canister state and creating one requires an access method.

The canister does not take a caller's word for which account it is calling about, because a caller never says. It resolves the session from the signature on the call, and the account from that session.

### Security properties

A reviewer will ask these first, so they are answered here rather than left to the
specification.

| Question                                                   | Answer                                                                                                                                                                                                                                                                                                                                                                                                                     |
| ---------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Can an attacker hide inside a browser the user recognises? | No. A browser is named by a key it proves possession of, that key is non-extractable and never leaves the browser, and it is replaced at every sign-in. An attacker with a stolen access method can sign in, but only as a browser the identity has never seen, which is the signal the list exists to give. Claiming an existing entry is refused: a key another browser holds cannot be announced as your own successor. |
| Can an app ask for a longer delegation?                    | No. Both halves of the mint derive the 5-minute ceiling themselves, and neither will witness a delegation that outlives its session.                                                                                                                                                                                                                                                                                       |
| What does a stolen session chain get?                      | 5-minute delegations until the session is revoked, and nothing else. It cannot create a session or extend its own life. The user's lever is the browser list, which is why it carries a last-used time: a browser they do not recognise, still in use, is what tells them to act.                                                                                                                                          |
| Can the chain be used against an app's own canister?       | No. Its final hop is restricted to the II canister, and that restriction is part of what is signed, so an app reaching for the chain where it meant its delegation fails immediately rather than appearing to work.                                                                                                                                                                                                        |
| Can cleaning up idle records destroy a live session?       | Yes, and deliberately. The row is what makes an app visible in settings, so sparing it would leave the user holding access they cannot see or revoke. A row and its sessions therefore go together, and what is lost is a ceremony rather than an account: the next visit is served a new session, since a session is not tied to the access method that created it.                                                       |
| Can a copied browser profile stay hidden?                  | No. Copying a profile off disk copies the browser key, but the key is retired at each sign-in, so two copies cannot both keep using it: whichever authenticates second appears as a new browser. What this does not do is say which of the two entries is the user's.                                                                                                                                                      |

### The flow, end to end

Bob signs in to `chat.example.com`, which needs to call its own canister on his behalf.

1. Bob taps sign in. The app's `AuthClient` sends an `ii_session_delegation` request with a freshly generated key.
2. II takes Bob through the regular sign-in flow, and Bob picks which account to use.
3. The II canister records a session on that account, noting the browser Bob is using, and signs the session's identity to the II frontend's own key.
4. The II frontend extends the chain to the app's key, restricted to the II canister, and returns it.
5. Whenever the app needs a delegation, it calls II with that chain and gets one valid for 5 minutes.
6. Weeks later, with the app still in daily use, Bob opens II settings, sees "Chrome on macOS", and signs it out. Every session that browser holds is deleted in one message.
7. The app's next mint fails. Bob is signed out of every app he used from that browser, within five minutes.

---

## Specification

The detail needed to build this, exact interfaces, storage shapes, algorithms, caps, and the requirement checklist, is in [revocable-app-sessions-spec.md](revocable-app-sessions-spec.md).

## Implementation stages

The order is chosen so that storage exists before anything writes to it, and so that
nothing user-facing appears before the mechanism behind it works.

### Stage 1. Add somewhere to keep a session

The record gains its place on the per-account row, with the fields listed in the approach. Nothing creates one yet, so this
changes no behaviour and can be released on its own.

### Stage 2. Add the browser registry

Each identity gets a list of the browsers it has signed in from, keyed by a key the browser
holds and proves, with a limit of 20 and least-recently-used replacement. Inert until
something registers a browser.

### Stage 3. Create sessions and mint from them

The behavioural change. Signing in records a session and registers the browser, and the two app-facing methods mint 5-minute
delegations against it. Nothing an app can reach exists yet: stage 7 is what hands an app the chain these methods authenticate.

### Stage 4. Record that a session is still in use

Each mint stamps the session, its account reference, and the browser, which is what makes the browser list worth reading and what the limits order on.

### Stage 5. Let an app end its own session

The app's sign-out becomes real: it deletes its own record and nothing else.

### Stage 6. Let the user end sessions from II

Every session a browser holds, from settings. Revoking one session lands as a method; its UI waits on the listing work.

### Stage 7. Hand apps a session instead of a plain delegation

The frontend request that returns a session chain rather than a delegation. Until this ships, no app can reach any
of the above.

### Stage 8. Show the browser list

The settings screen that lists browsers, when each was last used, and offers to sign one out.

Stages 1 and 2 change nothing observable. Stage 3 is where the data model commits. Stages 5
and 6 can land in any order after 4, but nothing an app can reach works until 7 ships, and
8 is the only stage a user sees.

Nothing here waits on work outside this repository.
