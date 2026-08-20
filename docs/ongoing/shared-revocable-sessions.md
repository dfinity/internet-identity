# Shared revocable sessions

**Authors:** sea-snake — **Date:** Aug 20, 2026

**Target audience:** Engineers, Security Reviewers, Community Developers

**Status:** Implementation

## Summary

A sign-in today is a bearer token nobody can take back. II hands an app a delegation valid for up to 30 days, verification never consults II again, and signing out clears the app's own storage without invalidating anything. Sibling subdomains cannot share a sign-in either, so a user signs in separately on each and signing out of one leaves the others signed in.

This is the shape of three designs that together replace that. The long-lived artifact becomes a **session** the canister holds and the user can end; the thing the app carries becomes a **5-minute delegation** it re-mints from that session. Revoking stops new mints, so access ends within one delegation lifetime. Sessions are grouped per browser, so a user can see the browsers they are signed in from and sign one out of everything. And because siblings sharing a `derivationOrigin` resolve to one account, they share one session — sign in on one and the others re-issue silently.

Read this page first. Each design is linked at the end, with a separate specification for implementers.

## Context

When a user signs in to a dapp with Internet Identity, the dapp receives a **delegation**: a signed artifact naming a key, valid for up to 30 days, chosen by the dapp.

Verifying it never calls back into II. That is what makes it fast and cheap, and it is also why, once II hands it over, II is out of the loop entirely.

```mermaid
flowchart LR
    U([user]) -->|"ceremony, once"| II["II"]
    II -->|"delegation, up to 30 days"| A["app"]
    A -->|"calls, for 30 days"| D[("dapp canister")]
    A -.->|"never contacts II again"| II
```

## Problem

A sign-in is a bearer token nobody can take back.

| | Today |
| -------- | ----- |
| Revocation | **None at all.** Nothing II can do reaches a delegation it has already issued |
| A stolen delegation | Full access at that dapp for whatever remains of its 30 days |
| Signing out | Local only. Clearing browser state invalidates nothing already issued |
| Visibility | Neither the user nor II can see, list, or end an active sign-in |

There is a second, related gap: sibling subdomains cannot share a sign-in.

```mermaid
flowchart TB
    subgraph now["today"]
        C1["chat.example.com"] -->|"own ceremony"| D1[["own delegation"]]
        H1["hr.example.com"] -->|"own ceremony"| D2[["own delegation"]]
        D1 -.->|"signing out here"| N1(["leaves the other signed in"])
    end
```

A user signs in separately on each, and signing out of one leaves the others signed in.

## Approach

Split the one long-lived artifact into two: a **session** the canister holds and the user can end, and a **short-lived delegation** the app carries.

```mermaid
flowchart LR
    U([user]) -->|"ceremony, once"| IIF["II frontend"]
    IIF -->|"session chain"| A["app"]
    IIF -->|"session record on<br/>the account reference"| C[("II canister")]
    A -->|"app delegation, 5 min"| D[("dapp canister")]
    A -->|"refresh, checked against<br/>the record every 5 min"| C
    U -->|"revoke a session<br/>or a whole browser"| C
```

Getting a fresh delegation means asking II, and that is what gives II a say again.

| | After | Why |
| -------- | ----- | --- |
| Delegation lifetime | 5 minutes | Matches what MCP already mints |
| Session lifetime | Up to 30 days, and revocable at any point in it | The length is not what changed; being able to end it is |
| Revocation | Takes effect within one delegation lifetime | Revoking stops new mints; one already issued runs out |
| A stolen delegation | At most 5 minutes of access | |
| A stolen session chain | Revocable, and `targets` stops it reaching dapp canisters directly | The real protection is that it can be revoked at all |
| Signing out | Actually revokes, canister-side | The app's own sign-out removes its session record |
| Visibility | Per browser, with a last-used timestamp | "This browser used this app 3 minutes ago" against "5 weeks ago" |
| Cost | One update call per active session per 5 minutes | The app calls the canister directly — no browser round trip |

Two properties that are easy to miss:

- **A session cannot renew or clone itself.** Creating one requires an anchor access method, which a session does not have. A stolen chain can only mint short delegations until it is revoked.
- **Refresh needs no browser.** The app talks to the canister directly, so there is no popup, iframe, or navigation on the five-minute cadence.

---

## How siblings share one sign-in

```mermaid
flowchart TB
    S[("one session record<br/>(anchor, application, account)")]
    C["chat.example.com"] -->|"prompt=none, by redirect"| S
    H["hr.example.com"] -->|"prompt=none, by redirect"| S
    S -->|"5-minute delegation"| C
    S -->|"5-minute delegation"| H
```

Apps sharing a `derivationOrigin` resolve to the same principal, so they resolve to one application, so they share **one** session record. Sign in on one and the others re-issue from it silently. Sign out of one and the others have nothing left to re-issue from.

That is not a copy between apps and not a cookie trick. There is one record and the siblings take turns.

---

## What changes where

| Method | Who calls it | New? | What it does |
| ------ | ------------ | ---- | ------------ |
| `app_prepare_delegation` / `app_get_delegation` | app frontend, via `AuthClient` | New | Mints a 5-minute delegation from a live session |
| `app_revoke_session` | app frontend, via `AuthClient` | New | The app's own sign-out. Deletes its session record |
| `check_session` | II frontend | New | Whether a session is still live, for the silent path |
| `prepare_account_session` / `get_account_session` | II frontend | New | Creates a session and signs its identity |
| `revoke_account_session` / `revoke_device_sessions` | II frontend | New | Ends one session, or every session a browser holds |
| `prepare_account_delegation` / `get_account_delegation` | app frontend | Untouched | The delegation flow as it works today |
| `icrc34_delegation` | app frontend | Untouched | Unchanged, and unaware of any of this |

The three `app_`-prefixed methods are the whole public surface, and none of them names an identity. Everything unprefixed is the II frontend's, which ships with the canister.

No existing method changes shape or behaviour, so nothing breaks and nothing has to move at once. An app opts in by upgrading its client; until it does, it gets exactly what it gets today.

---

## Read further

Each feature has a design doc for what and why, and a specification for how.

| Design | Specification | Covers |
| ------ | ------------- | ------ |
| [Account tracking](tracked-default-accounts.md) | [spec](tracked-default-accounts-spec.md) | The storage this is built on: recording which dapps an identity uses, reclaiming unused dapp records, and the index that resolves a principal to an account |
| [Revocable app sessions](revocable-app-sessions.md) | [spec](revocable-app-sessions-spec.md) | The session record, its identity and chain, refresh, revocation, and session devices |
| [Silent re-auth over the redirect transport](silent-reauth-redirect.md) | [spec](silent-reauth-redirect-spec.md) | `prompt=none`, `hint`, and what II supplies for the sibling flow |
| [Shared sessions, client side](https://github.com/dfinity/icp-js-auth/blob/5aa78d5f64714d6e8e7781e256562035c09018c6/docs/src/content/docs/shared-sessions.md) | — | What an app does: `derivationOrigin`, the cookie, and the `/reauth` page |

Apps do not implement any of the above. `AuthClient` holds the session, attaches the caller-info bundle and re-mints on its own schedule, so an app author sees a sign-in call and an identity, as today.
