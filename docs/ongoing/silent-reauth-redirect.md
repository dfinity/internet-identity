# Silent re-auth over the redirect transport

**Authors:** sea-snake

**Date:** Aug 20, 2026

**Target audience:** Engineers, Security Reviewers, Community Developers

**Status:** Implementation

**Depends on:** [revocable-app-sessions.md](revocable-app-sessions.md) for the session this re-issues from.

## Summary

Sibling subdomains of one domain should share a sign-in: sign in on `chat.example.com` and `hr.example.com` is signed in too, and signing out of one signs out the others.

The client half of this already exists in `@icp-sdk/auth`. A cookie scoped to the parent domain tells a sibling that a session exists and which account it belongs to, and the sibling sends the user to II to have a delegation issued for its own key. What is missing is II's half: it has no way to answer such a request without showing the user a screen, no way to be told which session to answer from when an identity has several at one origin, and no way to fail that the client can tell apart from a real error.

This adds two parameters to the authorize URL and a path through the authorize flow that renders nothing. `prompt=none` means answer only if you already can. `hint` carries the account principal from the cookie and selects among the sessions II holds for the requesting origin. If neither can be satisfied, II returns `interaction_required` and the client falls back to a normal sign-in.

The cookie never holds key material, and a hint can only pick from sessions II already has for that origin, so a page cannot use one to acquire authority it was not given.

## Context

A dapp often runs as several subdomains of one domain: `chat.example.com` and `hr.example.com` belonging to `example.com`. A user thinks of these as one product and expects one sign-in to cover them.

They do not get that today. Each subdomain is a separate origin, so each gets its own principal and its own sign-in, and signing out of one leaves the others signed in.

Two mechanisms already exist that get most of the way there. A dapp can nominate a shared **derivation origin**, so that every subdomain resolves to the same principal instead of one each, provided the nominated origin publishes a list authorising them. And a browser cookie can be scoped to the parent domain, so all the subdomains can read it.

The client half is built on those two and is already specified in `@icp-sdk/auth` ([shared-sessions.md](https://github.com/dfinity/icp-js-auth/blob/5aa78d5f64714d6e8e7781e256562035c09018c6/docs/src/content/docs/shared-sessions.md)). It puts three pieces in place:

| Piece                                                                                               | Effect                                                                                   |
| --------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------- |
| A shared derivation origin, authorised by a list the nominated origin publishes                     | Every subdomain resolves to the same principal, so to the same account                   |
| A cookie scoped to the parent domain, holding only a principal and an expiry, never key material    | Siblings can see _that_ a session exists                                                 |
| A page that sends the user to II asking for a silent answer, carrying the principal from the cookie | Gets a delegation for this subdomain's own key, then returns the user to where they were |

The cookie is not II's mechanism and II never sees it. It is how the _siblings_ discover that a session is worth asking for. All II sees is the `hint`.

```mermaid
flowchart LR
    C["chat.example.com"] -->|"writes principal + expiry"| K[["cookie on .example.com"]]
    K -->|"reads it"| H["hr.example.com"]
    H -->|"/reauth: prompt=none, hint=principal"| II["II"]
    II -.->|"never sees the cookie"| K
```

## Problem

A sibling arriving at II has no local session, only a hint that one exists. For the flow to feel like a shared sign-in, II has to answer **without rendering anything**, no consent screen, no account picker, no spinner, because the user did not ask to visit II and should never see it.

II cannot do that today.

```mermaid
flowchart TB
    A["sibling redirects the user to II"] --> B{"can II answer<br/>from a session it holds?"}
    B -->|"no way to ask this"| X1["II renders the full sign-in UI<br/>for a visit the user never asked for"]
    B --> C{"which session,<br/>if the origin holds several?"}
    C -->|"no way to say"| X2["II has to guess a persona,<br/>or ask"]
    C --> D{"if it cannot answer,<br/>how does it say so?"}
    D -->|"no distinct outcome"| X3["client cannot tell 'needs a ceremony'<br/>from a real error"]
```

Three gaps: no way to be told "answer only if you already can", no way to be told which session to answer from, and no way to fail that a client can distinguish from a real error.

Answering silently also must not become a way to get something for free. A page that can send a user to II must not be able to collect a delegation for a session it does not own.

## Out of scope

- **Anything the client already specifies.** The cookie, the `derivationOrigin` setup and the `/reauth` page belong to `@icp-sdk/auth`; II never sees the cookie.
- **Sharing across unrelated domains.** Only siblings that already resolve to the same principal through a shared `derivationOrigin` can share a session.
- **Creating a session silently.** `prompt=none` can only re-issue from one that exists; it has no access method with which to authorise a new one.

## Approach

Two new authorize-URL parameters and a path through the authorize flow that renders nothing.

| Item                                    | Change                                                                                               |
| --------------------------------------- | ---------------------------------------------------------------------------------------------------- |
| `prompt`                                | New. `none` answers from a held session or fails; `login` forces a ceremony; absent behaves as today |
| `hint`                                  | New. A principal in text form, selecting which of the origin's sessions to re-issue from             |
| A no-UI path through the authorize flow | Resolves the session, extends its chain, delivers the redirect response, renders nothing             |
| `interaction_required`                  | A failure outcome distinguishable from every other, so the client falls back to a ceremony           |

A hint selects; it does not grant. It can only pick from the sessions II already holds for the origin being authorized, and it is holding the session that confers anything, so a hint read out of a cookie an app can write is safe. `prompt=none` also cannot create a session, because creating one needs an access method and a silent request has none.

Almost all of this is the II frontend using methods `revocable-app-sessions.md` already specifies. It needs one addition, `check_session`: a query that confirms the canister still holds the session the frontend's local record names. Without it a session revoked from settings or from another app would still be answered from the local copy, and the app would fail at its first mint with an error it could not tell apart from a real one.

---

---

## Specification

The flow, the `prompt=none` and `hint` rules, failure modes and the requirement checklist are in [silent-reauth-redirect-spec.md](silent-reauth-redirect-spec.md).

## Implementation stages

**Stage 1. Read the two parameters.** `prompt` and `hint` are parsed off the authorize URL
and held for the request. Nothing acts on them yet, so this changes no behaviour.

**Stage 2. Add the liveness query.** `check_session` on the canister, so the frontend can
confirm a session it holds a record for still exists.

**Stage 3. Answer without rendering.** The path that resolves a session, checks it is
live, extends its chain and delivers the redirect response, with `interaction_required`
for every case it cannot satisfy. This is the point at which `prompt=none` starts working.

Stage 3 depends on the session methods in `revocable-app-sessions.md` being in place, so
this work follows that schedule rather than running beside it.
