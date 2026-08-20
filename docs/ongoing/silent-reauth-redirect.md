# Silent re-auth over the redirect transport

**Authors:** sea-snake — **Date:** Aug 20, 2026

**Target audience:** Engineers, Security Reviewers, Community Developers

**Status:** Implementation

**Depends on:** [revocable-app-sessions.md](revocable-app-sessions.md) for the session this re-issues from.

## Summary

We propose letting sibling subdomains of one domain share a sign-in: sign in on `chat.example.com` and `hr.example.com` is signed in too, and signing out of one signs out the others. The client half of this already exists in `@icp-sdk/auth`. What is missing is II's half — a way to answer an authorize request without showing the user anything.

It works as follows. First, a sibling that has no delegation of its own reads a cookie scoped to the parent domain, which tells it a session exists and which account it belongs to. Second, it redirects the user to II with `prompt=none` and that account's principal as a `hint`. Third, II checks it holds a live session for that origin and account, extends its chain to the sibling's key, and redirects straight back — rendering nothing at all. Finally, if it cannot answer, it returns a failure the client can tell apart from a real error and falls back to a normal sign-in.

The cookie never carries key material, and a hint can only select among sessions II already holds for the requesting origin, so a page cannot use one to acquire authority it does not have.

## Context

Sibling subdomains of one domain should share a sign-in: sign in on `chat.example.com` and `hr.example.com` is signed in too; sign out of one and the others follow.

The client half of that already exists, specified in `@icp-sdk/auth` ([shared-sessions.md](https://github.com/dfinity/icp-js-auth/blob/5aa78d5f64714d6e8e7781e256562035c09018c6/docs/src/content/docs/shared-sessions.md)). It puts three pieces in place:

| Piece | Effect |
| ----- | ------ |
| A shared `derivationOrigin`, authorized by `ii-alternative-origins` | Every sibling resolves to the same principal |
| A cookie scoped to the parent domain, holding only a principal and an expiry — never key material | Siblings can see *that* a session exists |
| A `/reauth` page using `transport: 'redirect'`, `prompt: 'none'`, `hint: <principal from the cookie>` | Re-issues this app's own delegation, then returns the user to its own `?next=` |

The cookie is not II's mechanism and II never sees it. It is how the *siblings* discover that a session is worth asking for. All II sees is the `hint`.

```mermaid
flowchart LR
    C["chat.example.com"] -->|"writes principal + expiry"| K[["cookie on .example.com"]]
    K -->|"reads it"| H["hr.example.com"]
    H -->|"/reauth: prompt=none, hint=principal"| II["II"]
    II -.->|"never sees the cookie"| K
```

## Problem

A sibling arriving at II has no local session, only a hint that one exists. For the flow to feel like a shared sign-in, II has to answer **without rendering anything** — no consent screen, no account picker, no spinner — because the user did not ask to visit II and should never see it.

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

| Item | Change |
| ---- | ------ |
| `prompt` | New. `none` answers from a held session or fails; `login` forces a ceremony; absent behaves as today |
| `hint` | New. A principal in text form, selecting which of the origin's sessions to re-issue from |
| A no-UI path through the authorize flow | Resolves the session, extends its chain, delivers the redirect response, renders nothing |
| `interaction_required` | A failure outcome distinguishable from every other, so the client falls back to a ceremony |

The safety property is that **a hint can only select, never confer.** It picks among the sessions II already holds for the origin being authorized, and holding the session is what grants anything — so a hint from a cookie an app can write is safe.

Almost all of this is the II frontend using methods `revocable-app-sessions.md` already specifies. It needs one addition: a query the frontend can use to confirm the canister still holds the session its local record names, so a session revoked elsewhere produces a clean fallback rather than a delegation that cannot mint.

An earlier attempt at `prompt` and `hint` predates sessions and re-issued by extending a stored delegation chain offline, which nothing could revoke. This specifies them on top of sessions instead, so a silent re-issue is a canister-checked mint like any other.

---

---

## Specification

The flow, the `prompt=none` and `hint` rules, failure modes and the requirement checklist are in [silent-reauth-redirect-spec.md](silent-reauth-redirect-spec.md).
