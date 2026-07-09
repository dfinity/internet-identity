# IdP-side per-app gating for enterprise SSO

**Status:** Draft — RFC for review. Nothing here is implemented.
**Last updated:** 2026-07-09
**Companion:** `enterprise-sso-per-app-access-control.md` specifies the *id.ai-side* gate
(directory + access canister). This document specifies an **independent, complementary
layer** that gates entirely on the **IdP side**. The two are composable and selected per app
(§9); neither depends on the other.

**Implementation status.** None. All sections are proposed.

---

## Glossary

| Term | Meaning |
| --- | --- |
| **IdP** | The org's corporate identity system (Okta, Microsoft Entra ID, OneLogin). |
| **id_token** | The signed OIDC JWT the IdP issues for a login; II verifies it against the IdP's JWKS. |
| **`aud`** | Audience claim — the OAuth `client_id` the token was minted for. |
| **`iss` / `sub`** | The issuer and the IdP's identifier for the human within a token. |
| **Stable identifier** | An identifier that is the **same across all of an org's OIDC clients**. `sub` qualifies on Okta (org authorization server), Google, and OneLogin, but **not on Entra**, whose `sub` is pairwise (per-client); Entra's cross-client-stable id is `oid`. Declared per org as `stable_identifier_claim` in the well-known (§5). |
| **App-assignment** | The IdP's native "which users/groups may use this application" control, evaluated at the app's authorization endpoint. |
| **Primary client** | The org's default OIDC client — the one meant for II itself. All identity/credential data is keyed on it. |
| **Per-app client** | A dedicated OIDC client the org registers in its IdP for a single gated dapp. Used **only** for the gate, never for identity. |
| **Well-known** | The org's `https://<domain>/.well-known/ii-openid-configuration`. |
| **Anchor** | The user's II identity number. |
| **sso_domain** | The verified domain a credential was authenticated through; already stored on SSO credentials. |

---

## 1. Background

II already supports enterprise SSO: an org publishes
`https://<domain>/.well-known/ii-openid-configuration`, II discovers the org's IdP, the user
authenticates, and II issues a delegation. Today an org's SSO uses a single OIDC client, so
authenticating gives access to every IC dapp reachable through II — all-or-nothing.

### 1.1 The idea

Give each restricted dapp its **own OIDC client** in the org's IdP, and let the admin gate it
with **native app-assignment** — the exact workflow they use for every other SaaS app. When a
user signs in to a gated dapp, II runs the ceremony against that dapp's client. If the user
isn't assigned, the IdP returns `access_denied` at its authorization endpoint and **II never
receives a token**. The gate is the IdP's, enforced before II is involved.

### 1.2 Why this is its own layer

It needs **no id.ai-side infrastructure** — no access canister, directory sync, signing proxy,
or admin panel. Policy authoring, audit, and enforcement stay in the IdP. As a bonus the org
gets per-app **conditions** for free (per-app MFA step-up, device posture, network zones) —
things a membership-only gate cannot express — because the IdP runs that app's full sign-on
policy on every access.

The cost is that it only works where the IdP can assign groups to an individual OIDC client
(Okta, Entra, OneLogin — not Google, §8), and the org must register and map one client per
gated app.

---

## 2. Goals & non-goals

**Goals**

- An IT admin gates a dapp exactly as they gate any SaaS app: register an OIDC client, assign
  a group. No id.ai-specific policy surface.
- Enforcement is **native IdP app-assignment**, fail-closed at the IdP.
- A user is the **same II identity** across all of an org's gated apps and its default SSO.
- **Identity data is always keyed on the primary II client.** A user has exactly **one
  OpenID access method** (the primary client's); per-app clients never become access methods
  or credentials — they are gate-only.
- **Zero new id.ai infrastructure** beyond the existing SSO ceremony and well-known.
- Per-app IdP conditions (MFA step-up, device, network) work unchanged.

**Non-goals**

- **Google Workspace** — cannot express per-OIDC-client group assignment (§8).
- **Any id.ai-side policy or directory** — that is the companion layer.
- **Forwarding groups/roles to dapps** — out of scope (that's the id.ai-side layer). Note
  this layer *does* rely on the dapp verifying the **certified `sso_domain`** (provenance) for
  the rogue-domain defense (§6, §7) — but that's the existing `sso:<domain>` label, not new
  attribute forwarding.
- **Changing the dapp-facing principal derivation** — it stays `f(anchor, origin)` (§6).

---

## 3. Threat model

**Trusted parties**

- The org's IdP — runs app-assignment and signs id_tokens.
- The org's DNS / web root — the well-known declares the org's client_ids; controlling it is
  proof of domain ownership.
- II core — identity, delegation issuance, id_token verification.

**Untrusted parties**

- **The public** — the well-known is world-readable, so the `origin -> client_id` map is
  disclosed.
- A user trying to reach an app they are not assigned to, including by reusing a token minted
  for a *different* app they are assigned to.
- A malicious OIDC client at the same issuer (relevant only for direct, non-SSO providers).

**Attacks defended**

| Attack | Defense |
| --- | --- |
| Token minted for app W replayed to reach app P | II re-checks `aud == the origin's declared client_id`; a W token is rejected for origin P (§6). |
| Reaching a gated app via the org's default client | No fallback: a gated origin is servable only by its declared client (same `aud` check). |
| **Insider re-routes a gated app through a rogue discovery domain** — an org member (assigned to the primary client, not to the gated app) publishes their own `evil.com` whose well-known points at the org's *real* IdP and declares a client they can pass, then signs in through it to reach the gated app as their real identity | The **certified `sso_domain`** is unforgeable (it is the discovery domain II actually resolved), so the login is certifiably `sso:evil.com`. A gated dapp trusts a specific org domain and rejects it (§6, §7). The anchor may collide (same human) but access is decided on the certified domain, not the principal — so no anchor re-key is needed. |
| A stray client at the same issuer hijacking an anchor | A per-app token resolves to identity only if its `aud` is a client the org's well-known declares; direct providers (no `sso_domain`, no `app_clients`) keep full `(iss, sub, aud)` isolation (§7). |

**Out of scope / operational (cannot be enforced by II)**

- **Entra `Assignment required?` defaults to OFF.** If the admin forgets to enable it, the
  app fail-opens to the whole tenant. This is IdP configuration; the onboarding guide must
  call it out (§8).
- A fully compromised org IdP.

---

## 4. How it works

```mermaid
sequenceDiagram
    autonumber
    participant U as User
    participant FE as II Frontend
    participant II as II Core
    participant IdP as org's IdP
    participant Dapp as gated dapp (payroll.com)
    U->>FE: sign in to payroll.com
    FE->>II: discover_sso(org.com); get_sso_discovery(org.com, payroll.com)
    II-->>FE: resolved_client_id = client_P
    FE->>IdP: authorize(client_P, nonce)
    Note over IdP: app-assignment for client_P
    alt not assigned
        IdP-->>FE: access_denied
        FE-->>U: no access
    else assigned
        IdP-->>FE: id_token (aud = client_P)
        FE->>II: openid_prepare_delegation(jwt)
        Note over II: session seeded (primary, client_P); anchor resolves to primary
        FE->>II: get_account_delegation(anchor, payroll.com, per_app_client=client_P)
        II-->>FE: delegation for f(account, payroll.com)
        FE->>II: prepare_icrc3_attributes(anchor, payroll.com, per_app_client=client_P)
        Note over II: client_P == app_clients[payroll]? then certify implicit:origin + sso:org.com
        II-->>FE: certified attributes
        FE->>Dapp: delegation + certified attributes
        Note over Dapp: verify sso:org.com AND implicit:origin, then grant
    end
```

II core (not the FE) fetches and caches the well-known via `discover_sso`; the FE reads the
per-origin client with `get_sso_discovery(domain, origin)`. Three responsibilities:

- **IdP — assignment.** App-assignment decides who can obtain a token for `client_P`. II holds
  no policy.
- **II — gate the certified `implicit:origin`.** The `openid_prepare_delegation` session is
  seeded with the per-app client; the origin-scoped calls prove it (§6.3), and II certifies
  `implicit:origin` for the gated origin only when the proven client matches
  `app_clients[origin]`. Identity resolves to the primary client (`aud` never enters identity).
- **Dapp — verify.** The dapp checks the certified `sso:<domain>` (which org) and relies on the
  certified `implicit:origin` (gated access). That's the boundary it enforces on.

---

## 5. Well-known additions

The org's existing `/.well-known/ii-openid-configuration` gains an `origin -> client_id` map
plus two flags. Additive; existing single-client SSO deployments keep working. As today,
**II core** fetches and caches this file (via `discover_sso`); the frontend never reads the
org's web root.

```jsonc
{
  // existing SSO discovery (the primary client, meant for II itself)
  "client_id": "0oaDEFAULT",
  "openid_configuration": "https://org.okta.com/.well-known/openid-configuration",
  "name": "Org",

  // per-app clients for gated dapps: origin -> client_id
  "app_clients": {
    "https://payroll.com": "0oaPAYROLL",
    "https://admin.internal.app": "0oaADMIN"
  },

  // when true, an origin NOT in app_clients is denied (default-deny).
  // when false/absent, an unlisted origin uses the primary client (open to any org user).
  "gate_all_apps": false,

  // claim holding the cross-client-stable identifier (default "sub"; Entra uses "oid"). Optional.
  "stable_identifier_claim": "sub"
}
```

- **Listed origin** -> gated: II uses that client_id and requires the returned `aud` to match.
- **Unlisted origin** -> depends on `gate_all_apps`: `false`/absent serves it via the primary
  client (open to any authenticated org user, exactly as today); `true` denies it.
  `gate_all_apps: true` lets an org lock II SSO down to an explicit set of dapps.
- **Declared client set** = the primary `client_id` plus every `app_clients` value; the
  allowlist used by the identity-resolution safety check (§6, §7).
- **Bounded: at most 100 `app_clients` per org** (aligns with II's `MAX_ATTRIBUTES_PER_REQUEST`
  and the discovery byte cap; keeps the O(n) hashed-key scan per login trivial). Real orgs gate
  a handful — 100 is generous headroom. A well-known exceeding it is **rejected, not
  truncated** — truncation could silently drop a gated origin into the `gate_all_apps: false`
  open fallback. The parsed map lives in II's **in-heap** SSO discovery cache (not stable
  memory), shared across up to `SSO_CACHE_MAX_ENTRIES` (5000) domains, so 100/org also bounds
  aggregate heap; `DISCOVERY_MAX_RESPONSE_BYTES` is sized to fit.
- **Propagation latency.** A change to the well-known (`app_clients`, `gate_all_apps`, …)
  takes effect only after II's cached copy refreshes. The backend discovery cache is fresh for
  `FRESH_FOR_SECONDS` (**1 h**) and then served up to `STALE_FOR_SECONDS` (**+1 h**)
  stale-if-error (`openid/sso.rs`), so an edit propagates within **~1 h**. There is no
  frontend cache — the frontend only reads the canister (`get_sso_discovery`) and drives the
  fetch (`discover_sso`). Both windows are single constants: shorten them for faster policy
  propagation, lengthen them to cut outcall volume.

### 5.1 Optional: hashed origins

`client_id`s are public by design (II uses public/SPA clients, no secret), so exposing them is
harmless. The **origins** may be sensitive — they reveal the org's internal app portfolio. An
org may hide them by replacing a cleartext origin key with a `hash:salt` key, keeping the same
`origin -> client_id` object:

```jsonc
"app_clients": {
  "https://oc.app": "0oaCHAT",                    // cleartext key, or:
  "b5d4045c...e21:9f3a7c2e...": "0oaPAYROLL"       // "sha256(origin || salt):<salt>", hex
}
```

II knows the target origin from the ceremony; for each `hash:salt` key it computes
`sha256(origin || salt)` and matches it against the key's hash. The per-key salt prevents
bulk precomputation and cross-org correlation of the same origin. It does not hide an origin
an attacker already guesses — they can confirm a guess against the published salt — but
public-dapp origins are guessable anyway, so this protects the non-obvious internal ones,
which is sufficient. Cleartext and hashed keys may coexist in one object.

---

## 6. II core changes

No `.did` change to `openid_prepare_delegation`, no stable-memory migration, no change to
`StorableOpenIdCredentialKey`. The delegation seed gains an **optional second input** (the
per-app client) used only for gated logins — a normal login's seed, and thus its principal, is
byte-identical to today. Enforcement is the certified **`implicit:origin`** attribute, gated
via that session principal. `openid_prepare_delegation` is *not* the gate (it's II-internal
auth), and neither is the dapp's `f(account, origin)` delegation.

### 6.1 Identity: per-app logins resolve to the primary identity

A gated dapp's login runs against a per-app client, so the token's `aud` is that per-app
client_id. For *identity*, II resolves the anchor as if the login had used the **primary**
client — it looks up `(iss, stable_id, primary_client_id)`, substituting the primary client for
the token's `aud`. So all of an org's per-app clients collapse to one primary-client identity,
and a per-app login creates no credential and no access method. Existing single-client
credentials are already primary-keyed, so this is a lookup-time substitution — **no stored key
change, no migration.** (When `stable_identifier_claim != "sub"`, an auxiliary lookup bridges
the per-client `sub`; see §6.6.)

`wellknown(sso_domain)` is II core's in-heap cached copy (populated by `discover_sso`); a cold
miss returns `Pending` (re-drive discovery), never "no `app_clients`" (which would fail open).

### 6.2 The session principal carries the per-app client

`openid_prepare_delegation` reads the JWT's `aud`. If it's a **per-app** client (not the org's
primary), the session delegation is seeded from **both** clients:

```
gated login:   S = seed(iss, sub, primary_client, per_app_client, anchor)
normal login:  S = seed(iss, sub, primary_client, anchor)   // per-app input absent = today's seed
```

The per-app input affects only the *session* principal, never identity (the anchor is resolved
to primary, §6.1). II gets the per-app client from the JWT's own `aud` — no new argument to
`openid_prepare_delegation`, and a normal login's principal is unchanged.

### 6.3 Verifying the per-app proof — a general `check_authorization` primitive

The origin-scoped calls — `prepare`/`get_account_delegation` and `prepare_icrc3_attributes` —
gain an **optional `per_app_client` argument**. `check_authorization` recomputes the expected
principal and requires the caller to match:

```
expected = seed(iss, sub, primary_client, per_app_client?, anchor)   // one hash — O(1)
require caller == expected
```

No stored set, no scan. The caller **can't lie**: passing a `per_app_client` they didn't
authenticate with yields a different `expected` → mismatch → rejected. With the argument absent
it expects the plain primary session, so nothing changes for normal logins.

### 6.4 The gate: certify `implicit:origin` only for the origin's per-app client

In `prepare_icrc3_attributes`, for a **gated** origin (the login's `sso_domain` well-known
lists it in `app_clients`, honoring `gate_all_apps`), certify `implicit:origin` **only if** the
proven `per_app_client == app_clients[origin]`. A relying dapp requires the certified
`implicit:origin`; without the right per-app client it is never produced → access denied. This
is the whole enforcement, bound to the session principal (§6.3), not to an omittable hint.

### 6.5 Composition with `sso:<domain>` — assignment vs. domain

Two independent layers:
- **`implicit:origin` gate (II-side):** you used the origin's per-app client → per-app
  **assignment within a domain**.
- **certified `sso:<domain>` (dapp-side check):** which **domain** you came from — the
  discovery domain II actually resolved from, so it's unforgeable.

An insider via a rogue `evil.com` (pointing at the org's real IdP) is certifiably
`sso:evil.com`, so the dapp rejects — regardless of what `evil.com`'s `app_clients` claims.
Within the org's real domain, the gated origin's per-app client is only issued to assigned
users by the IdP. Together: **assigned user, from the expected org.**

### 6.6 Cross-client identity when the stable identifier isn't `sub`

The credential index is keyed on `sub`. This is **config-driven, not IdP-brand-driven** — II
reads `stable_identifier_claim`, it never detects "Entra":

- When `stable_identifier_claim == "sub"` (Okta org server, Google, OneLogin), the
  primary-client substitution (§6) resolves per-app logins directly — nothing extra.
- When it **isn't** `sub`, a per-app login's `sub` differs from the primary credential's and
  can't be matched by substituting the client alone. II doesn't index the alternate claim
  today, so it decodes the configured `stable_identifier_claim` and keeps a small **auxiliary
  lookup** from it to the primary credential — *additive, not a migration; no credential, seed,
  or key is mutated*:

```
(iss, <stable_identifier_claim>)  ->  the primary credential's (iss, sub)
// currently only Entra needs this: its sub is pairwise, so the stable id is oid
```

- **Populated at primary-client login** — the only login carrying both the alternate stable id
  and the primary `sub`. Existing users of such an org get their entry on their next normal SSO
  login; nothing stored is changed, an entry is only *added*.
- **Per-app logins resolve through it:** the per-app token gives `(iss, <stable id>)`, the aux
  lookup yields the primary `(iss, sub)`, and the existing credential index resolves the anchor.
- **A miss fails safe** — no anchor found, so the user completes a normal primary login first,
  rather than resolving to the wrong identity.

Cross-domain isolation does **not** depend on this lookup — it comes from the certified
`sso_domain` (§6, §7).

---

## 7. Why this is safe

1. **The per-app proof is cryptographic, not a hint.** The gated-login session principal is
   `seed(iss, sub, primary, per_app_client, anchor)`, and `check_authorization` recomputes it
   (§6.3). A caller can only present a `per_app_client` they actually authenticated with;
   omitting or faking it changes `expected` → mismatch → rejected. There is no ungated path to
   the same certified `implicit:origin`.
2. **`implicit:origin` is the gate, and a dapp can't be reached without it.** II certifies it
   for a gated origin only when the proven client equals `app_clients[origin]` (§6.4). A
   primary-client or wrong-client session never produces it, so a dapp that requires the
   certified origin attribute denies access.
3. **The certified `sso:<domain>` is unforgeable** — it is the discovery domain II actually
   resolved from, so a rogue-domain login is `sso:evil.com` and the dapp rejects, regardless of
   what that domain's `app_clients` claims (§6.5). `implicit:origin` gates *assignment within a
   domain*; `sso:<domain>` gates *which domain* — both are required.
4. **IdP assignment gates the per-app token.** Within the real domain, the IdP only issues a
   token for the origin's per-app client to assigned users; freshness of the whole chain is the
   session delegation's own expiry.
5. **Identity is unchanged and minimal.** The anchor is primary-keyed (per-app `aud` never
   enters identity), `StorableOpenIdCredentialKey` and stored data are untouched, and a normal
   login's principal is byte-identical to today. Direct providers (no `sso_domain`, no
   `app_clients`) are entirely unaffected.

---

## 8. IdP setup and sharp edges

**Per gated app, once:** register an OIDC client for the dapp, set its redirect URI to id.ai
(multiple clients may share the same id.ai redirect — the `aud` distinguishes them), assign
the group, and add `origin -> client_id` to the well-known.

| IdP | Per-app assignment | Note |
| --- | --- | --- |
| Okta | Native; free. Unassigned user blocked at `/authorize`. | Denial is an HTML 400 page, not an OIDC error redirect — II infers denial from the failed ceremony. |
| Entra ID | Native, via "Assignment required" + user/group assignment. | **Defaults to OFF** — a forgotten toggle silently fail-opens to the whole tenant. Groups need P1/P2. **`sub` is pairwise (per client), so set `stable_identifier_claim: "oid"`** (§5, §6). |
| OneLogin | Native, via Roles, enforced at sign-in. | — |
| Google Workspace | **Not supported** — the user-access toggle is SAML-only and OAuth clients live in the GCP console; per-OIDC-client group assignment cannot be expressed. |

Recurring grant is then the single most familiar IdP action: open the app, assign the group.

---

## 9. Relationship to the id.ai-side layer

The two layers are independent and composable; each origin's gate is chosen in the
well-known:

| | IdP-side (this doc) | id.ai-side (companion doc) |
| --- | --- | --- |
| Where the gate runs | The IdP (`/authorize`) | The access canister (mint time) |
| id.ai infrastructure | None | Access canister + proxy + panel |
| Policy authoring / audit | In the IdP | In the id.ai admin panel |
| IdP coverage | Okta, Entra, OneLogin | + Google (manual/attribute groups) |
| Per-app conditions (MFA/device) | Yes, native | No |
| Setup cost per app | Register + map a client | Add a policy row |

Selection per origin:

- Origin in `app_clients` -> **IdP-side gated** (this layer).
- Origin in the access-canister policy -> **id.ai-side gated** (companion layer).

An org may use either, or both for different apps. Both layers share the well-known and the
existing SSO ceremony; this layer adds only the `aud` gate and primary-keyed anchor
resolution in II core (§6).

---

## 10. Implementation

One PR — a single cohesive feature. **No stable-memory migration, no `StorableOpenIdCredentialKey`
change, no `.did` change to `openid_prepare_delegation`.** The seed gains an optional per-app
input (normal logins byte-identical). Inert until an org adds `app_clients` to its well-known.

> **Security invariant:** enforcement is the **certified `implicit:origin`**, gated on the
> session-proven per-app client (§6.4), **plus** the dapp verifying the certified
> **`sso:<domain>`** (§6.5). The `aud` check at *login* is not the boundary. Both must hold:
> II won't certify `implicit:origin` for a gated origin without the matching per-app-client
> session, and a gated dapp must check `sso:<domain>` (existing lib — no new work).

Contents:
- **Config (§5):** parse `app_clients` / `gate_all_apps` / `stable_identifier_claim`;
  `get_sso_discovery(domain, opt origin)` returns the per-origin `resolved_client_id`.
- **Session principal (§6.2):** `openid_prepare_delegation` seeds the delegation with the
  optional per-app client (taken from the JWT's `aud`) alongside the primary; identity/anchor
  still resolves to primary (`aud` → primary, §6.1), with the aux lookup when
  `stable_identifier_claim != "sub"` (§6.6). No stored-key change, no migration.
- **Auth primitive (§6.3):** `check_authorization` takes an optional `per_app_client` and
  requires `caller == seed(iss, sub, primary, per_app_client?, anchor)`. The origin-scoped
  calls — `prepare`/`get_account_delegation`, `prepare_icrc3_attributes` — gain that arg.
- **The gate (§6.4):** `prepare_icrc3_attributes` certifies `implicit:origin` for a gated
  origin only if the proven `per_app_client == app_clients[origin]` (honoring `gate_all_apps`);
  cold cache → `Pending`.
- **Frontend:** route the gated-dapp ceremony to `resolved_client_id`, and pass the per-app
  client on the origin-scoped calls.
- **Tests:** gated == ungated → same dapp principal (linchpin); a primary/wrong-client session
  does **not** get `implicit:origin` certified for a gated origin; `gate_all_apps` default-deny;
  hashed keys; Entra `oid` identity + first-gated-login-needs-normal-first; direct providers
  unaffected.

`.did` deltas are additive only: `get_sso_discovery` origin arg + `resolved_client_id`, and an
optional `per_app_client` on the origin-scoped calls. No change to the delegation methods.

No canister beyond II core, no proxy, no panel, no migration, no new dapp lib. Admin/dapp docs
tracked separately.

---

## 11. References

- Existing II SSO discovery: `src/frontend/src/lib/utils/ssoDiscovery.ts`,
  `src/internet_identity/src/openid/`.
- Credential key and delegation seed: `src/internet_identity/src/openid.rs`
  (`OpenIdCredentialKey`, `calculate_delegation_seed`).
- Dapp-principal derivation: `src/internet_identity/src/delegation.rs`
  (`calculate_anchor_seed`).
- Companion design: `enterprise-sso-per-app-access-control.md` (id.ai-side gating).
