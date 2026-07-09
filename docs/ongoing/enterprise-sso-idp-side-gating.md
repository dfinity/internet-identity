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

**In-scope behavior change (deliberate).** `sso:<domain>:…` attributes become certifiable
**only from an SSO session** (`sso_prepare_delegation`), not from a
passkey/`openid_prepare_delegation` session on an anchor that merely has an SSO access method
(§6.3). This tightens the existing `sso:<domain>` flow (today cert reads the stored credential
in any session), so it affects current SSO-attribute consumers, not just the new gate — a
conscious part of this design. (`implicit:origin` is unchanged — it stays certified for every
flow as the general origin binding; the gate rides on `sso:<domain>`, §6.4.)

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
        FE->>II: sso_prepare_delegation(jwt, org.com, payroll.com)
        Note over II: GATE: aud == app_clients[payroll]? if not, refuse (no delegation)
        Note over II: anchor resolves to primary; seed = (iss, sub, org.com, payroll, anchor)
        II-->>FE: SSO delegation (bound to org.com + payroll)
        FE->>II: get_account_delegation(anchor, payroll.com)
        II-->>FE: delegation for f(account, payroll.com)
        FE->>II: prepare_icrc3_attributes(anchor, payroll.com)
        Note over II: caller == seed_sso(org.com, payroll)? then certify sso:org.com (implicit:origin as usual)
        II-->>FE: certified attributes
        FE->>Dapp: delegation + certified attributes
        Note over Dapp: require certified sso:org.com, then grant
    end
```

II core (not the FE) fetches and caches the well-known via `discover_sso`; the FE reads the
per-origin client with `get_sso_discovery(domain, origin)`. Three responsibilities:

- **IdP — assignment.** App-assignment decides who can obtain a token for `client_P`. II holds
  no policy.
- **II — the gate is mint-or-refuse.** A dedicated `sso_prepare_delegation` (separate from the
  untouched `openid_prepare_delegation`) mints an SSO session **only if** the JWT's `aud`
  matches the origin's declared client; the delegation's seed encodes `(sso_domain, origin)`.
  The origin-scoped calls then require that principal — so if the gate refused to mint, there
  is no session to authenticate with (§6). Identity still resolves to the primary client.
- **Dapp — verify.** The dapp checks the certified `sso:<domain>` (which org) and relies on the
  certified `sso:<domain>` (gated access), which is only produced from an SSO session (§6.3),
  so a passkey login on the same anchor shares no SSO attributes.

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

A dedicated `sso_prepare_delegation` / `sso_get_delegation` pair carries the SSO sign-in.
`openid_prepare_delegation` is **left unchanged** (direct providers, existing flows). No
stable-memory migration and no change to `StorableOpenIdCredentialKey`. The enforcement is
simply **whether the SSO delegation was minted**: its seed encodes `(sso_domain, origin)`, and
the origin-scoped calls require that principal, so a refused gate leaves nothing to
authenticate with.

### 6.1 Identity: the SSO session resolves to the primary identity

A gated dapp's login runs against a per-app client, so the token's `aud` is that per-app
client_id. For *identity*, `sso_prepare_delegation` resolves the anchor as if the login had
used the **primary** client — it looks up `(iss, stable_id, primary_client_id)`, substituting
the primary client for the token's `aud`. So all of an org's per-app clients collapse to one
primary-client identity, and an SSO login creates no extra credential and no access method.
Existing single-client credentials are already primary-keyed, so this is a lookup-time
substitution — **no stored key change, no migration.** (When `stable_identifier_claim != "sub"`,
an auxiliary lookup bridges the per-client `sub`; see §6.5.)

`wellknown(sso_domain)` is II core's in-heap cached copy (populated by `discover_sso`); a cold
miss returns `Pending` (re-drive discovery), never "no `app_clients`" (which would fail open).

### 6.2 `sso_prepare_delegation` — the gate is mint-or-refuse

`sso_prepare_delegation(jwt, discovery_domain, origin)`:

```
verify_id_token(jwt)                                  // iss/aud/nonce/exp/JWKS
expected = app_clients[origin]        if origin listed          // gated
         | primary_client             if unlisted and !gate_all_apps
         | DENY                        if unlisted and gate_all_apps
require jwt.aud == expected            else DENY (mint nothing)  // THE GATE
anchor = resolve_to_primary(iss, stable_id)           // §6.1
mint delegation seeded  seed(iss, sub, sso_domain, origin, anchor)
```

- The **one** `app_clients` / hashed-key (§5.1) / `gate_all_apps` lookup happens here, where
  discovery already has the well-known cached.
- The seed binds the session to `(sso_domain, origin)`. If the gate fails, **no delegation is
  minted** — there is no principal to make the follow-up calls with.
- This is the SSO path for **every** dapp sign-in, gated or not (gating only decides whether
  `expected` is a per-app client or the primary). `openid_prepare_delegation` is untouched.

### 6.3 Certification requires the SSO-session principal

The origin-scoped calls — `get_account_delegation` and `prepare_icrc3_attributes` — already
carry the `origin`. `check_authorization` recomputes the SSO-session principal and requires the
caller to match:

```
require caller == seed(iss, sub, sso_domain, origin, anchor)   // one hash — O(1), no config
```

- **Config-free at cert.** No `app_clients`, no client-matching, no hashed-key math — the gate
  already happened at mint time (§6.2), so a matching principal *exists only if* it passed.
- **SSO attributes require an SSO sign-in.** `sso:<domain>` (and any `sso:<domain>:…`) are
  certified only for this principal. A **passkey** login — or `openid_prepare_delegation` —
  produces a different principal, so an anchor that merely *has* an SSO access method shares
  **no** SSO attributes unless the user actually signed in through SSO this session. (This
  tightens today's behavior; see §2.) `implicit:origin` is *not* gated — it stays certified for
  every flow (direct providers, passkey) as the general origin binding; the gate rides on
  `sso:<domain>` (§6.4).
- **Own freshness.** The SSO delegation carries its own (tighter) expiry, so SSO attributes
  reflect a *recent* SSO ceremony, independent of the general/passkey session lifetime.

### 6.4 The gate is the origin-bound `sso:<domain>`

The dapp's boundary is a single certified attribute: **`sso:<domain>`**, and its *presence in a
message for origin X* proves both things at once, because it is only certified for the
SSO-session principal (§6.3), and that session:
- **exists only if the per-app gate passed** — `sso_prepare_delegation` mints it only when
  `aud == app_clients[X]` (assignment *within* the domain, §6.2), and
- **carries the domain II actually resolved from** — `sso:evil.com` for a rogue login, never
  `sso:acme.com` (which *domain*, unforgeable).

So a gated dapp that requires `sso:acme.com` gets "assigned user, from the expected org" from
that one check. An insider via a rogue `evil.com` (pointing at the org's real IdP) is certifiably
`sso:evil.com` → rejected, regardless of `evil.com`'s `app_clients`; a non-assigned user can't
mint the SSO session for X at all. `implicit:origin` is orthogonal — the usual anti-replay
origin binding, certified for every flow, not the gate.

> **Bounded reconfiguration window.** The seed binds the origin, not the client. So an SSO
> delegation minted while an app was *ungated* still matches after the app is flipped to gated,
> until it expires (~session) and the discovery cache refreshes (~1 h, §5). This is the same
> propagation class we already accept for `app_clients` edits; eliminable only by also seeding
> the client (at the cost of a config read back at cert). We accept the window.

### 6.5 Cross-client identity when the stable identifier isn't `sub`

The credential index is keyed on `sub`. This is **config-driven, not IdP-brand-driven** — II
reads `stable_identifier_claim`, it never detects "Entra":

- When `stable_identifier_claim == "sub"` (Okta org server, Google, OneLogin), the
  primary-client substitution (§6.1) resolves the SSO login directly — nothing extra.
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

1. **The gate is mint-or-refuse, and the proof is the session principal.** `sso_prepare_delegation`
   mints the SSO delegation only if `jwt.aud == app_clients[origin]` (§6.2), and its seed binds
   `(sso_domain, origin)`. The origin-scoped calls recompute that principal (§6.3), so a caller
   can't fake or omit anything — a wrong/absent SSO session simply doesn't match. There is no
   ungated path to the certified `sso:<domain>` for this origin.
2. **SSO attributes only exist for an SSO sign-in.** `sso:<domain>` (and `sso:<domain>:…`) are
   certified only for the SSO-session principal, so a passkey login (or
   `openid_prepare_delegation`) on an anchor that merely *has* an SSO access method shares none
   of them. The SSO delegation's own (tighter) expiry keeps them fresh.
3. **The origin-bound `sso:<domain>` is the whole gate.** Its presence for origin X proves both
   *which domain* (it is the discovery domain II resolved from — a rogue login is `sso:evil.com`,
   never `sso:acme.com`) and *assignment within it* (the SSO session was minted only if
   `aud == app_clients[X]`, §6.2). So a gated dapp requiring `sso:acme.com` gets both from one
   check; `implicit:origin` is orthogonal anti-replay, not the gate (§6.4).
4. **IdP assignment gates the per-app token.** Within the real domain, the IdP only issues a
   token for the origin's per-app client to assigned users, so `sso_prepare_delegation` can only
   mint an origin-bound session for them.
5. **Identity is unchanged and minimal.** The anchor is primary-keyed (the per-app `aud` never
   enters identity), `StorableOpenIdCredentialKey` and stored data are untouched, and
   `openid_prepare_delegation` / direct providers are entirely unaffected.
6. **Bounded reconfiguration window** (§6.4): flipping an app ungated→gated leaves already-minted
   SSO delegations valid until they expire; accepted, same class as `app_clients` propagation.

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
change, `openid_prepare_delegation` untouched.** Inert until an org adds `app_clients` to its
well-known.

> **Security invariant:** the gate is **mint-or-refuse at `sso_prepare_delegation`** (§6.2) — a
> refused gate mints no SSO delegation, so the origin-scoped calls have no principal to
> authenticate with. Certification is a config-free check that the caller *is* that SSO-session
> principal (§6.3). SSO attributes (`sso:<domain>`, `sso:<domain>:…`) are therefore only
> ever produced from an SSO sign-in, and the dapp additionally verifies `sso:<domain>` for the
> domain (existing lib — no new work).

Contents:
- **Config (§5):** parse `app_clients` / `gate_all_apps` / `stable_identifier_claim`;
  `get_sso_discovery(domain, opt origin)` returns the per-origin `resolved_client_id` for FE
  routing.
- **`sso_prepare_delegation` / `sso_get_delegation` (new, §6.2):** verify the JWT, enforce the
  gate (`aud == app_clients[origin]` / primary / DENY), resolve the anchor to primary (§6.1,
  aux lookup when `stable_identifier_claim != "sub"`, §6.5), and mint a delegation seeded
  `(iss, sub, sso_domain, origin, anchor)`. `openid_prepare_delegation` is left as-is.
- **Certification (§6.3):** `check_authorization` on the origin-scoped calls —
  `get_account_delegation`, `prepare_icrc3_attributes` — requires
  `caller == seed(iss, sub, sso_domain, origin, anchor)`. No `app_clients`/config at cert. SSO
  attributes gated on this principal (so passkey sessions get none); SSO delegation carries its
  own tighter expiry.
- **Frontend:** route the dapp SSO ceremony to `resolved_client_id`; sign in via
  `sso_prepare_delegation`; handle the first-gated-login-before-normal case for non-`sub` IdPs
  (§6.1/§6.5) with a "sign in normally first" prompt.
- **Tests:** gated == ungated → same dapp principal (linchpin); a passkey / non-SSO session
  gets **no** SSO attributes; a refused gate mints no SSO delegation → cert unreachable;
  `gate_all_apps` default-deny; hashed keys; Entra `oid` identity + first-gated-login flow;
  `openid_prepare_delegation` / direct providers unchanged.

`.did` deltas are additive only: **add** `sso_prepare_delegation` / `sso_get_delegation`, and
`get_sso_discovery` gains the `opt origin` arg + `resolved_client_id`. No change to
`openid_prepare_delegation` / `openid_get_delegation`.

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
