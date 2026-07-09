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
    participant Web as org.com web root
    participant IdP as org's IdP
    U->>FE: sign in to gated dapp (payroll.com)
    FE->>II: discover_sso(org.com)
    II->>Web: GET /.well-known/ii-openid-configuration
    Web-->>II: config incl. app_clients (cached by II)
    FE->>II: get_sso_discovery(org.com, payroll.com)
    II-->>FE: client_P + endpoints
    FE->>IdP: authorize(client_P, nonce)
    Note over IdP: IdP evaluates app-assignment for client_P
    alt user assigned to client_P
        IdP-->>FE: id_token (aud = client_P)
        FE->>II: openid_prepare_delegation(jwt, origin = payroll.com)
        II->>II: gate: check aud == declared client for payroll.com
        II->>II: identity: resolve anchor via the primary client credential (aud ignored)
        II-->>FE: delegation for (anchor, payroll.com)
    else user not assigned
        IdP-->>FE: access_denied
        FE-->>U: no access to this app
    end
```

The frontend never reads the org's web root. As today, II core fetches and caches the
well-known (the existing `discover_sso` update outcall); the frontend reads the resolved
config — here extended so `get_sso_discovery` returns the client for the target origin from
`app_clients` — via a query.

Two responsibilities split cleanly:

- **Gate = the IdP.** App-assignment decides who gets a token for `client_P`. II does not
  hold or evaluate any policy.
- **Binding = II.** II ensures the token is actually for *this* origin's client (`aud`
  check), then resolves identity. The `aud` check is what makes the IdP's decision
  origin-specific and non-transferable.

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

The OpenID delegation path gains two things; nothing else. **No public API change, no
stable-memory migration, no change to `StorableOpenIdCredentialKey`, and the delegation seed is
untouched** — the security property lives in the *certified `sso_domain`*, checked at
enforcement, not in the anchor key.

**1. Per-app logins resolve to the primary identity.** A gated dapp's login runs against a
per-app client, so the token's `aud` is that per-app client_id. II uses `aud` only for the
gate; for *identity* it resolves the anchor as if the login had used the **primary** client —
it looks up `(iss, stable_id, primary_client_id)`, substituting the primary client for the
token's `aud`. So all of an org's per-app clients collapse to the one primary-client identity,
and a per-app login creates no credential and no access method. Existing single-client
credentials are already keyed on the primary client, so this is a **lookup-time substitution —
no stored key change, no migration.**

`wellknown(sso_domain)` below is II core's **in-heap** cached copy of the org's well-known
(populated by `discover_sso`), not a fresh fetch. The cache is transient — empty after an
upgrade and evictable — so a miss must return `Pending` (re-drive discovery), never be read as
"this origin has no per-app client." Only a *loaded* config with the origin absent is
"unlisted."

```
fn resolve_and_gate(jwt, origin, sso_domain) -> Result<Anchor> {
    let claims = verify_id_token(jwt);              // iss, sub, aud, nonce, exp, JWKS — unchanged
    let wk = match wellknown(sso_domain) {          // II core's IN-HEAP discovery cache
        Cached(c) => c,
        Cold      => return Pending,                // cold/post-upgrade/evicted: re-drive discover_sso
    };                                              // NEVER read a cold cache as "no app_clients" (would fail open)

    // gate: the token must be for THIS origin's declared client
    let expected = match wk.client_for(origin) {    // app_clients lookup (cleartext or hashed, §5.1)
        Some(c) => c,                               // listed origin -> its per-app client
        None if wk.gate_all_apps => return Deny,    // unlisted + default-deny -> no access
        None => wk.client_id,                       // unlisted -> primary client (open)
    };
    if claims.aud != expected { return Deny; }      // wrong app / no-fallback

    // identity: resolve on the PRIMARY client. aud is gate-only; sso_domain is NOT in the key.
    require(claims.aud in wk.declared_clients());   // token from a client the org vouches for
    let stable_id = claims[wk.stable_identifier_claim];   // "sub" by default; "oid" on Entra
    lookup_or_create_anchor((claims.iss, stable_id, wk.client_id))
        // per-app aud -> primary, so per-app clients share one identity.
        // sub: existing index; oid (Entra): via the aux index (§6.1).
}

// client_for resolves the per-app client for an origin, over app_clients keys (§5):
//   cleartext key -> app_clients[origin]
//   "hash:salt" key -> the key where sha256(origin || salt) == hash
```

**Anti-rogue-domain: the certified `sso_domain`, not the anchor.** The security property — a
login via an attacker's discovery domain must not act as the org's identity — is enforced on
the **certified `sso_domain`**, which II stamps from the discovery domain it actually resolved
the config from. That value is verified and **unforgeable**: a caller cannot claim `acme.com`
while using `evil.com`'s well-known, because `sso_domain` *is* the domain II fetched from. A
gated dapp trusts a specific org domain and checks the certified `sso_domain` (II already
labels SSO credentials `sso:<domain>` and surfaces it via the verification lib); an `evil.com`
login is certifiably `sso:evil.com` and is rejected. The anchor may *collide* across domains
(same `(iss, stable_id, primary)`) — but that is harmless: it is the same human, and access is
decided on the certified domain, not the raw principal. So `sso_domain` stays **out** of the
anchor key and the seed, and there is no re-key migration.

- **One access method.** Identity keys on `(iss, stable_id, primary_client_id)` — the same key
  an ordinary org SSO login already produces. Exactly one OpenID credential per user; per-app
  clients never appear in credential state.
- **The identifier must be cross-client stable.** Canonicalizing per-app logins to the primary
  identity only works if the identifier is the same across the org's clients. `sub` is on Okta
  (org authorization server), Google, and OneLogin; on **Entra `sub` is pairwise** (per
  client), so Entra orgs set `stable_identifier_claim: "oid"` (§5, §6.1).
- **Per-app tokens are read-only for identity.** A per-app login gates and resolves the anchor
  but does **not** write profile metadata (email, name); that always comes from a primary-client
  login. The dapp-facing principal is `f(anchor, origin)` regardless
  (`delegation.rs::calculate_anchor_seed`), so every gated app sees the same identity.

### 6.1 Cross-client identity when the stable identifier isn't `sub`

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

1. **The certified `sso_domain` is unforgeable.** It is the discovery domain II actually
   resolved the config from — not anything the caller supplies or the token carries. A login
   via `evil.com` is certifiably `sso:evil.com`; it cannot present as `sso:acme.com`.
2. **Enforcement is on the certified domain, not the principal.** A gated dapp trusts a
   specific org domain and checks the certified `sso_domain`. So even though a rogue-domain
   login may resolve to the *same anchor* as the real user (same human — same
   `(iss, stable_id, primary)`), it carries the wrong certified domain and is rejected. The
   anchor collision is irrelevant to the access decision, which is why `sso_domain` need not be
   in the key.
3. **Assignment is enforced by the IdP.** Within the org's real domain, reaching a gated origin
   requires a token for that origin's per-app client (`aud` check, no fallback), which the IdP
   only issues to assigned users. An unassigned insider can't get that token via the org's real
   well-known — and via a rogue domain they fail the certified-domain check (rule 2).
4. **Declared clients only.** A per-app token resolves to identity only if its `aud` is a
   client the org's well-known declares; direct providers (no `sso_domain`, no `app_clients`)
   keep full `(iss, sub, aud)` isolation, so a stray OAuth client at a shared issuer can't
   resolve onto someone's anchor.

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

## 10. Build order (stacked PRs)

Each PR stacks on the previous. There is **no stable-memory migration** — identity keying,
the delegation seed, and the public API are all unchanged (§6).

> **Security invariant:** the `aud` gate alone does **not** stop a rogue discovery domain (the
> attacker controls their own well-known). A gated dapp's isolation comes from verifying the
> **certified `sso_domain`** (§7) — which dapps already do via the existing `sso:<domain>`
> certified-attribute lib. So no new lib work is required; the requirement is that a gated dapp
> actually checks `sso:<domain>`, not the `aud` gate on its own.

**PR 1 — BE plumbing: config parsing + `oid` capture + aux index.** No enforcement; invisible.
- Parse `app_clients` / `gate_all_apps` / `stable_identifier_claim` into the cached discovery
  config (§5).
- Decode `oid`; keep the additive `(iss, oid) -> (iss, sub)` aux index for Entra, populated at
  login (§6.1). Shipping first lets it pre-populate from normal Entra logins, so entries exist
  before gating turns on. Additive only — no migration.

**PR 2 — Gate + routing + identity (enforcement turns on).**
- `get_sso_discovery` resolves the per-origin client; the frontend routes the ceremony to it.
- BE gate: `aud == declared-client-for-origin`, no fallback; identity resolved on the primary
  client (`aud` -> primary); cold cache -> `Pending` (§6). No stored-key change, no seed change.
- The certified `sso_domain` is already surfaced per login via the existing `sso:<domain>`
  attribute path — no change needed here.
- Validate: an assigned user reaches the gated dapp with the same identity (and single access
  method) as their default SSO; an unassigned user is denied at the IdP; a token for one gated
  app can't open another; **a rogue-domain login is certifiably a different `sso_domain`**.

**PR 3 — Onboarding.** Per-IdP client setup, and guidance that a gated dapp must verify
`sso:<domain>` with the existing certified-attribute lib (the rogue-domain defense, §7 — no new
lib work). Covers Entra `stable_identifier_claim: "oid"` + assignment-required and the Okta
400-denial (§8).

No canister beyond II core, no proxy, no panel, no migration, no new dapp lib.

---

## 11. References

- Existing II SSO discovery: `src/frontend/src/lib/utils/ssoDiscovery.ts`,
  `src/internet_identity/src/openid/`.
- Credential key and delegation seed: `src/internet_identity/src/openid.rs`
  (`OpenIdCredentialKey`, `calculate_delegation_seed`).
- Dapp-principal derivation: `src/internet_identity/src/delegation.rs`
  (`calculate_anchor_seed`).
- Companion design: `enterprise-sso-per-app-access-control.md` (id.ai-side gating).
