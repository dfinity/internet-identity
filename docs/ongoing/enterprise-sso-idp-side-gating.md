# IdP-side per-app gating for enterprise SSO

**Status:** Implemented in `dfinity/internet-identity` PR #4096 (single PR). This doc is the spec.
**Last updated:** 2026-07-15
**Companion:** `enterprise-sso-per-app-access-control.md` specifies the *id.ai-side* gate
(directory + access canister). This document specifies an **independent, complementary
layer** that gates entirely on the **IdP side**. The two are composable and selected per app
(§9); neither depends on the other.

**Implementation status.** Implemented in PR #4096. The one deviation from the "zero
stable-memory changes" goal is the non-`sub` aux bridge (§6.5), which is a **new additive**
stable structure (a fresh memory region) — additive, not a migration; existing structures and
`StorableOpenIdCredentialKey` are unchanged.

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
**only when a certified SSO bundle is presented** (the bundle is issued by
`sso_prepare_delegation`, §6.3), not from a passkey/`openid_prepare_delegation` session on an
anchor that merely has an SSO access method. This tightens the existing `sso:<domain>` flow
(today cert reads the stored credential in any session), so it affects current SSO-attribute
consumers, not just the new gate — a conscious part of this design. (`implicit:origin` is
unchanged — it stays certified for every flow as the general origin binding; the gate rides on
`sso:<domain>`, §6.4.)

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
    participant Dapp as Gated dapp payroll.com
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
        Note over II: GATE: aud == app_clients[payroll]? if not, refuse (mint nothing)
        Note over II: anchor resolves to primary; mint a plain openid delegation
        II-->>FE: delegation + certified bundle (sso_domain=org.com, origin=payroll.com, expiry)
        FE->>II: get_account_delegation(anchor, payroll.com)
        Note over II: permissionless: derives f(anchor, payroll.com), no SSO check
        II-->>FE: delegation for f(account, payroll.com)
        FE->>II: prepare_icrc3_attributes(payroll.com) + sender_info bundle
        Note over II: read certified bundle; origin matches? then certify sso:org.com
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
  untouched `openid_prepare_delegation`) runs the gate: it mints a delegation **only if** the
  JWT's `aud` matches the origin's declared client. The minted delegation is an **ordinary
  openid-credential delegation** (identity resolves to the primary client); alongside it,
  `sso_prepare_delegation` returns a small **II-canister-signed bundle** carrying
  `(sso_domain, origin, expiry)`. There is no special SSO-session principal — account and
  delegation methods are permissionless. The gate rides on that certified bundle (§6).
- **Dapp — verify.** The dapp requires the certified `sso:<domain>`, which proves both *which
  org domain* the login came through and that the per-app gate passed. It is certified only
  when a matching bundle is presented (§6.3), so a passkey login on the same anchor shares no
  SSO attributes.

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
stable-memory migration and no change to `StorableOpenIdCredentialKey`. The SSO sign-in mints
an **ordinary openid-credential delegation** — identity resolves to the org's primary client
(§6.1) — so the session principal is a normal credential principal that the existing
`check_authorization` accepts, and account/delegation methods stay **permissionless**. The
enforcement instead rides on a separate, certified **`sender_info` bundle** that
`sso_prepare_delegation` returns and the attribute-certification methods read (§6.3): a refused
gate returns no bundle, so the certified `sso:<domain>` can never be produced for that origin.

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

**Registration analogue.** A user's *first* login can be a gated one (a new employee whose first
II touch is a gated dapp). Registration (`OpenIDRegFinishArg` gains `origin`) runs the **same
gate** as `sso_prepare_delegation` and, on pass, stores a **primary-client-keyed** credential via
the same stable-sub substitution — so a first gated login for a `sub` org registers directly, in
one IdP trip, never creating a per-app credential. For a non-`sub` org the per-app token's
pairwise `sub` can't be bridged yet, so registration **fails safe** (creates nothing, returns
`IdRegFinishError::SsoNormalLoginRequired`); the frontend then guides a normal primary-client
sign-in first (§6.5), after which the gated login registers. `IdRegFinishError::SsoNormalLoginRequired`
is the one deliberate non-additive `.did` change — inert for existing clients, which never send
`origin` and so never reach the path that returns it.

### 6.2 `sso_prepare_delegation` — the gate is mint-or-refuse

`sso_prepare_delegation(jwt, salt, session_key, discovery_domain, origin)`:

```
verify_id_token(jwt)                                  // iss/aud/nonce/exp/JWKS
expected = app_clients[origin]        if origin listed          // gated
         | primary_client             if unlisted and !gate_all_apps
         | DENY                        if unlisted and gate_all_apps
require jwt.aud == expected            else DENY (mint nothing)  // THE GATE
anchor = resolve_to_primary(iss, stable_id)           // §6.1
mint a plain openid-credential delegation (primary-keyed)
bundle = sign (sso_domain, origin, now()+SSO_SESSION_DURATION) under the credential seed
return { delegation, sso_attr_bundle: bundle, sso_attr_bundle_signature }
```

- The **one** `app_clients` / hashed-key (§5.1) / `gate_all_apps` lookup happens here, where
  discovery already has the well-known cached.
- The minted delegation is an **ordinary** primary-keyed openid delegation — no SSO-specific
  seed. The gating context travels separately, in the certified `sso_attr_bundle` (§6.3). If the
  gate fails, **nothing is minted and no bundle is returned**, so the certified `sso:<domain>`
  can never be produced for that origin.
- This is the SSO path for **every** dapp sign-in, gated or not (gating only decides whether
  `expected` is a per-app client or the primary). `openid_prepare_delegation` is untouched.

### 6.3 The gating context is a certified `sender_info` bundle

The session is an ordinary credential principal (§6.1), so the gate can't ride on *who* the
caller is. It rides on a separate, certified statement the caller carries: the **`sso_attr_bundle`**
— `(sso_domain, origin, expiry)`, signed by II as a **canister signature under the credential
seed** (the same seed backing the SSO delegation) and returned by `sso_prepare_delegation` /
`sso_get_delegation`.

- **How it travels.** The frontend attaches the bundle to signed requests via the SDK's
  `AttributesIdentity` (`@icp-sdk/core`): it wraps the delegation identity with
  `attributes = { data: bundle, signature }` and `signer = { canisterId: <II> }`, which injects a
  `sender_info` field into the signed request content.
- **How the canister reads it.** `read_certified_sso_bundle` reads it back through raw `ic0`
  (`msg_caller_info_data()` / `msg_caller_info_signer()` — the `ic-sender-info` mechanism). The
  **replica** verifies the `sender_info` signature under the **caller's own credential seed**, so
  a bundle is cryptographically bound to the identity that presents it and **cannot be replayed
  across identities**. II additionally requires the signer to be itself, caps the bundle at 4 KiB,
  and rejects an expired one.
- **Account/delegation methods stay permissionless.** They derive `f(anchor, origin)` — never
  SSO-specific — so they read no bundle and do no SSO check; only the attribute path does. This is
  a smaller change than the old model (no OR-branch on account/delegation methods, no SSO-session
  principal to recompute) and needs no config at read time — the gate already happened at mint
  (§6.2), so a valid bundle *exists only if* it passed.
- **Only the attribute-certification path reads the bundle.** `prepare_icrc3_attributes` and
  `list_available_attributes` call `read_certified_sso_bundle` to decide whether to certify /
  list the `sso:<domain>` rows (§6.4). `identity_info` is untouched (anchor-level, not
  origin-scoped; the consent path simply skips it for an SSO session).
- **SSO attributes require an SSO sign-in.** The bundle exists only from `sso_prepare_delegation`,
  so a **passkey** login (or `openid_prepare_delegation`) carries none — an anchor that merely
  *has* an SSO access method shares **no** SSO attributes unless the user actually signed in
  through SSO this session. (This tightens today's behavior; see §2.) The bundle's own `expiry`
  keeps SSO attributes fresh, independent of the general/passkey session lifetime.
- **Why raw `ic0 = "1.1"`.** The `msg_caller_info_*` accessors aren't in `ic-cdk 0.16`; importing
  them from raw `ic0` avoids an `ic-cdk 0.20` bump that conflicts with the transitive vc-sdk git
  deps. No `ic-cdk` bump.

### 6.4 The gate is the origin-bound `sso:<domain>`

The dapp's boundary is a single certified attribute: **`sso:<domain>`**, and its *presence in a
message for origin X* proves both things at once, because it is certified only when a valid
bundle is presented (§6.3), and that bundle:
- **exists only if the per-app gate passed** — `sso_prepare_delegation` issues it only when
  `aud == app_clients[X]` (assignment *within* the domain, §6.2), and
- **carries the domain II actually resolved from** — `sso:evil.com` for a rogue login, never
  `sso:acme.com` (which *domain*, unforgeable).

Concretely, `prepare_icrc3_attributes` reads the certified bundle and certifies `sso:<domain>`
only when the bundle's `origin` matches the origin it is certifying for; `list_available_attributes`
reads the bundle and offers the same rows in the consent listing. The bundle carries the origin,
so neither needs the caller to assert it. A passkey / `openid_prepare_delegation` session carries
no bundle → no SSO rows.

So a gated dapp that requires `sso:acme.com` gets "assigned user, from the expected org" from
that one check. An insider via a rogue `evil.com` (pointing at the org's real IdP) is certifiably
`sso:evil.com` → rejected, regardless of `evil.com`'s `app_clients`; a non-assigned user can't
obtain a bundle for X at all. `implicit:origin` is orthogonal — the usual anti-replay origin
binding, certified for every flow, not the gate.

> **Bounded reconfiguration window.** The bundle binds the origin, not the client. So a bundle
> minted while an app was *ungated* still certifies `sso:<domain>` after the app is flipped to
> gated, until the bundle expires (~session) and the discovery cache refreshes (~1 h, §5). This
> is the same propagation class we already accept for `app_clients` edits; eliminable only by also
> binding the client (at the cost of a config read back at cert). We accept the window.

### 6.5 Cross-client identity when the stable identifier isn't `sub`

The credential index is keyed on `sub`. This is **config-driven, not IdP-brand-driven** — II
reads `stable_identifier_claim`, it never detects "Entra":

- When `stable_identifier_claim == "sub"` (Okta org server, Google, OneLogin), the
  primary-client substitution (§6.1) resolves the SSO login directly — nothing extra.
- When it **isn't** `sub`, a per-app login's `sub` differs from the primary credential's and
  can't be matched by substituting the client alone. II doesn't index the alternate claim
  today, so it decodes the configured `stable_identifier_claim` and keeps a small **auxiliary
  bridge** from it to the primary credential's `sub`. This is a **new, additive stable
  structure** (its own memory region) — no existing credential, seed, key, or stable layout is
  mutated, so it is additive, *not* a migration:

```
(iss, primary_client_id, <stable_identifier_claim>)  ->  the primary credential's sub
// currently only Entra needs this: its sub is pairwise, so the stable id is oid
```

- **Why all three key components.** The bridged `sub` is **pairwise per client** on a non-`sub`
  IdP, so the key must name the client it belongs to. Keying on `(iss, stable_id)` alone would
  (a) collide when one tenant is exposed through two discovery domains with different primary
  clients — same `(iss, oid)`, two different primary `sub`s, last-writer-wins → wrong resolution
  — and (b) rest the entire cross-org boundary on `iss` being tenant-unique. Adding
  `primary_client_id` scopes the bridge per primary client and removes that single point of
  failure. Within a tenant, `iss` is tenant-scoped (Entra's `iss` carries the tenant id) and
  `oid` is per-user unique and IdP-signed, so no cross-user or cross-tenant collision arises;
  the `primary_client_id` component is the defense-in-depth that keeps this true even under a
  shared or misconfigured issuer.
- **Persistent across upgrades.** Because it lives in stable memory, the bridge survives canister
  upgrades: a non-`sub` user does the normal-login-first step **once, ever**, not on every
  deploy. (An in-heap cache would be wiped each upgrade, forcing the CTA repeatedly — the reason
  this is stable, not heap.)
- **Populated only on a primary-client login** — the only login carrying both the alternate
  stable id and the primary `sub`, and only from an **IdP-signed primary-client token** (the JWT
  is verified before the entry is written). The write happens exclusively on the update /
  registration paths; identity **resolution** (`resolve_primary_identity`) is side-effect-free,
  so the query delegation path only ever *reads* the bridge.
- **Per-app logins resolve through it:** the per-app token gives `(iss, primary_client_id, <stable
  id>)`, the aux lookup yields the primary `sub`, and the existing credential index resolves the
  anchor.
- **A miss fails safe** — no anchor found, so the user completes a normal primary login first,
  rather than resolving to the wrong identity.

Cross-domain isolation does **not** depend on this lookup — it comes from the certified
`sso_domain` (§6, §7).

### 6.6 Frontend: routing the ceremony to the per-app client

The ceremony must run against the per-app client for the dapp's **effective origin**, and that
origin must be the **same** one the gate later keys on (the delegation / attribute origin), or the
token's `aud` won't match `app_clients[origin]`. So the frontend resolves the origin at ceremony
time and applies the same canonicalization the gate does:

- **Manual "Sign in with SSO"** — the dapp's effective origin is already known, so the wizard
  routes to that origin's `resolved_client_id`.
- **1-click `?sso=<domain>`** — the dapp's origin isn't on the pending postMessage channel until
  the authorize request arrives *after* the IdP round-trip, so `initiateSso` resolves it as
  `remapToLegacyDomain(derivationOrigin ?? channel.origin)`:
  - `channel.origin` is the established channel's `event.origin` (browser-set, unspoofable) — the
    common case.
  - `?derivationOrigin=` is a launch-URL param for a dapp that uses a derivation origin, whose
    value provably can't be learned earlier (it only rides the later request). The SDK supplies it.
  - The `icp0.io / icp.net → ic0.app` remap is applied so client-selection keys the **same
    canonical origin the gate does** (the delegation path remaps too, `delegation.rs`); without it
    an aliased origin routes to the primary client and the gate then denies it.
- **Fail-closed.** The frontend-supplied origin only *selects the client*. The canister
  independently enforces `aud == app_clients[origin]` (and validates a derivation origin against
  the target's `ii-alternative-origins`), so a wrong or spoofed hint can only **deny**, never
  bypass.

---

## 7. Why this is safe

1. **The gate is mint-or-refuse, and the proof is a certified bundle.** `sso_prepare_delegation`
   issues the `sso_attr_bundle` — `(sso_domain, origin, expiry)` — only if
   `jwt.aud == app_clients[origin]` (§6.2). The bundle is a canister signature under the caller's
   credential seed, so the replica verifies it against the caller's own identity: it can't be
   forged and **can't be replayed across identities** (§6.3). The attribute methods certify
   `sso:<domain>` only from a valid bundle, so a wrong/absent/expired bundle simply yields no SSO
   rows. There is no ungated path to the certified `sso:<domain>` for this origin.
2. **SSO attributes only exist for an SSO sign-in.** The bundle exists only from
   `sso_prepare_delegation`, so a passkey login (or `openid_prepare_delegation`) on an anchor that
   merely *has* an SSO access method carries none — and shares no `sso:<domain>` (or
   `sso:<domain>:…`) attributes. The bundle's own `expiry` keeps them fresh.
3. **The origin-bound `sso:<domain>` is the whole gate.** Its presence for origin X proves both
   *which domain* (the certified `sso_domain` is the discovery domain II resolved from — a rogue
   login is `sso:evil.com`, never `sso:acme.com`) and *assignment within it* (the bundle was issued
   only if `aud == app_clients[X]`, §6.2). So a gated dapp requiring `sso:acme.com` gets both from
   one check; `implicit:origin` is orthogonal anti-replay, not the gate (§6.4).
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

> **The `app_clients` key must be the exact browser origin** (scheme + host + optional port, no
> path, no trailing slash — e.g. `https://payroll.com`). Matching is byte-exact against the
> origin the browser reports, and that exact string is also bound into the SSO-session seed. A
> mismatch (wrong case, stray slash, `www.` vs apex) **fails safe** — the origin routes to the
> primary client (gate off) or is denied (`gate_all_apps`), and a session minted for a
> non-canonical origin is useless at the real one — but the dapp then won't be gated as intended,
> so it's a config error to catch, not a security hole.

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

One PR — a single cohesive feature. **No stable-memory *migration*, no `StorableOpenIdCredentialKey`
change, `openid_prepare_delegation` untouched.** The only new persistent state is the non-`sub`
aux bridge (§6.5), an **additive** stable map in a fresh memory region — existing structures and
layout are byte-unchanged. Inert until an org adds `app_clients` to its well-known.

> **Security invariant:** the gate is **mint-or-refuse at `sso_prepare_delegation`** (§6.2) — a
> refused gate returns no `sso_attr_bundle`. Certification is a config-free read of that certified
> bundle (§6.3), which the replica verifies under the caller's own credential seed, so it can't be
> forged or replayed across identities. SSO attributes (`sso:<domain>`, `sso:<domain>:…`) are
> therefore only ever produced from an SSO sign-in, and the dapp additionally verifies
> `sso:<domain>` for the domain (existing lib — no new work).

Contents:
- **Config (§5):** parse `app_clients` / `gate_all_apps` / `stable_identifier_claim`;
  `get_sso_discovery(domain, opt origin)` returns the per-origin `resolved_client_id` for FE
  routing.
- **`sso_prepare_delegation` / `sso_get_delegation` (new, §6.2):** verify the JWT, enforce the
  gate (`aud == app_clients[origin]` / primary / DENY), resolve the anchor to primary (§6.1,
  aux lookup when `stable_identifier_claim != "sub"`, §6.5), mint an **ordinary primary-keyed
  openid delegation**, and return the certified `sso_attr_bundle` (`(sso_domain, origin, expiry)`
  signed under the credential seed) plus its signature. `openid_prepare_delegation` is left as-is.
- **SSO-aware registration (§6.1):** `OpenIDRegFinishArg.origin` — a first gated login runs the
  same gate and stores a primary-keyed credential (`sub` orgs register directly; non-`sub` fails
  safe with `SsoNormalLoginRequired`).
- **Certified `sender_info` bundle (§6.3):** `sso_prepare_delegation` returns the bundle; the
  frontend attaches it via the SDK's `AttributesIdentity`; the canister reads it with
  `read_certified_sso_bundle` (raw `ic0 1.1` `msg_caller_info_*`), the replica verifying it under
  the caller's own credential seed. **Only** `prepare_icrc3_attributes` and
  `list_available_attributes` read it, to certify / list the `sso:<domain>` rows for the bundle's
  origin. Account / delegation methods stay **permissionless**; `identity_info` is untouched (the
  consent path skips it for SSO). No `app_clients`/config at cert. A passkey session carries no
  bundle → no SSO attributes; the bundle's own `expiry` keeps them fresh.
- **Frontend:** route the dapp SSO ceremony to `resolved_client_id` — the manual wizard uses the
  dapp's effective origin, the 1-click path uses `remapToLegacyDomain(derivationOrigin ??
  channel.origin)` so client-selection keys the same origin the gate does (§6.6); sign in via
  `sso_prepare_delegation` and attach the returned bundle. For the non-`sub` first-gated-login case
  (§6.1/§6.5), guide the user with **button-driven CTAs** (a fresh user gesture per ceremony —
  "sign in with your organization", then "continue to <app>") rather than a passive prompt, since
  browsers block chained popups opened after an `await`.
- **Tests:** gated == ungated → same dapp principal (linchpin, proven at integration *and* e2e);
  a passkey / non-SSO session gets **no** SSO attributes; a refused gate returns no bundle → cert
  unreachable; a cross-identity / expired / cross-origin bundle is rejected; `gate_all_apps`
  default-deny; hashed keys; Entra `oid` identity + first-gated registration fail-safe (incl. a
  non-`sub` token missing the stable-id claim); **aux bridge survives upgrade** + **is scoped per
  primary client** (§6.5); 1-click gated routing via channel origin and via `derivationOrigin`
  (§6.6); `openid_prepare_delegation` / direct providers unchanged.

`.did` deltas are additive **except one authorized non-additive variant**: **add**
`sso_prepare_delegation` / `sso_get_delegation`, returning `SsoPrepareDelegationResponse` /
`SsoGetDelegationResponse` (which carry the `sso_attr_bundle` + its signature); `get_sso_discovery`
gains the `opt origin` arg + `resolved_client_id`; `OpenIDRegFinishArg` gains an `opt origin`
field; and `IdRegFinishError` gains the `SsoNormalLoginRequired` variant (the one non-additive
change — inert for existing clients, §6.1). No change to `openid_prepare_delegation` /
`openid_get_delegation`, and the attribute request records are unchanged. The committed `.did` is
candid-checked against the Rust service by `check_candid_interface_compatibility`.

No canister beyond II core, no proxy, no panel, no data migration, no new dapp lib (one additive
stable structure, §6.5). The one dependency change is `ic0 = "1.1"` for the `msg_caller_info_*`
accessors — no `ic-cdk` bump (§6.3). Admin/dapp docs tracked separately.

---

## 11. References

- SSO discovery + `app_clients` resolution: `src/internet_identity/src/openid/sso.rs`;
  frontend discovery `src/frontend/src/lib/utils/ssoDiscovery.ts`.
- The gate, registration, and the non-`sub` aux bridge:
  `src/internet_identity/src/openid/sso_gating.rs`.
- The certified `sender_info` bundle (codec, `prepare_sso_attr_bundle`,
  `read_certified_sso_bundle`, `msg_caller_info_*`): `src/internet_identity/src/openid/sso_bundle.rs`.
- Credential key and delegation seed: `src/internet_identity/src/openid.rs`
  (`OpenIdCredentialKey`, `calculate_delegation_seed`).
- Dapp-principal derivation: `src/internet_identity/src/delegation.rs`
  (`calculate_anchor_seed`).
- Frontend 1-click ceremony routing: `src/frontend/src/routes/(new-styling)/authorize/+page.ts`
  and `+page.svelte` (`initiateSso`); the `icp0.io → ic0.app` remap
  `src/frontend/src/lib/utils/iiConnection.ts` (`remapToLegacyDomain`).
- Companion design: `enterprise-sso-per-app-access-control.md` (id.ai-side gating).
