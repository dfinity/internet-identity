# Setting up an organization SSO server for Internet Identity

Generic flow plus per-provider setup for **Okta**, **Microsoft Entra ID**, and **OneLogin**.

## The model (why the steps are what they are)

Internet Identity (II) resolves an org domain through **two hops**, then runs a
**hybrid OIDC flow**:

```text
org domain ─[hop 1]→ /.well-known/ii-openid-configuration  → { client_id, openid_configuration, name }
           ─[hop 2]→ <openid_configuration>                 → { issuer, jwks_uri, authorization_endpoint }
```

The browser runs `response_type=code id_token` + `response_mode=form_post`; II
reads **only the front-channel `id_token`** — it never exchanges the code, never
uses PKCE or a client secret. At verify time the canister checks:

- `aud == client_id`
- `iss == issuer`
- the issuer host matches the `openid_configuration` host
- `authorization_endpoint` shares the issuer's host

Two hosts are involved: the **discovery domain** (what the user types — serves
hop 1) and the **IdP** (serves hop 2 + JWKS + auth). They can differ; the
discovery domain is just the indirection layer.

> **II has exactly two deployments.** The `<II-origin>` below is one of these,
> and its callback is `https://<II-origin>/callback`:
>
> - **Production:** `id.ai` → `https://id.ai/callback`
> - **Staging / beta:** `beta.id.ai` → `https://beta.id.ai/callback`
>
> Register the callback for whichever you're testing against (you can add both
> to the same app).

## Generic flow

Provider-neutral; the concrete per-IdP clicks are in the next section.

1. **Create a confidential OIDC app.** It must be a _web/confidential_ client
   (public/SPA clients force PKCE, which II doesn't send), it must issue a
   **front-channel `id_token` via the hybrid flow** (`response_type=code
   id_token`), and its redirect URI must be the II callback above. Request
   scopes `openid profile email`. Record the **`client_id`**.
2. **Authorize the app for your users.** Make sure the IdP actually permits the
   app + flow for the signing-in user — assignment / consent / an
   authorization-server access policy, depending on the provider.
3. **Publish the hop-1 doc** at `GET /.well-known/ii-openid-configuration` on
   your org domain, over **public HTTPS** with **CORS**
   (`Access-Control-Allow-Origin: *`). Set `openid_configuration` to your IdP's
   **full OIDC discovery document URL** — the complete
   `…/.well-known/openid-configuration` URL from your provider's section below,
   used verbatim. It already ends in `/.well-known/openid-configuration`, so
   don't append another one:

   ```json
   {
     "client_id": "<your-client-id>",
     "openid_configuration": "<full OIDC discovery URL, e.g. https://<org>.okta.com/oauth2/default/.well-known/openid-configuration>",
     "name": "Acme SSO"
   }
   ```

4. **Verify & sign in.** `curl` both well-known docs (hosts must be public
   HTTPS), then on `id.ai` / `beta.id.ai` choose SSO, enter the discovery
   domain, wait for **Continue**, and complete the IdP login.

> **Domain allowlisting — not needed on the hosted deployments.** When the
> target canister has the `sso_allow_any_domain` deploy flag enabled — as the
> hosted `id.ai` / `beta.id.ai` deployments do — any bare-authority discovery
> host served over HTTPS just works, with no domain registration. That flag
> does **not** relax the HTTPS rule — only explicitly-allowlisted hosts may
> serve discovery over plain `http` (e.g. `localhost` in e2e).
>
> The flag defaults to `false`, though. On a canister without it (a local or
> self-hosted II), the `sso_discoverable_domains` allowlist still gates
> discovery: an II admin adds your domain via an upgrade arg (the vec fully
> replaces the previous list; entries are lowercased). This allowlist was the
> original **canary gate** used before SSO went generally available.

## Provider setup (the concrete clicks for steps 1–3)

### Okta

- **Create:** Applications → Create App Integration → OIDC → **Web Application**.
- **Confidential:** Client authentication = **Client secret**; **uncheck
  "Require PKCE."**
- **Hybrid `id_token`:** Grant type = **Authorization Code + Implicit
  (hybrid)**, enable **"Allow ID Token with implicit grant type."**
- **Authorize:** Security → API → Authorization Servers → **default** → Access
  Policies — the rule must allow **Authorization Code + Implicit (hybrid)** and
  the scopes. _(This is a separate gate from the app's grants — the source of
  the `access_denied` "policy evaluation failed.")_
- **Discovery URL** (use the **non-admin** host + `default` server):
  `https://<org>.okta.com/oauth2/default/.well-known/openid-configuration`

### Microsoft Entra ID (Azure AD)

- **Create:** Microsoft Entra admin center → **App registrations → New
  registration**; add the redirect URI under the **Web** platform (_not_ SPA —
  SPA forces PKCE).
- **Confidential:** the Web platform already avoids forced PKCE; a client secret
  (Certificates & secrets) is optional since II never calls the token endpoint.
- **Hybrid `id_token`:** **Authentication → Implicit grant and hybrid flows →
  tick "ID tokens (used for implicit and hybrid flows)."** Without it Entra
  rejects `code id_token` (_"response_type isn't allowed… Expected value is
  'code'"_).
- **Authorize:** ensure delegated `openid profile email` are consented; assign
  users if your tenant requires assignment.
- **Discovery URL** — use the **tenant-specific** URL (not `/common` or
  `/organizations`, whose `issuer` is templated and won't match the token's
  concrete `iss`):
  `https://login.microsoftonline.com/<tenant-id>/v2.0/.well-known/openid-configuration`

### OneLogin

- **Create:** Administration → **Applications → Add App → "OpenID Connect
  (OIDC)"**; set **Application Type = Web**. Add the redirect URI under
  Configuration.
- **Confidential:** SSO tab → **Token Endpoint → Authentication Method = POST**
  (or Basic) — _not_ "None (PKCE)," which forces PKCE.
- **Hybrid `id_token`:** OneLogin supports the **Hybrid Flow + `form_post`** with
  `code id_token`; ensure Authorization Code + Implicit are enabled for the app.
- **Authorize:** **assign the app** to your test user (or a role the user
  holds).
- **Discovery URL** (issuer is `…/oidc/2`; the newer generic host
  `openid-connect.onelogin.com` also works):
  `https://<subdomain>.onelogin.com/oidc/2/.well-known/openid-configuration`

## Troubleshooting

| Symptom | Cause | Fix |
| --- | --- | --- |
| Continue stays disabled / `NotAllowed` | hop-1 isn't reachable over public HTTPS, or the input isn't a bare host (scheme/path/userinfo etc.) | Serve hop-1 over HTTPS; enter a bare `host[:port]`. Check the curls in step 4 |
| `invalid_request: PKCE code challenge is required` | Public/SPA client forces PKCE | Confidential/Web client, PKCE off (Okta: uncheck Require PKCE; OneLogin: auth method POST/Basic; Entra: Web platform) |
| `unsupported_response_type` / Entra: "expected 'code'" | App doesn't allow the hybrid flow / front-channel `id_token` | Enable Implicit (hybrid) + ID token (Okta) or tick "ID tokens" (Entra) |
| `access_denied: Policy evaluation failed` | IdP authorization gate doesn't permit the grant/scopes/user | Okta AS access policy; Entra/OneLogin: app assignment/consent |
| `redirect_uri` mismatch | Wrong callback registered | Must be `https://id.ai/callback` or `https://beta.id.ai/callback` |
| `iss` mismatch at verify | Discovery `issuer` ≠ token `iss` (e.g. Entra `/common` templated issuer) | Use the tenant/issuer-specific discovery URL |
| Changed `client_id` but old one still sent | Canister caches SSO discovery **in-memory ~1 h, keyed by domain** | Wait the hour, use a fresh discovery host, or have an admin upgrade the canister |

> **Tunnels:** a quick `*.trycloudflare.com` host changes on every restart.
> Since SSO is GA any HTTPS host works — and a fresh host is also the quickest
> way to bypass the ~1 h discovery cache (keyed by domain). Use a
> reserved/named tunnel domain if you'll reuse this.
