# Test OpenID provider

A throwaway OpenID Connect identity provider (an Okta/OneLogin stand-in) used to
exercise Internet Identity's OpenID and **SSO** sign-in flows. It is built on
[`node-oidc-provider`](https://github.com/panva/node-oidc-provider) and serves
everything II's two-hop SSO discovery needs:

| Endpoint                                   | Purpose                                                                                       |
| ------------------------------------------ | --------------------------------------------------------------------------------------------- |
| `GET /.well-known/ii-openid-configuration` | II's custom SSO indirection doc (`client_id`, `openid_configuration` URL, `name`) — **hop 1** |
| `GET /.well-known/openid-configuration`    | Standard OIDC discovery (`issuer`, `jwks_uri`, `authorization_endpoint`, …) — **hop 2**       |
| `GET /jwks`                                | Signing keys II verifies the ID token against                                                 |
| `GET /auth`, `GET/POST /token`             | Authorization + token endpoints (via `provider.callback()`)                                   |
| `POST /account/:id/claims`                 | Test helper: set the claims an account returns in its ID token                                |

> **Test/dev only.** It uses in-memory storage and dev signing keys, accepts a
> hardcoded `client_id`/`client_secret`, and sets `Access-Control-Allow-Origin: *`.
> Never run it as a real IdP.

## How II consumes it (two-hop SSO discovery)

```text
domain ─[hop 1: /.well-known/ii-openid-configuration]─▶ { client_id, openid_configuration, name }
       ─[hop 2: openid_configuration URL]─────────────▶ { issuer, jwks_uri, authorization_endpoint }
       ─[jwks_uri]────────────────────────────────────▶ keys
```

At verify time the canister cross-checks the ID token: `aud` must equal the
discovered `client_id`, `iss` must equal the discovered `issuer`, the issuer
host must match the `openid_configuration` host, and `authorization_endpoint`
must share the issuer's host. The browser uses
`response_type=code id_token` + `response_mode=form_post`, posting to the II
origin's `/callback`.

## Local usage (default — what CI and `scripts/dev-e2e-setup` use)

```sh
npm ci
npm start -- 11105        # issuer defaults to http://localhost:11105
```

`scripts/dev-e2e-setup` runs three instances (`11105 11106` direct OpenID,
`11107` SSO). Loopback hosts are the only ones II lets serve discovery over
plain `http`, which is why this Just Works locally with no TLS.

## Deploying against a hosted II (e.g. `beta.id.ai`)

Against a non-loopback target two things change versus the localhost flow:

1. **Public HTTPS.** The _canister_ (not just the browser) fetches discovery +
   JWKS via IC HTTP outcalls, and II requires `https://` with a publicly trusted
   certificate for any non-loopback host. So the server must sit behind a
   public HTTPS URL.
2. **Reachable over HTTPS.** There's no domain allowlist — II accepts any
   bare-authority domain — but discovery outcalls to a non-loopback host require
   a valid public `https` URL. That TLS requirement is the trust gate.

### Configuration (env vars)

| Var                  | Default                  | Set it to…                                                                               |
| -------------------- | ------------------------ | ---------------------------------------------------------------------------------------- |
| `OIDC_PORT`          | `11105` (or argv)        | local bind port                                                                          |
| `OIDC_ISSUER`        | `http://localhost:$PORT` | the **public HTTPS base URL** (your tunnel host) — a bare origin, no trailing slash/path |
| `OIDC_REDIRECT_URIS` | dev + e2e callbacks      | comma-separated, must include the target's callback, e.g. `https://beta.id.ai/callback`  |
| `OIDC_SSO_NAME`      | `Test SSO $PORT`         | label shown on II's consent screen                                                       |

When `OIDC_ISSUER` is `https://…` the server sets `provider.proxy = true` so it
trusts the tunnel's `X-Forwarded-Proto`/`Host` headers and treats requests as
secure.

### 1. Expose it over HTTPS with a tunnel

The lowest-effort option is a tunnel — no TLS or infra to stand up. The tunnel
must forward the public `Host` header (cloudflared and ngrok both do), because
`node-oidc-provider` derives `jwks_uri`/`authorization_endpoint` from the
request host; otherwise those endpoints would point back at `localhost` and II's
host self-assertion checks would fail.

**cloudflared** (random URL):

```sh
# Start the tunnel first to learn the URL; it 502s until the provider is up.
cloudflared tunnel --url http://localhost:11105
# → https://<random>.trycloudflare.com
```

**ngrok** (stable URL if you have a reserved domain):

```sh
ngrok http --domain=my-toy-idp.example.com 11105
```

### 2. Start the provider pointed at the public URL

```sh
OIDC_PORT=11105 \
OIDC_ISSUER="https://my-toy-idp.example.com" \
OIDC_REDIRECT_URIS="https://beta.id.ai/callback" \
OIDC_SSO_NAME="Acme SSO" \
  npm start
```

Sanity-check through the tunnel (endpoints must show the **public https** host):

```sh
curl https://my-toy-idp.example.com/.well-known/ii-openid-configuration
curl https://my-toy-idp.example.com/.well-known/openid-configuration
```

### 3. Allowlist the discovery domain on the target canister

The domain a user types on the SSO screen (your tunnel host, e.g.
`my-toy-idp.example.com`) needs no allowlisting — II accepts any bare-authority
domain and resolves it over `https`. Just make sure your tunnel host serves the
discovery endpoints over a valid public HTTPS URL.

With no override, the default allowlist is `dfinity.org` on a production
canister and `beta.dfinity.org` otherwise — so an arbitrary tunnel host will be
rejected (`NotAllowed`) until it's added here.

### 4. (Optional) set the claims your test account returns

```sh
curl -X POST https://my-toy-idp.example.com/account/alice/claims \
  -H 'Content-Type: application/json' \
  -d '{"sub":"alice","email":"alice@example.com","email_verified":true,"name":"Alice"}'
```

### 5. Sign in

On `beta.id.ai`, choose the SSO entry, enter your discovery domain
(`my-toy-idp.example.com`), wait for **Continue** to enable (discovery
resolved), and complete the IdP flow.

## Troubleshooting

- **Continue stays disabled / discovery times out** — the discovery endpoints
  aren't reachable over a valid public HTTPS URL. Re-check step 1.
- **`jwks_uri`/`authorization_endpoint` point at `localhost`** — the tunnel
  isn't forwarding the public `Host` header; the canister's host self-assertion
  check then fails. Verify with the `curl` in step 2.
- **`invalid_request` / cookie errors at `/auth`** — issuer is `https` but the
  proxy headers aren't arriving; confirm the tunnel sets `X-Forwarded-Proto: https`.
- **`redirect_uri` mismatch** — add the exact target callback (e.g.
  `https://beta.id.ai/callback`) to `OIDC_REDIRECT_URIS`.
