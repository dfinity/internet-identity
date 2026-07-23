import * as oidc from "oidc-provider";
import express from "express";
import { createHash } from "crypto";

const app = express();
// Port the server binds to locally. CI/dev pass it as argv (`npm start -- 11105`);
// a public deployment behind a tunnel can instead set OIDC_PORT.
const port = parseInt(process.env.OIDC_PORT ?? process.argv[2], 10) || 11105; // "OpenID" (O = 111, I = 105)

// Public base URL (the OIDC issuer). Defaults to the localhost form used by the
// e2e/dev setup; set OIDC_ISSUER to the public HTTPS URL (e.g. a tunnel host)
// when deploying so the issuer, jwks_uri and authorization_endpoint all resolve
// to a host the IC's HTTP outcalls — and the browser — can reach. II requires
// https for any non-loopback discovery host.
//
// Normalize to a bare origin: the issuer is concatenated into discovery URLs
// and the provider's routes mount at "/", so a trailing slash would emit
// "//.well-known/..." and a path component (e.g. https://host/oidc) would never
// actually be served. Fail fast with a clear message instead of booting a
// server that hands out broken discovery URLs.
const issuer = normalizeIssuer(
  process.env.OIDC_ISSUER ?? `http://localhost:${port}`,
);

function normalizeIssuer(raw) {
  let url;
  try {
    url = new URL(raw);
  } catch {
    console.error(`OIDC_ISSUER is not a valid URL: "${raw}"`);
    process.exit(1);
  }
  if (url.pathname !== "/" || url.search !== "" || url.hash !== "") {
    console.error(
      `OIDC_ISSUER must be a bare origin (scheme://host[:port]) with no path, ` +
        `query, or fragment — got "${raw}". Try "${url.origin}".`,
    );
    process.exit(1);
  }
  // `url.origin` drops any trailing slash, so http://host:port/ → http://host:port.
  return url.origin;
}

// Redirect URIs the IdP will accept. Defaults to the dev + e2e callbacks; set
// OIDC_REDIRECT_URIS (comma-separated) to point at a deployed II origin's
// callback, e.g. "https://beta.id.ai/callback".
const redirectUris = (
  process.env.OIDC_REDIRECT_URIS ??
  "http://localhost:5173/callback,https://id.ai/callback"
)
  .split(",")
  .map((uri) => uri.trim())
  .filter(Boolean);

// Human-readable SSO name surfaced in II's consent screen (see the
// ii-openid-configuration endpoint below).
const ssoName = process.env.OIDC_SSO_NAME ?? `Test SSO ${port}`;

const accountClaims = new Map();

// The `client_id` a gated dapp runs its ceremony against, registered alongside
// the primary client so a gated login gets a token whose `aud` is this client.
const PER_APP_CLIENT_ID = "ii-per-app-gated-client";

// Mutable per-app gating config surfaced in the well-known; tests set it via
// `POST /sso-config`.
let ssoGating = {
  app_clients: {},
  gate_all_apps: false,
  stable_identifier_claim: "sub",
};

// Entra-style mode (OIDC_SUBJECT_TYPE=pairwise): the `sub` claim is pairwise —
// different per OIDC client — while a stable `oid` claim (set per account) stays
// constant across clients. Lets the gating e2e exercise the non-`sub` (oid)
// bridge faithfully. Default is a public (stable across clients) `sub`.
const pairwise = process.env.OIDC_SUBJECT_TYPE === "pairwise";
const clientBase = {
  client_secret: "secret", // Not used but required here
  redirect_uris: redirectUris,
  response_types: ["code id_token"],
  grant_types: ["implicit", "authorization_code"],
  // Pairwise `sub`. oidc-provider only demands a (https) sector_identifier_uri
  // when redirect_uris span more than one host, so the pairwise instance is
  // configured with a single-host redirect (OIDC_REDIRECT_URIS, see
  // scripts/dev-e2e-setup) and needs no sector document.
  ...(pairwise ? { subject_type: "pairwise" } : {}),
};

const provider = new oidc.Provider(issuer, {
  clients: [
    { client_id: "internet_identity", ...clientBase },
    { client_id: PER_APP_CLIENT_ID, ...clientBase },
  ],
  subjectTypes: pairwise ? ["public", "pairwise"] : ["public"],
  ...(pairwise
    ? {
        pairwiseIdentifier(_ctx, accountId, client) {
          return createHash("sha256")
            .update(`${accountId}:${client.clientId}`)
            .digest("hex");
        },
      }
    : {}),
  // `oid` is declared so the pairwise (Entra-style) accounts can carry a stable
  // identifier; it is only emitted when an account actually sets it.
  claims: {
    openid: [
      "sub",
      "name",
      "email",
      "preferred_username",
      "email_verified",
      "oid",
    ],
  },
  async findAccount(_, id) {
    return {
      accountId: id,
      claims() {
        // Return stored claims (can be set with the custom endpoint below)
        return accountClaims.get(id) ?? {};
      },
    };
  },
});

// When the issuer is HTTPS the server runs behind a TLS-terminating tunnel that
// forwards plain HTTP locally. Trust the proxy's X-Forwarded-* headers so
// oidc-provider treats the request as secure — otherwise it refuses to set the
// secure session cookie and the authorization flow fails.
if (issuer.startsWith("https://")) {
  provider.proxy = true;
}

// Bypass "`select_account` prompt is unsupported" error
provider.use(async (ctx, next) => {
  if (ctx.path === "/auth") {
    // Change prompt to `login`, which is supported
    ctx.request.url = ctx.request.url.replace("select_account", "login");
  }
  await next();
});

// Allow the II frontend (https://id.ai in e2e, http://localhost:5173 in
// dev) to fetch the SSO discovery + OIDC discovery endpoints from the
// browser. Without this the cross-origin GETs fail silently and SSO
// sign-in stalls with the wizard's Continue button stuck disabled.
// `*` matches our test/dev origins; production deployments wouldn't run
// this server.
app.use((req, res, next) => {
  res.setHeader("Access-Control-Allow-Origin", "*");
  if (req.method === "OPTIONS") {
    res.setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS");
    res.setHeader("Access-Control-Allow-Headers", "Content-Type");
    res.status(204).end();
    return;
  }
  next();
});

// Bypass http and localhost restrictions: https://github.com/panva/node-oidc-provider/discussions/1301
const { invalidate: orig } = provider.Client.Schema.prototype;
provider.Client.Schema.prototype.invalidate = function invalidate(
  message,
  code,
) {
  if (code === "implicit-force-https" || code === "implicit-forbid-localhost") {
    return;
  }
  orig.call(this, message);
};

// Endpoint to set claims for an account
app.post("/account/:id/claims", express.json(), async (req, res) => {
  const accountId = req.params.id;
  accountClaims.set(accountId, req.body);
  res.status(201).send();
});

// Set per-app gating fields surfaced in the well-known; omitted fields keep
// their defaults.
app.post("/sso-config", express.json(), (req, res) => {
  ssoGating = {
    app_clients: req.body.app_clients ?? {},
    gate_all_apps: req.body.gate_all_apps ?? false,
    stable_identifier_claim: req.body.stable_identifier_claim ?? "sub",
  };
  res.status(200).json(ssoGating);
});

// Endpoint for SSO discoverability
app.get("/.well-known/ii-openid-configuration", (req, res) => {
  res.status(200).json({
    client_id: "internet_identity",
    openid_configuration: `${issuer}/.well-known/openid-configuration`,
    // Optional human-readable SSO name. Surfaced in the II consent
    // screen as `<name> email:` etc., so e2e tests can verify the
    // prefix branch instead of the bare-domain fallback.
    name: ssoName,
    // IdP-side per-app gating (inert unless a test sets it via /sso-config).
    app_clients: ssoGating.app_clients,
    gate_all_apps: ssoGating.gate_all_apps,
    stable_identifier_claim: ssoGating.stable_identifier_claim,
  });
});

// Register provider and start server
app.use(provider.callback());
app.listen(port, () => {
  console.log(`Test OpenID provider listening on http://localhost:${port}`);
  console.log(`OIDC discovery:  ${issuer}/.well-known/openid-configuration`);
  console.log(`SSO discovery:   ${issuer}/.well-known/ii-openid-configuration`);
});
