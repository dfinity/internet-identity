import * as oidc from "oidc-provider";
import express from "express";

const app = express();
const port = parseInt(process.argv[2], 10) || 11105; // "OpenID" (O = 111, I = 105)
const accountClaims = new Map();
const provider = new oidc.Provider(`http://localhost:${port}`, {
  clients: [
    {
      client_id: "internet_identity",
      client_secret: "secret", // Not used but required here
      redirect_uris: [
        "http://localhost:5173/callback", // Local development
        "https://id.ai/callback", // e2e tests
      ],
      response_types: ["code id_token"],
      grant_types: ["implicit", "authorization_code"],
    },
  ],
  claims: {
    openid: ["sub"],
  },
  features: {
    claimsParameter: {
      enabled: true,
      // Allow any claim that's stored in the account
      async assertClaimsParameter(ctx, claims) {
        const accountId = ctx.oidc.session?.accountId;
        if (!accountId) return;
        
        const stored = accountClaims.get(accountId) ?? {};
        
        // For each requested claim, allow it if it exists in stored data
        if (claims?.id_token) {
          for (const claimName of Object.keys(claims.id_token)) {
            if (claimName !== "sub" && !(claimName in stored)) {
              delete claims.id_token[claimName];
            }
          }
        }
        if (claims?.userinfo) {
          for (const claimName of Object.keys(claims.userinfo)) {
            if (claimName !== "sub" && !(claimName in stored)) {
              delete claims.userinfo[claimName];
            }
          }
        }
      },
    },
  },
  async findAccount(ctx, id) {
    return {
      accountId: id,
      async claims(use, scope, claimsParameter, rejected) {
        // Return sub (required) plus all stored claims
        const stored = accountClaims.get(id) ?? {};
        return {
          sub: id,
          ...stored,
        };
      },
    };
  },
});

// Bypass "`select_account` prompt is unsupported" error
provider.use(async (ctx, next) => {
  if (ctx.path === "/auth") {
    // Change prompt to `login`, which is supported
    ctx.request.url = ctx.request.url.replace("select_account", "login");
  }
  await next();
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

// Register provider and start server
app.use(provider.callback());
app.listen(port, () => {
  console.log(
    `For endpoints see: http://localhost:${port}/.well-known/openid-configuration`,
  );
});
