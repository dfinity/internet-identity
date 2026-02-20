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
    openid: ["sub", "name", "email", "preferred_username"],
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
