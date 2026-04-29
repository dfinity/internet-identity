# Dfinsight

A "common matters of interest" board for DFINITY members. Users sign in
1-click via the DFINITY SSO at [id.ai](https://id.ai), post one
anonymous Issue per 24h, and upvote others — without ever seeing the
ranking. Admins (a fixed allowlist) can read scores, delete spam, and
post a public response that closes voting.

## How the auth works

| Role  | Sign-in URL                                | Attributes requested  | Backend gate                          |
|-------|--------------------------------------------|------------------------|---------------------------------------|
| User  | `https://id.ai/authorize?sso=dfinity.org`  | none                   | `Principal.isAnonymous == false`      |
| Admin | `https://id.ai/authorize?sso=dfinity.org`  | `sso:dfinity.org:name` | `mo:identity-attributes` `II.verify`  |

The user flow yields an SSO-scoped principal — stable per `(user, dapp)`
pair, but anonymous in the sense that the dapp never learns the user's
name or email. That's enough for the backend to dedupe upvotes and
enforce the rolling 24-hour post limit.

The admin flow additionally pulls a certified `name` attribute via
`AuthClient.requestAttributes`, wraps it into an `AttributesIdentity`,
and burns it once on `establishAdminSession`. After that, the canister
caches `(principal, name)` for 30 minutes so subsequent admin clicks
don't trigger fresh II popups.

## Layout

```
.
├── icp.yaml                              # icp-cli canister manifest
├── scripts/
│   └── build-backend.sh                  # mops install + moc
└── src/
    ├── dfinsight_backend/
    │   ├── main.mo                       # the canister
    │   └── mops.toml                     # core 2.5 + identity-attributes 0.1
    └── dfinsight_frontend/
        ├── index.html
        ├── package.json
        ├── tsconfig.json
        ├── vite.config.ts
        └── src/
            ├── App.tsx                   # router
            ├── main.tsx
            ├── styles.css
            ├── lib/
            │   ├── auth.ts               # signInAnonymous / signInAdmin
            │   ├── backend.ts            # actor factory
            │   ├── config.ts             # II URL, canister id, host
            │   ├── sessionStore.ts
            │   └── declarations/         # IDL + TS types (hand-rolled)
            └── pages/
                ├── Home.tsx              # landing
                ├── Issues.tsx            # signed-in user view
                ├── AdminLanding.tsx      # admin sign-in
                └── AdminPanel.tsx        # admin moderation
```

## Local development

You need:

- Node 22+
- [`icp-cli`](https://github.com/dfinity/icp-cli) v0.2.x — `npm i -g @icp-sdk/icp-cli @icp-sdk/ic-wasm`
- [`mops`](https://mops.one/) — `npm i -g ic-mops`, then `cd src/dfinsight_backend && mops toolchain use moc` once

```sh
# 1. Install JS + Motoko deps
npm install
(cd src/dfinsight_backend && mops install)

# 2. Spin up a local replica
icp network start

# 3. Build + deploy both canisters
icp deploy

# 4. Start the Vite dev server
npm run dev
```

The dev server runs on `http://localhost:5173` and talks to the local
replica on `http://127.0.0.1:4943`. Override either with the
`VITE_IC_HOST` / `VITE_DFINSIGHT_BACKEND_CANISTER_ID` env vars if your
setup differs.

## Deploying to mainnet

A handful of extra steps beyond `icp deploy --environment ic`:

1. **Set `rpOrigin` in `main.mo`** to the production frontend URL,
   e.g. `"https://<frontend-id>.icp0.io"`. The Authorization-tier
   verify rejects bundles whose `implicit:origin` doesn't match.
2. **Set `trusted_attribute_signers`** on the backend canister to II's
   production principal (`rdmx6-jaaaa-aaaaa-aaadq-cai`). Without this
   the IC ingress layer strips `sender_info` before it hits
   `mo:core/CallerAttributes`, and every admin verify returns
   `#NoAttributes`.
3. **Update the admin list** in `main.mo` if you need more than the
   bootstrap admin (`Arshavir Ter-Gabrielyan`).

## Anti-bias design

Two things keep regular users from inferring rank:

- The list returned by `listIssuesForUser` is **shuffled** with
  on-chain entropy (`Random.blob`) on every call.
- **Scores are hidden** until an admin posts a response — only then
  does the issue surface its upvote count to everyone.

Once an admin responds, voting on that issue is locked
(`#VotesLocked`).

## What's where

| Concern                                | File                                           |
|----------------------------------------|------------------------------------------------|
| 24h post limit                         | `main.mo:createIssue` + `lastPostAt` map       |
| Hidden scores                          | `main.mo:listIssuesForUser` (`upvotes : ?Nat`) |
| Admin verify via `sender_info`         | `main.mo:verifyAdminAttributes`                |
| 30-min admin session cache             | `main.mo:adminSessions`                        |
| 1-click SSO                            | `auth.ts:signInAnonymous`                      |
| 1-click SSO + name attribute           | `auth.ts:signInAdmin`                          |
| Shuffle                                | `main.mo:shuffle`                              |
