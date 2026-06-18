# IMCP (IC MCP) status dashboard

A small, dependency-free monitoring tool for the **IC MCP** server
([https://mcp.beta.id.ai](https://mcp.beta.id.ai) by default) and the **Internet
Identity** instance it is paired with.

It answers three questions and adds a few suggestions:

1. **Is the server running and responding on all advertised endpoints with the
   correct status codes?** — probes the landing page, the two OAuth discovery
   documents, the `/mcp` endpoint's unauthenticated `401` challenge, dynamic
   client registration, and the `/oauth/authorize` + `/oauth/token` endpoints,
   plus TLS certificate freshness.
2. **Which Internet Identity instance is it linked to?** — resolves the II
   origin (derived from the `mcp.<env>.id.ai` ↔ `<env>.id.ai` convention,
   overridable, and confirmed live via the `/oauth/authorize` redirect when the
   server exposes one).
3. **Is that II instance healthy and does it recognise this MCP server?** —
   checks the II frontend is reachable and IC-certified, reports its frontend
   canister id and related origins, and verifies that the II's response CSP
   `form-action` directive lists this MCP origin. That directive is derived
   server-side from the II canister's `mcp_server_origin` config, so it is the
   authoritative signal that the II trusts this MCP server and that the `/mcp`
   delegation flow is enabled for it.

## Usage

No install step and no dependencies — just Node ≥ 20 (uses the global `fetch`).

```bash
# Text report for the default beta deployment (exit code 0 = healthy)
npm run mcp-status
node monitoring/mcp-status/cli.js

# Point it at another deployment (e.g. production)
node monitoring/mcp-status/cli.js --mcp https://mcp.id.ai

# Machine-readable output for alerting / CI
node monitoring/mcp-status/cli.js --json

# Live web dashboard at http://localhost:8080 (auto-refreshing)
npm run mcp-status:serve
node monitoring/mcp-status/server.js --port 8080

# Unit tests
npm run mcp-status:test
```

CLI options: `--mcp <origin>`, `--ii <origin>`, `--timeout <ms>`, `--json`,
`--no-color`, `--strict` (exit non-zero on warnings too), `--help`. The exit
code is `0` when healthy and `1` on failures, so it slots straight into cron, a
CI job, or an uptime check.

### Allowed targets (SSRF guard)

Because `server.js` accepts `?mcp=`/`?ii=` overrides over HTTP, probe targets
are validated against a host allowlist: only `https` origins on `id.ai` (and its
sub-domains), plus loopback hosts for local development, may be probed. This
stops the dashboard from being used to reach arbitrary or internal hosts. To
monitor a deployment on another domain, extend the allowlist via the
`MCP_STATUS_ALLOWED_HOSTS` environment variable (comma-separated host suffixes).

## Why a standalone tool (and not a page in the II frontend)?

The II frontend is a **static, prerendered, `ssr: false` SvelteKit app** served
from a canister — it has no server runtime to probe from. More importantly, the
signals that matter here are **not readable from a browser**: the MCP server's
`/mcp` `401` challenge and HTML landing page send no CORS headers, and the II
instance's `Content-Security-Policy` header (where recognition is verified)
can't be inspected cross-origin. Running the probes server-side in this small
Node tool sidesteps CORS entirely and lets the dashboard check everything that
actually matters.

## Files

| File               | Purpose                                                        |
| ------------------ | -------------------------------------------------------------- |
| `config.js`        | Target resolution (defaults, env vars, II-origin derivation).  |
| `checks.js`        | All probing logic; exports `runDashboard()`.                   |
| `report.js`        | ANSI/plain-text rendering for the CLI.                         |
| `cli.js`           | Command-line entry point.                                      |
| `server.js`        | HTTP server: serves the dashboard and runs probes server-side. |
| `public/index.html`| Self-contained auto-refreshing web dashboard.                  |
| `checks.test.js`   | `node:test` unit tests (stubbed `fetch`, no network).          |

## Current beta findings (snapshot)

Against `https://mcp.beta.id.ai` the server passes all endpoint checks; the
linked II is `https://beta.id.ai` (frontend canister `gjxif-ryaaa-aaaad-ae4ka-cai`),
which **does** recognise the MCP server via its `form-action` CSP. Live link
discovery via `/oauth/authorize` is reported as a warning because the redirect
to II only happens after interactive client setup and isn't readable headlessly
— informational, not an outage.
