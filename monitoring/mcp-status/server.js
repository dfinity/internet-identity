#!/usr/bin/env node
// Tiny HTTP server that hosts the IMCP status dashboard.
//
// It serves a self-contained HTML page at `/` and runs the health probes
// server-side at `/api/status`. Doing the probes server-side is what makes the
// dashboard work at all: the MCP server's `/mcp` 401 challenge, its landing
// page, and the II instance's CSP header are not CORS-readable from a browser.
//
// Usage:
//   node monitoring/mcp-status/server.js [--port 8080] [--mcp <origin>] [--ii <origin>]
//
// The page also accepts `?mcp=` / `?ii=` query overrides so a single running
// server can be pointed at different deployments.

import http from "node:http";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";
import { runDashboard } from "./checks.js";

const here = dirname(fileURLToPath(import.meta.url));

const parsePort = () => {
  const idx = process.argv.indexOf("--port");
  if (idx !== -1) return Number(process.argv[idx + 1]);
  if (process.env.PORT) return Number(process.env.PORT);
  return 8080;
};
const argValue = (flag) => {
  const idx = process.argv.indexOf(flag);
  return idx !== -1 ? process.argv[idx + 1] : undefined;
};

const defaults = {
  mcpOrigin: argValue("--mcp"),
  iiOrigin: argValue("--ii"),
};

const sendJson = (res, code, body) => {
  const payload = JSON.stringify(body);
  res.writeHead(code, {
    "content-type": "application/json; charset=utf-8",
    "cache-control": "no-store",
    "content-length": Buffer.byteLength(payload),
  });
  res.end(payload);
};

const server = http.createServer(async (req, res) => {
  try {
    const url = new URL(req.url ?? "/", "http://localhost");

    if (url.pathname === "/" || url.pathname === "/index.html") {
      const html = await readFile(join(here, "public", "index.html"));
      res.writeHead(200, {
        "content-type": "text/html; charset=utf-8",
        "cache-control": "no-store",
      });
      res.end(html);
      return;
    }

    if (url.pathname === "/api/status") {
      const report = await runDashboard({
        mcpOrigin: url.searchParams.get("mcp") ?? defaults.mcpOrigin,
        iiOrigin: url.searchParams.get("ii") ?? defaults.iiOrigin,
      });
      sendJson(res, report.overall === "fail" ? 503 : 200, report);
      return;
    }

    sendJson(res, 404, { error: "not found" });
  } catch (e) {
    sendJson(res, 500, { error: String(e?.message ?? e) });
  }
});

const port = parsePort();
server.listen(port, () => {
  process.stdout.write(
    `IMCP status dashboard listening on http://localhost:${port}\n` +
      `  monitoring: ${defaults.mcpOrigin ?? "https://mcp.beta.id.ai (default)"}\n`,
  );
});
