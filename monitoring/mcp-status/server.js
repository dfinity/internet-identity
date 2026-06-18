#!/usr/bin/env node
// Tiny HTTP server that hosts the IMCP status dashboard.
//
// It serves a self-contained HTML page at `/` and runs the health probes
// server-side at `/api/status`. Doing the probes server-side is what makes the
// dashboard work at all: the MCP server's `/mcp` 401 challenge, its landing
// page, and the II instance's CSP header are not CORS-readable from a browser.
//
// The probe targets are fixed by the operator when the server is started — via
// the defaults, `--mcp`/`--ii` flags, or the MCP_ORIGIN/II_ORIGIN env vars —
// and are deliberately NOT taken from the incoming request. A hosted status
// page must never let a visitor steer server-side requests at arbitrary hosts.
//
// Usage:
//   node monitoring/mcp-status/server.js [--port 8080] [--mcp <origin>] [--ii <origin>]

import http from "node:http";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";
import { runDashboard } from "./checks.js";

const here = dirname(fileURLToPath(import.meta.url));

const DEFAULT_PORT = 8080;
const parsePort = () => {
  const idx = process.argv.indexOf("--port");
  const raw = idx !== -1 ? process.argv[idx + 1] : process.env.PORT;
  if (raw === undefined || raw === "") return DEFAULT_PORT;
  const port = Number(raw);
  if (!Number.isInteger(port) || port < 0 || port > 65535) {
    process.stderr.write(
      `Invalid port value; falling back to ${DEFAULT_PORT}\n`,
    );
    return DEFAULT_PORT;
  }
  return port;
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

// Replace control characters (incl. CR/LF) with spaces and cap the length, so
// that a logged error message can never forge or inject additional log entries.
// Implemented with a codepoint filter to avoid embedding control-char literals.
const sanitizeForLog = (value) => {
  const input = String((value && value.message) || value).slice(0, 300);
  let out = "";
  for (const ch of input) {
    const code = ch.codePointAt(0);
    out += code < 0x20 || code === 0x7f ? " " : ch;
  }
  return out;
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
      const report = await runDashboard(defaults);
      sendJson(res, report.overall === "fail" ? 503 : 200, report);
      return;
    }

    sendJson(res, 404, { error: "not found" });
  } catch (e) {
    // A misconfigured target is a client/operator error with a fixed, safe
    // message; any other failure is logged (sanitised) server-side and reported
    // generically so that no stack-trace or internal detail leaks to clients.
    if (e && e.code === "DISALLOWED_ORIGIN") {
      sendJson(res, 400, { error: "invalid or disallowed origin" });
    } else {
      console.error("mcp-status: request failed:", sanitizeForLog(e));
      sendJson(res, 500, { error: "internal error" });
    }
  }
});

const port = parsePort();
server.listen(port, () => {
  process.stdout.write(
    `IMCP status dashboard listening on http://localhost:${port}\n` +
      `  monitoring: ${defaults.mcpOrigin ?? "https://mcp.beta.id.ai (default)"}\n`,
  );
});
