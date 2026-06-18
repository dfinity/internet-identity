#!/usr/bin/env node
// CLI entry point for the IMCP status dashboard.
//
// Usage:
//   node monitoring/mcp-status/cli.js [options]
//
// Options:
//   --mcp <origin>     MCP server origin to monitor (default https://mcp.beta.id.ai)
//   --ii <origin>      Internet Identity origin (default: derived from the MCP origin)
//   --timeout <ms>     Per-probe timeout in milliseconds (default 10000)
//   --json             Emit the raw JSON report instead of the text view
//   --no-color         Disable ANSI colours
//   --strict           Exit non-zero on warnings as well as failures
//   -h, --help         Show this help
//
// Exit code: 0 = healthy, 1 = failures (or warnings with --strict), 2 = usage error.

import { runDashboard } from "./checks.js";
import { renderText } from "./report.js";

const parseArgs = (argv) => {
  /** @type {Record<string, string | boolean>} */
  const opts = {};
  for (let i = 0; i < argv.length; i++) {
    const arg = argv[i];
    switch (arg) {
      case "--mcp":
        opts.mcp = argv[++i];
        break;
      case "--ii":
        opts.ii = argv[++i];
        break;
      case "--timeout":
        opts.timeout = argv[++i];
        break;
      case "--json":
        opts.json = true;
        break;
      case "--no-color":
        opts.noColor = true;
        break;
      case "--strict":
        opts.strict = true;
        break;
      case "-h":
      case "--help":
        opts.help = true;
        break;
      default:
        throw new Error(`Unknown argument: ${arg}`);
    }
  }
  return opts;
};

const HELP = `IMCP (IC MCP) status dashboard

Usage: node monitoring/mcp-status/cli.js [options]

  --mcp <origin>   MCP server origin (default https://mcp.beta.id.ai)
  --ii <origin>    Internet Identity origin (default: derived from --mcp)
  --timeout <ms>   Per-probe timeout (default 10000)
  --json           Emit raw JSON instead of the text report
  --no-color       Disable ANSI colours
  --strict         Exit non-zero on warnings as well as failures
  -h, --help       Show this help
`;

const main = async () => {
  let opts;
  try {
    opts = parseArgs(process.argv.slice(2));
  } catch (e) {
    process.stderr.write(`${e.message}\n\n${HELP}`);
    process.exit(2);
  }

  if (opts.help) {
    process.stdout.write(HELP);
    return;
  }

  const report = await runDashboard({
    mcpOrigin: typeof opts.mcp === "string" ? opts.mcp : undefined,
    iiOrigin: typeof opts.ii === "string" ? opts.ii : undefined,
    timeoutMs: opts.timeout ? Number(opts.timeout) : undefined,
  });

  if (opts.json) {
    process.stdout.write(`${JSON.stringify(report, null, 2)}\n`);
  } else {
    const noColor = opts.noColor || !process.stdout.isTTY;
    process.stdout.write(renderText(report, { color: !noColor }));
  }

  const failed = report.overall === "fail";
  const warned = report.overall === "warn";
  process.exit(failed || (warned && opts.strict) ? 1 : 0);
};

main().catch((e) => {
  process.stderr.write(`Unexpected error: ${e?.stack ?? e}\n`);
  process.exit(1);
});
