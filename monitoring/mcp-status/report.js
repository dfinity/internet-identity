// Plain-text / ANSI rendering of a dashboard report for the CLI.

const ANSI = {
  reset: "\x1b[0m",
  bold: "\x1b[1m",
  dim: "\x1b[2m",
  green: "\x1b[32m",
  yellow: "\x1b[33m",
  red: "\x1b[31m",
  cyan: "\x1b[36m",
};

const ICON = { pass: "✔", warn: "▲", fail: "✘" };

/**
 * @param {import("./checks.js").DashboardReport} report
 * @param {{ color?: boolean }} [opts]
 * @returns {string}
 */
export const renderText = (report, opts = {}) => {
  const color = opts.color ?? true;
  const c = (code, text) => (color ? `${code}${text}${ANSI.reset}` : text);
  const statusColor = { pass: ANSI.green, warn: ANSI.yellow, fail: ANSI.red };
  const tag = (status) =>
    c(statusColor[status], `${ICON[status]} ${status.toUpperCase()}`);

  const lines = [];
  lines.push("");
  lines.push(c(ANSI.bold, "IMCP (IC MCP) Status Dashboard"));
  lines.push(
    c(ANSI.dim, `Generated: ${report.generatedAt}`),
  );
  lines.push(
    `MCP server: ${c(ANSI.cyan, report.targets.mcpOrigin)}   ` +
      `II instance: ${c(ANSI.cyan, report.targets.iiOrigin ?? "(unresolved)")}`,
  );
  lines.push(`Overall: ${tag(report.overall)}`);

  for (const section of report.sections) {
    lines.push("");
    lines.push(`${tag(section.status)}  ${c(ANSI.bold, section.title)}`);
    for (const check of section.checks) {
      const latency =
        check.latencyMs != null ? c(ANSI.dim, ` (${check.latencyMs}ms)`) : "";
      lines.push(`  ${tag(check.status)}  ${check.label}${latency}`);
      lines.push(c(ANSI.dim, `      ${check.target}`));
      lines.push(`      ${check.detail}`);
    }
  }

  if (report.suggestions.length > 0) {
    lines.push("");
    lines.push(c(ANSI.bold, "Suggestions"));
    for (const s of report.suggestions) {
      lines.push(`  ${c(ANSI.cyan, "•")} ${s}`);
    }
  }

  lines.push("");
  return lines.join("\n");
};
