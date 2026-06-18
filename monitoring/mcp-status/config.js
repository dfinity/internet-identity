// Configuration and target resolution for the IMCP (IC MCP) status dashboard.
//
// The dashboard is environment-agnostic: by default it monitors the beta
// deployment (mcp.beta.id.ai ↔ beta.id.ai) but any MCP origin / II origin
// pair can be passed via CLI flags, environment variables, or query string.

/** Default MCP server origin to monitor. */
export const DEFAULT_MCP_ORIGIN = "https://mcp.beta.id.ai";

/** Per-probe network timeout in milliseconds. */
export const DEFAULT_TIMEOUT_MS = 10_000;

/**
 * Derive the Internet Identity origin an MCP server is expected to be paired
 * with, using the naming convention `mcp.<env>.id.ai` ↔ `<env>.id.ai`
 * (e.g. `mcp.beta.id.ai` → `beta.id.ai`, `mcp.id.ai` → `id.ai`).
 *
 * This is only a best-effort default; the true pairing is confirmed at runtime
 * by checking the II instance's `form-action` CSP directive (see checks.js) and,
 * when possible, by following the MCP server's `/oauth/authorize` redirect.
 *
 * @param {string} mcpOrigin
 * @returns {string | undefined}
 */
export const deriveIiOrigin = (mcpOrigin) => {
  try {
    const url = new URL(mcpOrigin);
    const host = url.host;
    if (host.startsWith("mcp.")) {
      return `${url.protocol}//${host.slice("mcp.".length)}`;
    }
    return undefined;
  } catch {
    return undefined;
  }
};

/**
 * Normalise an origin string (strip trailing slash, lower-case host) or throw.
 * @param {string} value
 * @returns {string}
 */
export const normaliseOrigin = (value) => {
  const url = new URL(value);
  if (url.pathname !== "/" && url.pathname !== "") {
    throw new Error(`Expected an origin (no path), got: ${value}`);
  }
  return url.origin;
};

/**
 * Resolve the effective configuration from explicit overrides, falling back to
 * environment variables and finally the built-in defaults.
 *
 * @param {{ mcpOrigin?: string, iiOrigin?: string, timeoutMs?: number }} [overrides]
 * @returns {{ mcpOrigin: string, iiOrigin: string | undefined, iiOriginSource: "explicit" | "derived" | "none", timeoutMs: number }}
 */
export const resolveConfig = (overrides = {}) => {
  const mcpOrigin = normaliseOrigin(
    overrides.mcpOrigin ?? process.env.MCP_ORIGIN ?? DEFAULT_MCP_ORIGIN,
  );

  const explicitIi = overrides.iiOrigin ?? process.env.II_ORIGIN;
  let iiOrigin;
  /** @type {"explicit" | "derived" | "none"} */
  let iiOriginSource;
  if (explicitIi) {
    iiOrigin = normaliseOrigin(explicitIi);
    iiOriginSource = "explicit";
  } else {
    iiOrigin = deriveIiOrigin(mcpOrigin);
    iiOriginSource = iiOrigin ? "derived" : "none";
  }

  const timeoutMs =
    overrides.timeoutMs ??
    (process.env.MCP_STATUS_TIMEOUT_MS
      ? Number(process.env.MCP_STATUS_TIMEOUT_MS)
      : DEFAULT_TIMEOUT_MS);

  return { mcpOrigin, iiOrigin, iiOriginSource, timeoutMs };
};
