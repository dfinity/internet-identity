// Configuration and target resolution for the IMCP (IC MCP) status dashboard.
//
// The dashboard is environment-agnostic: by default it monitors the beta
// deployment (mcp.beta.id.ai ↔ beta.id.ai) but any allowed MCP origin / II
// origin pair can be passed via CLI flags, environment variables, or query
// string.
//
// Because `server.js` exposes the probes over HTTP with `?mcp=`/`?ii=` query
// overrides, the resolved origins are validated against a host allowlist before
// any request is made. This prevents the dashboard from being used as a
// server-side request forgery (SSRF) proxy against arbitrary or internal hosts.

/** Default MCP server origin to monitor. */
export const DEFAULT_MCP_ORIGIN = "https://mcp.beta.id.ai";

/** Per-probe network timeout in milliseconds. */
export const DEFAULT_TIMEOUT_MS = 10_000;

/**
 * Host suffixes that may be probed. A hostname is allowed when it equals one of
 * these or ends with `.<suffix>`. The list can be extended for other
 * deployments via the `MCP_STATUS_ALLOWED_HOSTS` environment variable
 * (comma-separated), but never narrowed below the built-in id.ai domains.
 */
const DEFAULT_ALLOWED_HOST_SUFFIXES = ["id.ai"];

/** Loopback hosts allowed over http/https for local development and testing. */
const LOOPBACK_HOSTS = new Set(["localhost", "127.0.0.1", "[::1]", "::1"]);

/** @returns {string[]} the effective list of allowed host suffixes. */
const allowedHostSuffixes = () => {
  const extra = (process.env.MCP_STATUS_ALLOWED_HOSTS ?? "")
    .split(",")
    .map((s) => s.trim().toLowerCase())
    .filter(Boolean);
  return [...DEFAULT_ALLOWED_HOST_SUFFIXES, ...extra];
};

/**
 * Whether an origin is allowed to be probed. Only https origins whose hostname
 * is within the allowlist are accepted (plus loopback hosts over http/https for
 * local development). Userinfo, custom ports and non-http schemes are rejected.
 *
 * @param {string} origin
 * @returns {boolean}
 */
export const isAllowedOrigin = (origin) => {
  let url;
  try {
    url = new URL(origin);
  } catch {
    return false;
  }
  if (url.username || url.password) return false;
  const host = url.hostname.toLowerCase();
  if (LOOPBACK_HOSTS.has(host)) {
    return url.protocol === "http:" || url.protocol === "https:";
  }
  if (url.protocol !== "https:") return false;
  return allowedHostSuffixes().some(
    (suffix) => host === suffix || host.endsWith(`.${suffix}`),
  );
};

/**
 * Assert that an origin is allowed to be probed, throwing a sanitised,
 * non-sensitive error otherwise. The thrown error carries
 * `code === "DISALLOWED_ORIGIN"` so callers can map it to a 400 response.
 *
 * @param {string} origin
 * @returns {string} the same origin, when allowed
 */
export const assertAllowedOrigin = (origin) => {
  if (!isAllowedOrigin(origin)) {
    const err = new Error(
      `origin not allowed: ${origin}. Allowed hosts: ${allowedHostSuffixes().join(", ")} (and loopback). Extend with MCP_STATUS_ALLOWED_HOSTS.`,
    );
    // @ts-expect-error augmenting the error with a discriminator code
    err.code = "DISALLOWED_ORIGIN";
    throw err;
  }
  return origin;
};

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
 * environment variables and finally the built-in defaults. All resolved origins
 * are validated against the host allowlist.
 *
 * @param {{ mcpOrigin?: string, iiOrigin?: string, timeoutMs?: number }} [overrides]
 * @returns {{ mcpOrigin: string, iiOrigin: string | undefined, iiOriginSource: "explicit" | "derived" | "none", timeoutMs: number }}
 */
export const resolveConfig = (overrides = {}) => {
  const mcpOrigin = assertAllowedOrigin(
    normaliseOrigin(
      overrides.mcpOrigin ?? process.env.MCP_ORIGIN ?? DEFAULT_MCP_ORIGIN,
    ),
  );

  const explicitIi = overrides.iiOrigin ?? process.env.II_ORIGIN;
  let iiOrigin;
  /** @type {"explicit" | "derived" | "none"} */
  let iiOriginSource;
  if (explicitIi) {
    iiOrigin = assertAllowedOrigin(normaliseOrigin(explicitIi));
    iiOriginSource = "explicit";
  } else {
    iiOrigin = deriveIiOrigin(mcpOrigin);
    // A derived origin is a sub-domain of an already-allowed origin, but
    // validate defensively so the allowlist is the single source of truth.
    if (iiOrigin && !isAllowedOrigin(iiOrigin)) iiOrigin = undefined;
    iiOriginSource = iiOrigin ? "derived" : "none";
  }

  const timeoutMs =
    overrides.timeoutMs ??
    (process.env.MCP_STATUS_TIMEOUT_MS
      ? Number(process.env.MCP_STATUS_TIMEOUT_MS)
      : DEFAULT_TIMEOUT_MS);

  return { mcpOrigin, iiOrigin, iiOriginSource, timeoutMs };
};
