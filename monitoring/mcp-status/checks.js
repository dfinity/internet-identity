// Health probing logic for the IMCP (IC MCP) status dashboard.
//
// All probes run server-side (Node), which sidesteps the CORS restrictions that
// would block a browser from reading the MCP server's `/mcp` 401 challenge, its
// HTML landing page, or the Internet Identity instance's CSP header.
//
// The module is dependency-free: it uses the global `fetch`, `node:tls` for
// certificate inspection, and exports a single `runDashboard(config)` entry
// point that returns a fully structured, JSON-serialisable report.

import tls from "node:tls";
import { deriveIiOrigin, isAllowedOrigin, resolveConfig } from "./config.js";

/**
 * @typedef {"pass" | "warn" | "fail"} Status
 *
 * @typedef {Object} CheckResult
 * @property {string} id
 * @property {string} label
 * @property {string} target        Human-readable target (method + url).
 * @property {string} expected      What a healthy server should return.
 * @property {Status} status
 * @property {number | null} httpStatus
 * @property {number | null} latencyMs
 * @property {string} detail        What was actually observed.
 *
 * @typedef {Object} Section
 * @property {string} id
 * @property {string} title
 * @property {Status} status
 * @property {CheckResult[]} checks
 *
 * @typedef {Object} DashboardReport
 * @property {string} generatedAt
 * @property {{ mcpOrigin: string, iiOrigin: string | undefined, iiOriginSource: string }} targets
 * @property {Status} overall
 * @property {Section[]} sections
 * @property {Record<string, unknown>} facts
 * @property {string[]} suggestions
 */

const STATUS_RANK = { pass: 0, warn: 1, fail: 2 };

/**
 * Aggregate a list of statuses into the worst (most severe) one.
 * @param {Status[]} statuses
 * @returns {Status}
 */
export const worstStatus = (statuses) =>
  statuses.reduce(
    (acc, s) => (STATUS_RANK[s] > STATUS_RANK[acc] ? s : acc),
    /** @type {Status} */ ("pass"),
  );

/**
 * Perform an HTTP request with a timeout, capturing status, headers, body and
 * latency without ever throwing (network errors are returned as `error`).
 *
 * @param {string} url
 * @param {RequestInit & { timeoutMs?: number }} [init]
 */
const probe = async (url, init = {}) => {
  const { timeoutMs = 10_000, ...rest } = init;
  const start = Date.now();
  try {
    const res = await fetch(url, {
      redirect: "manual",
      ...rest,
      signal: AbortSignal.timeout(timeoutMs),
    });
    const bodyText = await res.text().catch(() => "");
    return {
      ok: true,
      status: res.status,
      headers: res.headers,
      bodyText,
      latencyMs: Date.now() - start,
      error: /** @type {Error | null} */ (null),
    };
  } catch (err) {
    return {
      ok: false,
      status: /** @type {number | null} */ (null),
      headers: new Headers(),
      bodyText: "",
      latencyMs: Date.now() - start,
      error: /** @type {Error} */ (err),
    };
  }
};

/** Safely JSON-parse a string, returning undefined on failure. */
const tryJson = (text) => {
  try {
    return JSON.parse(text);
  } catch {
    return undefined;
  }
};

/**
 * Inspect the TLS certificate of an https origin and report days-to-expiry.
 * @param {string} origin
 * @param {number} timeoutMs
 * @returns {Promise<{ validTo: string, daysRemaining: number } | { error: string }>}
 */
const inspectCertificate = (origin, timeoutMs) =>
  new Promise((resolve) => {
    let settled = false;
    const done = (value) => {
      if (settled) return;
      settled = true;
      try {
        socket.destroy();
      } catch {
        /* noop */
      }
      resolve(value);
    };
    let host;
    try {
      host = new URL(origin).hostname;
    } catch {
      return done({ error: "invalid origin" });
    }
    const socket = tls.connect(
      { host, port: 443, servername: host, timeout: timeoutMs },
      () => {
        const cert = socket.getPeerCertificate();
        if (!cert || !cert.valid_to) {
          return done({ error: "no peer certificate" });
        }
        const validTo = new Date(cert.valid_to);
        const daysRemaining = Math.floor(
          (validTo.getTime() - Date.now()) / 86_400_000,
        );
        done({ validTo: validTo.toISOString(), daysRemaining });
      },
    );
    socket.on("error", (e) => done({ error: e.message }));
    socket.on("timeout", () => done({ error: "tls timeout" }));
  });

/**
 * Parse a single directive (e.g. "form-action") out of a CSP header value.
 * @param {string | null} csp
 * @param {string} directive
 * @returns {string[] | undefined} the directive's sources, or undefined if absent
 */
export const parseCspDirective = (csp, directive) => {
  if (!csp) return undefined;
  for (const part of csp.split(";")) {
    const tokens = part.trim().split(/\s+/);
    if (tokens[0] === directive) return tokens.slice(1);
  }
  return undefined;
};

// ---------------------------------------------------------------------------
// Section 1 — MCP server: advertised endpoints respond with correct codes
// ---------------------------------------------------------------------------

/**
 * @param {string} mcpOrigin
 * @param {number} timeoutMs
 * @returns {Promise<{ section: Section, facts: Record<string, unknown> }>}
 */
export const checkMcpEndpoints = async (mcpOrigin, timeoutMs) => {
  /** @type {CheckResult[]} */
  const checks = [];
  /** @type {Record<string, unknown>} */
  const facts = { origin: mcpOrigin };

  // 1. Landing page.
  {
    const r = await probe(`${mcpOrigin}/`, { timeoutMs });
    const ct = r.headers.get("content-type") ?? "";
    const pass = r.ok && r.status === 200 && /text\/html/i.test(ct);
    checks.push({
      id: "root",
      label: "Landing page",
      target: `GET ${mcpOrigin}/`,
      expected: "200 text/html",
      status: pass ? "pass" : "fail",
      httpStatus: r.status,
      latencyMs: r.latencyMs,
      detail: r.error
        ? `request failed: ${r.error.message}`
        : `${r.status}, content-type: ${ct || "(none)"}`,
    });
  }

  // 2. OAuth Protected Resource Metadata (RFC 9728).
  let protectedResource;
  {
    const url = `${mcpOrigin}/.well-known/oauth-protected-resource`;
    const r = await probe(url, { timeoutMs });
    protectedResource = tryJson(r.bodyText);
    const hasFields =
      protectedResource &&
      Array.isArray(protectedResource.authorization_servers) &&
      typeof protectedResource.resource === "string";
    const resourceOk =
      hasFields && protectedResource.resource === `${mcpOrigin}/mcp`;
    const pass = r.ok && r.status === 200 && hasFields;
    facts.protectedResource = protectedResource;
    checks.push({
      id: "protected-resource",
      label: "OAuth Protected Resource Metadata",
      target: `GET ${url}`,
      expected: "200 JSON with authorization_servers + resource",
      status: pass ? (resourceOk ? "pass" : "warn") : "fail",
      httpStatus: r.status,
      latencyMs: r.latencyMs,
      detail: !pass
        ? r.error
          ? `request failed: ${r.error.message}`
          : `${r.status}, missing required fields`
        : resourceOk
          ? `resource=${protectedResource.resource}, AS=${protectedResource.authorization_servers.join(", ")}`
          : `resource=${protectedResource.resource} (expected ${mcpOrigin}/mcp)`,
    });
  }

  // 3. OAuth Authorization Server Metadata (RFC 8414).
  let asMeta;
  {
    const url = `${mcpOrigin}/.well-known/oauth-authorization-server`;
    const r = await probe(url, { timeoutMs });
    asMeta = tryJson(r.bodyText);
    const required = [
      "issuer",
      "authorization_endpoint",
      "token_endpoint",
      "registration_endpoint",
    ];
    const missing = asMeta
      ? required.filter((k) => typeof asMeta[k] !== "string")
      : required;
    const pass = r.ok && r.status === 200 && missing.length === 0;
    facts.authorizationServer = asMeta;
    checks.push({
      id: "as-metadata",
      label: "OAuth Authorization Server Metadata",
      target: `GET ${url}`,
      expected: "200 JSON with issuer + authorize/token/register endpoints",
      status: pass ? "pass" : "fail",
      httpStatus: r.status,
      latencyMs: r.latencyMs,
      detail: !pass
        ? r.error
          ? `request failed: ${r.error.message}`
          : `${r.status}, missing fields: ${missing.join(", ") || "n/a"}`
        : `issuer=${asMeta.issuer}, PKCE=${(asMeta.code_challenge_methods_supported || []).join(",") || "none"}`,
    });
  }

  // 3b. Cross-consistency of the two discovery documents.
  {
    const issuer = asMeta?.issuer;
    const asList = protectedResource?.authorization_servers;
    const consistent =
      typeof issuer === "string" &&
      Array.isArray(asList) &&
      asList.includes(issuer) &&
      issuer === mcpOrigin;
    checks.push({
      id: "metadata-consistency",
      label: "Discovery documents are self-consistent",
      target: "oauth-protected-resource ↔ oauth-authorization-server",
      expected: "issuer === origin and listed as authorization_server",
      status: consistent ? "pass" : "warn",
      httpStatus: null,
      latencyMs: null,
      detail: consistent
        ? `issuer ${issuer} matches advertised authorization_servers`
        : `issuer=${issuer ?? "?"}, authorization_servers=${JSON.stringify(asList ?? null)}`,
    });
  }

  // 4. The MCP endpoint must answer an unauthenticated call with a 401 + a
  //    standards-compliant WWW-Authenticate challenge pointing at the resource
  //    metadata. This is the contract MCP clients rely on to discover auth.
  {
    const url = `${mcpOrigin}/mcp`;
    const r = await probe(url, {
      timeoutMs,
      method: "POST",
      headers: {
        "content-type": "application/json",
        accept: "application/json, text/event-stream",
      },
      body: JSON.stringify({
        jsonrpc: "2.0",
        id: 1,
        method: "initialize",
        params: {
          protocolVersion: "2025-06-18",
          capabilities: {},
          clientInfo: { name: "imcp-status-dashboard", version: "1.0.0" },
        },
      }),
    });
    const wwwAuth = r.headers.get("www-authenticate") ?? "";
    const expectedMetadata = `${mcpOrigin}/.well-known/oauth-protected-resource`;
    const challengeOk =
      r.status === 401 &&
      /bearer/i.test(wwwAuth) &&
      wwwAuth.includes(expectedMetadata);
    checks.push({
      id: "mcp-challenge",
      label: "MCP endpoint OAuth challenge",
      target: `POST ${url} (no token)`,
      expected: `401 + WWW-Authenticate: Bearer resource_metadata="${expectedMetadata}"`,
      status: challengeOk ? "pass" : r.status === 401 ? "warn" : "fail",
      httpStatus: r.status,
      latencyMs: r.latencyMs,
      detail: r.error
        ? `request failed: ${r.error.message}`
        : `${r.status}, www-authenticate: ${wwwAuth || "(missing)"}`,
    });
  }

  // 5. Dynamic Client Registration (RFC 7591) must mint a client_id.
  {
    const url = `${mcpOrigin}/oauth/register`;
    const r = await probe(url, {
      timeoutMs,
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({
        client_name: "imcp-status-dashboard",
        redirect_uris: ["https://example.org/callback"],
        token_endpoint_auth_method: "none",
        grant_types: ["authorization_code"],
        response_types: ["code"],
      }),
    });
    const json = tryJson(r.bodyText);
    const pass =
      (r.status === 200 || r.status === 201) &&
      json &&
      typeof json.client_id === "string";
    checks.push({
      id: "oauth-register",
      label: "OAuth Dynamic Client Registration",
      target: `POST ${url}`,
      expected: "200/201 JSON with client_id",
      status: pass ? "pass" : "fail",
      httpStatus: r.status,
      latencyMs: r.latencyMs,
      detail: r.error
        ? `request failed: ${r.error.message}`
        : pass
          ? `registered client_id=${json.client_id}`
          : `${r.status}, body: ${r.bodyText.slice(0, 120)}`,
    });
  }

  // 6. Authorization endpoint liveness: rejects malformed input with 4xx
  //    (rather than 5xx / connection error). It is interactive, so we only
  //    assert it is alive and validating, not a full successful redirect.
  {
    const url = `${mcpOrigin}/oauth/authorize`;
    const r = await probe(url, { timeoutMs });
    const alive = r.ok && r.status >= 400 && r.status < 500;
    checks.push({
      id: "oauth-authorize",
      label: "OAuth Authorization endpoint liveness",
      target: `GET ${url} (no params)`,
      expected: "4xx (validates input; does not 5xx / hang)",
      status: alive ? "pass" : "fail",
      httpStatus: r.status,
      latencyMs: r.latencyMs,
      detail: r.error
        ? `request failed: ${r.error.message}`
        : `${r.status}, ${r.bodyText.slice(0, 100)}`,
    });
  }

  // 7. Token endpoint liveness: a bogus grant must be rejected with 400.
  {
    const url = `${mcpOrigin}/oauth/token`;
    const r = await probe(url, {
      timeoutMs,
      method: "POST",
      headers: { "content-type": "application/x-www-form-urlencoded" },
      body: "grant_type=authorization_code&code=invalid&code_verifier=x&client_id=x",
    });
    const json = tryJson(r.bodyText);
    const alive = r.status === 400 && json && typeof json.error === "string";
    checks.push({
      id: "oauth-token",
      label: "OAuth Token endpoint liveness",
      target: `POST ${url} (invalid grant)`,
      expected: "400 with OAuth error (e.g. invalid_grant)",
      status: alive ? "pass" : r.status === 400 ? "warn" : "fail",
      httpStatus: r.status,
      latencyMs: r.latencyMs,
      detail: r.error
        ? `request failed: ${r.error.message}`
        : `${r.status}, error: ${json?.error ?? r.bodyText.slice(0, 80)}`,
    });
  }

  // TLS certificate freshness for the MCP host.
  {
    const cert = await inspectCertificate(mcpOrigin, timeoutMs);
    facts.mcpCertificate = cert;
    if ("error" in cert) {
      checks.push({
        id: "mcp-tls",
        label: "TLS certificate",
        target: mcpOrigin,
        expected: "valid certificate, > 21 days remaining",
        status: "warn",
        httpStatus: null,
        latencyMs: null,
        detail: `could not inspect certificate: ${cert.error}`,
      });
    } else {
      checks.push({
        id: "mcp-tls",
        label: "TLS certificate",
        target: mcpOrigin,
        expected: "valid certificate, > 21 days remaining",
        status:
          cert.daysRemaining < 0
            ? "fail"
            : cert.daysRemaining < 21
              ? "warn"
              : "pass",
        httpStatus: null,
        latencyMs: null,
        detail: `expires ${cert.validTo} (${cert.daysRemaining} days remaining)`,
      });
    }
  }

  return {
    section: {
      id: "endpoints",
      title: "MCP server endpoints",
      status: worstStatus(checks.map((c) => c.status)),
      checks,
    },
    facts,
  };
};

// ---------------------------------------------------------------------------
// Section 2 — Which II instance is the MCP server linked to?
// ---------------------------------------------------------------------------

/**
 * Best-effort discovery of the II origin the MCP server redirects to during the
 * OAuth authorization flow. Registers a throwaway client and inspects the
 * `Location` header of `/oauth/authorize`. Returns `undefined` if the server
 * does not (yet) issue a cross-origin redirect we can read headlessly.
 *
 * @param {string} mcpOrigin
 * @param {number} timeoutMs
 * @returns {Promise<{ iiOrigin?: string, detail: string }>}
 */
export const discoverIiViaAuthorize = async (mcpOrigin, timeoutMs) => {
  const redirectUri = "https://example.org/callback";
  const reg = await probe(`${mcpOrigin}/oauth/register`, {
    timeoutMs,
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify({
      client_name: "imcp-status-dashboard",
      redirect_uris: [redirectUri],
      token_endpoint_auth_method: "none",
      grant_types: ["authorization_code"],
      response_types: ["code"],
    }),
  });
  const client = tryJson(reg.bodyText);
  if (!client?.client_id) {
    return { detail: `client registration failed (HTTP ${reg.status})` };
  }
  const params = new URLSearchParams({
    response_type: "code",
    client_id: client.client_id,
    redirect_uri: redirectUri,
    code_challenge: "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM",
    code_challenge_method: "S256",
    state: "imcp-status-probe",
  });
  const r = await probe(`${mcpOrigin}/oauth/authorize?${params}`, {
    timeoutMs,
  });
  const location = r.headers.get("location");
  if (r.status >= 300 && r.status < 400 && location) {
    try {
      const target = new URL(location, mcpOrigin);
      if (target.origin !== mcpOrigin) {
        // Only adopt a discovered origin if it is on the allowlist; otherwise a
        // monitored server could redirect us into probing an arbitrary host.
        if (!isAllowedOrigin(target.origin)) {
          return {
            detail: `/oauth/authorize redirects to ${target.origin} (not on the allowlist; ignored)`,
          };
        }
        return {
          iiOrigin: target.origin,
          detail: `/oauth/authorize redirects to ${target.origin}`,
        };
      }
      return { detail: `/oauth/authorize redirects within ${mcpOrigin}` };
    } catch {
      return { detail: `unparseable redirect target: ${location}` };
    }
  }
  return {
    detail: `no cross-origin redirect from /oauth/authorize (HTTP ${r.status})`,
  };
};

/**
 * @param {string} mcpOrigin
 * @param {string | undefined} configuredIi
 * @param {string} iiOriginSource
 * @param {number} timeoutMs
 * @returns {Promise<{ section: Section, iiOrigin: string | undefined, facts: Record<string, unknown> }>}
 */
export const checkLinkage = async (
  mcpOrigin,
  configuredIi,
  iiOriginSource,
  timeoutMs,
) => {
  /** @type {CheckResult[]} */
  const checks = [];
  /** @type {Record<string, unknown>} */
  const facts = {};

  const discovery = await discoverIiViaAuthorize(mcpOrigin, timeoutMs);
  facts.authorizeDiscovery = discovery;

  // Resolve the II origin we will health-check: prefer a live-discovered one,
  // then an explicitly configured one, then the naming-convention default.
  const iiOrigin =
    discovery.iiOrigin ?? configuredIi ?? deriveIiOrigin(mcpOrigin);
  const resolvedSource = discovery.iiOrigin
    ? "discovered via /oauth/authorize redirect"
    : iiOriginSource === "explicit"
      ? "explicitly configured"
      : iiOriginSource === "derived"
        ? "derived from naming convention (mcp.<env>.id.ai → <env>.id.ai)"
        : "unknown";

  checks.push({
    id: "ii-target",
    label: "Linked Internet Identity instance",
    target: mcpOrigin,
    expected: "a resolvable II origin",
    status: iiOrigin ? "pass" : "fail",
    httpStatus: null,
    latencyMs: null,
    detail: iiOrigin
      ? `${iiOrigin} (${resolvedSource})`
      : "could not resolve a linked II origin",
  });

  checks.push({
    id: "ii-discovery",
    label: "Live link discovery via OAuth authorize",
    target: `${mcpOrigin}/oauth/authorize`,
    expected: "302 redirect to the II /mcp delegation page",
    // Inconclusive discovery is informational, not a failure: the MCP server
    // performs the redirect only after interactive client setup.
    status: discovery.iiOrigin ? "pass" : "warn",
    httpStatus: null,
    latencyMs: null,
    detail: discovery.detail,
  });

  return {
    section: {
      id: "linkage",
      title: "Linked Internet Identity instance",
      status: worstStatus(checks.map((c) => c.status)),
      checks,
    },
    iiOrigin,
    facts,
  };
};

// ---------------------------------------------------------------------------
// Section 3 — Is the linked II healthy, and does it recognise this MCP server?
// ---------------------------------------------------------------------------

/**
 * @param {string | undefined} iiOrigin
 * @param {string} mcpOrigin
 * @param {number} timeoutMs
 * @returns {Promise<{ section: Section, facts: Record<string, unknown> }>}
 */
export const checkIiHealth = async (iiOrigin, mcpOrigin, timeoutMs) => {
  /** @type {CheckResult[]} */
  const checks = [];
  /** @type {Record<string, unknown>} */
  const facts = {};

  if (!iiOrigin) {
    checks.push({
      id: "ii-unresolved",
      label: "Internet Identity health",
      target: "(unknown)",
      expected: "a resolved II origin to probe",
      status: "fail",
      httpStatus: null,
      latencyMs: null,
      detail: "no II origin resolved; cannot assess health",
    });
    return {
      section: {
        id: "ii-health",
        title: "Internet Identity health & recognition",
        status: "fail",
        checks,
      },
      facts,
    };
  }

  facts.origin = iiOrigin;
  const r = await probe(`${iiOrigin}/`, { timeoutMs });
  const csp = r.headers.get("content-security-policy");
  const canisterId = r.headers.get("x-ic-canister-id");
  const icCertificate = r.headers.get("ic-certificate");
  facts.canisterId = canisterId;

  // 1. Frontend reachability.
  checks.push({
    id: "ii-reachable",
    label: "II frontend reachable",
    target: `GET ${iiOrigin}/`,
    expected: "200",
    status: r.ok && r.status === 200 ? "pass" : "fail",
    httpStatus: r.status,
    latencyMs: r.latencyMs,
    detail: r.error
      ? `request failed: ${r.error.message}`
      : `${r.status}${canisterId ? `, canister ${canisterId}` : ""}`,
  });

  // 2. Served & certified by the Internet Computer (canister is live).
  checks.push({
    id: "ii-certified",
    label: "IC-certified response (canister live)",
    target: `${iiOrigin}/`,
    expected: "ic-certificate header present",
    status: icCertificate ? "pass" : "warn",
    httpStatus: r.status,
    latencyMs: r.latencyMs,
    detail: icCertificate
      ? `ic-certificate present${canisterId ? ` for canister ${canisterId}` : ""}`
      : "no ic-certificate header (response not certified by the IC?)",
  });

  // 3. Recognition: the II's mcp_server_origin config is reflected verbatim
  //    into the response CSP `form-action` directive. If our MCP origin is
  //    listed, the II trusts it and the /mcp delegation flow will accept its
  //    callbacks; if not, the flow is disabled for this MCP server.
  const formAction = parseCspDirective(csp, "form-action");
  facts.formAction = formAction;
  const recognised = !!formAction && formAction.includes(mcpOrigin);
  checks.push({
    id: "ii-recognises-mcp",
    label: "II recognises this MCP server",
    target: `${iiOrigin} CSP form-action`,
    expected: `form-action contains ${mcpOrigin}`,
    status: recognised ? "pass" : "fail",
    httpStatus: null,
    latencyMs: null,
    detail: formAction
      ? recognised
        ? `form-action lists ${mcpOrigin} → /mcp delegation enabled`
        : `form-action = [${formAction.join(", ")}] (does NOT include ${mcpOrigin})`
      : "no form-action directive found in CSP",
  });

  // 4. Report the II's configured related origins (context, not pass/fail).
  const frameAncestors = parseCspDirective(csp, "frame-ancestors");
  const relatedOrigins = (frameAncestors ?? []).filter((o) =>
    o.startsWith("http"),
  );
  facts.relatedOrigins = relatedOrigins;
  checks.push({
    id: "ii-related-origins",
    label: "II related origins",
    target: `${iiOrigin} CSP frame-ancestors`,
    expected: "the II's alternative front-end origins",
    status: relatedOrigins.length > 0 ? "pass" : "warn",
    httpStatus: null,
    latencyMs: null,
    detail:
      relatedOrigins.length > 0
        ? relatedOrigins.join(", ")
        : "no related origins advertised",
  });

  // 5. TLS certificate freshness for the II host.
  const cert = await inspectCertificate(iiOrigin, timeoutMs);
  facts.certificate = cert;
  if ("error" in cert) {
    checks.push({
      id: "ii-tls",
      label: "TLS certificate",
      target: iiOrigin,
      expected: "valid certificate, > 21 days remaining",
      status: "warn",
      httpStatus: null,
      latencyMs: null,
      detail: `could not inspect certificate: ${cert.error}`,
    });
  } else {
    checks.push({
      id: "ii-tls",
      label: "TLS certificate",
      target: iiOrigin,
      expected: "valid certificate, > 21 days remaining",
      status:
        cert.daysRemaining < 0
          ? "fail"
          : cert.daysRemaining < 21
            ? "warn"
            : "pass",
      httpStatus: null,
      latencyMs: null,
      detail: `expires ${cert.validTo} (${cert.daysRemaining} days remaining)`,
    });
  }

  return {
    section: {
      id: "ii-health",
      title: "Internet Identity health & recognition",
      status: worstStatus(checks.map((c) => c.status)),
      checks,
    },
    facts,
  };
};

// ---------------------------------------------------------------------------
// Suggestions — actionable, partly derived from the live findings
// ---------------------------------------------------------------------------

/**
 * @param {Section[]} sections
 * @param {Record<string, unknown>} facts
 * @returns {string[]}
 */
export const buildSuggestions = (sections, facts) => {
  const suggestions = [];
  const checkById = {};
  for (const s of sections) for (const c of s.checks) checkById[c.id] = c;

  if (checkById["ii-recognises-mcp"]?.status === "fail") {
    suggestions.push(
      "The linked II does not list this MCP origin in its CSP form-action. " +
        "The /mcp delegation flow will reject callbacks until the II canister " +
        "is (re)deployed with mcp_server_origin set to this exact origin.",
    );
  }
  if (checkById["mcp-challenge"]?.status !== "pass") {
    suggestions.push(
      "The unauthenticated /mcp response should be a 401 carrying " +
        'WWW-Authenticate: Bearer resource_metadata="…/.well-known/oauth-protected-resource". ' +
        "MCP clients rely on this header to discover the authorization server.",
    );
  }
  if (checkById["ii-discovery"]?.status !== "pass") {
    suggestions.push(
      "The MCP→II link could not be confirmed live via /oauth/authorize " +
        "(no readable cross-origin redirect headlessly). Consider exposing the " +
        "configured II origin in the MCP server metadata so the pairing is " +
        "independently verifiable, not just inferred by naming convention.",
    );
  }
  // The catch-all returns 401 for unknown paths, so uptime monitors can't use a
  // plain GET. Recommend a dedicated unauthenticated liveness endpoint.
  suggestions.push(
    "Add an unauthenticated GET /healthz (or /livez) that returns 200. " +
      "Unknown paths currently fall through to the OAuth 401 catch-all, so " +
      "external uptime monitors have no clean liveness probe.",
  );
  suggestions.push(
    "POST /oauth/register accepts anonymous dynamic client registration. " +
      "Ensure it is rate-limited and that stale/unused clients are pruned to " +
      "avoid unbounded growth, and that registrations are shared across all " +
      "server replicas (a freshly registered client_id was not immediately " +
      "usable at /oauth/authorize during probing).",
  );

  const certWarn = [facts?.mcp, facts?.ii]
    .map((f) => /** @type {any} */ (f)?.certificate ?? f?.mcpCertificate)
    .filter((c) => c && typeof c.daysRemaining === "number" && c.daysRemaining < 21);
  if (certWarn.length > 0) {
    suggestions.push(
      "A TLS certificate is within 21 days of expiry — verify automatic renewal.",
    );
  }

  suggestions.push(
    "Wire this dashboard into alerting: run `node monitoring/mcp-status/cli.js " +
      "--json` on a schedule (cron/CI) and page on a non-zero exit code, and/or " +
      "host `server.js` behind your status page. Track per-endpoint latency over " +
      "time to catch slow degradations before they become outages.",
  );

  return suggestions;
};

// ---------------------------------------------------------------------------
// Orchestration
// ---------------------------------------------------------------------------

/**
 * Run the full dashboard against the resolved configuration.
 * @param {{ mcpOrigin?: string, iiOrigin?: string, timeoutMs?: number }} [overrides]
 * @returns {Promise<DashboardReport>}
 */
export const runDashboard = async (overrides = {}) => {
  const cfg = resolveConfig(overrides);

  const endpoints = await checkMcpEndpoints(cfg.mcpOrigin, cfg.timeoutMs);
  const linkage = await checkLinkage(
    cfg.mcpOrigin,
    cfg.iiOrigin,
    cfg.iiOriginSource,
    cfg.timeoutMs,
  );
  const iiHealth = await checkIiHealth(
    linkage.iiOrigin,
    cfg.mcpOrigin,
    cfg.timeoutMs,
  );

  const sections = [endpoints.section, linkage.section, iiHealth.section];
  const facts = {
    mcp: endpoints.facts,
    linkage: linkage.facts,
    ii: iiHealth.facts,
  };

  return {
    generatedAt: new Date().toISOString(),
    targets: {
      mcpOrigin: cfg.mcpOrigin,
      iiOrigin: linkage.iiOrigin,
      iiOriginSource: cfg.iiOriginSource,
    },
    overall: worstStatus(sections.map((s) => s.status)),
    sections,
    facts,
    suggestions: buildSuggestions(sections, facts),
  };
};
