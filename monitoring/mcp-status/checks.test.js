// Unit tests for the IMCP status dashboard probing logic.
// Run with:  node --test monitoring/mcp-status/
//
// These tests stub the global `fetch` so they make no real network calls.
// (TLS certificate inspection targets unresolvable test hostnames and so
// degrades to a "warn" without hitting the network meaningfully.)

import { test } from "node:test";
import assert from "node:assert/strict";
import {
  worstStatus,
  parseCspDirective,
  checkMcpEndpoints,
  checkIiHealth,
  buildSuggestions,
} from "./checks.js";
import { deriveIiOrigin, normaliseOrigin, resolveConfig } from "./config.js";

const byId = (section, id) => section.checks.find((c) => c.id === id);

/** Build a minimal Response-like object honouring the fields checks.js reads. */
const resp = (status, { headers = {}, body = "" } = {}) => ({
  status,
  headers: new Headers(headers),
  text: async () => body,
});

/**
 * Install a fetch stub that dispatches on "METHOD url".
 * @param {Record<string, ReturnType<typeof resp>>} routes
 */
const stubFetch = (routes) => {
  const original = globalThis.fetch;
  globalThis.fetch = async (url, init = {}) => {
    const key = `${init.method ?? "GET"} ${url.split("?")[0]}`;
    if (routes[key]) return routes[key];
    throw new Error(`unexpected fetch: ${key}`);
  };
  return () => {
    globalThis.fetch = original;
  };
};

test("worstStatus picks the most severe status", () => {
  assert.equal(worstStatus(["pass", "pass"]), "pass");
  assert.equal(worstStatus(["pass", "warn", "pass"]), "warn");
  assert.equal(worstStatus(["warn", "fail", "pass"]), "fail");
  assert.equal(worstStatus([]), "pass");
});

test("parseCspDirective extracts a directive's sources", () => {
  const csp =
    "default-src 'none';form-action 'self' http://127.0.0.1:* https://mcp.beta.id.ai;base-uri 'none'";
  assert.deepEqual(parseCspDirective(csp, "form-action"), [
    "'self'",
    "http://127.0.0.1:*",
    "https://mcp.beta.id.ai",
  ]);
  assert.equal(parseCspDirective(csp, "missing-directive"), undefined);
  assert.equal(parseCspDirective(null, "form-action"), undefined);
});

test("deriveIiOrigin strips the mcp. label", () => {
  assert.equal(deriveIiOrigin("https://mcp.beta.id.ai"), "https://beta.id.ai");
  assert.equal(deriveIiOrigin("https://mcp.id.ai"), "https://id.ai");
  assert.equal(deriveIiOrigin("https://example.com"), undefined);
});

test("normaliseOrigin rejects origins with a path", () => {
  assert.equal(normaliseOrigin("https://mcp.beta.id.ai/"), "https://mcp.beta.id.ai");
  assert.throws(() => normaliseOrigin("https://mcp.beta.id.ai/mcp"));
});

test("resolveConfig derives the II origin from the MCP origin", () => {
  const cfg = resolveConfig({ mcpOrigin: "https://mcp.beta.id.ai" });
  assert.equal(cfg.mcpOrigin, "https://mcp.beta.id.ai");
  assert.equal(cfg.iiOrigin, "https://beta.id.ai");
  assert.equal(cfg.iiOriginSource, "derived");
});

test("checkMcpEndpoints passes for a well-behaved server", async () => {
  const origin = "https://mcp.beta.test";
  const restore = stubFetch({
    [`GET ${origin}/`]: resp(200, { headers: { "content-type": "text/html" } }),
    [`GET ${origin}/.well-known/oauth-protected-resource`]: resp(200, {
      headers: { "content-type": "application/json" },
      body: JSON.stringify({
        authorization_servers: [origin],
        resource: `${origin}/mcp`,
      }),
    }),
    [`GET ${origin}/.well-known/oauth-authorization-server`]: resp(200, {
      body: JSON.stringify({
        issuer: origin,
        authorization_endpoint: `${origin}/oauth/authorize`,
        token_endpoint: `${origin}/oauth/token`,
        registration_endpoint: `${origin}/oauth/register`,
        code_challenge_methods_supported: ["S256"],
      }),
    }),
    [`POST ${origin}/mcp`]: resp(401, {
      headers: {
        "www-authenticate": `Bearer resource_metadata="${origin}/.well-known/oauth-protected-resource"`,
      },
      body: JSON.stringify({ error: "invalid_token" }),
    }),
    [`POST ${origin}/oauth/register`]: resp(201, {
      body: JSON.stringify({ client_id: "client-123" }),
    }),
    [`GET ${origin}/oauth/authorize`]: resp(400, { body: "missing client_id" }),
    [`POST ${origin}/oauth/token`]: resp(400, {
      body: JSON.stringify({ error: "invalid_grant" }),
    }),
  });
  try {
    const { section } = await checkMcpEndpoints(origin, 2000);
    assert.equal(byId(section, "root").status, "pass");
    assert.equal(byId(section, "protected-resource").status, "pass");
    assert.equal(byId(section, "as-metadata").status, "pass");
    assert.equal(byId(section, "metadata-consistency").status, "pass");
    assert.equal(byId(section, "mcp-challenge").status, "pass");
    assert.equal(byId(section, "oauth-register").status, "pass");
    assert.equal(byId(section, "oauth-authorize").status, "pass");
    assert.equal(byId(section, "oauth-token").status, "pass");
  } finally {
    restore();
  }
});

test("checkMcpEndpoints flags a missing OAuth challenge", async () => {
  const origin = "https://mcp.beta.test";
  const restore = stubFetch({
    [`GET ${origin}/`]: resp(200, { headers: { "content-type": "text/html" } }),
    [`GET ${origin}/.well-known/oauth-protected-resource`]: resp(200, {
      body: JSON.stringify({ authorization_servers: [origin], resource: `${origin}/mcp` }),
    }),
    [`GET ${origin}/.well-known/oauth-authorization-server`]: resp(200, {
      body: JSON.stringify({
        issuer: origin,
        authorization_endpoint: `${origin}/oauth/authorize`,
        token_endpoint: `${origin}/oauth/token`,
        registration_endpoint: `${origin}/oauth/register`,
      }),
    }),
    // 200 instead of a 401 challenge → wrong contract.
    [`POST ${origin}/mcp`]: resp(200, { body: "{}" }),
    [`POST ${origin}/oauth/register`]: resp(201, {
      body: JSON.stringify({ client_id: "x" }),
    }),
    [`GET ${origin}/oauth/authorize`]: resp(400),
    [`POST ${origin}/oauth/token`]: resp(400, {
      body: JSON.stringify({ error: "invalid_grant" }),
    }),
  });
  try {
    const { section } = await checkMcpEndpoints(origin, 2000);
    assert.equal(byId(section, "mcp-challenge").status, "fail");
  } finally {
    restore();
  }
});

test("checkIiHealth detects MCP recognition via form-action", async () => {
  const ii = "https://beta.test";
  const mcp = "https://mcp.beta.test";
  const restore = stubFetch({
    [`GET ${ii}/`]: resp(200, {
      headers: {
        "x-ic-canister-id": "gjxif-ryaaa-aaaad-ae4ka-cai",
        "ic-certificate": "certificate=:abc:",
        "content-security-policy": `default-src 'none';form-action 'self' http://127.0.0.1:* ${mcp};frame-ancestors 'self' ${ii} https://beta.identity.ic0.app`,
      },
    }),
  });
  try {
    const { section, facts } = await checkIiHealth(ii, mcp, 2000);
    assert.equal(byId(section, "ii-reachable").status, "pass");
    assert.equal(byId(section, "ii-certified").status, "pass");
    assert.equal(byId(section, "ii-recognises-mcp").status, "pass");
    assert.equal(facts.canisterId, "gjxif-ryaaa-aaaad-ae4ka-cai");
    assert.deepEqual(facts.relatedOrigins, [ii, "https://beta.identity.ic0.app"]);
  } finally {
    restore();
  }
});

test("checkIiHealth fails recognition when MCP origin is absent", async () => {
  const ii = "https://beta.test";
  const mcp = "https://mcp.beta.test";
  const restore = stubFetch({
    [`GET ${ii}/`]: resp(200, {
      headers: {
        "content-security-policy": `form-action 'self' http://127.0.0.1:*`,
      },
    }),
  });
  try {
    const { section } = await checkIiHealth(ii, mcp, 2000);
    assert.equal(byId(section, "ii-recognises-mcp").status, "fail");
  } finally {
    restore();
  }
});

test("buildSuggestions surfaces a recognition failure", () => {
  const sections = [
    {
      id: "ii-health",
      title: "",
      status: "fail",
      checks: [{ id: "ii-recognises-mcp", status: "fail" }],
    },
  ];
  const suggestions = buildSuggestions(sections, {});
  assert.ok(
    suggestions.some((s) => s.includes("mcp_server_origin")),
    "expected a suggestion about mcp_server_origin",
  );
});
