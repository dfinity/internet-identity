import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import {
  validateDomain,
  discoverSsoConfig,
  clearSsoDiscoveryCache,
  DomainNotConfiguredError,
} from "./ssoDiscovery";

const DFINITY_II_CONFIG = {
  client_id: "dfinity-sso-client-id",
  openid_configuration:
    "https://dfinity.okta.com/.well-known/openid-configuration",
};

const OKTA_DISCOVERY = {
  issuer: "https://dfinity.okta.com",
  authorization_endpoint: "https://dfinity.okta.com/oauth2/v1/authorize",
  scopes_supported: ["openid", "profile", "email"],
};

describe("ssoDiscovery", () => {
  beforeEach(() => {
    clearSsoDiscoveryCache();
    vi.restoreAllMocks();
  });

  afterEach(() => {
    clearSsoDiscoveryCache();
  });

  describe("validateDomain", () => {
    it("accepts valid domains", () => {
      expect(validateDomain("dfinity.org")).toBe("dfinity.org");
      expect(validateDomain("example.com")).toBe("example.com");
      expect(validateDomain("sub.domain.co.uk")).toBe("sub.domain.co.uk");
    });

    it("trims whitespace and lowercases", () => {
      expect(validateDomain("  DFINITY.ORG  ")).toBe("dfinity.org");
    });

    it("rejects empty strings", () => {
      expect(() => validateDomain("")).toThrow("Domain cannot be empty");
      expect(() => validateDomain("   ")).toThrow("Domain cannot be empty");
    });

    it("rejects domains without two labels", () => {
      expect(() => validateDomain("localhost")).toThrow(
        "Domain must have at least two labels",
      );
    });

    it("rejects domains with invalid characters", () => {
      expect(() => validateDomain("exam ple.com")).toThrow(
        "Invalid domain format",
      );
      expect(() => validateDomain("exam_ple.com")).toThrow(
        "Invalid domain format",
      );
      expect(() => validateDomain("example.com/path")).toThrow(
        "Invalid domain format",
      );
    });

    it("rejects domains exceeding max length", () => {
      const longDomain = "a".repeat(250) + ".com";
      expect(() => validateDomain(longDomain)).toThrow("Domain too long");
    });

    it("rejects labels exceeding max length", () => {
      const longLabel = "a".repeat(64) + ".com";
      expect(() => validateDomain(longLabel)).toThrow("Domain label too long");
    });
  });

  describe("discoverSsoConfig", () => {
    it("rejects invalid domain format before making network requests", async () => {
      const fetchSpy = vi.spyOn(globalThis, "fetch");
      await expect(discoverSsoConfig("not a domain")).rejects.toThrow(
        "Invalid domain format",
      );
      expect(fetchSpy).not.toHaveBeenCalled();
    });

    it("performs two-hop discovery for a registered domain", async () => {
      vi.spyOn(globalThis, "fetch")
        .mockResolvedValueOnce(
          new Response(JSON.stringify(DFINITY_II_CONFIG), { status: 200 }),
        )
        .mockResolvedValueOnce(
          new Response(JSON.stringify(OKTA_DISCOVERY), { status: 200 }),
        );

      const result = await discoverSsoConfig("dfinity.org");

      expect(result.clientId).toBe("dfinity-sso-client-id");
      expect(result.discovery.issuer).toBe("https://dfinity.okta.com");
      expect(result.discovery.authorization_endpoint).toBe(
        "https://dfinity.okta.com/oauth2/v1/authorize",
      );
      expect(result.discovery.scopes_supported).toEqual([
        "openid",
        "profile",
        "email",
      ]);
    });

    it("caches successful results", async () => {
      const fetchSpy = vi
        .spyOn(globalThis, "fetch")
        .mockResolvedValueOnce(
          new Response(JSON.stringify(DFINITY_II_CONFIG), { status: 200 }),
        )
        .mockResolvedValueOnce(
          new Response(JSON.stringify(OKTA_DISCOVERY), { status: 200 }),
        );

      const first = await discoverSsoConfig("dfinity.org");
      const second = await discoverSsoConfig("dfinity.org");

      expect(first).toEqual(second);
      expect(fetchSpy).toHaveBeenCalledTimes(2);
    });

    // `mockImplementation` (not `mockResolvedValue`) so each retry gets
    // a fresh Response — reusing one throws `Body has already been read`
    // after the first `.json()`. The network- and invalid-response tests
    // still run through the full 3 retry attempts (2s + 4s = 6s of
    // exponential backoff), so they need the bumped timeout. The 404 case
    // fails fast — deterministic 4xx aren't retried, see
    // `isTransientHttpStatus` in `ssoDiscovery.ts`.
    it("wraps HTTP 404 on hop 1 as DomainNotConfiguredError(http-error, 404) without retrying", async () => {
      const fetchSpy = vi.spyOn(globalThis, "fetch").mockImplementation(() =>
        Promise.resolve(
          new Response("not found", {
            status: 404,
            headers: { "content-type": "text/html" },
          }),
        ),
      );

      await expect(discoverSsoConfig("dfinity.org")).rejects.toMatchObject({
        name: "DomainNotConfiguredError",
        reason: "http-error",
        httpStatus: 404,
      });
      // Single attempt — retrying 404 would just burn ~14s of backoff
      // for the same deterministic answer.
      expect(fetchSpy).toHaveBeenCalledTimes(1);
    });

    it("retries hop 1 on HTTP 503 (transient) and gives up with http-error", async () => {
      const fetchSpy = vi.spyOn(globalThis, "fetch").mockImplementation(() =>
        Promise.resolve(
          new Response("Service Unavailable", {
            status: 503,
          }),
        ),
      );

      await expect(discoverSsoConfig("dfinity.org")).rejects.toMatchObject({
        name: "DomainNotConfiguredError",
        reason: "http-error",
        httpStatus: 503,
      });
      // 3 attempts = `MAX_RETRIES` in `ssoDiscovery.ts`.
      expect(fetchSpy).toHaveBeenCalledTimes(3);
    }, 10000);

    it("wraps a 200 HTML response on hop 1 as DomainNotConfiguredError(invalid-response)", async () => {
      // e.g. `dfinity.org` serves an SPA fallback with 200 + text/html for
      // unknown `.well-known` paths — the hop-1 JSON parse throws
      // `SyntaxError: Unexpected token '<'`, which we remap.
      vi.spyOn(globalThis, "fetch").mockImplementation(() =>
        Promise.resolve(
          new Response("<!DOCTYPE html><html>…</html>", {
            status: 200,
            headers: { "content-type": "text/html" },
          }),
        ),
      );

      await expect(discoverSsoConfig("dfinity.org")).rejects.toMatchObject({
        name: "DomainNotConfiguredError",
        reason: "invalid-response",
      });
    }, 10000);

    it("wraps a network failure on hop 1 as DomainNotConfiguredError(network)", async () => {
      vi.spyOn(globalThis, "fetch").mockImplementation(() =>
        Promise.reject(new TypeError("Failed to fetch")),
      );

      await expect(discoverSsoConfig("dfinity.org")).rejects.toMatchObject({
        name: "DomainNotConfiguredError",
        reason: "network",
      });
    }, 10000);

    it("wraps a malformed-but-decoded ii-openid-configuration as invalid-response", async () => {
      // JSON is fine, shape isn't — still "domain isn't correctly configured"
      // as far as the user is concerned.
      vi.spyOn(globalThis, "fetch").mockResolvedValueOnce(
        new Response(JSON.stringify({ something_else: "yep" }), {
          status: 200,
          headers: { "content-type": "application/json" },
        }),
      );

      await expect(discoverSsoConfig("dfinity.org")).rejects.toBeInstanceOf(
        DomainNotConfiguredError,
      );
    });

    it("rejects ii-openid-configuration missing client_id", async () => {
      vi.spyOn(globalThis, "fetch").mockResolvedValueOnce(
        new Response(
          JSON.stringify({
            openid_configuration:
              "https://dfinity.okta.com/.well-known/openid-configuration",
          }),
          { status: 200 },
        ),
      );

      await expect(discoverSsoConfig("dfinity.org")).rejects.toThrow(
        /ii-openid-configuration.*client_id/,
      );
    });

    it("rejects ii-openid-configuration missing openid_configuration", async () => {
      vi.spyOn(globalThis, "fetch").mockResolvedValueOnce(
        new Response(JSON.stringify({ client_id: "some-client-id" }), {
          status: 200,
        }),
      );

      await expect(discoverSsoConfig("dfinity.org")).rejects.toThrow(
        /ii-openid-configuration.*openid_configuration/,
      );
    });

    it("rejects non-HTTPS openid_configuration URL", async () => {
      vi.spyOn(globalThis, "fetch").mockResolvedValueOnce(
        new Response(
          JSON.stringify({
            client_id: "some-client-id",
            openid_configuration:
              "http://dfinity.okta.com/.well-known/openid-configuration",
          }),
          { status: 200 },
        ),
      );

      await expect(discoverSsoConfig("dfinity.org")).rejects.toThrow(
        "Provider URL must use HTTPS",
      );
    });

    it("rejects provider discovery with missing issuer", async () => {
      vi.spyOn(globalThis, "fetch")
        .mockResolvedValueOnce(
          new Response(JSON.stringify(DFINITY_II_CONFIG), { status: 200 }),
        )
        .mockResolvedValueOnce(
          new Response(
            JSON.stringify({
              authorization_endpoint:
                "https://dfinity.okta.com/oauth2/v1/authorize",
            }),
            { status: 200 },
          ),
        );

      await expect(discoverSsoConfig("dfinity.org")).rejects.toThrow(
        /Provider discovery.*issuer/,
      );
    });

    it("rejects provider discovery with non-HTTPS issuer", async () => {
      vi.spyOn(globalThis, "fetch")
        .mockResolvedValueOnce(
          new Response(JSON.stringify(DFINITY_II_CONFIG), { status: 200 }),
        )
        .mockResolvedValueOnce(
          new Response(
            JSON.stringify({
              ...OKTA_DISCOVERY,
              issuer: "http://dfinity.okta.com",
            }),
            { status: 200 },
          ),
        );

      await expect(discoverSsoConfig("dfinity.org")).rejects.toThrow(
        "Provider issuer must use HTTPS",
      );
    });

    it("rejects provider discovery with non-HTTPS authorization_endpoint", async () => {
      vi.spyOn(globalThis, "fetch")
        .mockResolvedValueOnce(
          new Response(JSON.stringify(DFINITY_II_CONFIG), { status: 200 }),
        )
        .mockResolvedValueOnce(
          new Response(
            JSON.stringify({
              ...OKTA_DISCOVERY,
              authorization_endpoint:
                "http://dfinity.okta.com/oauth2/v1/authorize",
            }),
            { status: 200 },
          ),
        );

      await expect(discoverSsoConfig("dfinity.org")).rejects.toThrow(
        "Provider authorization endpoint must use HTTPS",
      );
    });

    it("rejects provider discovery with issuer hostname mismatch", async () => {
      vi.spyOn(globalThis, "fetch")
        .mockResolvedValueOnce(
          new Response(JSON.stringify(DFINITY_II_CONFIG), { status: 200 }),
        )
        .mockResolvedValueOnce(
          new Response(
            JSON.stringify({
              ...OKTA_DISCOVERY,
              issuer: "https://evil.example.com",
            }),
            { status: 200 },
          ),
        );

      await expect(discoverSsoConfig("dfinity.org")).rejects.toThrow(
        "Provider issuer hostname mismatch",
      );
    });

    it("rejects look-alike issuer hostname (no subdomain separator)", async () => {
      // `endsWith("dfinity.okta.com")` alone would accept
      // `evildfinity.okta.com`. The exact-or-subdomain check rejects it.
      vi.spyOn(globalThis, "fetch")
        .mockResolvedValueOnce(
          new Response(JSON.stringify(DFINITY_II_CONFIG), { status: 200 }),
        )
        .mockResolvedValueOnce(
          new Response(
            JSON.stringify({
              ...OKTA_DISCOVERY,
              issuer: "https://evildfinity.okta.com",
            }),
            { status: 200 },
          ),
        );

      await expect(discoverSsoConfig("dfinity.org")).rejects.toThrow(
        "Provider issuer hostname mismatch",
      );
    });

    it("rejects authorization_endpoint on a different host than the issuer", async () => {
      // A tampered provider discovery pointing auth at an off-host URL must
      // be rejected even though both `issuer` and `authorization_endpoint`
      // are individually HTTPS.
      vi.spyOn(globalThis, "fetch")
        .mockResolvedValueOnce(
          new Response(JSON.stringify(DFINITY_II_CONFIG), { status: 200 }),
        )
        .mockResolvedValueOnce(
          new Response(
            JSON.stringify({
              ...OKTA_DISCOVERY,
              authorization_endpoint: "https://attacker.example/auth",
            }),
            { status: 200 },
          ),
        );

      await expect(discoverSsoConfig("dfinity.org")).rejects.toThrow(
        "Provider authorization endpoint hostname mismatch",
      );
    });

    it("handles ii-openid-configuration fetch failure with retry", async () => {
      const fetchSpy = vi
        .spyOn(globalThis, "fetch")
        .mockResolvedValueOnce(
          new Response("Service Unavailable", { status: 503 }),
        )
        .mockResolvedValueOnce(
          new Response(JSON.stringify(DFINITY_II_CONFIG), { status: 200 }),
        )
        .mockResolvedValueOnce(
          new Response(JSON.stringify(OKTA_DISCOVERY), { status: 200 }),
        );

      const result = await discoverSsoConfig("dfinity.org");
      expect(result.clientId).toBe("dfinity-sso-client-id");
      expect(fetchSpy).toHaveBeenCalledTimes(3);
    });

    it("handles non-object ii-openid-configuration response", async () => {
      vi.spyOn(globalThis, "fetch").mockResolvedValueOnce(
        new Response(JSON.stringify("not an object"), { status: 200 }),
      );

      await expect(discoverSsoConfig("dfinity.org")).rejects.toThrow(
        /ii-openid-configuration.*expected object/,
      );
    });

    it("handles non-object provider discovery response", async () => {
      vi.spyOn(globalThis, "fetch")
        .mockResolvedValueOnce(
          new Response(JSON.stringify(DFINITY_II_CONFIG), { status: 200 }),
        )
        .mockResolvedValueOnce(
          new Response(JSON.stringify("not an object"), { status: 200 }),
        );

      await expect(discoverSsoConfig("dfinity.org")).rejects.toThrow(
        /Provider discovery.*expected object/,
      );
    });
  });
});
