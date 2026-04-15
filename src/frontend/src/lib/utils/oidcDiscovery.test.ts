import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import {
  fetchDiscoveryDocument,
  clearDiscoveryCache,
  findCachedDiscoveryByIssuer,
} from "./oidcDiscovery";

const GOOGLE_DISCOVERY_URL =
  "https://accounts.google.com/.well-known/openid-configuration";
const APPLE_DISCOVERY_URL =
  "https://appleid.apple.com/.well-known/openid-configuration";

const VALID_GOOGLE_DISCOVERY = {
  issuer: "https://accounts.google.com",
  authorization_endpoint: "https://accounts.google.com/o/oauth2/v2/auth",
  scopes_supported: ["openid", "profile", "email"],
};

const VALID_APPLE_DISCOVERY = {
  issuer: "https://appleid.apple.com",
  authorization_endpoint: "https://appleid.apple.com/auth/authorize",
  scopes_supported: ["openid", "name", "email"],
};

describe("oidcDiscovery", () => {
  beforeEach(() => {
    clearDiscoveryCache();
    vi.restoreAllMocks();
  });

  afterEach(() => {
    clearDiscoveryCache();
  });

  describe("fetchDiscoveryDocument", () => {
    it("rejects non-HTTPS discovery URLs", async () => {
      await expect(
        fetchDiscoveryDocument(
          "http://accounts.google.com/.well-known/openid-configuration",
        ),
      ).rejects.toThrow("Discovery URL must use HTTPS");
    });

    it("rejects disallowed domains", async () => {
      await expect(
        fetchDiscoveryDocument(
          "https://evil.example.com/.well-known/openid-configuration",
        ),
      ).rejects.toThrow("Discovery domain not allowed");
    });

    it("fetches and validates a Google discovery document", async () => {
      vi.spyOn(globalThis, "fetch").mockResolvedValueOnce(
        new Response(JSON.stringify(VALID_GOOGLE_DISCOVERY), {
          status: 200,
        }),
      );

      const result = await fetchDiscoveryDocument(GOOGLE_DISCOVERY_URL);
      expect(result).toEqual(VALID_GOOGLE_DISCOVERY);
      expect(globalThis.fetch).toHaveBeenCalledWith(
        GOOGLE_DISCOVERY_URL,
        expect.objectContaining({ signal: expect.any(AbortSignal) }),
      );
    });

    it("caches successful responses", async () => {
      vi.spyOn(globalThis, "fetch").mockResolvedValueOnce(
        new Response(JSON.stringify(VALID_GOOGLE_DISCOVERY), {
          status: 200,
        }),
      );

      const first = await fetchDiscoveryDocument(GOOGLE_DISCOVERY_URL);
      const second = await fetchDiscoveryDocument(GOOGLE_DISCOVERY_URL);

      expect(first).toEqual(second);
      expect(globalThis.fetch).toHaveBeenCalledTimes(1);
    });

    it("rejects discovery documents missing issuer", async () => {
      vi.spyOn(globalThis, "fetch").mockResolvedValueOnce(
        new Response(
          JSON.stringify({
            authorization_endpoint:
              "https://accounts.google.com/o/oauth2/v2/auth",
          }),
          { status: 200 },
        ),
      );

      await expect(
        fetchDiscoveryDocument(GOOGLE_DISCOVERY_URL),
      ).rejects.toThrow("Discovery document missing required field: issuer");
    });

    it("rejects discovery documents missing authorization_endpoint", async () => {
      vi.spyOn(globalThis, "fetch").mockResolvedValueOnce(
        new Response(
          JSON.stringify({ issuer: "https://accounts.google.com" }),
          { status: 200 },
        ),
      );

      await expect(
        fetchDiscoveryDocument(GOOGLE_DISCOVERY_URL),
      ).rejects.toThrow(
        "Discovery document missing required field: authorization_endpoint",
      );
    });

    it("rejects issuer hostname mismatch", async () => {
      vi.spyOn(globalThis, "fetch").mockResolvedValueOnce(
        new Response(
          JSON.stringify({
            issuer: "https://evil.example.com",
            authorization_endpoint:
              "https://accounts.google.com/o/oauth2/v2/auth",
          }),
          { status: 200 },
        ),
      );

      await expect(
        fetchDiscoveryDocument(GOOGLE_DISCOVERY_URL),
      ).rejects.toThrow("Issuer hostname mismatch");
    });

    it("rejects non-HTTPS issuer", async () => {
      vi.spyOn(globalThis, "fetch").mockResolvedValueOnce(
        new Response(
          JSON.stringify({
            issuer: "http://accounts.google.com",
            authorization_endpoint:
              "https://accounts.google.com/o/oauth2/v2/auth",
          }),
          { status: 200 },
        ),
      );

      await expect(
        fetchDiscoveryDocument(GOOGLE_DISCOVERY_URL),
      ).rejects.toThrow("Issuer must use HTTPS");
    });

    it("rejects non-HTTPS authorization_endpoint", async () => {
      vi.spyOn(globalThis, "fetch").mockResolvedValueOnce(
        new Response(
          JSON.stringify({
            issuer: "https://accounts.google.com",
            authorization_endpoint:
              "http://accounts.google.com/o/oauth2/v2/auth",
          }),
          { status: 200 },
        ),
      );

      await expect(
        fetchDiscoveryDocument(GOOGLE_DISCOVERY_URL),
      ).rejects.toThrow("Authorization endpoint must use HTTPS");
    });

    it("handles non-200 responses with retry", async () => {
      const fetchSpy = vi
        .spyOn(globalThis, "fetch")
        .mockResolvedValueOnce(
          new Response("Service Unavailable", { status: 503 }),
        )
        .mockResolvedValueOnce(
          new Response(JSON.stringify(VALID_GOOGLE_DISCOVERY), {
            status: 200,
          }),
        );

      const result = await fetchDiscoveryDocument(GOOGLE_DISCOVERY_URL);
      expect(result).toEqual(VALID_GOOGLE_DISCOVERY);
      expect(fetchSpy).toHaveBeenCalledTimes(2);
    });

    it("handles non-object discovery document", async () => {
      vi.spyOn(globalThis, "fetch").mockResolvedValueOnce(
        new Response(JSON.stringify("not an object"), { status: 200 }),
      );

      await expect(
        fetchDiscoveryDocument(GOOGLE_DISCOVERY_URL),
      ).rejects.toThrow("Discovery document is not a valid object");
    });

    it("filters scopes_supported to only include relevant scopes", async () => {
      vi.spyOn(globalThis, "fetch").mockResolvedValueOnce(
        new Response(
          JSON.stringify({
            ...VALID_GOOGLE_DISCOVERY,
            scopes_supported: [
              "openid",
              "profile",
              "email",
              "custom_scope",
              42,
            ],
          }),
          { status: 200 },
        ),
      );

      const result = await fetchDiscoveryDocument(GOOGLE_DISCOVERY_URL);
      // Non-string values are filtered out
      expect(result.scopes_supported).toEqual([
        "openid",
        "profile",
        "email",
        "custom_scope",
      ]);
    });
  });

  describe("findCachedDiscoveryByIssuer", () => {
    it("returns undefined when cache is empty", () => {
      expect(
        findCachedDiscoveryByIssuer("https://accounts.google.com"),
      ).toBeUndefined();
    });

    it("finds cached discovery by issuer", async () => {
      vi.spyOn(globalThis, "fetch").mockResolvedValueOnce(
        new Response(JSON.stringify(VALID_GOOGLE_DISCOVERY), {
          status: 200,
        }),
      );

      await fetchDiscoveryDocument(GOOGLE_DISCOVERY_URL);
      const result = findCachedDiscoveryByIssuer(
        "https://accounts.google.com",
      );

      expect(result).toBeDefined();
      expect(result!.discoveryUrl).toBe(GOOGLE_DISCOVERY_URL);
      expect(result!.document.issuer).toBe("https://accounts.google.com");
    });

    it("returns undefined for non-matching issuer", async () => {
      vi.spyOn(globalThis, "fetch").mockResolvedValueOnce(
        new Response(JSON.stringify(VALID_GOOGLE_DISCOVERY), {
          status: 200,
        }),
      );

      await fetchDiscoveryDocument(GOOGLE_DISCOVERY_URL);
      expect(
        findCachedDiscoveryByIssuer("https://other-issuer.example.com"),
      ).toBeUndefined();
    });

    it("finds correct issuer among multiple cached providers", async () => {
      vi.spyOn(globalThis, "fetch")
        .mockResolvedValueOnce(
          new Response(JSON.stringify(VALID_GOOGLE_DISCOVERY), {
            status: 200,
          }),
        )
        .mockResolvedValueOnce(
          new Response(JSON.stringify(VALID_APPLE_DISCOVERY), {
            status: 200,
          }),
        );

      await fetchDiscoveryDocument(GOOGLE_DISCOVERY_URL);
      await fetchDiscoveryDocument(APPLE_DISCOVERY_URL);

      const google = findCachedDiscoveryByIssuer(
        "https://accounts.google.com",
      );
      expect(google?.discoveryUrl).toBe(GOOGLE_DISCOVERY_URL);

      const apple = findCachedDiscoveryByIssuer("https://appleid.apple.com");
      expect(apple?.discoveryUrl).toBe(APPLE_DISCOVERY_URL);
    });
  });
});
