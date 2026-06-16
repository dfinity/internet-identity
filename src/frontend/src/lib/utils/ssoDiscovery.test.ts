import { describe, it, expect, vi, beforeEach } from "vitest";
import {
  validateDomain,
  discoverSsoConfig,
  DomainNotConfiguredError,
} from "./ssoDiscovery";
import { anonymousActor } from "$lib/globals";
import type { SsoDiscovery } from "$lib/generated/internet_identity_types";

vi.mock("$lib/globals", () => ({
  anonymousActor: {
    discover_sso: vi.fn(),
    get_sso_discovery: vi.fn(),
  },
}));

const DISCOVERY: SsoDiscovery = {
  discovery_domain: "dfinity.org",
  client_id: "dfinity-sso-client-id",
  issuer: "https://dfinity.okta.com",
  authorization_endpoint: "https://dfinity.okta.com/oauth2/v1/authorize",
  scopes: ["openid", "profile", "email"],
  name: ["DFINITY"],
};

describe("ssoDiscovery", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.useRealTimers();
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

    it("accepts loopback hosts with a port", () => {
      expect(validateDomain("localhost:11107")).toBe("localhost:11107");
    });

    it("rejects empty strings", () => {
      expect(() => validateDomain("")).toThrow("Domain cannot be empty");
      expect(() => validateDomain("   ")).toThrow("Domain cannot be empty");
    });

    it("rejects domains without two labels", () => {
      expect(() => validateDomain("nodots")).toThrow(
        "Domain must have at least two labels",
      );
    });

    it("rejects domains with invalid characters", () => {
      expect(() => validateDomain("exam ple.com")).toThrow(
        "Invalid domain format",
      );
      expect(() => validateDomain("example.com/path")).toThrow(
        "Invalid domain format",
      );
    });

    it("rejects domains exceeding max length", () => {
      const longDomain = `${"a".repeat(254)}.com`;
      expect(() => validateDomain(longDomain)).toThrow("Domain too long");
    });

    it("rejects labels exceeding max length", () => {
      const longLabel = `${"a".repeat(64)}.com`;
      expect(() => validateDomain(longLabel)).toThrow("Domain label too long");
    });
  });

  describe("discoverSsoConfig", () => {
    it("rejects an invalid domain before calling the canister", async () => {
      await expect(discoverSsoConfig("not a domain")).rejects.toThrow(
        "Invalid domain format",
      );
      expect(anonymousActor.get_sso_discovery).not.toHaveBeenCalled();
      expect(anonymousActor.discover_sso).not.toHaveBeenCalled();
    });

    it("returns the mapped result when the query reads a value immediately", async () => {
      vi.mocked(anonymousActor.get_sso_discovery).mockResolvedValue({
        Ok: [DISCOVERY],
      });

      const result = await discoverSsoConfig("dfinity.org");

      expect(result).toEqual({
        domain: "dfinity.org",
        clientId: "dfinity-sso-client-id",
        name: "DFINITY",
        discovery: {
          issuer: "https://dfinity.okta.com",
          authorization_endpoint:
            "https://dfinity.okta.com/oauth2/v1/authorize",
          scopes_supported: ["openid", "profile", "email"],
        },
      });
      // The value was already there, so no update was needed to drive a fetch.
      expect(anonymousActor.discover_sso).not.toHaveBeenCalled();
    });

    it("throws DomainNotConfiguredError(rejected) when the query rejects the domain", async () => {
      vi.mocked(anonymousActor.get_sso_discovery).mockResolvedValue({
        Err: { DomainNotAllowed: null },
      });

      const error = await discoverSsoConfig("evil.example").catch(
        (e: unknown) => e,
      );
      expect(error).toBeInstanceOf(DomainNotConfiguredError);
      if (error instanceof DomainNotConfiguredError) {
        expect(error.reason).toBe("rejected");
      }
    });

    it("drives the update when the query reads no value, then resolves", async () => {
      vi.useFakeTimers();
      // Query reads nothing twice, then the resolved config.
      vi.mocked(anonymousActor.get_sso_discovery)
        .mockResolvedValueOnce({ Ok: [] })
        .mockResolvedValueOnce({ Ok: [] })
        .mockResolvedValueOnce({ Ok: [DISCOVERY] });
      vi.mocked(anonymousActor.discover_sso).mockResolvedValue({ Ok: null });

      const promise = discoverSsoConfig("dfinity.org");
      await vi.advanceTimersByTimeAsync(500);
      await vi.advanceTimersByTimeAsync(500);

      const result = await promise;
      expect(result.domain).toBe("dfinity.org");
      // Each not-ready query drove the update.
      expect(anonymousActor.discover_sso).toHaveBeenCalledTimes(2);
    });

    it("stops polling when the abort signal is already aborted", async () => {
      vi.mocked(anonymousActor.get_sso_discovery).mockResolvedValue({ Ok: [] });
      vi.mocked(anonymousActor.discover_sso).mockResolvedValue({ Ok: null });

      const controller = new AbortController();
      controller.abort();
      await expect(
        discoverSsoConfig("dfinity.org", controller.signal),
      ).rejects.toThrow("aborted");
    });
  });
});
