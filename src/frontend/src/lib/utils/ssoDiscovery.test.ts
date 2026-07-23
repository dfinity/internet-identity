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
    get_sso_discovery_status: vi.fn(),
  },
}));

const DISCOVERY: SsoDiscovery = {
  discovery_domain: "dfinity.org",
  client_id: "dfinity-sso-client-id",
  issuer: "https://dfinity.okta.com",
  authorization_endpoint: "https://dfinity.okta.com/oauth2/v1/authorize",
  scopes: ["openid", "profile", "email"],
  name: ["DFINITY"],
  resolved_client_id: ["dfinity-sso-client-id"],
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
      expect(validateDomain("127.0.0.1:11107")).toBe("127.0.0.1:11107");
      expect(validateDomain("localhost")).toBe("localhost");
    });

    it("rejects a loopback host carrying a path", () => {
      // `localhost/evil` is not a bare host, so it falls through to the
      // DNS-format check (which rejects the slash) instead of being treated
      // as loopback.
      expect(() => validateDomain("localhost/evil")).toThrow(
        "Invalid domain format",
      );
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
      expect(anonymousActor.get_sso_discovery_status).not.toHaveBeenCalled();
      expect(anonymousActor.discover_sso).not.toHaveBeenCalled();
    });

    it("returns the mapped result when the query reads Resolved immediately", async () => {
      vi.mocked(anonymousActor.get_sso_discovery_status).mockResolvedValue({
        Resolved: DISCOVERY,
      });

      const result = await discoverSsoConfig("dfinity.org");

      expect(result).toEqual({
        domain: "dfinity.org",
        clientId: "dfinity-sso-client-id",
        resolvedClientId: "dfinity-sso-client-id",
        name: "DFINITY",
        discovery: {
          issuer: "https://dfinity.okta.com",
          authorization_endpoint:
            "https://dfinity.okta.com/oauth2/v1/authorize",
          scopes_supported: ["openid", "profile", "email"],
        },
      });
      expect(anonymousActor.get_sso_discovery_status).toHaveBeenCalledWith({
        org_domain: "dfinity.org",
        target_app_origin: [],
      });
      // Already resolved, so no update was needed to drive a fetch.
      expect(anonymousActor.discover_sso).not.toHaveBeenCalled();
    });

    it("passes the target origin and returns the per-app resolvedClientId", async () => {
      vi.mocked(anonymousActor.get_sso_discovery_status).mockResolvedValue({
        Resolved: {
          ...DISCOVERY,
          resolved_client_id: ["per-app-client-id"],
        },
      });

      const result = await discoverSsoConfig(
        "dfinity.org",
        undefined,
        "https://payroll.example",
      );

      expect(result.resolvedClientId).toBe("per-app-client-id");
      expect(result.clientId).toBe("dfinity-sso-client-id");
      expect(anonymousActor.get_sso_discovery_status).toHaveBeenCalledWith({
        org_domain: "dfinity.org",
        target_app_origin: ["https://payroll.example"],
      });
    });

    it("throws DomainNotConfiguredError(origin-denied) when the origin is gated off", async () => {
      vi.mocked(anonymousActor.get_sso_discovery_status).mockResolvedValue({
        Resolved: {
          ...DISCOVERY,
          resolved_client_id: [],
        },
      });

      const error = await discoverSsoConfig(
        "dfinity.org",
        undefined,
        "https://denied.example",
      ).catch((e: unknown) => e);
      expect(error).toBeInstanceOf(DomainNotConfiguredError);
      if (error instanceof DomainNotConfiguredError) {
        expect(error.reason).toBe("origin-denied");
      }
    });

    it("drives the update while the query reads Pending, then resolves", async () => {
      vi.useFakeTimers();
      vi.mocked(anonymousActor.get_sso_discovery_status)
        .mockResolvedValueOnce({ Pending: null })
        .mockResolvedValueOnce({ Pending: null })
        .mockResolvedValueOnce({ Resolved: DISCOVERY });
      vi.mocked(anonymousActor.discover_sso).mockResolvedValue(undefined);

      const promise = discoverSsoConfig("dfinity.org");
      await vi.advanceTimersByTimeAsync(500);
      await vi.advanceTimersByTimeAsync(500);

      const result = await promise;
      expect(result.domain).toBe("dfinity.org");
      // Each Pending read drove the update.
      expect(anonymousActor.discover_sso).toHaveBeenCalledTimes(2);
    });

    it("stops polling when the abort signal is already aborted", async () => {
      vi.mocked(anonymousActor.get_sso_discovery_status).mockResolvedValue({
        Pending: null,
      });
      vi.mocked(anonymousActor.discover_sso).mockResolvedValue(undefined);

      const controller = new AbortController();
      controller.abort();
      await expect(
        discoverSsoConfig("dfinity.org", controller.signal),
      ).rejects.toThrow("aborted");
    });

    it("aborting mid-sleep stops the poll without firing another update", async () => {
      vi.useFakeTimers();
      vi.mocked(anonymousActor.get_sso_discovery_status).mockResolvedValue({
        Pending: null,
      });
      vi.mocked(anonymousActor.discover_sso).mockResolvedValue(undefined);

      const controller = new AbortController();
      const settled = discoverSsoConfig("dfinity.org", controller.signal).catch(
        (e: unknown) => e,
      );

      // First iteration: query reads Pending → one update → parked in the sleep.
      await vi.advanceTimersByTimeAsync(0);
      expect(anonymousActor.discover_sso).toHaveBeenCalledTimes(1);

      // Abort during the 500ms sleep: it resolves early and the loop-top check
      // throws before a second query/update can fire.
      controller.abort();
      await vi.advanceTimersByTimeAsync(0);

      const error = await settled;
      expect(error).toBeInstanceOf(Error);
      expect((error as Error).message).toContain("aborted");
      expect(anonymousActor.discover_sso).toHaveBeenCalledTimes(1);
    });
  });
});
