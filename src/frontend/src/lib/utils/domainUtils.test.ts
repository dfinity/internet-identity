import type { InternetIdentityInit } from "$lib/generated/internet_identity_types";
import { describe, expect, it, vi } from "vitest";
import { isOfficialOrigin } from "./domainUtils";

describe("isOfficialOrigin", () => {
  // Base mock config with all required fields - requires explicit related_origins parameter
  const createBaseMockConfig = (
    related_origins: string[],
  ): InternetIdentityInit => ({
    related_origins: [related_origins],
    fetch_root_key: [false],
    openid_google: [],
    enable_dapps_explorer: [],
    assigned_user_number_range: [],
    archive_config: [],
    canister_creation_cycles_cost: [],
    analytics_config: [],
    captcha_config: [],
    register_rate_limit: [],
    is_production: [],
    new_flow_origins: [],
    dummy_auth: [],
  });

  // Default related origins for most tests
  const defaultRelatedOrigins = [
    "https://identity.internetcomputer.org",
    "https://identity.ic0.app",
    "https://test.internetcomputer.org",
    "http://localhost:8080",
  ];

  // Create the different mock configs from the base
  const mockConfig = createBaseMockConfig(defaultRelatedOrigins);
  const emptyConfig = createBaseMockConfig([]);
  const nullConfig = createBaseMockConfig(defaultRelatedOrigins);
  nullConfig.related_origins = [];

  it("should return true if origin is in the related origins list", () => {
    expect(
      isOfficialOrigin("https://identity.internetcomputer.org", mockConfig),
    ).toBe(true);
    expect(isOfficialOrigin("https://identity.ic0.app", mockConfig)).toBe(true);
    expect(
      isOfficialOrigin("https://test.internetcomputer.org", mockConfig),
    ).toBe(true);
    expect(isOfficialOrigin("http://localhost:8080", mockConfig)).toBe(true);
  });

  it("should return false if origin is not in the related origins list", () => {
    expect(isOfficialOrigin("https://example.com", mockConfig)).toBe(false);
    expect(isOfficialOrigin("https://identity.example.org", mockConfig)).toBe(
      false,
    );
  });

  it("should compare origins correctly, ignoring path and query params", () => {
    // Same origin but with paths should still match
    expect(
      isOfficialOrigin(
        "https://identity.internetcomputer.org/path",
        mockConfig,
      ),
    ).toBe(true);
    expect(
      isOfficialOrigin("https://identity.ic0.app/path/to/resource", mockConfig),
    ).toBe(true);
    expect(
      isOfficialOrigin(
        "https://identity.internetcomputer.org?query=param",
        mockConfig,
      ),
    ).toBe(true);
  });

  it("should handle empty related origins", () => {
    expect(
      isOfficialOrigin("https://identity.internetcomputer.org", emptyConfig),
    ).toBe(false);
  });

  it("should handle null related origins", () => {
    expect(
      isOfficialOrigin("https://identity.internetcomputer.org", nullConfig),
    ).toBe(false);
  });

  it("should handle invalid URLs gracefully", () => {
    // Test with invalid current origin
    expect(isOfficialOrigin("invalid-url", mockConfig)).toBe(false);

    // Mock console.error for the next test to avoid cluttering test output
    const consoleErrorSpy = vi
      .spyOn(console, "error")
      .mockImplementation(() => {});

    // Create a config with an invalid URL in related_origins
    const invalidConfig = createBaseMockConfig([
      ...defaultRelatedOrigins,
      "invalid-url",
    ]);

    // Should still work with valid URLs and skip the invalid one
    expect(
      isOfficialOrigin("https://identity.internetcomputer.org", invalidConfig),
    ).toBe(true);

    // Restore console.error
    consoleErrorSpy.mockRestore();
  });

  it("should handle URLs with different protocols", () => {
    // Different protocol should not match
    expect(
      isOfficialOrigin("http://identity.internetcomputer.org", mockConfig),
    ).toBe(false);
  });

  it("should handle subdomains correctly", () => {
    // Subdomain should be treated as a different origin
    expect(
      isOfficialOrigin("https://sub.identity.internetcomputer.org", mockConfig),
    ).toBe(false);
    expect(isOfficialOrigin("https://sub.identity.ic0.app", mockConfig)).toBe(
      false,
    );
  });

  it("should handle ports correctly", () => {
    // Different port should be treated as a different origin
    expect(
      isOfficialOrigin(
        "https://identity.internetcomputer.org:8080",
        mockConfig,
      ),
    ).toBe(false);

    // Test with a config that includes a port
    const configWithPort = createBaseMockConfig([
      ...defaultRelatedOrigins,
      "https://identity.internetcomputer.org:8080",
    ]);

    expect(
      isOfficialOrigin(
        "https://identity.internetcomputer.org:8080",
        configWithPort,
      ),
    ).toBe(true);
    expect(
      isOfficialOrigin("https://identity.internetcomputer.org", configWithPort),
    ).toBe(true);
  });
});
