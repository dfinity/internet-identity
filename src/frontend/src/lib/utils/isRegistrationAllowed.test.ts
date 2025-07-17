import type { InternetIdentityInit } from "$lib/generated/internet_identity_types";
import { describe, expect, it } from "vitest";
import { isRegistrationAllowed } from "./isRegistrationAllowed";

describe("isRegistrationAllowed", () => {
  const createConfig = (
    relatedOrigins: string[] | null = null,
  ): InternetIdentityInit => {
    return {
      fetch_root_key: [],
      enable_dapps_explorer: [],
      assigned_user_number_range: [],
      archive_config: [],
      canister_creation_cycles_cost: [],
      related_origins: relatedOrigins === null ? [] : [relatedOrigins],
      captcha_config: [],
      register_rate_limit: [],
      analytics_config: [],
      openid_google: [],
      is_production: [],
      new_flow_origins: [],
      dummy_auth: [],
    };
  };

  it("allows registration when related_origins is empty (undefined)", () => {
    const config = createConfig();
    expect(isRegistrationAllowed(config, "https://example.com")).toBe(true);
  });

  it("allows registration when related_origins is an empty array", () => {
    const config = createConfig([]);
    expect(isRegistrationAllowed(config, "https://example.com")).toBe(true);
  });

  it("allows registration when the origin is in the related_origins list", () => {
    const config = createConfig([
      "https://identity.ic0.app",
      "https://identity.icp0.io",
      "https://example.com",
    ]);
    expect(isRegistrationAllowed(config, "https://example.com")).toBe(true);
  });

  it("disallows registration when the origin is not in the related_origins list", () => {
    const config = createConfig([
      "https://identity.ic0.app",
      "https://identity.icp0.io",
    ]);
    expect(isRegistrationAllowed(config, "https://example.com")).toBe(false);
  });

  it("handles case-sensitive origins correctly", () => {
    const config = createConfig([
      "https://identity.ic0.app",
      "https://Example.com",
    ]);
    expect(isRegistrationAllowed(config, "https://example.com")).toBe(true);
    expect(isRegistrationAllowed(config, "https://Example.com")).toBe(true);
  });
});
