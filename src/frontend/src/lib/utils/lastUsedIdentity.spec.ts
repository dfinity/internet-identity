import { describe, it, expect, vi, afterEach } from "vitest";

import { InternetIdentityInit } from "$lib/generated/internet_identity_types";

// Helper to build a fully-typed InternetIdentityInit with sensible defaults
const mockInternetIdentityInit = (
  overrides: Partial<InternetIdentityInit> = {},
): InternetIdentityInit => ({
  fetch_root_key: [],
  openid_google: [],
  is_production: [],
  enable_dapps_explorer: [],
  assigned_user_number_range: [],
  new_flow_origins: [],
  archive_config: [],
  canister_creation_cycles_cost: [],
  analytics_config: [],
  related_origins: [],
  feature_flag_continue_from_another_device: [],
  openid_configs: [],
  captcha_config: [],
  dummy_auth: [],
  register_rate_limit: [],
  ...overrides,
});

// Mock globals to control canisterConfig consumed by findConfig
let mockCanisterConfig: InternetIdentityInit = mockInternetIdentityInit();
vi.mock("$lib/globals", () => ({
  get canisterConfig() {
    return mockCanisterConfig;
  },
}));

import { lastUsedIdentityTypeName } from "./lastUsedIdentity";
import type { LastUsedIdentity } from "../stores/last-used-identities.store";

const baseTimestamp = 1_725_000_000_000;

describe("lastUsedIdentityTypeName", () => {
  afterEach(() => {
    vi.clearAllMocks();
    mockCanisterConfig = mockInternetIdentityInit();
  });

  it("returns 'Example Provider' for OpenID when config found is OpenIdConfig", () => {
    // Arrange: provide a valid OpenIdConfig in canisterConfig.openid_configs
    mockCanisterConfig = mockInternetIdentityInit({
      openid_configs: [
        [
          {
            name: "Example Provider",
            issuer: "https://example.com",
            client_id: "client-id",
            jwks_uri: "https://example.com/jwks",
            auth_uri: "https://example.com/auth",
            logo: "https://example.com/logo.png",
            fedcm_uri: [],
            auth_scope: ["openid"],
          },
        ],
      ],
    });

    const identity: LastUsedIdentity = {
      identityNumber: BigInt(1),
      name: "Alice",
      authMethod: { openid: { iss: "https://example.com", sub: "sub-1" } },
      lastUsedTimestampMillis: baseTimestamp,
    };

    const result = lastUsedIdentityTypeName(identity);
    expect(result).toBe("Example Provider");
  });

  it("returns 'Google' for OpenID when config found is GoogleConfig", () => {
    // Arrange: provide Google config (GoogleOpenIdConfig) via canisterConfig
    mockCanisterConfig = mockInternetIdentityInit({
      openid_google: [[{ client_id: "google-client-id" }]],
    });

    const identity: LastUsedIdentity = {
      identityNumber: BigInt(2),
      authMethod: {
        openid: { iss: "https://accounts.google.com", sub: "sub-2" },
      },
      lastUsedTimestampMillis: baseTimestamp,
    };

    const result = lastUsedIdentityTypeName(identity);
    expect(result).toBe("Google");
  });

  it("returns 'Unknown' for OpenID when issuer is not found", () => {
    // Arrange: no matching config in canisterConfig
    mockCanisterConfig = mockInternetIdentityInit({
      // No openid_google and no openid_configs matching issuer
      openid_configs: [
        [
          {
            name: "Other Provider",
            issuer: "https://other.example",
            client_id: "client-id",
            jwks_uri: "https://other.example/jwks",
            auth_uri: "https://other.example/auth",
            logo: "https://other.example/logo.png",
            fedcm_uri: [],
            auth_scope: ["openid"],
          },
        ],
      ],
    });

    const identity: LastUsedIdentity = {
      identityNumber: BigInt(3),
      authMethod: {
        openid: { iss: "https://unknown-idp.example", sub: "sub-3" },
      },
      lastUsedTimestampMillis: baseTimestamp,
    };

    const result = lastUsedIdentityTypeName(identity);
    expect(result).toBe("Unknown");
  });

  it("returns 'Passkey' for passkey auth method", () => {
    const identity: LastUsedIdentity = {
      identityNumber: BigInt(4),
      name: "Bob",
      authMethod: { passkey: { credentialId: new Uint8Array([1, 2, 3]) } },
      lastUsedTimestampMillis: baseTimestamp,
    };

    const result = lastUsedIdentityTypeName(identity);
    expect(result).toBe("Passkey");
  });
});
