import { describe, it, expect, vi, afterEach } from "vitest";

// Mock globals to control canisterConfig consumed by findConfig
let mockCanisterConfig: any = {};
vi.mock("$lib/globals", () => ({
  get canisterConfig() {
    return mockCanisterConfig;
  },
}));

import { lastUsedIdentityTypeName } from "./lastUsedIdentity";
import type { LastUsedIdentity } from "../stores/last-used-identities.store";

const baseTimestamp = 1_725_000_000_000; // fixed timestamp for determinism

afterEach(() => {
  vi.clearAllMocks();
  mockCanisterConfig = {};
});

describe("lastUsedIdentityTypeName", () => {
  it("returns provider name for OpenID when config is OpenIdConfig", () => {
    // Arrange: provide a valid OpenIdConfig in canisterConfig.openid_configs
    mockCanisterConfig = {
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
    };

    const identity: LastUsedIdentity = {
      identityNumber: 1n,
      name: "Alice",
      authMethod: { openid: { iss: "https://example.com", sub: "sub-1" } },
      lastUsedTimestampMillis: baseTimestamp,
    };

    // Act
    const result = lastUsedIdentityTypeName(identity);

    // Assert
    expect(result).toBe("Example Provider");
  });

  it("returns 'Google' for OpenID when config is not OpenIdConfig", () => {
    // Arrange: provide Google config (GoogleOpenIdConfig) via canisterConfig
    mockCanisterConfig = {
      openid_google: [[{ client_id: "google-client-id" }]],
    };

    const identity: LastUsedIdentity = {
      identityNumber: 2n,
      authMethod: {
        openid: { iss: "https://accounts.google.com", sub: "sub-2" },
      },
      lastUsedTimestampMillis: baseTimestamp,
    };

    // Act
    const result = lastUsedIdentityTypeName(identity);

    // Assert
    expect(result).toBe("Google");
  });

  it("returns 'Google' for OpenID when config is undefined", () => {
    // Arrange: no matching config in canisterConfig
    mockCanisterConfig = {
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
    };

    const identity: LastUsedIdentity = {
      identityNumber: 3n,
      authMethod: {
        openid: { iss: "https://unknown-idp.example", sub: "sub-3" },
      },
      lastUsedTimestampMillis: baseTimestamp,
    };

    // Act
    const result = lastUsedIdentityTypeName(identity);

    // Assert
    expect(result).toBe("Google");
  });

  it("returns 'Passkey' for passkey auth method", () => {
    const identity: LastUsedIdentity = {
      identityNumber: 4n,
      name: "Bob",
      authMethod: { passkey: { credentialId: new Uint8Array([1, 2, 3]) } },
      lastUsedTimestampMillis: baseTimestamp,
    };

    const result = lastUsedIdentityTypeName(identity);

    expect(result).toBe("Passkey");
  });
});
