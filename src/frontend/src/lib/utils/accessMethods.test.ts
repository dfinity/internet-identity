import {
  getLastUsedAccessMethod,
  isLegacyAuthnMethod,
  getOrigin,
  haveMultipleOrigins,
  getRpId,
  isSameAccessMethod,
  isNewOriginDevice,
  getOpenIdTitles,
} from "./accessMethods";
import type {
  AuthnMethodData,
  OpenIdCredential,
  AuthnMethodPurpose,
  AuthnMethodProtection,
  MetadataMapV2,
  DeviceData,
} from "$lib/generated/internet_identity_types";
import { vi } from "vitest";
import { nonNullish } from "@dfinity/utils";
import { ENABLE_GENERIC_OPEN_ID } from "$lib/state/featureFlags";

// Mock the canisterConfig, including OpenID configs used by getOpenIdTitles via findConfig
vi.mock("$lib/globals", () => ({
  canisterConfig: {
    new_flow_origins: [
      ["https://id.ai", "https://rdmx6-jaaaa-aaaah-qdrqq-cai.ic0.app"],
    ],
    // Shape matches access in findConfig: openid_google?.[0]?.[0]
    openid_google: [
      [
        {
          client_id: "test-google-client-id",
        },
      ],
    ],
    // Shape matches access in findConfig: openid_configs?.[0]?.find(...)
    openid_configs: [
      [
        {
          issuer: "https://issuer.acme",
          name: "AcmeID",
          auth_scope: "openid profile email",
          logo: "<svg></svg>",
          client_id: "test-client-id",
          jwks_uri: "https://issuer.acme/.well-known/jwks.json",
          auth_uri: "https://issuer.acme/auth",
          fedcm_uri: [],
        },
        {
          issuer: "https://issuer.acme/{tid}/v2.0",
          name: "Tenantable AcmeID",
          auth_scope: "openid profile email",
          logo: "<svg></svg>",
          client_id: "test-client-id-2",
          jwks_uri: "https://issuer.acme/.well-known/jwks.json",
          auth_uri: "https://issuer.acme/auth",
          fedcm_uri: [],
        },
      ],
    ],
    // Provide feature flag defaults to satisfy optional initialization logic
    feature_flag_enable_generic_open_id_fe: [false],
  },
}));

// Ensure deterministic default for tests that rely on Google-specific path
beforeEach(() => {
  ENABLE_GENERIC_OPEN_ID.set(false);
});

const makeAuthnMethodWithOrigin = (origin?: string): AuthnMethodData => {
  const metadata: MetadataMapV2 = nonNullish(origin)
    ? [["origin", { String: origin }]]
    : [];

  return {
    id: "test-id",
    last_authentication: [],
    security_settings: {
      purpose: { Authentication: null },
      protection: { Unprotected: null },
    },
    metadata,
    authn_method: {
      WebAuthn: { pubkey: new Uint8Array(), credential_id: new Uint8Array() },
    },
  } as AuthnMethodData;
};

describe("getLastUsedAccessMethod", () => {
  const makeAuthnMethod = (id: string, last_auth: number[] = []) =>
    ({
      id,
      last_authentication: last_auth,
      security_settings: {},
      metadata: {},
      authn_method: "mock",
    }) as unknown as AuthnMethodData;

  const makeOpenIdCredential = (id: string, last_usage: number[] = []) =>
    ({
      id,
      last_usage_timestamp: last_usage,
      aud: "mock-aud",
      iss: "mock-iss",
      sub: "mock-sub",
      metadata: {},
    }) as unknown as OpenIdCredential;

  it("returns null if both arrays are empty", () => {
    expect(getLastUsedAccessMethod([], [])).toBeNull();
  });

  it("returns the only authnMethod if present", () => {
    const method = makeAuthnMethod("a", [123]);
    expect(getLastUsedAccessMethod([method], [])).toBe(method);
  });

  it("returns the only openIdCredential if present", () => {
    const cred = makeOpenIdCredential("oidc", [456]);
    const result = getLastUsedAccessMethod([], [cred]);
    expect(result).toMatchObject({ id: "oidc", last_authentication: [456] });
  });

  it("returns the most recently used method", () => {
    const method1 = makeAuthnMethod("a", [100]);
    const method2 = makeAuthnMethod("b", [200]);
    const cred = makeOpenIdCredential("oidc", [150]);
    const result = getLastUsedAccessMethod([method1, method2], [cred]);
    expect(result).toBe(method2);
  });

  it("handles nullish last_authentication", () => {
    const method1 = makeAuthnMethod("a", []);
    const method2 = makeAuthnMethod("b", [200]);
    const result = getLastUsedAccessMethod([method1, method2], []);
    expect(result).toBe(method2);
  });

  it("handles all nullish timestamps", () => {
    const method1 = makeAuthnMethod("a", []);
    const cred = makeOpenIdCredential("oidc", []);
    const result = getLastUsedAccessMethod([method1], [cred]);
    // Should return the first, as all are equally nullish
    expect(result).toBe(method1);
  });

  it("prefers non-nullish over nullish", () => {
    const method1 = makeAuthnMethod("a", []);
    const cred = makeOpenIdCredential("oidc", [999]);
    const result = getLastUsedAccessMethod([method1], [cred]);
    expect(result).toMatchObject({ id: "oidc", last_authentication: [999] });
  });
});

describe("getOpenIdTitles", () => {
  const makeOpenIdCredential = (
    iss: string,
    {
      name,
      email,
      sub = "sub",
      aud = "audience",
      tid,
    }: {
      name?: string;
      email?: string;
      sub?: string;
      aud?: string;
      tid?: string;
    },
  ): OpenIdCredential => ({
    last_usage_timestamp: [],
    aud,
    iss,
    sub,
    metadata: [
      ...(nonNullish(name) ? [["name", { String: name }]] : []),
      ...(nonNullish(email) ? [["email", { String: email }]] : []),
      ...(nonNullish(tid) ? [["tid", { String: tid }]] : []),
    ] as MetadataMapV2,
  });

  it("returns name and email with provider from OpenID config", () => {
    const cred = makeOpenIdCredential("https://issuer.acme", {
      name: "Alice",
      email: "alice@example.com",
    });
    const res = getOpenIdTitles(cred);
    expect(res).toEqual({
      title: { ellipsis: false, text: "Alice" },
      subtitle: {
        ellipsis: true,
        text: "AcmeID Account - alice@example.com",
      },
    });
  });

  it("returns name and email with provider from OpenID config with template", () => {
    const tid = "123";
    const cred = makeOpenIdCredential(`https://issuer.acme/${tid}/v2.0`, {
      name: "Alice",
      email: "alice@example.com",
      tid,
    });
    const res = getOpenIdTitles(cred);
    expect(res).toEqual({
      title: { ellipsis: false, text: "Alice" },
      subtitle: {
        ellipsis: true,
        text: "Tenantable AcmeID Account - alice@example.com",
      },
    });
  });

  it("returns name only with OpenID provider when email missing", () => {
    const cred = makeOpenIdCredential("https://issuer.acme", {
      name: "Bob",
    });
    const res = getOpenIdTitles(cred);
    expect(res).toEqual({
      title: { ellipsis: false, text: "Bob" },
      subtitle: { ellipsis: false, text: "AcmeID Account" },
    });
  });

  it("returns Unknown account if Google provider but not in config", () => {
    const cred = makeOpenIdCredential("https://accounts.google.com", {
      name: "Bob",
    });
    const res = getOpenIdTitles(cred);
    expect(res).toEqual({
      title: { ellipsis: false, text: "Bob" },
      subtitle: { ellipsis: false, text: "Google Account" },
    });
  });

  it("returns email only with Unknown provider when config not found", () => {
    const cred = makeOpenIdCredential("https://unknown.provider", {
      email: "charlie@example.com",
    });
    const res = getOpenIdTitles(cred);
    expect(res).toEqual({
      title: { ellipsis: true, text: "charlie@example.com" },
      subtitle: { ellipsis: false, text: "Unknown Account" },
    });
  });

  it("returns Unknown account when neither name nor email is present", () => {
    const cred = makeOpenIdCredential("https://issuer.acme", {});
    const res = getOpenIdTitles(cred);
    expect(res).toEqual({
      title: { ellipsis: false, text: "Unknown Account" },
      subtitle: undefined,
    });
  });
});

describe("getOrigin", () => {
  it("returns the origin when present in metadata", () => {
    const method = makeAuthnMethodWithOrigin("https://example.com");
    expect(getOrigin(method)).toBe("https://example.com");
  });

  it("returns undefined when no origin in metadata", () => {
    const method = makeAuthnMethodWithOrigin();
    expect(getOrigin(method)).toBeUndefined();
  });

  it("returns undefined when metadata is empty", () => {
    const method: AuthnMethodData = {
      id: "test-id",
      last_authentication: [],
      security_settings: {
        purpose: { Authentication: null },
        protection: { Unprotected: null },
      },
      metadata: [],
      authn_method: {
        WebAuthn: { pubkey: new Uint8Array(), credential_id: new Uint8Array() },
      },
    } as AuthnMethodData;
    expect(getOrigin(method)).toBeUndefined();
  });

  it("returns undefined when origin value is not a String type", () => {
    const method: AuthnMethodData = {
      id: "test-id",
      last_authentication: [],
      security_settings: {
        purpose: { Authentication: null },
        protection: { Unprotected: null },
      },
      metadata: [["origin", { Bytes: [1, 2, 3] }]],
      authn_method: {
        WebAuthn: { pubkey: new Uint8Array(), credential_id: new Uint8Array() },
      },
    } as AuthnMethodData;
    expect(getOrigin(method)).toBeUndefined();
  });
});

describe("haveMultipleOrigins", () => {
  it("returns false when empty array", () => {
    expect(haveMultipleOrigins([])).toBe(false);
  });

  it("returns false when single auth method", () => {
    const methods = [makeAuthnMethodWithOrigin("https://example.com")];
    expect(haveMultipleOrigins(methods)).toBe(false);
  });

  it("returns false when all methods have same origin", () => {
    const methods = [
      makeAuthnMethodWithOrigin("https://example.com"),
      makeAuthnMethodWithOrigin("https://example.com"),
      makeAuthnMethodWithOrigin("https://example.com"),
    ];
    expect(haveMultipleOrigins(methods)).toBe(false);
  });

  it("returns true when methods have different origins", () => {
    const methods = [
      makeAuthnMethodWithOrigin("https://example.com"),
      makeAuthnMethodWithOrigin("https://different.com"),
    ];
    expect(haveMultipleOrigins(methods)).toBe(true);
  });

  it("returns true when only one method has origin", () => {
    const methods = [
      makeAuthnMethodWithOrigin("https://example.com"),
      makeAuthnMethodWithOrigin(),
      makeAuthnMethodWithOrigin(),
    ];
    expect(haveMultipleOrigins(methods)).toBe(true);
  });

  it("returns true when multiple methods have different origins mixed with no-origin methods", () => {
    const methods = [
      makeAuthnMethodWithOrigin("https://example.com"),
      makeAuthnMethodWithOrigin(),
      makeAuthnMethodWithOrigin("https://different.com"),
    ];
    expect(haveMultipleOrigins(methods)).toBe(true);
  });

  it("returns false when all methods have no origin", () => {
    const methods = [
      makeAuthnMethodWithOrigin(),
      makeAuthnMethodWithOrigin(),
      makeAuthnMethodWithOrigin(),
    ];
    expect(haveMultipleOrigins(methods)).toBe(false);
  });
});

describe("isLegacyAuthnMethod", () => {
  const makeAuthnMethodForLegacyTest = ({
    purpose = { Authentication: null },
    origin,
    protection = { Unprotected: null },
  }: {
    purpose?: AuthnMethodPurpose;
    origin?: string;
    protection?: AuthnMethodProtection;
  }): AuthnMethodData => {
    const metadata: MetadataMapV2 =
      origin !== undefined && origin !== null && origin !== ""
        ? [["origin", { String: origin }]]
        : [];

    return {
      id: "test-id",
      last_authentication: [],
      security_settings: {
        purpose,
        protection,
      },
      metadata,
      authn_method: {
        WebAuthn: { pubkey: new Uint8Array(), credential_id: new Uint8Array() },
      },
    } as AuthnMethodData;
  };

  describe("Recovery method tests", () => {
    it("returns true for access method with Recovery purpose", () => {
      const method = makeAuthnMethodForLegacyTest({
        purpose: { Recovery: null },
      });
      expect(isLegacyAuthnMethod(method)).toBe(true);
    });

    it("returns true for access method with Recovery purpose and matching origin", () => {
      const method = makeAuthnMethodForLegacyTest({
        purpose: { Recovery: null },
        origin: "https://id.ai",
      });
      expect(isLegacyAuthnMethod(method)).toBe(true);
    });
  });

  describe("Origin scenarios", () => {
    it("returns true for access method with legacy origin", () => {
      const method = makeAuthnMethodForLegacyTest({
        purpose: { Authentication: null },
        origin: "https://identity.ic0.app",
      });
      expect(isLegacyAuthnMethod(method)).toBe(true);
    });

    it("returns false for access method with origin matching new_flow_origins", () => {
      const method = makeAuthnMethodForLegacyTest({
        purpose: { Authentication: null },
        origin: "https://id.ai",
      });
      expect(isLegacyAuthnMethod(method)).toBe(false);
    });

    it("returns false for access method with origin matching second new_flow_origin", () => {
      const method = makeAuthnMethodForLegacyTest({
        purpose: { Authentication: null },
        origin: "https://rdmx6-jaaaa-aaaah-qdrqq-cai.ic0.app",
      });
      expect(isLegacyAuthnMethod(method)).toBe(false);
    });

    it("returns true for access method with origin NOT matching new_flow_origins", () => {
      const method = makeAuthnMethodForLegacyTest({
        purpose: { Authentication: null },
        origin: "https://different-origin.com",
      });
      expect(isLegacyAuthnMethod(method)).toBe(true);
    });

    it("returns true for access method with no origin metadata", () => {
      const method = makeAuthnMethodForLegacyTest({
        purpose: { Authentication: null },
      });
      expect(isLegacyAuthnMethod(method)).toBe(true);
    });
  });

  describe("Combined scenarios", () => {
    it("returns true for access method with both Recovery purpose AND legacy origin", () => {
      const method = makeAuthnMethodForLegacyTest({
        purpose: { Recovery: null },
        origin: "https://identity.ic0.app",
      });
      expect(isLegacyAuthnMethod(method)).toBe(true);
    });

    it("returns true for access method with Authentication purpose and non-matching origin", () => {
      const method = makeAuthnMethodForLegacyTest({
        purpose: { Authentication: null },
        origin: "https://different-origin.com",
      });
      expect(isLegacyAuthnMethod(method)).toBe(true);
    });
  });
});

describe("getRpId", () => {
  it("should return the hostname from the origin metadata", () => {
    const accessMethod: AuthnMethodData = makeAuthnMethodWithOrigin(
      "https://example.com",
    );
    expect(getRpId(accessMethod)).toBe("example.com");
  });

  it("should return undefined if origin metadata is not present", () => {
    const accessMethod: AuthnMethodData = makeAuthnMethodWithOrigin();
    expect(getRpId(accessMethod)).toBeUndefined();
  });

  it("should return undefined if origin metadata is not a string", () => {
    const accessMethod: AuthnMethodData = makeAuthnMethodWithOrigin("foo");
    expect(getRpId(accessMethod)).toBeUndefined();
  });
});

describe("isSameAccessMethod", () => {
  const makeWebAuthnMethod = (pubkeyBytes: number[]): AuthnMethodData => {
    return {
      id: "webauthn-id",
      last_authentication: [],
      security_settings: {
        purpose: { Authentication: null },
        protection: { Unprotected: null },
      },
      metadata: [],
      authn_method: {
        WebAuthn: {
          pubkey: new Uint8Array(pubkeyBytes),
          credential_id: new Uint8Array([1, 2, 3]),
        },
      },
    } as AuthnMethodData;
  };

  const makeOpenIdCredential = (iss: string, sub: string): OpenIdCredential =>
    ({
      id: "oidc-id",
      last_usage_timestamp: [],
      aud: "audience",
      iss,
      sub,
      metadata: [],
    }) as unknown as OpenIdCredential;

  it("returns true for identical WebAuthn methods (same pubkey)", () => {
    const a = makeWebAuthnMethod([1, 2, 3]);
    const b = makeWebAuthnMethod([1, 2, 3]);
    expect(isSameAccessMethod(a, b)).toBe(true);
  });

  it("returns false for different WebAuthn methods (different pubkey)", () => {
    const a = makeWebAuthnMethod([1, 2, 3]);
    const b = makeWebAuthnMethod([4, 5, 6]);
    expect(isSameAccessMethod(a, b)).toBe(false);
  });

  it("returns true for identical OpenID credentials (same iss and sub)", () => {
    const a = makeOpenIdCredential("https://issuer", "user-sub");
    const b = makeOpenIdCredential("https://issuer", "user-sub");
    expect(isSameAccessMethod(a, b)).toBe(true);
  });

  it("returns false for different OpenID credentials (different sub)", () => {
    const a = makeOpenIdCredential("https://issuer", "user-sub");
    const b = makeOpenIdCredential("https://issuer", "other-sub");
    expect(isSameAccessMethod(a, b)).toBe(false);
  });

  it("returns false for mixed types (WebAuthn vs OpenID)", () => {
    const webAuthn = makeWebAuthnMethod([1, 2, 3]);
    const oidc = makeOpenIdCredential("https://issuer", "user-sub");
    expect(isSameAccessMethod(webAuthn, oidc)).toBe(false);
  });
});

describe("isNewOriginDevice", () => {
  const makeDevice = ({
    origin,
    purpose = { Authentication: null },
  }: {
    origin?: string;
    purpose?: { Authentication?: null; Recovery?: null };
  }) =>
    ({
      origin: origin !== undefined ? [origin] : [],
      purpose,
    }) as unknown as Omit<DeviceData, "alias">;

  it("returns true when origin matches first new_flow_origin and not Recovery", () => {
    const device = makeDevice({ origin: "https://id.ai" });
    expect(isNewOriginDevice(device)).toBe(true);
  });

  it("returns true when origin matches second new_flow_origin and not Recovery", () => {
    const device = makeDevice({
      origin: "https://rdmx6-jaaaa-aaaah-qdrqq-cai.ic0.app",
    });
    expect(isNewOriginDevice(device)).toBe(true);
  });

  it("returns false for Recovery device even with matching origin", () => {
    const device = makeDevice({
      origin: "https://id.ai",
      purpose: { Recovery: null },
    });
    expect(isNewOriginDevice(device)).toBe(false);
  });

  it("returns false when origin does not match any new_flow_origin", () => {
    const device = makeDevice({ origin: "https://different-origin.com" });
    expect(isNewOriginDevice(device)).toBe(false);
  });

  it("returns false when device has no origin (defaults to legacy URL)", () => {
    const device = makeDevice({});
    expect(isNewOriginDevice(device)).toBe(false);
  });
});
