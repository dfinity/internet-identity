// ... existing code ...
import { getLastUsedAccessMethod } from "./accessMethods";
import type {
  AuthnMethodData,
  OpenIdCredential,
} from "$lib/generated/internet_identity_types";

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
