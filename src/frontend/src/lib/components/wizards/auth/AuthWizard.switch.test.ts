import { describe, it, expect } from "vitest";
import { shouldRequestMethodSwitch } from "./AuthWizard.switch";
import type { LastUsedIdentity } from "$lib/stores/last-used-identities.store";

const passkeyIdentity: LastUsedIdentity = {
  identityNumber: BigInt(1),
  authMethod: { passkey: { credentialId: new Uint8Array([1]) } },
  lastUsedTimestampMillis: 0,
};

const googleIdentity: LastUsedIdentity = {
  identityNumber: BigInt(2),
  authMethod: {
    openid: { iss: "https://accounts.google.com", sub: "sub-google" },
  },
  lastUsedTimestampMillis: 0,
};

const appleIdentity: LastUsedIdentity = {
  identityNumber: BigInt(3),
  authMethod: {
    openid: { iss: "https://appleid.apple.com", sub: "sub-apple" },
  },
  lastUsedTimestampMillis: 0,
};

const ssoFooIdentity: LastUsedIdentity = {
  identityNumber: BigInt(4),
  authMethod: { sso: { domain: "sso.foo.com" } },
  lastUsedTimestampMillis: 0,
};

const ssoBarIdentity: LastUsedIdentity = {
  identityNumber: BigInt(5),
  authMethod: { sso: { domain: "sso.bar.com" } },
  lastUsedTimestampMillis: 0,
};

describe("shouldRequestMethodSwitch", () => {
  describe("no previous snapshot", () => {
    it("returns false when previousSnapshot is undefined", () => {
      expect(shouldRequestMethodSwitch("passkey", undefined)).toBe(false);
      expect(shouldRequestMethodSwitch("openid", undefined)).toBe(false);
      expect(shouldRequestMethodSwitch("sso", undefined)).toBe(false);
    });
  });

  describe("same family — passkey", () => {
    it("returns false for passkey→passkey", () => {
      expect(shouldRequestMethodSwitch("passkey", passkeyIdentity)).toBe(false);
    });
  });

  describe("cross-family from passkey", () => {
    it("passkey→openid triggers", () => {
      expect(
        shouldRequestMethodSwitch("openid", passkeyIdentity, {
          providerIssuer: "https://accounts.google.com",
        }),
      ).toBe(true);
    });

    it("passkey→sso triggers", () => {
      expect(
        shouldRequestMethodSwitch("sso", passkeyIdentity, {
          providerDomain: "sso.foo.com",
        }),
      ).toBe(true);
    });
  });

  describe("cross-family from openid", () => {
    it("openid→passkey triggers", () => {
      expect(shouldRequestMethodSwitch("passkey", googleIdentity)).toBe(true);
    });

    it("openid→sso triggers", () => {
      expect(
        shouldRequestMethodSwitch("sso", googleIdentity, {
          providerDomain: "sso.foo.com",
        }),
      ).toBe(true);
    });
  });

  describe("cross-family from sso", () => {
    it("sso→passkey triggers", () => {
      expect(shouldRequestMethodSwitch("passkey", ssoFooIdentity)).toBe(true);
    });

    it("sso→openid triggers", () => {
      expect(
        shouldRequestMethodSwitch("openid", ssoFooIdentity, {
          providerIssuer: "https://accounts.google.com",
        }),
      ).toBe(true);
    });
  });

  describe("same-family openid", () => {
    it("same issuer (Google→Google) does not trigger", () => {
      expect(
        shouldRequestMethodSwitch("openid", googleIdentity, {
          providerIssuer: "https://accounts.google.com",
        }),
      ).toBe(false);
    });

    it("different issuer (Google→Apple) triggers", () => {
      expect(
        shouldRequestMethodSwitch("openid", googleIdentity, {
          providerIssuer: "https://appleid.apple.com",
        }),
      ).toBe(true);
    });

    it("different issuer (Apple→Google) triggers", () => {
      expect(
        shouldRequestMethodSwitch("openid", appleIdentity, {
          providerIssuer: "https://accounts.google.com",
        }),
      ).toBe(true);
    });

    it("missing providerIssuer does not trigger (defensive)", () => {
      expect(shouldRequestMethodSwitch("openid", googleIdentity)).toBe(false);
      expect(shouldRequestMethodSwitch("openid", googleIdentity, {})).toBe(
        false,
      );
    });
  });

  describe("same-family sso", () => {
    it("same domain does not trigger", () => {
      expect(
        shouldRequestMethodSwitch("sso", ssoFooIdentity, {
          providerDomain: "sso.foo.com",
        }),
      ).toBe(false);
    });

    it("different domain triggers", () => {
      expect(
        shouldRequestMethodSwitch("sso", ssoFooIdentity, {
          providerDomain: "sso.bar.com",
        }),
      ).toBe(true);
    });

    it("different domain reverse (bar→foo) triggers", () => {
      expect(
        shouldRequestMethodSwitch("sso", ssoBarIdentity, {
          providerDomain: "sso.foo.com",
        }),
      ).toBe(true);
    });

    it("missing providerDomain does not trigger (defensive)", () => {
      expect(shouldRequestMethodSwitch("sso", ssoFooIdentity)).toBe(false);
      expect(shouldRequestMethodSwitch("sso", ssoFooIdentity, {})).toBe(false);
    });
  });
});
