import { describe, it, expect } from "vitest";
import { shouldRequestMethodSwitch } from "./AuthWizard.switch";
import type { LastUsedIdentity } from "$lib/stores/last-used-identities.store";

type AuthMethod = LastUsedIdentity["authMethod"];

const passkey: AuthMethod = { passkey: { credentialId: new Uint8Array([1]) } };
const google: AuthMethod = {
  openid: { iss: "https://accounts.google.com", sub: "sub-google" },
};
const apple: AuthMethod = {
  openid: { iss: "https://appleid.apple.com", sub: "sub-apple" },
};
const ssoFoo: AuthMethod = { sso: { domain: "sso.foo.com" } };
const ssoBar: AuthMethod = { sso: { domain: "sso.bar.com" } };

describe("shouldRequestMethodSwitch", () => {
  describe("no previous", () => {
    it("returns false when previous is undefined", () => {
      expect(shouldRequestMethodSwitch(undefined, { passkey: {} })).toBe(false);
      expect(
        shouldRequestMethodSwitch(undefined, {
          openid: { iss: "https://accounts.google.com" },
        }),
      ).toBe(false);
      expect(
        shouldRequestMethodSwitch(undefined, {
          sso: { domain: "sso.foo.com" },
        }),
      ).toBe(false);
    });
  });

  describe("same family — passkey", () => {
    it("returns false for passkey→passkey", () => {
      expect(shouldRequestMethodSwitch(passkey, { passkey: {} })).toBe(false);
    });
  });

  describe("cross-family from passkey", () => {
    it("passkey→openid triggers", () => {
      expect(
        shouldRequestMethodSwitch(passkey, {
          openid: { iss: "https://accounts.google.com" },
        }),
      ).toBe(true);
    });

    it("passkey→sso triggers", () => {
      expect(
        shouldRequestMethodSwitch(passkey, { sso: { domain: "sso.foo.com" } }),
      ).toBe(true);
    });
  });

  describe("cross-family from openid", () => {
    it("openid→passkey triggers", () => {
      expect(shouldRequestMethodSwitch(google, { passkey: {} })).toBe(true);
    });

    it("openid→sso triggers", () => {
      expect(
        shouldRequestMethodSwitch(google, { sso: { domain: "sso.foo.com" } }),
      ).toBe(true);
    });
  });

  describe("cross-family from sso", () => {
    it("sso→passkey triggers", () => {
      expect(shouldRequestMethodSwitch(ssoFoo, { passkey: {} })).toBe(true);
    });

    it("sso→openid triggers", () => {
      expect(
        shouldRequestMethodSwitch(ssoFoo, {
          openid: { iss: "https://accounts.google.com" },
        }),
      ).toBe(true);
    });
  });

  describe("same-family openid", () => {
    it("same issuer (Google→Google) does not trigger", () => {
      expect(
        shouldRequestMethodSwitch(google, {
          openid: { iss: "https://accounts.google.com" },
        }),
      ).toBe(false);
    });

    it("different issuer (Google→Apple) triggers", () => {
      expect(
        shouldRequestMethodSwitch(google, {
          openid: { iss: "https://appleid.apple.com" },
        }),
      ).toBe(true);
    });

    it("different issuer (Apple→Google) triggers", () => {
      expect(
        shouldRequestMethodSwitch(apple, {
          openid: { iss: "https://accounts.google.com" },
        }),
      ).toBe(true);
    });
  });

  describe("same-family sso", () => {
    it("same domain does not trigger", () => {
      expect(
        shouldRequestMethodSwitch(ssoFoo, { sso: { domain: "sso.foo.com" } }),
      ).toBe(false);
    });

    it("different domain triggers", () => {
      expect(
        shouldRequestMethodSwitch(ssoFoo, { sso: { domain: "sso.bar.com" } }),
      ).toBe(true);
    });

    it("different domain reverse (bar→foo) triggers", () => {
      expect(
        shouldRequestMethodSwitch(ssoBar, { sso: { domain: "sso.foo.com" } }),
      ).toBe(true);
    });
  });
});
