import { accessMethods } from "./access-methods.derived.svelte";
import identityInfo from "./identity-info.state.svelte";
import { canisterConfig } from "$lib/globals";
import type {
  AuthnMethodData,
  OpenIdCredential,
} from "$lib/generated/internet_identity_types";
import { afterEach, describe, expect, it, vi } from "vitest";

// Test helpers
const createAuthnMethod = (n: number): AuthnMethodData => ({
  authn_method: {
    WebAuthn: {
      pubkey: new Uint8Array(Array.from({ length: n + 2 }, (_, i) => 4 + i)),
      credential_id: new Uint8Array(
        Array.from({ length: n + 2 }, (_, i) => 7 + i),
      ),
    },
  },
  last_authentication: [],
  metadata: [],
  security_settings: {
    protection: { Unprotected: null },
    purpose: { Authentication: null },
  },
});

const openIdCredential: OpenIdCredential = {
  aud: "test",
  iss: "test",
  sub: "test",
  metadata: [],
  last_usage_timestamp: [],
};

describe("Access methods derived stores", () => {
  afterEach(() => {
    vi.unstubAllEnvs();
  });
  describe("lastUsedAccessMethod", () => {
    it("should return the last used access method", () => {
      const am1 = createAuthnMethod(1);
      const am2 = createAuthnMethod(2);
      const authnMethods = [am1, am2];
      const openIdCredentials = [openIdCredential];
      identityInfo.authnMethods = authnMethods;
      identityInfo.openIdCredentials = openIdCredentials;
      expect(accessMethods.lastUsedAccessMethod).toEqual(am2);
    });
  });

  describe("isMaxPasskeysReached", () => {
    it("should return true if the max number of passkeys is reached", () => {
      const authnMethods = Array.from({ length: 8 }, (_, i) =>
        createAuthnMethod(i + 1),
      );
      identityInfo.authnMethods = authnMethods;
      expect(accessMethods.isMaxPasskeysReached).toBe(true);
    });

    it("should return false if the max number of passkeys is not reached", () => {
      const authnMethods = [createAuthnMethod(1)];
      identityInfo.authnMethods = authnMethods;
      expect(accessMethods.isMaxPasskeysReached).toBe(false);
    });

    it("should return true if the number of passkeys is over the max", () => {
      const authnMethods = Array.from({ length: 9 }, (_, i) =>
        createAuthnMethod(i + 1),
      );
      identityInfo.authnMethods = authnMethods;
      expect(accessMethods.isMaxPasskeysReached).toBe(true);
    });
  });

  describe("isMaxOpenIdCredentialsReached", () => {
    it("should return true if the max number of openId credentials is reached", () => {
      const openIdCredentials: OpenIdCredential[] = [openIdCredential];
      identityInfo.openIdCredentials = openIdCredentials;
      expect(accessMethods.isMaxOpenIdCredentialsReached).toBe(true);
    });

    it("should return false if the max number of openId credentials is not reached", () => {
      const openIdCredentials: OpenIdCredential[] = [];
      identityInfo.openIdCredentials = openIdCredentials;
      expect(accessMethods.isMaxOpenIdCredentialsReached).toBe(false);
    });

    describe("When ENABLE_GENERIC_OPEN_ID is true", () => {
      it("should return true if the max number of openId credentials is reached", () => {
        vi.stubEnv("ENABLE_GENERIC_OPEN_ID", "true");
        canisterConfig.openid_configs = [
          [
            {
              auth_uri: "test",
              jwks_uri: "test",
              logo: "test",
              name: "test",
              fedcm_uri: [],
              issuer: "test",
              auth_scope: ["test"],
              client_id: "test",
            },
          ],
        ];
        const openIdCredentials: OpenIdCredential[] = [openIdCredential];
        identityInfo.openIdCredentials = openIdCredentials;
        expect(accessMethods.isMaxOpenIdCredentialsReached).toBe(true);
      });

      it("should return false if the max number of openId credentials is not reached", () => {
        vi.stubEnv("ENABLE_GENERIC_OPEN_ID", "true");
        canisterConfig.openid_configs = [
          [
            {
              auth_uri: "test",
              jwks_uri: "test",
              logo: "test",
              name: "test",
              fedcm_uri: [],
              issuer: "test",
              auth_scope: ["test"],
              client_id: "test",
            },
          ],
        ];
        const openIdCredentials: OpenIdCredential[] = [];
        identityInfo.openIdCredentials = openIdCredentials;
        expect(accessMethods.isMaxOpenIdCredentialsReached).toBe(false);
      });
    });
  });
});
