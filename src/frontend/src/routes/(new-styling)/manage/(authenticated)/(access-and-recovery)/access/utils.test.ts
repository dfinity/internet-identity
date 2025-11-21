import { describe, it, expect } from "vitest";
import { bytesToHex } from "@noble/hashes/utils";
import {
  type AccessMethod,
  compareAccessMethods,
  toAccessMethods,
  toKey,
  isCurrentAccessMethod,
} from "./utils";
import type {
  AuthnMethodData,
  IdentityInfo,
  OpenIdCredential,
} from "$lib/generated/internet_identity_types";

const makePasskey = (id: number, lastAuth?: bigint): AuthnMethodData => ({
  authn_method: {
    WebAuthn: {
      credential_id: new Uint8Array([id]),
      pubkey: new Uint8Array(),
      aaguid: [],
    },
  },
  last_authentication: lastAuth !== undefined ? [lastAuth] : [],
  security_settings: {
    protection: {
      Unprotected: null,
    },
    purpose: {
      Authentication: null,
    },
  },
  metadata: [],
});

const makeOpenId = (
  iss: string,
  sub: string,
  lastUse?: bigint,
): OpenIdCredential => ({
  iss,
  sub,
  last_usage_timestamp: lastUse !== undefined ? [lastUse] : [],
  aud: "",
  metadata: [],
});

describe("compareAccessMethods", () => {
  it("sorts OpenID before passkey when timestamps are equal or both undefined", () => {
    const openid: AccessMethod = { openid: makeOpenId("iss", "sub") };
    const passkey: AccessMethod = { passkey: makePasskey(1) };
    expect(compareAccessMethods(openid, passkey)).toBe(-1);
    expect(compareAccessMethods(passkey, openid)).toBe(1);
  });

  it("puts undefined timestamps first", () => {
    const oldOpenId: AccessMethod = {
      openid: makeOpenId("iss", "sub", BigInt(100)),
    };
    const newOpenId: AccessMethod = { openid: makeOpenId("iss", "sub2") };
    expect(compareAccessMethods(newOpenId, oldOpenId)).toBe(-1);
  });

  it("sorts by descending timestamp", () => {
    const newer = { openid: makeOpenId("iss", "sub", BigInt(200)) };
    const older = { openid: makeOpenId("iss", "sub", BigInt(100)) };
    expect(compareAccessMethods(newer, older)).toBe(-1);
    expect(compareAccessMethods(older, newer)).toBe(1);
  });
});

describe("toAccessMethods", () => {
  it("merges and sorts openid and passkey methods", () => {
    const identityInfo: Pick<
      IdentityInfo,
      "authn_methods" | "openid_credentials"
    > = {
      openid_credentials: [[makeOpenId("iss1", "sub1", BigInt(100))]],
      authn_methods: [makePasskey(1, BigInt(200)), makePasskey(2, BigInt(50))],
    };

    const result = toAccessMethods(identityInfo);

    // Expect total 3 entries
    expect(result).toHaveLength(3);

    // The first should be the newest (timestamp 200)
    const first = result[0];
    expect("passkey" in first && first.passkey.last_authentication[0]).toBe(
      BigInt(200),
    );
  });

  it("filters out non-passkeys", () => {
    const recoveryMethod: AuthnMethodData = {
      authn_method: { PubKey: { pubkey: new Uint8Array() } },
      last_authentication: [BigInt(100)],
      security_settings: {
        protection: {
          Unprotected: null,
        },
        purpose: {
          Recovery: null,
        },
      },
      metadata: [],
    };
    const identityInfo: Pick<
      IdentityInfo,
      "authn_methods" | "openid_credentials"
    > = {
      openid_credentials: [[]],
      authn_methods: [recoveryMethod],
    };
    const result = toAccessMethods(identityInfo);
    expect(result).toHaveLength(0);
  });
});

describe("toKey", () => {
  it("creates key for passkey using credential_id", () => {
    const accessMethod: AccessMethod = {
      passkey: makePasskey(42),
    };
    const expected = bytesToHex(new Uint8Array([42]));
    expect(toKey(accessMethod)).toBe(expected);
  });

  it("creates key for openid using iss + sub", () => {
    const accessMethod: AccessMethod = {
      openid: makeOpenId("issuer", "subject"),
    };
    expect(toKey(accessMethod)).toBe("issuersubject");
  });

  it("throws on unknown type", () => {
    // @ts-expect-error intentionally invalid
    expect(() => toKey({ foo: {} })).toThrow();
  });
});

describe("isCurrentAccessMethod", () => {
  it("returns true for matching passkey credential IDs", () => {
    const credentialId = 9;
    const authenticated = {
      authMethod: { passkey: { credentialId: new Uint8Array([credentialId]) } },
    };
    const accessMethod: AccessMethod = { passkey: makePasskey(credentialId) };
    expect(isCurrentAccessMethod(authenticated, accessMethod)).toBe(true);
  });

  it("returns false for non-matching passkeys", () => {
    const authenticated = {
      authMethod: { passkey: { credentialId: new Uint8Array([1]) } },
    };
    const accessMethod: AccessMethod = { passkey: makePasskey(2) };
    expect(isCurrentAccessMethod(authenticated, accessMethod)).toBe(false);
  });

  it("returns true for matching OpenID", () => {
    const authenticated = {
      authMethod: { openid: { iss: "i", sub: "s" } },
    };
    const accessMethod: AccessMethod = { openid: makeOpenId("i", "s") };
    expect(isCurrentAccessMethod(authenticated, accessMethod)).toBe(true);
  });

  it("returns false for mismatched OpenID", () => {
    const authenticated = {
      authMethod: { openid: { iss: "i", sub: "s" } },
    };
    const accessMethod: AccessMethod = { openid: makeOpenId("i", "x") };
    expect(isCurrentAccessMethod(authenticated, accessMethod)).toBe(false);
  });

  it("returns false for different method types", () => {
    const authenticated = {
      authMethod: { openid: { iss: "i", sub: "s" } },
    };
    const accessMethod: AccessMethod = { passkey: makePasskey(1) };
    expect(isCurrentAccessMethod(authenticated, accessMethod)).toBe(false);
  });
});
