import { describe, it, expect } from "vitest";
import { deriveSmartActions, type SmartActionId } from "./smartActions";
import type {
  AuthnMethodData,
  IdentityInfo,
} from "$lib/generated/internet_identity_types";

const baseAuthnMethod: AuthnMethodData = {
  security_settings: {
    purpose: { Authentication: null },
    protection: { Unprotected: null },
  },
  metadata: [],
  last_authentication: [],
  authn_method: { PubKey: { pubkey: new Uint8Array() } },
};

const recoveryPhraseMethod: AuthnMethodData = {
  ...baseAuthnMethod,
  security_settings: {
    purpose: { Recovery: null },
    protection: { Unprotected: null },
  },
  metadata: [["usage", { String: "recovery_phrase" }]],
};

const baseIdentityInfo: IdentityInfo = {
  authn_methods: [baseAuthnMethod],
  metadata: [],
  name: [],
  email_recovery: [],
  created_at: [],
  authn_method_registration: [],
  openid_credentials: [],
};

const withRecoveryPhrase = (info: IdentityInfo): IdentityInfo => ({
  ...info,
  authn_methods: [...info.authn_methods, recoveryPhraseMethod],
});

const withEmailRecovery = (info: IdentityInfo): IdentityInfo => ({
  ...info,
  email_recovery: [
    {
      address: "user@example.com",
      created_at: BigInt(0),
      last_used: [],
    },
  ],
});

const ids = (actions: { id: SmartActionId }[]): SmartActionId[] =>
  actions.map((a) => a.id);

describe("deriveSmartActions", () => {
  it("offers setup actions when no recovery method is configured", () => {
    const result = deriveSmartActions(baseIdentityInfo, {
      emailRecoveryEnabled: true,
    });
    expect(ids(result)).toEqual([
      "add-access-method",
      "setup-email",
      "setup-phrase",
    ]);
  });

  it("swaps to update/reset labels when methods are already configured", () => {
    const info = withEmailRecovery(withRecoveryPhrase(baseIdentityInfo));
    const result = deriveSmartActions(info, { emailRecoveryEnabled: true });
    expect(ids(result)).toEqual([
      "add-access-method",
      "update-email",
      "reset-phrase",
    ]);
  });

  it("keeps the same position regardless of which methods exist", () => {
    const info = withEmailRecovery(baseIdentityInfo);
    const result = deriveSmartActions(info, { emailRecoveryEnabled: true });
    expect(ids(result)).toEqual([
      "add-access-method",
      "update-email",
      "setup-phrase",
    ]);
  });

  it("omits email actions entirely when the feature flag is off", () => {
    const info = withEmailRecovery(baseIdentityInfo);
    const result = deriveSmartActions(info, { emailRecoveryEnabled: false });
    expect(ids(result)).toEqual(["add-access-method", "setup-phrase"]);
  });

  it("produces identical output for identical input (stable sort)", () => {
    const a = deriveSmartActions(baseIdentityInfo, {
      emailRecoveryEnabled: true,
    });
    const b = deriveSmartActions(baseIdentityInfo, {
      emailRecoveryEnabled: true,
    });
    expect(a).toEqual(b);
  });
});
