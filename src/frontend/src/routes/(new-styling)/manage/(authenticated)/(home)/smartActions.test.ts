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

const verifiedRecoveryPhraseMethod: AuthnMethodData = {
  ...baseAuthnMethod,
  security_settings: {
    purpose: { Recovery: null },
    protection: { Unprotected: null },
  },
  metadata: [["usage", { String: "recovery_phrase" }]],
  // A non-empty last_authentication marks the phrase as verified.
  last_authentication: [BigInt(1)],
};

const unverifiedRecoveryPhraseMethod: AuthnMethodData = {
  ...verifiedRecoveryPhraseMethod,
  last_authentication: [],
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

const withVerifiedRecoveryPhrase = (info: IdentityInfo): IdentityInfo => ({
  ...info,
  authn_methods: [...info.authn_methods, verifiedRecoveryPhraseMethod],
});

const withUnverifiedRecoveryPhrase = (info: IdentityInfo): IdentityInfo => ({
  ...info,
  authn_methods: [...info.authn_methods, unverifiedRecoveryPhraseMethod],
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
  it("leads with both setup actions when phrase and email are both missing", () => {
    const result = deriveSmartActions(baseIdentityInfo, {
      emailRecoveryEnabled: true,
    });
    expect(ids(result)).toEqual([
      "setup-phrase",
      "setup-email",
      "add-access-method",
    ]);
  });

  it("demotes update and reset actions below add-access-method when both methods are configured", () => {
    const info = withEmailRecovery(
      withVerifiedRecoveryPhrase(baseIdentityInfo),
    );
    const result = deriveSmartActions(info, { emailRecoveryEnabled: true });
    expect(ids(result)).toEqual([
      "add-access-method",
      "reset-phrase",
      "update-email",
    ]);
  });

  it("leads with setup-phrase and pushes update-email to the end when only email is configured", () => {
    const info = withEmailRecovery(baseIdentityInfo);
    const result = deriveSmartActions(info, { emailRecoveryEnabled: true });
    expect(ids(result)).toEqual([
      "setup-phrase",
      "add-access-method",
      "update-email",
    ]);
  });

  it("leads with setup-email and pushes reset-phrase to the end when only the phrase is configured", () => {
    const info = withVerifiedRecoveryPhrase(baseIdentityInfo);
    const result = deriveSmartActions(info, { emailRecoveryEnabled: true });
    expect(ids(result)).toEqual([
      "setup-email",
      "add-access-method",
      "reset-phrase",
    ]);
  });

  it("surfaces verify-phrase above setup-email when the phrase exists but is unverified", () => {
    const info = withUnverifiedRecoveryPhrase(baseIdentityInfo);
    const result = deriveSmartActions(info, { emailRecoveryEnabled: true });
    expect(ids(result)).toEqual([
      "verify-phrase",
      "setup-email",
      "add-access-method",
    ]);
  });

  it("never shows reset-phrase alongside verify-phrase", () => {
    const info = withEmailRecovery(
      withUnverifiedRecoveryPhrase(baseIdentityInfo),
    );
    const result = deriveSmartActions(info, { emailRecoveryEnabled: true });
    const phraseActions = ids(result).filter((id) => id.endsWith("-phrase"));
    expect(phraseActions).toEqual(["verify-phrase"]);
  });

  it("omits email actions entirely when the feature flag is off", () => {
    const info = withEmailRecovery(baseIdentityInfo);
    const result = deriveSmartActions(info, { emailRecoveryEnabled: false });
    expect(ids(result)).toEqual(["setup-phrase", "add-access-method"]);
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
