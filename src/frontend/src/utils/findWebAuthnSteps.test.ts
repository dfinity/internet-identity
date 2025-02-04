import { LEGACY_II_URL } from "$src/config";
import { CredentialData } from "./credential-devices";
import { PROD_DOMAINS } from "./findWebAuthnRpId";
import { findWebAuthnSteps } from "./findWebAuthnSteps";

describe("findWebAuthnSteps", () => {
  const currentOrigin = "https://identity.internetcomputer.org";
  const nonCurrentOrigin1 = "https://identity.ic0.app";
  const nonCurrentOrigin1RpId = new URL(nonCurrentOrigin1).hostname;
  const nonCurrentOrigin2 = "https://identity.icp0.io";
  const nonCurrentOrigin2RpId = new URL(nonCurrentOrigin2).hostname;
  const relatedOrigins = PROD_DOMAINS;

  const createMockCredential = (
    origin: string | undefined
  ): CredentialData => ({
    pubkey: new ArrayBuffer(32),
    credentialId: new ArrayBuffer(16),
    origin,
  });

  it("should return an empty array if no devices are provided", () => {
    const result = findWebAuthnSteps({
      supportsRor: true,
      devices: [],
      currentOrigin: currentOrigin,
      relatedOrigins,
    });
    expect(result).toEqual([]);
  });

  it("should use iframe if the RP ID does not match the current origin", () => {
    const result = findWebAuthnSteps({
      supportsRor: true,
      devices: [createMockCredential(nonCurrentOrigin1)],
      currentOrigin: currentOrigin,
      relatedOrigins,
    });

    expect(result).toEqual([{ useIframe: true, rpId: nonCurrentOrigin1RpId }]);
  });

  it("should not use iframe if the RP ID matches the current origin", () => {
    const result = findWebAuthnSteps({
      supportsRor: true,
      devices: [
        createMockCredential(currentOrigin),
        createMockCredential(currentOrigin),
      ],
      currentOrigin: currentOrigin,
      relatedOrigins,
    });

    expect(result).toEqual([{ useIframe: false, rpId: undefined }]);
  });

  it("should use ic0.app when origin is undefined", () => {
    const result = findWebAuthnSteps({
      supportsRor: true,
      devices: [
        createMockCredential(undefined),
        createMockCredential(LEGACY_II_URL),
      ],
      currentOrigin: LEGACY_II_URL,
      relatedOrigins,
    });

    expect(result).toEqual([{ useIframe: false, rpId: undefined }]);
  });

  it("should handle multiple RP IDs and filter credentials accordingly", () => {
    const result = findWebAuthnSteps({
      supportsRor: true,
      devices: [
        createMockCredential(currentOrigin),
        createMockCredential(currentOrigin),
        createMockCredential(nonCurrentOrigin1),
        createMockCredential(nonCurrentOrigin2),
        createMockCredential(nonCurrentOrigin2),
      ],
      currentOrigin: currentOrigin,
      relatedOrigins,
    });

    expect(result).toEqual([
      { useIframe: false, rpId: undefined },
      { useIframe: true, rpId: nonCurrentOrigin1RpId },
      { useIframe: true, rpId: nonCurrentOrigin2RpId },
    ]);
  });
});
