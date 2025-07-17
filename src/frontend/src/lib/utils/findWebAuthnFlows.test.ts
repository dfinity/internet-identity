import { LEGACY_II_URL } from "$lib/config";
import { CredentialData } from "./credential-devices";
import { findWebAuthnFlows } from "./findWebAuthnFlows";

describe("findWebAuthnFlows", () => {
  const currentOrigin = "https://identity.internetcomputer.org";
  const nonCurrentOrigin1 = "https://identity.ic0.app";
  const nonCurrentOrigin1RpId = new URL(nonCurrentOrigin1).hostname;
  const nonCurrentOrigin2 = "https://identity.icp0.io";
  const nonCurrentOrigin2RpId = new URL(nonCurrentOrigin2).hostname;
  const relatedOrigins = [
    "https://identity.ic0.app",
    "https://identity.internetcomputer.org",
    "https://identity.icp0.io",
  ];

  const createMockCredential = (
    origin: string | undefined,
  ): CredentialData => ({
    pubkey: new ArrayBuffer(32),
    credentialId: new ArrayBuffer(16),
    origin,
  });

  it("should return default flow if no devices are provided", () => {
    const result = findWebAuthnFlows({
      supportsRor: true,
      devices: [],
      currentOrigin: currentOrigin,
      relatedOrigins,
    });
    expect(result).toEqual([{ useIframe: false, rpId: undefined }]);
  });

  it("should use iframe if the RP ID does not match the current origin", () => {
    const result = findWebAuthnFlows({
      supportsRor: true,
      devices: [createMockCredential(nonCurrentOrigin1)],
      currentOrigin: currentOrigin,
      relatedOrigins,
    });

    expect(result).toEqual([{ useIframe: true, rpId: nonCurrentOrigin1RpId }]);
  });

  it("should not use iframe if the RP ID matches the current origin", () => {
    const result = findWebAuthnFlows({
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
    const result = findWebAuthnFlows({
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
    const result = findWebAuthnFlows({
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

  it("should return undefined flow if no related origins", () => {
    const result = findWebAuthnFlows({
      supportsRor: true,
      devices: [
        createMockCredential(currentOrigin),
        createMockCredential(currentOrigin),
        createMockCredential(nonCurrentOrigin1),
        createMockCredential(nonCurrentOrigin2),
        createMockCredential(nonCurrentOrigin2),
      ],
      currentOrigin: currentOrigin,
      relatedOrigins: [],
    });

    expect(result).toEqual([{ useIframe: false, rpId: undefined }]);
  });

  it("should return default flow for wrong related origins", () => {
    const result = findWebAuthnFlows({
      supportsRor: true,
      devices: [
        createMockCredential(currentOrigin),
        createMockCredential(currentOrigin),
        createMockCredential(nonCurrentOrigin1),
        createMockCredential(nonCurrentOrigin2),
        createMockCredential(nonCurrentOrigin2),
      ],
      currentOrigin: currentOrigin,
      relatedOrigins: [
        "https://not-identity.ic0.app",
        "https://not-identity.icp0.io",
      ],
    });

    expect(result).toEqual([{ useIframe: false, rpId: undefined }]);
  });

  it("should return flows in order of devices (recently used)", () => {
    const result = findWebAuthnFlows({
      supportsRor: true,
      devices: [
        createMockCredential(nonCurrentOrigin2),
        createMockCredential(currentOrigin),
        createMockCredential(currentOrigin),
        createMockCredential(nonCurrentOrigin1),
        createMockCredential(nonCurrentOrigin2),
      ],
      currentOrigin: currentOrigin,
      relatedOrigins: [currentOrigin, nonCurrentOrigin1, nonCurrentOrigin2],
    });

    expect(result).toEqual([
      { useIframe: true, rpId: nonCurrentOrigin2RpId },
      { useIframe: false, rpId: undefined },
      { useIframe: true, rpId: nonCurrentOrigin1RpId },
    ]);
  });
});
