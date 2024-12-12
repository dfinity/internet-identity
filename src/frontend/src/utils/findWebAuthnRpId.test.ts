import { CredentialData } from "./credential-devices";
import {
  BETA_DOMAINS,
  PROD_DOMAINS,
  findWebAuthnRpId,
} from "./findWebAuthnRpId";

describe("findWebAuthnRpId", () => {
  const mockDeviceData = (origin?: string): CredentialData => ({
    origin,
    credentialId: new ArrayBuffer(1),
    pubkey: new ArrayBuffer(1),
  });

  test("returns undefined if a device is registered for the current domain", () => {
    const devices: CredentialData[] = [
      mockDeviceData("https://identity.ic0.app"),
      mockDeviceData("https://identity.internetcomputer.org"),
      mockDeviceData("https://identity.icp0.io"),
    ];
    const currentUrl = "https://identity.ic0.app";

    expect(findWebAuthnRpId(currentUrl, devices, PROD_DOMAINS)).toBeUndefined();
  });

  test("returns undefined for devices with default domain when the current domain matches", () => {
    const devices: CredentialData[] = [
      mockDeviceData(), // Empty origin defaults to defaultDomain `https://identity.ic0.ap`
      mockDeviceData("https://identity.internetcomputer.org"),
      mockDeviceData("https://identity.icp0.io"),
    ];
    const currentUrl = "https://identity.ic0.app";

    expect(findWebAuthnRpId(currentUrl, devices, PROD_DOMAINS)).toBeUndefined();
  });

  test("returns undefined if a device is registered for the current domain for beta domains", () => {
    const devices: CredentialData[] = [
      mockDeviceData("https://beta.identity.ic0.app"),
      mockDeviceData("https://beta.identity.internetcomputer.org"),
    ];
    const currentUrl = "https://beta.identity.ic0.app";

    expect(findWebAuthnRpId(currentUrl, devices, BETA_DOMAINS)).toBeUndefined();
  });

  test("returns undefined if a device is registered for the current domain", () => {
    const devices: CredentialData[] = [
      mockDeviceData("https://identity.ic0.app"),
      mockDeviceData("https://identity.internetcomputer.org"),
      mockDeviceData("https://identity.icp0.io"),
    ];
    const currentUrl = "https://identity.internetcomputer.org";

    expect(findWebAuthnRpId(currentUrl, devices, PROD_DOMAINS)).toBeUndefined();
  });

  test("returns undefined if a device is registered for the current domain", () => {
    const devices: CredentialData[] = [
      mockDeviceData("https://beta.identity.ic0.app"),
      mockDeviceData("https://fgte5-ciaaa-aaaad-aaatq-cai.ic0.app"),
    ];
    const currentUrl = "https://fgte5-ciaaa-aaaad-aaatq-cai.ic0.app";

    expect(findWebAuthnRpId(currentUrl, devices, BETA_DOMAINS)).toBeUndefined();
  });

  test("returns the second default preferred domain if no device is registered for the current domain", () => {
    const devices: CredentialData[] = [
      mockDeviceData("https://identity.internetcomputer.org"),
      mockDeviceData("https://identity.icp0.io"),
    ];
    const currentUrl = "https://identity.ic0.app";

    expect(findWebAuthnRpId(currentUrl, devices, PROD_DOMAINS)).toBe(
      "identity.internetcomputer.org"
    );
  });

  test("returns last beta if a device is registered for the current domain", () => {
    const devices: CredentialData[] = [
      mockDeviceData("https://fgte5-ciaaa-aaaad-aaatq-cai.ic0.app"),
    ];
    const currentUrl = "https://beta.identity.ic0.app";

    expect(findWebAuthnRpId(currentUrl, devices, BETA_DOMAINS)).toBe(
      "fgte5-ciaaa-aaaad-aaatq-cai.ic0.app"
    );
  });

  test("returns the first default preferred domain if no device is registered for the current domain", () => {
    const devices: CredentialData[] = [
      mockDeviceData("https://identity.ic0.app"),
      mockDeviceData("https://identity.icp0.io"),
    ];
    const currentUrl = "https://identity.internetcomputer.org";

    expect(findWebAuthnRpId(currentUrl, devices, PROD_DOMAINS)).toBe(
      "identity.ic0.app"
    );
  });

  test("returns the least preferred domain if devices are only on that domain", () => {
    const devices: CredentialData[] = [
      mockDeviceData("https://identity.icp0.io"),
    ];
    const currentUrl = "https://identity.ic0.app";

    expect(findWebAuthnRpId(currentUrl, devices, PROD_DOMAINS)).toBe(
      "identity.icp0.io"
    );
  });

  test("uses preferred domains when provided", () => {
    // Switch the order of the domains, internetcomputer.org is moved to last.
    const switchedDomains = [
      "https://identity.ic0.app",
      "https://identity.icp0.io",
      "https://identity.internetcomputer.org",
    ];

    const devices: CredentialData[] = [
      mockDeviceData("https://identity.internetcomputer.org"),
      mockDeviceData("https://identity.icp0.io"),
    ];
    const currentUrl = "https://identity.ic0.app";

    expect(findWebAuthnRpId(currentUrl, devices, switchedDomains)).toBe(
      "identity.icp0.io"
    );
  });

  test("throws an error if the current domain is invalid", () => {
    const devices: CredentialData[] = [
      mockDeviceData("https://identity.ic0.app"),
    ];
    const currentUrl = "not-a-valid-url";

    expect(() =>
      findWebAuthnRpId(currentUrl, devices, PROD_DOMAINS)
    ).toThrowError("Invalid URL: not-a-valid-url");
  });

  test("throws an error if no devices are registered for the current or preferred domains", () => {
    const devices: CredentialData[] = [
      mockDeviceData("https://otherdomain.com"),
    ];
    const currentUrl = "https://identity.ic0.app";

    expect(() =>
      findWebAuthnRpId(currentUrl, devices, PROD_DOMAINS)
    ).toThrowError(
      "Not possible. Devices must be registered for at least one of the following domains: ic0.app, internetcomputer.org, icp0.io"
    );
  });

  test("throws an error if there are no registered devices", () => {
    const devices: CredentialData[] = [];
    const currentUrl = "https://identity.ic0.app";

    expect(() =>
      findWebAuthnRpId(currentUrl, devices, PROD_DOMAINS)
    ).toThrowError(
      "Not possible. Every registered user has at least one device."
    );
  });
});
