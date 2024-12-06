import { DeviceData } from "$generated/internet_identity_types";
import { findWebAuthnRpId } from "./findWebAuthnRpId";

describe("findWebAuthnRpId", () => {
  const mockDeviceData = (origin: [] | [string]): DeviceData => ({
    origin,
    alias: "test-device",
    metadata: [],
    protection: { protected: null },
    pubkey: [],
    key_type: { platform: null },
    purpose: { authentication: null },
    credential_id: [],
  });

  beforeEach(() => {
    vi.spyOn(console, "error").mockImplementation(() => {});
  });

  test("returns undefined if a device is registered for the current domain", () => {
    const devices: DeviceData[] = [
      mockDeviceData(["https://identity.ic0.app"]),
      mockDeviceData(["https://identity.internetcomputer.org"]),
      mockDeviceData(["https://identity.icp0.io"]),
    ];
    const currentUrl = "https://identity.ic0.app";

    expect(findWebAuthnRpId(currentUrl, devices)).toBeUndefined();
  });

  test("returns undefined for devices with default domain when the current domain matches", () => {
    const devices: DeviceData[] = [
      mockDeviceData([]), // Empty origin defaults to defaultDomain `https://identity.ic0.app`
      mockDeviceData(["https://identity.internetcomputer.org"]),
      mockDeviceData(["https://identity.icp0.io"]),
    ];
    const currentUrl = "https://identity.ic0.app";

    expect(findWebAuthnRpId(currentUrl, devices)).toBeUndefined();
  });

  test("returns undefined if a device is registered for the current domain", () => {
    const devices: DeviceData[] = [
      mockDeviceData(["https://beta.identity.ic0.app"]),
      mockDeviceData(["https://beta.identity.internetcomputer.org"]),
    ];
    const currentUrl = "https://beta.identity.ic0.app";

    expect(findWebAuthnRpId(currentUrl, devices)).toBeUndefined();
  });

  test("returns undefined if a device is registered for the current domain", () => {
    const devices: DeviceData[] = [
      mockDeviceData(["https://identity.ic0.app"]),
      mockDeviceData(["https://identity.internetcomputer.org"]),
      mockDeviceData(["https://identity.icp0.io"]),
    ];
    const currentUrl = "https://identity.internetcomputer.org";

    expect(findWebAuthnRpId(currentUrl, devices)).toBeUndefined();
  });

  test("returns the second default preferred domain if no device is registered for the current domain", () => {
    const devices: DeviceData[] = [
      mockDeviceData(["https://identity.internetcomputer.org"]),
      mockDeviceData(["https://identity.icp0.io"]),
    ];
    const currentUrl = "https://identity.ic0.app";

    expect(findWebAuthnRpId(currentUrl, devices)).toBe(
      "https://identity.internetcomputer.org"
    );
  });

  test("returns the first default preferred domain if no device is registered for the current domain", () => {
    const devices: DeviceData[] = [
      mockDeviceData(["https://identity.ic0.app"]),
      mockDeviceData(["https://identity.icp0.io"]),
    ];
    const currentUrl = "https://identity.internetcomputer.org";

    expect(findWebAuthnRpId(currentUrl, devices)).toBe(
      "https://identity.ic0.app"
    );
  });

  test("returns the least preferred domain if devices are only on that domain", () => {
    const devices: DeviceData[] = [
      mockDeviceData(["https://identity.icp0.io"]),
    ];
    const currentUrl = "https://identity.ic0.app";

    expect(findWebAuthnRpId(currentUrl, devices)).toBe(
      "https://identity.icp0.io"
    );
  });

  test("uses preferred domains when provided", () => {
    const preferredDomains = ["ic0.app", "icp0.io", "internetcomputer.org"];

    const devices: DeviceData[] = [
      mockDeviceData(["https://identity.internetcomputer.org"]),
      mockDeviceData(["https://identity.icp0.io"]),
    ];
    const currentUrl = "https://identity.ic0.app";

    expect(findWebAuthnRpId(currentUrl, devices, preferredDomains)).toBe(
      "https://identity.icp0.io"
    );
  });

  test("throws an error if the current domain is invalid", () => {
    const devices: DeviceData[] = [
      mockDeviceData(["https://identity.ic0.app"]),
    ];
    const currentUrl = "not-a-valid-url";

    expect(() => findWebAuthnRpId(currentUrl, devices)).toThrowError(
      "Invalid URL: not-a-valid-url"
    );
  });

  test("throws an error if no devices are registered for the current or preferred domains", () => {
    const devices: DeviceData[] = [mockDeviceData(["https://otherdomain.com"])];
    const currentUrl = "https://identity.ic0.app";

    expect(() => findWebAuthnRpId(currentUrl, devices)).toThrowError(
      "Not possible. Devices must be registered for at least one of the following domains: ic0.app, internetcomputer.org, icp0.io"
    );
  });

  test("throws an error if there are no registered devices", () => {
    const devices: DeviceData[] = [];
    const currentUrl = "https://identity.ic0.app";

    expect(() => findWebAuthnRpId(currentUrl, devices)).toThrowError(
      "Not possible. Every registered user has at least one device."
    );
  });
});
