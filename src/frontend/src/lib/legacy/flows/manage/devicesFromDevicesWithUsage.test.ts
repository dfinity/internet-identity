import type {
  DeviceWithUsage,
  PublicKey,
} from "$lib/generated/internet_identity_types";
import { DOMAIN_COMPATIBILITY } from "$lib/state/featureFlags";
import { AuthenticatedConnection } from "$lib/utils/iiConnection";
import { isNullish } from "@dfinity/utils";
import { devicesFromDevicesWithUsage } from "./index";

describe("devicesFromDevicesWithUsage", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.stubGlobal("location", {
      origin: "https://identity.ic0.app",
    });
    vi.stubGlobal("navigator", {
      // Supports RoR
      userAgent:
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.0 Safari/605.1.15",
    });
    DOMAIN_COMPATIBILITY.getFeatureFlag()?.reset();
  });

  const currentDevicePubKey = new Uint8Array([1]);
  const mockConnection = {
    getSignIdentityPubKey: () => currentDevicePubKey,
  } as unknown as AuthenticatedConnection;

  const createDevice = (
    origin: string | undefined,
    pubkey?: PublicKey,
  ): DeviceWithUsage => ({
    alias: "alias",
    last_usage: [],
    metadata: [],
    origin: isNullish(origin) ? [] : [origin],
    protection: { unprotected: null },
    pubkey: pubkey ?? [],
    key_type: { platform: null },
    purpose: { authentication: null },
    credential_id: [],
  });

  describe("domains compatibility flag disabled", () => {
    beforeEach(() => {
      DOMAIN_COMPATIBILITY.getFeatureFlag()?.set(false);
    });

    it("returns warning icon in the device in different origin als current", () => {
      const devices = [
        createDevice(undefined, currentDevicePubKey),
        createDevice("https://identity.ic0.app"),
        createDevice("https://identity.internetcomputer.org"),
      ];
      const expectedDevices = devicesFromDevicesWithUsage({
        devices,
        reload: () => undefined,
        connection: mockConnection,
        userNumber: BigInt(12345),
        hasOtherAuthMethods: false,
      });

      expect(expectedDevices.authenticators[0].warn).toBeUndefined();
      expect(expectedDevices.authenticators[1].warn).toBeUndefined();
      expect(expectedDevices.authenticators[2].warn?.strings[0]).toContain(
        "This Passkey may not be usable on the current URL",
      );
    });
  });

  describe("domains compatibility flag enabled", () => {
    beforeEach(() => {
      DOMAIN_COMPATIBILITY.getFeatureFlag()?.set(true);
    });

    it("returns isCurrent as expected", () => {
      const devices = [
        createDevice(undefined, currentDevicePubKey),
        createDevice("https://identity.ic0.app"),
        createDevice("https://identity.internetcomputer.org"),
      ];
      const expectedDevices = devicesFromDevicesWithUsage({
        devices,
        reload: () => undefined,
        connection: mockConnection,
        userNumber: BigInt(12345),
        hasOtherAuthMethods: false,
      });

      // We rely on the order of the devices to determine the current device
      expect(expectedDevices.authenticators[0].isCurrent).toBe(true);
    });

    it("returns an rpId icon on each device if they are not all in the same origin", () => {
      const devices = [
        createDevice(undefined, currentDevicePubKey),
        createDevice("https://identity.ic0.app"),
        createDevice("https://identity.internetcomputer.org"),
      ];
      const expectedDevices = devicesFromDevicesWithUsage({
        devices,
        reload: () => undefined,
        connection: mockConnection,
        userNumber: BigInt(12345),
        hasOtherAuthMethods: false,
      });

      for (const device of expectedDevices.authenticators) {
        expect(device.rpId).toBeDefined();
        expect(device.warn).toBeUndefined();
      }
    });

    it("returns no info nor warning if all devices are in the same origin", () => {
      const devices = [
        createDevice(undefined),
        createDevice("https://identity.ic0.app", currentDevicePubKey),
      ];
      const expectedDevices = devicesFromDevicesWithUsage({
        devices,
        reload: () => undefined,
        connection: mockConnection,
        userNumber: BigInt(12345),
        hasOtherAuthMethods: false,
      });

      for (const device of expectedDevices.authenticators) {
        expect(device.info).toBeUndefined();
        expect(device.warn).toBeUndefined();
      }
    });
  });
});
