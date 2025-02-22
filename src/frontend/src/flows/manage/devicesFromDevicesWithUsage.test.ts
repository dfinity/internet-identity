import { DeviceWithUsage } from "$generated/internet_identity_types";
import { DOMAIN_COMPATIBILITY } from "$src/featureFlags";
import { AuthenticatedConnection } from "$src/utils/iiConnection";
import { isNullish } from "@dfinity/utils";
import { devicesFromDevicesWithUsage } from ".";

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
    DOMAIN_COMPATIBILITY.reset();
  });

  const mockConnection = {} as unknown as AuthenticatedConnection;

  const createDevice = (origin: string | undefined): DeviceWithUsage => ({
    alias: "alias",
    last_usage: [],
    metadata: [],
    origin: isNullish(origin) ? [] : [origin],
    protection: { unprotected: null },
    pubkey: [],
    key_type: { platform: null },
    purpose: { authentication: null },
    credential_id: [],
  });

  describe("domains compatibility flag disabled", () => {
    beforeEach(() => {
      DOMAIN_COMPATIBILITY.set(false);
    });

    it("returns warning icon in the device in different origin als current", () => {
      const devices = [
        createDevice(undefined),
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
        "This Passkey may not be usable on the current URL"
      );
    });
  });

  describe("domains compatibility flag enabled", () => {
    beforeEach(() => {
      DOMAIN_COMPATIBILITY.set(true);
    });

    it("returns an info icon on each device if they are not all in the same origin", () => {
      const devices = [
        createDevice(undefined),
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
        expect(device.info?.strings[0]).toContain(
          "This passkey was registered in"
        );
        expect(device.warn).toBeUndefined();
      }
    });

    it("returns no info nor warning if all devices are in the same origin", () => {
      const devices = [
        createDevice(undefined),
        createDevice("https://identity.ic0.app"),
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
