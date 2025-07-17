import type { DeviceData } from "$lib/generated/internet_identity_types";
import { getCredentialsOrigin } from "./credential-devices";

describe("credetial-devices test", () => {
  describe("getCredentialsOrigin", () => {
    const createDevice = (
      origin: string | undefined,
    ): Omit<DeviceData, "alias"> => ({
      origin: origin === undefined ? [] : [origin],
      protection: { unprotected: null },
      // eslint-disable-next-line
      pubkey: undefined as any,
      key_type: { unknown: null },
      purpose: { authentication: null },
      credential_id: [],
      metadata: [],
    });

    const undefinedOriginDevice: Omit<DeviceData, "alias"> =
      createDevice(undefined);
    const ic0OriginDevice: Omit<DeviceData, "alias"> = createDevice(
      "https://identity.ic0.app",
    );
    const icOrgOriginDevice: Omit<DeviceData, "alias"> = createDevice(
      "https://identity.internetcomputer.org",
    );
    const icIoOriginDevice: Omit<DeviceData, "alias"> = createDevice(
      "https://identity.icp0.io",
    );

    it("should return a set of origins or `undefined`", () => {
      expect(
        getCredentialsOrigin({
          credentials: [ic0OriginDevice, icOrgOriginDevice, icIoOriginDevice],
        }),
      ).toBeUndefined();

      expect(
        getCredentialsOrigin({
          credentials: [
            ic0OriginDevice,
            { ...ic0OriginDevice },
            icIoOriginDevice,
          ],
        }),
      ).toBeUndefined();

      expect(
        getCredentialsOrigin({
          credentials: [ic0OriginDevice, { ...ic0OriginDevice }],
        }),
      ).toBe("https://identity.ic0.app");

      expect(
        getCredentialsOrigin({
          credentials: [icOrgOriginDevice, { ...icOrgOriginDevice }],
        }),
      ).toBe("https://identity.internetcomputer.org");
    });

    it("should consider `undefined` as the default domain", () => {
      expect(
        getCredentialsOrigin({
          credentials: [undefinedOriginDevice],
        }),
      ).toBe("https://identity.ic0.app");

      expect(
        getCredentialsOrigin({
          credentials: [undefinedOriginDevice, ic0OriginDevice],
        }),
      ).toBe("https://identity.ic0.app");

      expect(
        getCredentialsOrigin({
          credentials: [undefinedOriginDevice, icOrgOriginDevice],
        }),
      ).toBeUndefined();
    });
  });
});
