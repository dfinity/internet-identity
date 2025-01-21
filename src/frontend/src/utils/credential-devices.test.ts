import { DeviceData } from "$generated/internet_identity_types";
import { getCredentialsOrigin } from "./credential-devices";

describe("credetial-devices test", () => {
  describe("getCredentialsOrigin", () => {
    const createDevice = (
      origin: string | undefined
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
      "https://identity.ic0.app"
    );
    const icOrgOriginDevice: Omit<DeviceData, "alias"> = createDevice(
      "https://identity.internetcomputer.org"
    );
    const icIoOriginDevice: Omit<DeviceData, "alias"> = createDevice(
      "https://identity.icp0.io"
    );
    const userAgentSupportingRoR =
      "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36";
    const userAgentNotSupportingRoR =
      "Mozilla/5.0 (Android 13; Mobile; rv:132.0) Gecko/132.0 Firefox/132.0";

    it("should return a set of origins", () => {
      expect(
        getCredentialsOrigin({
          credentials: [ic0OriginDevice, icOrgOriginDevice, icIoOriginDevice],
          userAgent: userAgentSupportingRoR,
        })
      ).toBeUndefined();

      expect(
        getCredentialsOrigin({
          credentials: [
            ic0OriginDevice,
            { ...ic0OriginDevice },
            icIoOriginDevice,
          ],
          userAgent: userAgentSupportingRoR,
        })
      ).toBeUndefined();

      expect(
        getCredentialsOrigin({
          credentials: [ic0OriginDevice, { ...ic0OriginDevice }],
          userAgent: userAgentSupportingRoR,
        })
      ).toBe("https://identity.ic0.app");

      expect(
        getCredentialsOrigin({
          credentials: [icOrgOriginDevice, { ...icOrgOriginDevice }],
          userAgent: userAgentSupportingRoR,
        })
      ).toBe("https://identity.internetcomputer.org");
    });

    it("should consider `undefined` as the default domain", () => {
      expect(
        getCredentialsOrigin({
          credentials: [undefinedOriginDevice],
          userAgent: userAgentSupportingRoR,
        })
      ).toBe("https://identity.ic0.app");

      expect(
        getCredentialsOrigin({
          credentials: [undefinedOriginDevice, ic0OriginDevice],
          userAgent: userAgentSupportingRoR,
        })
      ).toBe("https://identity.ic0.app");

      expect(
        getCredentialsOrigin({
          credentials: [undefinedOriginDevice, icOrgOriginDevice],
          userAgent: userAgentSupportingRoR,
        })
      ).toBeUndefined();
    });

    it("returns `undefined` if user doesn't support RoR", () => {
      expect(
        getCredentialsOrigin({
          credentials: [ic0OriginDevice, icOrgOriginDevice, icIoOriginDevice],
          userAgent: userAgentNotSupportingRoR,
        })
      ).toBeUndefined();
    });
  });
});
