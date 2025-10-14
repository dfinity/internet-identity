import type { IdentityAnchorInfo } from "$lib/generated/internet_identity_types";
import { createdAtMillis } from "./identityInfo";

describe("identityInfo", () => {
  it("should return created at in millis", () => {
    const createdAt = BigInt(1728889824000000000);
    const identityInfo: IdentityAnchorInfo = {
      name: [],
      devices: [],
      openid_credentials: [],
      device_registration: [],
      created_at: [createdAt],
    };

    expect(createdAtMillis(identityInfo)).toEqual(
      Number(createdAt / BigInt(1000000)),
    );
  });

  it("should return undefined if created at is not defined", () => {
    const identityInfo: IdentityAnchorInfo = {
      name: [],
      devices: [],
      openid_credentials: [],
      device_registration: [],
      created_at: [],
    };

    expect(createdAtMillis(identityInfo)).toBeUndefined();
  });
});
