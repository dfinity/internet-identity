import { beforeEach, describe, expect, it, vi } from "vitest";
import {
  publishDeviceRecord,
  readDeviceRecord,
  recordClaimedIdentity,
  type DeviceRecord,
} from "$lib/utils/notifications/deviceRecord";

/** The Cache API, as much of it as this module uses. Bodies are kept rather than the
 *  responses themselves, since a real cache answers every match with its own. */
const cacheStore = new Map<string, string>();
const open = vi.fn((_name?: string) =>
  Promise.resolve({
    match: (url: string) => {
      const body = cacheStore.get(url);
      return Promise.resolve(
        body === undefined ? undefined : new Response(body),
      );
    },
    put: async (url: string, response: Response) => {
      cacheStore.set(url, await response.text());
    },
  } as unknown as Cache),
);

vi.stubGlobal("caches", { open: (name: string) => open(name) });

const RECORD: DeviceRecord = {
  appKey: Uint8Array.from([1, 2, 3]),
  endpoint: "https://web.push.apple.com/abc",
  vapidPublicKey: Uint8Array.from([4, 5, 6]),
  jwtSignatures: [Uint8Array.from([7]), Uint8Array.from([8])],
  jwtIssuedAtNs: BigInt("1700000000000000000"),
  identityNumbers: [BigInt(10_000)],
};

beforeEach(() => {
  cacheStore.clear();
  vi.clearAllMocks();
});

describe("the device record", () => {
  it("reads back everything it published", async () => {
    expect(await publishDeviceRecord(RECORD)).toBe(true);

    expect(await readDeviceRecord()).toEqual(RECORD);
  });

  it("answers nothing where this device has published nothing", async () => {
    expect(await readDeviceRecord()).toBeUndefined();
  });

  // A browser that cannot see the app's storage is a device with no app as far as
  // anything here can tell, and the install is what it is offered.
  it("answers nothing where the cache cannot be opened", async () => {
    open.mockRejectedValueOnce(new Error("denied"));

    expect(await readDeviceRecord()).toBeUndefined();
  });

  it("says so where it could not publish", async () => {
    open.mockRejectedValueOnce(new Error("denied"));

    expect(await publishDeviceRecord(RECORD)).toBe(false);
  });

  it("adds an identity a browser claimed", async () => {
    await publishDeviceRecord(RECORD);

    await recordClaimedIdentity(BigInt(10_001));

    expect((await readDeviceRecord())?.identityNumbers).toEqual([
      BigInt(10_000),
      BigInt(10_001),
    ]);
  });

  it("leaves the record alone for an identity it already names", async () => {
    await publishDeviceRecord(RECORD);

    await recordClaimedIdentity(BigInt(10_000));

    expect((await readDeviceRecord())?.identityNumbers).toEqual([
      BigInt(10_000),
    ]);
  });

  // Nothing published means no app, so there is no record to add an identity to and
  // writing one would claim an app that does not exist.
  it("writes nothing where there is no record to add to", async () => {
    await recordClaimedIdentity(BigInt(10_000));

    expect(await readDeviceRecord()).toBeUndefined();
  });
});
