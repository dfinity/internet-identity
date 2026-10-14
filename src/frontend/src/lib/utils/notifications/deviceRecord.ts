/**
 * What the Home Screen app leaves where the browsers on this device can read it.
 *
 * iOS gives a Home Screen app its own cookies, local storage and IndexedDB, but the
 * Cache API is shared across every icon of a site and the browser it was installed
 * from. That is the one channel between the app and a browser tab, and it is same
 * origin, so only our own code can write what is read back here.
 *
 * What travels through it is the app's public key and the subscription it holds, which
 * is everything a browser needs to have the app deliver for another identity, plus the
 * identities that have been claimed so far, which is how the app learns about the ones
 * it did not claim itself.
 *
 * Every call answers rather than throws: a browser where the Cache API is missing,
 * refused or not shared has no record, which is the same answer as a device with no app
 * and leads to the install.
 */
import { fromBase64, toBase64 } from "$lib/utils/utils";

const CACHE_NAME = "ii-notification-device";
/** Never fetched. A URL is what the Cache API keys on, and this one is ours. */
const RECORD_URL = "/notifications/device-record";

/** The record as it is stored, which JSON has to be able to hold. */
interface StoredDeviceRecord {
  appKey: string;
  endpoint: string;
  vapidPublicKey: string;
  jwtSignatures: string[];
  jwtIssuedAtNs: string;
  identityNumbers: string[];
}

export interface DeviceRecord {
  /** The app's public key, DER-encoded. */
  appKey: Uint8Array;
  endpoint: string;
  vapidPublicKey: Uint8Array;
  /** One signature per validity window, as the canister reassembles them. */
  jwtSignatures: Uint8Array[];
  jwtIssuedAtNs: bigint;
  /** The identities this device's app already delivers for. */
  identityNumbers: bigint[];
}

const encode = (record: DeviceRecord): StoredDeviceRecord => ({
  appKey: toBase64(record.appKey),
  endpoint: record.endpoint,
  vapidPublicKey: toBase64(record.vapidPublicKey),
  jwtSignatures: record.jwtSignatures.map(toBase64),
  jwtIssuedAtNs: record.jwtIssuedAtNs.toString(),
  identityNumbers: record.identityNumbers.map((one) => one.toString()),
});

const decode = (stored: StoredDeviceRecord): DeviceRecord => ({
  appKey: fromBase64(stored.appKey),
  endpoint: stored.endpoint,
  vapidPublicKey: fromBase64(stored.vapidPublicKey),
  jwtSignatures: stored.jwtSignatures.map(fromBase64),
  jwtIssuedAtNs: BigInt(stored.jwtIssuedAtNs),
  identityNumbers: stored.identityNumbers.map(BigInt),
});

const openCache = async (): Promise<Cache | undefined> => {
  if (typeof caches === "undefined") {
    return undefined;
  }
  try {
    return await caches.open(CACHE_NAME);
  } catch {
    return undefined;
  }
};

/** What this device's app published, or `undefined` where there is nothing to read. */
export const readDeviceRecord = async (): Promise<DeviceRecord | undefined> => {
  try {
    const cache = await openCache();
    const response = await cache?.match(RECORD_URL);
    if (response === undefined) {
      return undefined;
    }
    return decode((await response.json()) as StoredDeviceRecord);
  } catch {
    return undefined;
  }
};

/** Answers whether it was written, since nothing downstream works if it was not. */
export const publishDeviceRecord = async (
  record: DeviceRecord,
): Promise<boolean> => {
  try {
    const cache = await openCache();
    if (cache === undefined) {
      return false;
    }
    await cache.put(
      RECORD_URL,
      new Response(JSON.stringify(encode(record)), {
        headers: { "content-type": "application/json" },
      }),
    );
    return true;
  } catch {
    return false;
  }
};

/**
 * Adds an identity to the record a browser has just claimed the app for.
 *
 * Written by the browser rather than by the app, because the app is not running: a
 * claim happens in a tab, and the app reads this list on its next wake-up to know whose
 * queue it now has to ask about.
 */
export const recordClaimedIdentity = async (
  identityNumber: bigint,
): Promise<void> => {
  const record = await readDeviceRecord();
  if (record === undefined || record.identityNumbers.includes(identityNumber)) {
    return;
  }
  await publishDeviceRecord({
    ...record,
    identityNumbers: [...record.identityNumbers, identityNumber],
  });
};
