/**
 * What the app leaves behind for the browsers on this device.
 *
 * Published on every launch rather than once at install: the subscription can be
 * replaced, and the pool it carries is spent by elapsed time, so a browser claiming
 * weeks later needs one signed more recently than the install.
 */
import { loadVapidKey } from "$lib/utils/notifications/vapidKeyStore";
import { relayOriginOf, signJwtPool } from "$lib/utils/notifications/vapidPool";
import {
  publishDeviceRecord,
  readDeviceRecord,
} from "$lib/utils/notifications/deviceRecord";
import {
  appPublicKey,
  type AppKeyRecord,
} from "$lib/utils/notifications/notificationAppKey";

/**
 * Publishes this app's key and subscription, answering whether anything was written.
 *
 * `false` where the app holds no subscription yet, or where this device keeps the
 * app's storage to itself: both leave the browsers on it with the install steps, which
 * is what they show today.
 */
export const publishAppRecord = async (
  record: AppKeyRecord,
): Promise<boolean> => {
  if (record.identityNumber === undefined) {
    return false;
  }
  const stored = await loadVapidKey();
  if (stored === undefined) {
    return false;
  }

  const issuedAtNs = BigInt(Date.now()) * BigInt(1_000_000);
  const [appKey, jwtSignatures, existing] = await Promise.all([
    appPublicKey(record),
    signJwtPool(stored.privateKey, relayOriginOf(stored.endpoint), issuedAtNs),
    readDeviceRecord(),
  ]);

  return publishDeviceRecord({
    appKey,
    endpoint: stored.endpoint,
    vapidPublicKey: stored.publicKeyRaw,
    jwtSignatures,
    jwtIssuedAtNs: issuedAtNs,
    // Identities claimed by a browser are kept: the app did not claim them and has no
    // other way to hear about them.
    identityNumbers: [
      ...new Set([...(existing?.identityNumbers ?? []), record.identityNumber]),
    ],
  });
};
