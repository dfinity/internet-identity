/**
 * The notification app this device has, as a browser can find it.
 *
 * On iOS what reaches the user is a Home Screen app, and it keeps its key in a storage
 * partition no browser can read. A browser that wants the same app to deliver for
 * another identity therefore has to be told what it is, and the canister is what tells
 * it: the app is an entry on the identity that installed it, and that identity's browser
 * is entitled to read its own entry.
 *
 * So the question "is there an app on this device" is asked of every identity this
 * browser holds a key for, not of local storage.
 */
import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { registeredIdentityNumbers } from "$lib/stores/browser-key.store";
import { browserKeyActor } from "./browserActor";

/** Everything the canister needs to give another identity the same app. */
export interface DeviceApp {
  /** The app's public key, DER-encoded. */
  appKey: Uint8Array;
  endpoint: string;
  vapidPublicKey: Uint8Array;
  /** One signature per validity window, carrying no identity, so the same pool
   *  registers this endpoint under any anchor. */
  jwtSignatures: Uint8Array[];
  jwtIssuedAtNs: bigint;
}

/** What `identityNumber`'s own browser entry has linked, where it has one. */
const linkedFor = async (
  identityNumber: bigint,
  caller?: ActorSubclass<_SERVICE>,
): Promise<DeviceApp | undefined> => {
  try {
    const actor = caller ?? (await browserKeyActor(identityNumber));
    const [link] = await actor.notification_app_link({
      anchor_number: identityNumber,
    });
    if (link === undefined) {
      return undefined;
    }
    return {
      appKey: new Uint8Array(link.app_key),
      endpoint: link.endpoint,
      vapidPublicKey: new Uint8Array(link.vapid_public_key),
      jwtSignatures: link.jwt_signatures.map(
        (signature) => new Uint8Array(signature),
      ),
      jwtIssuedAtNs: link.jwt_issued_at_ns,
    };
  } catch {
    // An identity this browser cannot call as answers nothing, which is the same as
    // having no app: the next one is asked.
    return undefined;
  }
};

/**
 * The app installed from this browser, found through any identity signed in here.
 *
 * `exclude` is the identity being asked about, since an app it already has is not
 * something it needs to be given.
 */
export const findDeviceApp = async (
  exclude: bigint,
): Promise<DeviceApp | undefined> => {
  const identityNumbers = (
    await registeredIdentityNumbers().catch(() => [])
  ).filter((identityNumber) => identityNumber !== exclude);
  // Side by side: this runs while the user waits on the answer, and a browser holds
  // few enough identities for that to stay one round trip.
  const links = await Promise.all(
    identityNumbers.map((identityNumber) => linkedFor(identityNumber)),
  );
  return links.find((link) => link !== undefined);
};
