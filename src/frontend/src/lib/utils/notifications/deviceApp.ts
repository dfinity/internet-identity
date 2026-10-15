/**
 * Pointing this identity at the Home Screen app another identity on this browser has.
 *
 * The app is one thing on the device and delivers for every identity pointing at it, but
 * nothing here can reach it: it runs in a storage partition no browser can read. What
 * can reach both is this browser, which holds a key per identity signed in on it. So one
 * of those identities offers a handle and the identity being added spends it, and the
 * app is not involved at all: it is installed, it has its record, and it is already
 * registered for push, so delivery starts the moment the pointing is written.
 */
import { registeredIdentityNumbers } from "$lib/stores/browser-key.store";
import { throwCanisterError } from "$lib/utils/utils";
import { browserKeyActor } from "./browserActor";

/** A handle from another identity signed in here, or `undefined` where none of them has
 *  an app to offer. */
const offerFromAnother = async (
  exclude: bigint,
): Promise<Uint8Array | undefined> => {
  const identityNumbers = (
    await registeredIdentityNumbers().catch(() => [])
  ).filter((identityNumber) => identityNumber !== exclude);

  for (const identityNumber of identityNumbers) {
    try {
      const actor = await browserKeyActor(identityNumber);
      const handle = await actor
        .offer_notification_app({ anchor_number: identityNumber })
        .then(throwCanisterError);
      return new Uint8Array(handle);
    } catch {
      // An identity with no app, or one this browser can no longer call as, answers
      // nothing: the next one is asked.
      continue;
    }
  }
  return undefined;
};

/**
 * Points `identityNumber` at the app this device already has, where another identity on
 * this browser can offer it.
 *
 * Answers whether it now has one. `false` is not a failure: it is a device whose app
 * nothing here can vouch for, and the install is what is offered instead.
 */
export const claimDeviceApp = async (
  identityNumber: bigint,
): Promise<boolean> => {
  const handle = await offerFromAnother(identityNumber);
  if (handle === undefined) {
    return false;
  }
  try {
    const actor = await browserKeyActor(identityNumber);
    await actor
      .claim_notification_app({
        anchor_number: identityNumber,
        handle,
      })
      .then(throwCanisterError);
    return true;
  } catch (error) {
    console.error(error);
    return false;
  }
};
