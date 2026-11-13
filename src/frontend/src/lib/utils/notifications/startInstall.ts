/** The first half of an install: a one-shot key this browser registers and hands to the app it is about to add. */
import type { ActorSubclass } from "@icp-sdk/core/agent";
import { ECDSAKeyIdentity } from "@icp-sdk/core/identity";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { throwCanisterError } from "$lib/utils/utils";
import { browserKeyActor, UnregisteredBrowserError } from "./browserActor";
import { exportLinkKey, generateLinkKey } from "./linkKey";

/** `undefined` where no sign-in has registered this browser; rejects where its key could not be read, which is not the same thing. */
const registeredBrowserActor = async (
  identityNumber: bigint,
): Promise<ActorSubclass<_SERVICE> | undefined> => {
  try {
    return await browserKeyActor(identityNumber);
  } catch (error) {
    if (error instanceof UnregisteredBrowserError) {
      return undefined;
    }
    throw error;
  }
};

/** Starts an install for this browser and answers the key it is waiting on; `undefined` where this browser is not registered.
 *  Rejects where its key could not be read or the canister refused the install, which it does while the app it would replace is still answering, so a caller that offers an install asks the app to prove itself first. */
export const startInstall = async (
  identityNumber: bigint,
): Promise<Uint8Array | undefined> => {
  const actor = await registeredBrowserActor(identityNumber);
  if (actor === undefined) {
    return undefined;
  }
  const keyPair = await generateLinkKey();
  const [identity, linkKey] = await Promise.all([
    ECDSAKeyIdentity.fromKeyPair(keyPair),
    exportLinkKey(keyPair),
  ]);
  // The slot is written before the key is handed out, so a key that exists is one the canister is waiting for.
  await actor
    .prepare_notification_app_link({
      anchor_number: identityNumber,
      link_principal: identity.getPrincipal(),
    })
    .then(throwCanisterError);
  return linkKey;
};
