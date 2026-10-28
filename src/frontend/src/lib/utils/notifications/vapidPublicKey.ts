// Internet Identity's own VAPID public key, which a browser subscribes with.
//
// Held by the canister and the same for every browser, so it is asked for once and kept
// for the life of the page.

import { anonymousActor } from "./browserActor";

let held: Promise<Uint8Array> | undefined;

/** Answers the key, asking the canister the first time. */
export const vapidPublicKey = (): Promise<Uint8Array> => {
  held ??= (async () => {
    const actor = await anonymousActor();
    const [key] = await actor.get_vapid_public_key();
    if (key === undefined) {
      held = undefined;
      throw new Error("Internet Identity has not published its VAPID key yet");
    }
    return new Uint8Array(key);
  })();
  return held;
};
