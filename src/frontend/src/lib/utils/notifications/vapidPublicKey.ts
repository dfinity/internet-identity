// Internet Identity's own VAPID public key, which a browser subscribes with.
//
// Held by the canister and the same for every browser, so it is asked for once and kept
// for the life of the page.

import { anonymousActor } from "./browserActor";

let held: Promise<Uint8Array> | undefined;

/**
 * Answers the key, asking the canister the first time.
 *
 * Only an answer is kept. A rejection clears what is held, so the next caller asks again:
 * one that stayed would answer every later call from the same failure, and nothing short
 * of a reload could turn notifications on after the network came back.
 */
export const vapidPublicKey = (): Promise<Uint8Array> => {
  held ??= (async () => {
    const actor = await anonymousActor();
    const [key] = await actor.get_vapid_public_key();
    if (key === undefined) {
      throw new Error("Internet Identity has not published its VAPID key yet");
    }
    return new Uint8Array(key);
  })().catch((error: unknown) => {
    held = undefined;
    throw error;
  });
  return held;
};
