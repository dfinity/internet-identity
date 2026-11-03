// The Web Push endpoints read the browser off the caller, so they are the one place a
// call is signed by the browser key instead of by the identity's session.

import { Actor, HttpAgent, type ActorSubclass } from "@icp-sdk/core/agent";
import { idlFactory as internetIdentityIDL } from "$lib/generated/internet_identity_idl";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { agentOptions, canisterId } from "$lib/globals";
import { browserKeyIdentity } from "$lib/stores/browser-key.store";

/** Thrown where no sign-in has registered this browser. */
export class UnregisteredBrowserError extends Error {
  constructor() {
    super(
      "this browser has not completed a sign-in, so it cannot be registered",
    );
    this.name = "UnregisteredBrowserError";
  }
}

/** An actor signing as this browser rather than as the identity. */
export const browserKeyActor = async (
  identityNumber: bigint,
): Promise<ActorSubclass<_SERVICE>> => {
  const identity = await browserKeyIdentity(identityNumber);
  if (identity === undefined) {
    throw new UnregisteredBrowserError();
  }
  return Actor.createActor<_SERVICE>(internetIdentityIDL, {
    agent: HttpAgent.createSync({ ...agentOptions, identity }),
    canisterId,
  });
};
