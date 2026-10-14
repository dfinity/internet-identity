/**
 * An actor signing as this app, which is how the canister knows whose rows these are.
 *
 * Takes the canister and the agent's options rather than reading either, because the two
 * places that build one cannot read the same source: the page has `$lib/globals`, which
 * reads the document, and the service worker has only what the page put on its
 * registration URL. A module that reached for `$lib/globals` here would pull the page's
 * DOM-bound code into a worker that has no `document`.
 */
import { ECDSAKeyIdentity } from "@icp-sdk/core/identity";
import {
  Actor,
  HttpAgent,
  type ActorSubclass,
  type HttpAgentOptions,
} from "@icp-sdk/core/agent";
import { idlFactory as internetIdentityIDL } from "$lib/generated/internet_identity_idl";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import type { Principal } from "@icp-sdk/core/principal";
import type { AppKeyRecord } from "./notificationAppKey";

export const appKeyActorWith = async (
  record: AppKeyRecord,
  {
    agentOptions,
    canisterId,
  }: { agentOptions: HttpAgentOptions; canisterId: Principal },
): Promise<ActorSubclass<_SERVICE>> => {
  const identity = await ECDSAKeyIdentity.fromKeyPair(record.keyPair);
  return Actor.createActor<_SERVICE>(internetIdentityIDL, {
    agent: HttpAgent.createSync({ ...agentOptions, identity }),
    canisterId,
  });
};
