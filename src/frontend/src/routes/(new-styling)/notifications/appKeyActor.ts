/**
 * An actor signing as this app, which is how the canister knows whose rows these are.
 *
 * Here rather than beside the key it signs with, because it reads `$lib/globals`, and
 * that reaches the page's DOM-bound modules. The service worker reads the key store.
 */
import { ECDSAKeyIdentity } from "@icp-sdk/core/identity";
import { Actor, HttpAgent, type ActorSubclass } from "@icp-sdk/core/agent";
import { idlFactory as internetIdentityIDL } from "$lib/generated/internet_identity_idl";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { agentOptions, canisterId } from "$lib/globals";
import type { AppKeyRecord } from "$lib/utils/notifications/notificationAppKey";

export const appKeyActor = async (
  record: AppKeyRecord,
): Promise<ActorSubclass<_SERVICE>> => {
  const identity = await ECDSAKeyIdentity.fromKeyPair(record.keyPair);
  return Actor.createActor<_SERVICE>(internetIdentityIDL, {
    agent: HttpAgent.createSync({ ...agentOptions, identity }),
    canisterId,
  });
};
