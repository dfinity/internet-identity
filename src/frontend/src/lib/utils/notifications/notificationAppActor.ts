/**
 * An actor signing as this app, which is how the canister knows whose rows these are.
 *
 * Takes its host and canister from the worker's own config, so the service worker can
 * build one too: `$lib/globals` reads the page's DOM and a worker has none.
 */
import { ECDSAKeyIdentity } from "@icp-sdk/core/identity";
import { Actor, HttpAgent, type ActorSubclass } from "@icp-sdk/core/agent";
import { idlFactory as internetIdentityIDL } from "$lib/generated/internet_identity_idl";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { Principal } from "@icp-sdk/core/principal";
import { config } from "./workerConfig";
import type { AppKeyRecord } from "./notificationAppKey";

export const appKeyActor = async (
  record: AppKeyRecord,
): Promise<ActorSubclass<_SERVICE>> => {
  const identity = await ECDSAKeyIdentity.fromKeyPair(record.keyPair);
  return Actor.createActor<_SERVICE>(internetIdentityIDL, {
    agent: HttpAgent.createSync({ ...config.agentOptions, identity }),
    canisterId: Principal.fromText(config.canisterId),
  });
};
