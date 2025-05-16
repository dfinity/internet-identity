import { Principal } from "@dfinity/principal";
import {
  type _SERVICE,
  InternetIdentityInit,
} from "$lib/generated/internet_identity_types";
import { readCanisterConfig, readCanisterId } from "$lib/utils/init";
import {
  Actor,
  ActorSubclass,
  HttpAgent,
  HttpAgentOptions,
} from "@dfinity/agent";
import { inferHost } from "$lib/utils/iiConnection";
import { idlFactory as internet_identity_idl } from "$lib/generated/internet_identity_idl";
import { features } from "$lib/legacy/features";
import { LazyHttpAgent } from "$lib/utils/lazyHttpAgent";

export let canisterId: Principal;
export let canisterConfig: InternetIdentityInit;
export let agentOptions: HttpAgentOptions;
export let anonymousAgent: HttpAgent;
export let anonymousActor: ActorSubclass<_SERVICE>;

export const initGlobals = () => {
  canisterId = Principal.fromText(readCanisterId());
  canisterConfig = readCanisterConfig();
  agentOptions = {
    host: inferHost(),
    shouldFetchRootKey:
      features.FETCH_ROOT_KEY || (canisterConfig.fetch_root_key[0] ?? false),
  };
  anonymousAgent = LazyHttpAgent.createLazy(agentOptions);
  anonymousActor = Actor.createActor<_SERVICE>(internet_identity_idl, {
    agent: anonymousAgent,
    canisterId,
  });
};
