import { Actor, AnonymousIdentity, HttpAgent } from "@icp-sdk/core/agent";
import type { Identity } from "@icp-sdk/core/agent";

import { idlFactory } from "./declarations/dfinsight_backend.did";
import type { DfinsightBackend } from "./declarations/dfinsight_backend.types";
import { BACKEND_CANISTER_ID, IC_HOST, isLocal } from "./config";

async function makeAgent(identity: Identity): Promise<HttpAgent> {
  const agent = await HttpAgent.create({
    host: IC_HOST,
    identity,
    shouldFetchRootKey: isLocal,
  });
  return agent;
}

export async function makeBackend(identity: Identity): Promise<DfinsightBackend> {
  const agent = await makeAgent(identity);
  return Actor.createActor<DfinsightBackend>(idlFactory, {
    agent,
    canisterId: BACKEND_CANISTER_ID,
  });
}

export async function makeAnonymousBackend(): Promise<DfinsightBackend> {
  return makeBackend(new AnonymousIdentity());
}
