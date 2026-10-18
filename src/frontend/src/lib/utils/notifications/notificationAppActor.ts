/** Takes the canister and the agent's options rather than reading either: a module reaching for `$lib/globals` here would pull the page's DOM-bound code into a worker that has no `document`. */
import { ECDSAKeyIdentity } from "@icp-sdk/core/identity";
import {
  Actor,
  HttpAgent,
  type ActorSubclass,
  type HttpAgentOptions,
  type SignIdentity,
} from "@icp-sdk/core/agent";
import { idlFactory as internetIdentityIDL } from "$lib/generated/internet_identity_idl";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import type { Principal } from "@icp-sdk/core/principal";
import { importLinkKey } from "./linkKey";
import type { AppKeyRecord } from "./notificationAppKey";

interface AgentTarget {
  agentOptions: HttpAgentOptions;
  canisterId: Principal;
}

const actorAs = (
  identity: SignIdentity,
  { agentOptions, canisterId }: AgentTarget,
): ActorSubclass<_SERVICE> =>
  Actor.createActor<_SERVICE>(internetIdentityIDL, {
    agent: HttpAgent.createSync({ ...agentOptions, identity }),
    canisterId,
  });

export const appKeyActorWith = async (
  record: AppKeyRecord,
  target: AgentTarget,
): Promise<ActorSubclass<_SERVICE>> =>
  actorAs(await ECDSAKeyIdentity.fromKeyPair(record.keyPair), target);

/** The link call is signed by the key the install URL carried, not by the app's own: the canister matches the caller against the slot the browser registered. */
export const linkKeyActorWith = async (
  linkKey: Uint8Array,
  target: AgentTarget,
): Promise<ActorSubclass<_SERVICE>> =>
  actorAs(
    await ECDSAKeyIdentity.fromKeyPair(await importLinkKey(linkKey)),
    target,
  );
