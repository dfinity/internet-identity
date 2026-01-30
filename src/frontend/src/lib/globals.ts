import { Principal } from "@icp-sdk/core/principal";
import type {
  _SERVICE,
  InternetIdentityInit,
} from "$lib/generated/internet_identity_types";
import { readCanisterConfig, readCanisterId } from "$lib/utils/init";
import {
  Actor,
  ActorSubclass,
  HttpAgent,
  HttpAgentOptions,
} from "@icp-sdk/core/agent";
import { inferHost } from "$lib/utils/iiConnection";
import { idlFactory as internet_identity_idl } from "$lib/generated/internet_identity_idl";
import { features } from "$lib/legacy/features";

export let canisterId: Principal;
export let canisterConfig: InternetIdentityInit;
export let agentOptions: HttpAgentOptions;
export let anonymousAgent: HttpAgent;
export let anonymousActor: ActorSubclass<_SERVICE>;
export let parentIFrameOrigin: string | undefined;

// Search param passed by parent window to indicate its origin to child window
export const IFRAME_PARENT_PARAM = "parent_origin";

export const initGlobals = () => {
  canisterId = Principal.fromText(readCanisterId());
  canisterConfig = readCanisterConfig();
  agentOptions = {
    host: inferHost(),
    shouldFetchRootKey:
      features.FETCH_ROOT_KEY || (canisterConfig.fetch_root_key[0] ?? false),
  };
  anonymousAgent = HttpAgent.createSync(agentOptions);
  // Fetch subnet keys to speed up queries during authentication,
  // this avoids having to fetch them later on user interaction.
  void anonymousAgent.fetchSubnetKeys(canisterId);
  anonymousActor = Actor.createActor<_SERVICE>(internet_identity_idl, {
    agent: anonymousAgent,
    canisterId,
  });
  // Set when `IFRAME_PARENT_PARAM` search param contains a valid related origin
  parentIFrameOrigin = canisterConfig.related_origins[0]?.find(
    (origin) =>
      origin ===
      new URL(window.location.href).searchParams.get(IFRAME_PARENT_PARAM),
  );
};

// Get primary origin (either https://id.ai or https://beta.id.ai) when deployed on beta or prod
export const getPrimaryOrigin = () =>
  canisterConfig.related_origins[0]?.find((origin) => origin.endsWith("id.ai"));
