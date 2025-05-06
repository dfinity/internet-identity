import { type Readable, derived, writable } from "svelte/store";
import { DelegationIdentity } from "@dfinity/identity";
import { isNullish, nonNullish } from "@dfinity/utils";
import {
  Actor,
  ActorSubclass,
  HttpAgent,
  HttpAgentOptions,
} from "@dfinity/agent";
import { Principal } from "@dfinity/principal";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { idlFactory as internet_identity_idl } from "$lib/generated/internet_identity_idl";

export interface Authenticated {
  identityNumber: bigint;
  identity: DelegationIdentity;
  agent: HttpAgent;
  actor: ActorSubclass<_SERVICE>;
}

type AuthenticationStore = Readable<Authenticated | undefined> & {
  init: (params: {
    canisterId: Principal;
    agentOptions: HttpAgentOptions;
  }) => Promise<void>;
  set: (value: Omit<Authenticated, "agent" | "actor">) => void;
  reset: () => void;
};

const internalStore = writable<{
  authenticated?: Omit<Authenticated, "agent" | "actor">;
  initialized?: Pick<Authenticated, "agent" | "actor">;
}>();

export const authenticationStore: AuthenticationStore = {
  init: async ({ canisterId, agentOptions }) => {
    const agent = await HttpAgent.create(agentOptions);
    const actor = Actor.createActor<_SERVICE>(internet_identity_idl, {
      agent,
      canisterId,
    });
    internalStore.set({ initialized: { agent, actor } });
  },
  subscribe: derived(internalStore, ({ authenticated, initialized }) => {
    if (isNullish(initialized)) {
      throw new Error("Not initialized");
    }
    if (isNullish(authenticated)) {
      return undefined;
    }
    return { ...authenticated, ...initialized };
  }).subscribe,
  set: (authenticated) =>
    internalStore.update(({ initialized }) => {
      if (isNullish(initialized)) {
        throw new Error("Not initialized");
      }
      initialized.agent.replaceIdentity(authentication.identity);
      return { authenticated, initialized };
    }),
  reset: () =>
    internalStore.update(({ initialized }) => {
      if (isNullish(initialized)) {
        throw new Error("Not initialized");
      }
      initialized.agent.invalidateIdentity();
      return { authenticated: undefined, initialized };
    }),
};

export const authenticatedStore: Readable<Authenticated> = derived(
  authenticationStore,
  (authenticated) => {
    if (isNullish(authentication)) {
      throw new Error("Not authenticated");
    }
    return authenticated;
  },
);

export const isAuthenticatedStore: Readable<boolean> = derived(
  authenticationStore,
  (authenticated) => nonNullish(authenticated),
);
