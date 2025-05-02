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

export interface Authentication {
  identityNumber: bigint;
  identity: DelegationIdentity;
  agent: HttpAgent;
  actor: ActorSubclass<_SERVICE>;
}

type AuthenticationStore = Readable<Authentication> & {
  init: (params: {
    canisterId: Principal;
    agentOptions: HttpAgentOptions;
  }) => Promise<void>;
  isAuthenticated: Readable<boolean>;
  set: (value: Omit<Authentication, "agent" | "actor">) => void;
  reset: () => void;
};

const createAuthenticationStore = (): AuthenticationStore => {
  const store = writable<{
    authentication?: Omit<Authentication, "agent" | "actor">;
    initialized?: Pick<Authentication, "agent" | "actor">;
  }>();

  return {
    init: async ({ canisterId, agentOptions }) => {
      const agent = await HttpAgent.create(agentOptions);
      const actor = Actor.createActor<_SERVICE>(internet_identity_idl, {
        agent,
        canisterId,
      });
      store.set({ initialized: { agent, actor } });
    },
    isAuthenticated: derived(store, ({ authentication }) =>
      nonNullish(authentication),
    ),
    subscribe: derived(store, ({ authentication, initialized }) => {
      if (isNullish(initialized)) {
        throw new Error("Not initialized");
      }
      if (isNullish(authentication)) {
        throw new Error("Not authenticated");
      }
      return { ...authentication, ...initialized };
    }).subscribe,
    set: (authentication) =>
      store.update(({ initialized }) => {
        if (isNullish(initialized)) {
          throw new Error("Not initialized");
        }
        initialized.agent.replaceIdentity(authentication.identity);
        return { authentication, initialized };
      }),
    reset: () =>
      store.update(({ initialized }) => {
        if (isNullish(initialized)) {
          throw new Error("Not initialized");
        }
        initialized.agent.invalidateIdentity();
        return { authentication: undefined, initialized };
      }),
  };
};

export const authenticationStore = createAuthenticationStore();
