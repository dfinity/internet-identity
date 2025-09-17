import { type Readable, derived, writable } from "svelte/store";
import { ECDSAKeyIdentity } from "@dfinity/identity";
import { isNullish } from "@dfinity/utils";
import {
  Actor,
  ActorSubclass,
  HttpAgent,
  HttpAgentOptions,
  SignIdentity,
} from "@dfinity/agent";
import { createAnonymousNonce } from "$lib/utils/openID";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { Principal } from "@dfinity/principal";
import { idlFactory as internet_identity_idl } from "$lib/generated/internet_identity_idl";
import { LazyHttpAgent } from "$lib/utils/lazyHttpAgent";

export interface Session {
  identity: SignIdentity;
  agent: HttpAgent;
  actor: ActorSubclass<_SERVICE>;
  nonce: string;
  salt: Uint8Array;
}

type SessionStore = Readable<Session> & {
  init: (params: {
    canisterId: Principal;
    agentOptions: HttpAgentOptions;
  }) => Promise<void>;
  reset: () => Promise<void>;
};

const internalStore = writable<Session | undefined>();

export const sessionStore: SessionStore = {
  init: async ({ canisterId, agentOptions }) => {
    const identity = await ECDSAKeyIdentity.generate({
      extractable: false,
    });
    const agent = LazyHttpAgent.createLazy({ ...agentOptions, identity });
    const actor = Actor.createActor<_SERVICE>(internet_identity_idl, {
      agent,
      canisterId,
    });
    const { nonce, salt } = await createAnonymousNonce(identity.getPrincipal());
    internalStore.set({ identity, agent, actor, nonce, salt });
  },
  subscribe: derived(internalStore, (session) => {
    if (isNullish(session)) {
      throw new Error("Not initialized");
    }
    return session;
  }).subscribe,
  reset: async () => {
    const identity = await ECDSAKeyIdentity.generate({
      extractable: false,
    });
    const { nonce, salt } = await createAnonymousNonce(identity.getPrincipal());
    internalStore.update((session) => {
      if (isNullish(session)) {
        throw new Error("Not initialized");
      }
      const { agent, actor } = session;
      agent.replaceIdentity(identity);
      return { identity, agent, actor, nonce, salt };
    });
  },
};
