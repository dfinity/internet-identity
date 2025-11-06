import { type Readable, derived, writable } from "svelte/store";
import { DelegationIdentity } from "@icp-sdk/core/identity";
import { isNullish, nonNullish } from "@dfinity/utils";
import {
  Actor,
  ActorSubclass,
  HttpAgent,
  HttpAgentOptions,
} from "@icp-sdk/core/agent";
import { Principal } from "@icp-sdk/core/principal";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { idlFactory as internet_identity_idl } from "$lib/generated/internet_identity_idl";
import { createAnonymousNonce } from "$lib/utils/openID";

export interface Authenticated {
  identityNumber: bigint;
  nonce: string;
  salt: Uint8Array;
  identity: DelegationIdentity;
  agent: HttpAgent;
  actor: ActorSubclass<_SERVICE>;
  authMethod:
    | { passkey: { credentialId: Uint8Array } }
    | { openid: { iss: string; sub: string } }
    | { recoveryPhrase: { identityNumber: bigint } };
}

type AuthenticationStore = Readable<Authenticated | undefined> & {
  init: (params: {
    canisterId: Principal;
    agentOptions: HttpAgentOptions;
  }) => void;
  set: (
    value: Omit<Authenticated, "agent" | "actor" | "salt" | "nonce">,
  ) => Promise<void>;
  reset: () => void;
};

const internalStore = writable<{
  authenticated?: Omit<Authenticated, "agent" | "actor">;
  initialized?: Pick<Authenticated, "agent" | "actor">;
}>();

export const authenticationStore: AuthenticationStore = {
  init: ({ canisterId, agentOptions }) => {
    const agent = HttpAgent.createSync(agentOptions);
    // Fetch subnet keys to speed up queries during authentication,
    // this avoids having to fetch them later on user interaction.
    void agent.fetchSubnetKeys(canisterId);
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
  set: async (authenticated) => {
    const { nonce, salt } = await createAnonymousNonce(
      authenticated.identity.getPrincipal(),
    );
    internalStore.update(({ initialized }) => {
      if (isNullish(initialized)) {
        throw new Error("Not initialized");
      }
      initialized.agent.replaceIdentity(authenticated.identity);
      return { authenticated: { ...authenticated, nonce, salt }, initialized };
    });
  },
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
    if (isNullish(authenticated)) {
      throw new Error("Not authenticated");
    }
    return authenticated;
  },
);

export const isAuthenticatedStore: Readable<boolean> = derived(
  authenticationStore,
  (authenticated) => nonNullish(authenticated),
);
