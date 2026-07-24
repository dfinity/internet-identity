import { type Readable, derived, writable } from "svelte/store";
import {
  Actor,
  ActorSubclass,
  HttpAgent,
  HttpAgentOptions,
  type Identity,
} from "@icp-sdk/core/agent";
import { Principal } from "@icp-sdk/core/principal";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { idlFactory as internet_identity_idl } from "$lib/generated/internet_identity_idl";
import { createAnonymousNonce } from "$lib/utils/openID";

export interface Authenticated {
  identityNumber: bigint;
  nonce: string;
  salt: Uint8Array;
  // An SSO gate session is an `AttributesIdentity` wrapping the delegation; otherwise a `DelegationIdentity`.
  identity: Identity;
  agent: HttpAgent;
  actor: ActorSubclass<_SERVICE>;
  authMethod:
    | { passkey: { credentialId: Uint8Array } }
    | { openid: { iss: string; sub: string } }
    | { recoveryPhrase: { principal: Principal } }
    | { emailRecovery: { principal: Principal } };
}

// `agent`/`actor` are created once in `init()` and shared by the store;
// `salt`/`nonce` are derived inside `set()` from `identity.getPrincipal()`.
// Callers never supply any of the four, so flows yield this shape.
export type AuthenticationResult = Omit<
  Authenticated,
  "agent" | "actor" | "salt" | "nonce"
>;

type AuthenticationStore = Readable<Authenticated | undefined> & {
  init: (params: {
    canisterId: Principal;
    agentOptions: HttpAgentOptions;
  }) => void;
  set: (value: AuthenticationResult) => Promise<void>;
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
    void agent.fetchSubnetKeys({ canisterId });
    const actor = Actor.createActor<_SERVICE>(internet_identity_idl, {
      agent,
      canisterId,
    });
    internalStore.set({ initialized: { agent, actor } });
  },
  subscribe: derived(internalStore, ({ authenticated, initialized }) => {
    if (initialized === undefined) {
      throw new Error("Not initialized");
    }
    if (authenticated === undefined) {
      return undefined;
    }
    return { ...authenticated, ...initialized };
  }).subscribe,
  set: async (authenticated) => {
    const { nonce, salt } = await createAnonymousNonce(
      authenticated.identity.getPrincipal(),
    );
    internalStore.update(({ initialized }) => {
      if (initialized === undefined) {
        throw new Error("Not initialized");
      }
      initialized.agent.replaceIdentity(authenticated.identity);
      return { authenticated: { ...authenticated, nonce, salt }, initialized };
    });
  },
  reset: () =>
    internalStore.update(({ initialized }) => {
      if (initialized === undefined) {
        throw new Error("Not initialized");
      }
      initialized.agent.invalidateIdentity();
      return { authenticated: undefined, initialized };
    }),
};

export const authenticatedStore: Readable<Authenticated> = derived(
  authenticationStore,
  (authenticated) => {
    if (authenticated === undefined) {
      throw new Error("Not authenticated");
    }
    return authenticated;
  },
);

export const isAuthenticatedStore: Readable<boolean> = derived(
  authenticationStore,
  (authenticated) => authenticated !== undefined,
);
