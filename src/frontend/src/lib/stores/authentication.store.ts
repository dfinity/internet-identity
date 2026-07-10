import { type Readable, derived, writable } from "svelte/store";
import { DelegationIdentity } from "@icp-sdk/core/identity";
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
    | { recoveryPhrase: { principal: Principal } }
    | { emailRecovery: { principal: Principal } };
  /**
   * Present when this session was authenticated through the SSO gate path
   * (`sso_prepare_delegation`) — i.e. the session principal is the origin-scoped
   * SSO-session principal, not a device/OpenID one (IdP-side per-app gating,
   * §6.3). Carries the dapp `origin` the session is bound to. Consumers key
   * SSO-specific handling on this (not on the 1-click vs manual flow type), so
   * both the `?sso=` 1-click and the manual "Sign in with SSO" paths are
   * covered, and passkey/direct-OpenID sessions are not. `domain` is the SSO
   * discovery domain (the `sso:<domain>:…` attribute scope).
   */
  sso?: { origin: string; domain: string };
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
    void agent.fetchSubnetKeys(canisterId);
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
