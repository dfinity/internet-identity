import { type Readable, derived, writable } from "svelte/store";
import { ECDSAKeyIdentity } from "@icp-sdk/core/identity";
import { isNullish } from "@dfinity/utils";
import {
  Actor,
  ActorSubclass,
  HttpAgent,
  HttpAgentOptions,
  SignIdentity,
} from "@icp-sdk/core/agent";
import { createAnonymousNonce } from "$lib/utils/openID";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { Principal } from "@icp-sdk/core/principal";
import { idlFactory as internet_identity_idl } from "$lib/generated/internet_identity_idl";
import { LazyHttpAgent } from "$lib/utils/lazyHttpAgent";
import { fromBase64, toBase64 } from "$lib/utils/utils";

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

const STORAGE_KEY = "ii-session";

const internalStore = writable<Session | undefined>();

const create = async () => {
  const identity = await ECDSAKeyIdentity.generate({
    extractable: true,
  });
  const keyPair = identity.getKeyPair();
  const privateJwk = await crypto.subtle.exportKey("jwk", keyPair.privateKey);
  const publicJwk = await crypto.subtle.exportKey("jwk", keyPair.publicKey);
  const { nonce, salt } = await createAnonymousNonce(identity.getPrincipal());
  sessionStorage.setItem(
    STORAGE_KEY,
    JSON.stringify({
      privateJwk,
      publicJwk,
      nonce,
      salt: toBase64(salt),
      createdAt: new Date().getTime(),
    }),
  );
  return {
    identity,
    nonce,
    salt,
  };
};

const read = async () => {
  const item = sessionStorage.getItem(STORAGE_KEY);
  if (isNullish(item)) {
    return undefined;
  }
  const { privateJwk, publicJwk, nonce, salt, createdAt } = JSON.parse(item);
  // Ignore persisted sessions older than 5 minutes
  const expiresAt = parseInt(createdAt, 10) + 5 * 60 * 1000;
  if (Date.now() > expiresAt) {
    return undefined;
  }
  const privateKey = await crypto.subtle.importKey(
    "jwk",
    privateJwk,
    { name: "ECDSA", namedCurve: "P-256" },
    true,
    ["sign"],
  );
  const publicKey = await crypto.subtle.importKey(
    "jwk",
    publicJwk,
    { name: "ECDSA", namedCurve: "P-256" },
    true,
    ["verify"],
  );
  return {
    identity: await ECDSAKeyIdentity.fromKeyPair({ publicKey, privateKey }),
    nonce,
    salt: new Uint8Array(fromBase64(salt)),
  };
};

export const sessionStore: SessionStore = {
  init: async ({ canisterId, agentOptions }) => {
    const { identity, nonce, salt } = (await read()) ?? (await create());
    const agent = LazyHttpAgent.createLazy({ ...agentOptions, identity });
    const actor = Actor.createActor<_SERVICE>(internet_identity_idl, {
      agent,
      canisterId,
    });
    internalStore.set({ identity, agent, actor, nonce, salt });
  },
  subscribe: derived(internalStore, (session) => {
    if (isNullish(session)) {
      throw new Error("Not initialized");
    }
    return session;
  }).subscribe,
  reset: async () => {
    const { identity, nonce, salt } = await create();
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
