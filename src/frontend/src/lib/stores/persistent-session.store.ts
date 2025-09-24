import { derived, writable } from "svelte/store";
import { ECDSAKeyIdentity } from "@dfinity/identity";
import { isNullish } from "@dfinity/utils";
import { Actor } from "@dfinity/agent";
import { createAnonymousNonce } from "$lib/utils/openID";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { idlFactory as internet_identity_idl } from "$lib/generated/internet_identity_idl";
import { LazyHttpAgent } from "$lib/utils/lazyHttpAgent";
import { Session, SessionStore } from "$lib/stores/session.store";
import { toBase64, fromBase64 } from "$lib/utils/utils";

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
  const { privateJwk, publicJwk, nonce, salt } = JSON.parse(item);
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

export const persistentSessionStore: SessionStore = {
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
