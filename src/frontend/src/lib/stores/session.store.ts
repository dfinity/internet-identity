import { type Readable, derived, writable } from "svelte/store";
import { ECDSAKeyIdentity } from "@icp-sdk/core/identity";
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
import { fromBase64, toBase64 } from "$lib/utils/utils";
import { isNullish } from "@dfinity/utils";

export interface Session {
  identity: SignIdentity;
  agent: HttpAgent;
  actor: ActorSubclass<_SERVICE>;
  nonce: string;
  salt: Uint8Array;
}

type SessionData = Pick<Session, "identity" | "nonce" | "salt">;

type CreatedSession = {
  data: SessionData;
  persist: () => void;
};

type SessionStore = Readable<Session> & {
  init: (params: {
    canisterId: Principal;
    agentOptions: HttpAgentOptions;
  }) => Promise<void>;
  reset: () => void;
};

const STORAGE_KEY = "ii-session";

const internalStore = writable<Session | undefined>();

const createSession = async (): Promise<CreatedSession> => {
  const identity = await ECDSAKeyIdentity.generate({
    extractable: true,
  });
  const keyPair = identity.getKeyPair();
  const privateJwk = await crypto.subtle.exportKey("jwk", keyPair.privateKey);
  const publicJwk = await crypto.subtle.exportKey("jwk", keyPair.publicKey);
  const { nonce, salt } = await createAnonymousNonce(identity.getPrincipal());
  const data = { identity, nonce, salt };
  const persist = () => {
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
  };
  return { data, persist };
};

const readSession = async (): Promise<SessionData | undefined> => {
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

let preCreatedSession: CreatedSession | undefined = undefined;
const nextSession = (): CreatedSession => {
  // Consume the pre-created session by assigning it to a local variable
  // and clearing it afterwards. This ensures that if nextSession is called
  // again before the background creation completes, it will throw an error.
  const session = preCreatedSession;
  preCreatedSession = undefined;
  if (session === undefined) {
    throw new Error("No pre-created session available");
  }
  void (async () => {
    // Pre-create the next session in the background to make reset synchronous
    preCreatedSession = await createSession();
  })();
  return session;
};

export const sessionStore: SessionStore = {
  init: async ({ canisterId, agentOptions }) => {
    // Try to read an existing session from sessionStorage,
    // if it doesn't exist or is expired create a new one.
    let data = await readSession();
    if (data === undefined) {
      const session = await createSession();
      data = session.data;
      session.persist();
    }
    // Pre-create the next session for synchronous reset later on.
    preCreatedSession = await createSession();
    // Read session data and initialize agent
    const { identity, nonce, salt } = data;
    const agent = HttpAgent.createSync({ ...agentOptions, identity });
    // Fetch subnet keys to speed up queries during authentication,
    // this avoids having to fetch them later on user interaction.
    void agent.fetchSubnetKeys(canisterId);
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
  reset: () => {
    const session = nextSession();
    session.persist();
    const { identity, nonce, salt } = session.data;
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
