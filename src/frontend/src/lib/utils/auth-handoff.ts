import {
  DelegationChain,
  DelegationIdentity,
  ECDSAKeyIdentity,
  isDelegationValid,
} from "@icp-sdk/core/identity";
import { Principal } from "@icp-sdk/core/principal";
import type { Authenticated } from "$lib/stores/authentication.store";
import { fromBase64, toBase64 } from "$lib/utils/utils";

const MSG_READY = "ii-handoff:ready";
const MSG_AUTH = "ii-handoff:auth";

interface AuthHandoffReady {
  type: typeof MSG_READY;
  nonce: string;
}

// Per-handoff nonce shared via the new tab's URL fragment. Defends against a
// same-origin attacker that can post a forged `ii-handoff:ready` to the
// opener: without the matching nonce the opener will not send the JWK.
export const HANDOFF_HASH_KEY = "h";

export function generateHandoffNonce(): string {
  return crypto.randomUUID();
}

interface AuthHandoffPayload {
  type: typeof MSG_AUTH;
  identityNumber: string;
  sessionPrivateJwk: JsonWebKey;
  sessionPublicJwk: JsonWebKey;
  chainJson: string;
  authMethod: SerializedAuthMethod;
}

type SerializedAuthMethod =
  | { kind: "passkey"; credentialId: string }
  | { kind: "openid"; iss: string; sub: string }
  | { kind: "recoveryPhrase"; principal: string }
  | { kind: "emailRecovery"; principal: string };

function isAuthHandoffReady(data: unknown): data is AuthHandoffReady {
  return (
    typeof data === "object" &&
    data !== null &&
    "type" in data &&
    data.type === MSG_READY &&
    "nonce" in data &&
    typeof data.nonce === "string"
  );
}

function isAuthHandoffPayload(data: unknown): data is AuthHandoffPayload {
  return (
    typeof data === "object" &&
    data !== null &&
    "type" in data &&
    data.type === MSG_AUTH
  );
}

function serializeAuthMethod(
  method: Authenticated["authMethod"],
): SerializedAuthMethod {
  if ("passkey" in method) {
    return {
      kind: "passkey",
      credentialId: toBase64(method.passkey.credentialId),
    };
  }
  if ("openid" in method) {
    return { kind: "openid", iss: method.openid.iss, sub: method.openid.sub };
  }
  if ("recoveryPhrase" in method) {
    return {
      kind: "recoveryPhrase",
      principal: method.recoveryPhrase.principal.toText(),
    };
  }
  return {
    kind: "emailRecovery",
    principal: method.emailRecovery.principal.toText(),
  };
}

function deserializeAuthMethod(
  method: SerializedAuthMethod,
): Authenticated["authMethod"] {
  if (method.kind === "passkey") {
    return { passkey: { credentialId: fromBase64(method.credentialId) } };
  }
  if (method.kind === "openid") {
    return { openid: { iss: method.iss, sub: method.sub } };
  }
  if (method.kind === "recoveryPhrase") {
    return {
      recoveryPhrase: { principal: Principal.fromText(method.principal) },
    };
  }
  return {
    emailRecovery: { principal: Principal.fromText(method.principal) },
  };
}

export async function serializeAuth(
  auth: Omit<Authenticated, "agent" | "actor" | "salt" | "nonce">,
): Promise<AuthHandoffPayload> {
  // _inner is private in TypeScript but accessible at runtime; no public
  // accessor exists in the SDK for the underlying ECDSAKeyIdentity.
  const inner = (auth.identity as unknown as { _inner: ECDSAKeyIdentity })
    ._inner;
  const keyPair = inner.getKeyPair();
  const sessionPrivateJwk = await crypto.subtle.exportKey(
    "jwk",
    keyPair.privateKey,
  );
  const sessionPublicJwk = await crypto.subtle.exportKey(
    "jwk",
    keyPair.publicKey,
  );
  const chainJson = JSON.stringify(auth.identity.getDelegation().toJSON());

  return {
    type: MSG_AUTH,
    identityNumber: auth.identityNumber.toString(),
    sessionPrivateJwk,
    sessionPublicJwk,
    chainJson,
    authMethod: serializeAuthMethod(auth.authMethod),
  };
}

export async function deserializeAuth(
  payload: AuthHandoffPayload,
): Promise<Omit<Authenticated, "agent" | "actor" | "salt" | "nonce"> | null> {
  try {
    const privateKey = await crypto.subtle.importKey(
      "jwk",
      payload.sessionPrivateJwk,
      { name: "ECDSA", namedCurve: "P-256" },
      true,
      ["sign"],
    );
    const publicKey = await crypto.subtle.importKey(
      "jwk",
      payload.sessionPublicJwk,
      { name: "ECDSA", namedCurve: "P-256" },
      true,
      ["verify"],
    );
    const inner = await ECDSAKeyIdentity.fromKeyPair({ privateKey, publicKey });
    const chain = DelegationChain.fromJSON(JSON.parse(payload.chainJson));

    if (!isDelegationValid(chain)) {
      return null;
    }

    const identity = DelegationIdentity.fromDelegation(inner, chain);

    return {
      identityNumber: BigInt(payload.identityNumber),
      identity,
      authMethod: deserializeAuthMethod(payload.authMethod),
    };
  } catch {
    return null;
  }
}

export function sendAuthToOpenedTab(
  target: Window,
  auth: Omit<Authenticated, "agent" | "actor" | "salt" | "nonce">,
  expectedNonce: string,
  timeoutMs = 2000,
): { cancel: () => void } {
  let cancelled = false;
  const payloadPromise = serializeAuth(auth);

  const listener = async (event: MessageEvent) => {
    if (
      event.source !== target ||
      event.origin !== location.origin ||
      !isAuthHandoffReady(event.data) ||
      event.data.nonce !== expectedNonce
    ) {
      return;
    }
    cleanup();
    const payload = await payloadPromise;
    if (!cancelled) {
      target.postMessage(payload, location.origin);
    }
  };

  window.addEventListener("message", listener);

  function cleanup() {
    clearTimeout(timer);
    window.removeEventListener("message", listener);
  }
  const timer = setTimeout(cleanup, timeoutMs);

  return {
    cancel: () => {
      cancelled = true;
      cleanup();
    },
  };
}

export function receiveAuthFromOpener({
  timeoutMs = 2000,
}: {
  timeoutMs?: number;
} = {}): Promise<Omit<
  Authenticated,
  "agent" | "actor" | "salt" | "nonce"
> | null> {
  const opener = window.opener as Window | null;
  if (opener === null || opener.closed) {
    return Promise.resolve(null);
  }

  const hashParams = new URLSearchParams(window.location.hash.slice(1));
  const nonce = hashParams.get(HANDOFF_HASH_KEY);
  if (nonce === null) {
    return Promise.resolve(null);
  }

  // Strip the nonce from the address bar so it doesn't linger in history,
  // copy-paste, or share targets.
  hashParams.delete(HANDOFF_HASH_KEY);
  const remainingHash = hashParams.toString();
  history.replaceState(
    null,
    "",
    window.location.pathname +
      window.location.search +
      (remainingHash.length > 0 ? `#${remainingHash}` : ""),
  );

  return new Promise((resolve) => {
    let settled = false;

    const settle = (
      value: Omit<Authenticated, "agent" | "actor" | "salt" | "nonce"> | null,
    ) => {
      if (settled) {
        return;
      }
      settled = true;
      clearTimeout(timer);
      window.removeEventListener("message", listener);
      resolve(value);
    };

    const listener = async (event: MessageEvent) => {
      if (
        event.source !== opener ||
        event.origin !== location.origin ||
        !isAuthHandoffPayload(event.data)
      ) {
        return;
      }
      const auth = await deserializeAuth(event.data);
      settle(auth);
    };

    window.addEventListener("message", listener);

    const timer = setTimeout(() => settle(null), timeoutMs);

    opener.postMessage(
      { type: MSG_READY, nonce } satisfies AuthHandoffReady,
      location.origin,
    );
  });
}
