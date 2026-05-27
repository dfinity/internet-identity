import {
  DelegationChain,
  DelegationIdentity,
  ECDSAKeyIdentity,
  isDelegationValid,
} from "@icp-sdk/core/identity";
import type { DerEncodedPublicKey } from "@icp-sdk/core/agent";
import { Principal } from "@icp-sdk/core/principal";
import type { Authenticated } from "$lib/stores/authentication.store";
import { canisterId } from "$lib/globals";
import { fromBase64, toBase64 } from "$lib/utils/utils";

const MSG_READY = "ii-handoff:ready";
const MSG_AUTH = "ii-handoff:auth";

interface AuthHandoffReady {
  type: typeof MSG_READY;
  nonce: string;
  publicKeyDer: string;
}

interface AuthHandoffPayload {
  type: typeof MSG_AUTH;
  identityNumber: string;
  chainJson: string;
  authMethod: SerializedAuthMethod;
}

type SerializedAuthMethod =
  | { kind: "passkey"; credentialId: string }
  | { kind: "openid"; iss: string; sub: string }
  | { kind: "recoveryPhrase"; principal: string }
  | { kind: "emailRecovery"; principal: string };

// Nonce in the new tab's URL fragment; opener requires a match before sending auth.
export const HANDOFF_HASH_KEY = "h";

export function generateHandoffNonce(): string {
  return crypto.randomUUID();
}

function isAuthHandoffReady(data: unknown): data is AuthHandoffReady {
  return (
    typeof data === "object" &&
    data !== null &&
    "type" in data &&
    data.type === MSG_READY &&
    "nonce" in data &&
    typeof data.nonce === "string" &&
    "publicKeyDer" in data &&
    typeof data.publicKeyDer === "string"
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

export function sendAuthToOpenedTab(
  target: Window,
  auth: Omit<Authenticated, "agent" | "actor" | "salt" | "nonce">,
  expectedNonce: string,
  timeoutMs = 2000,
): { cancel: () => void } {
  let cancelled = false;

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

    if (cancelled) {
      return;
    }

    try {
      const receiverDer = fromBase64(
        event.data.publicKeyDer,
      ) as DerEncodedPublicKey;

      console.log(receiverDer);
      const receiverPublicKey = { toDer: () => receiverDer };
      console.log(receiverPublicKey);

      const newChain = await DelegationChain.create(
        auth.identity,
        receiverPublicKey,
        new Date(Date.now() + 30 * 60 * 1000),
        {
          previous: auth.identity.getDelegation(),
          targets: [canisterId],
        },
      );
      console.log("New delegation chain created:", newChain);

      if (!cancelled) {
        console.log("Sending auth payload to opened tab...", auth);
        const payload: AuthHandoffPayload = {
          type: MSG_AUTH,
          identityNumber: auth.identityNumber.toString(),
          chainJson: JSON.stringify(newChain.toJSON()),
          authMethod: serializeAuthMethod(auth.authMethod),
        };
        target.postMessage(payload, location.origin);
      }
    } catch {
      // Chain creation failed — receiver will time out and fall back to login.
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

  const nonce = new URLSearchParams(window.location.hash.slice(1)).get(
    HANDOFF_HASH_KEY,
  );
  if (nonce === null) {
    return Promise.resolve(null);
  }

  return new Promise((resolve) => {
    let settled = false;
    let localInnerKey: ECDSAKeyIdentity | undefined;

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

    const listener = (event: MessageEvent) => {
      if (
        event.source !== opener ||
        event.origin !== location.origin ||
        !isAuthHandoffPayload(event.data)
      ) {
        return;
      }

      const innerKey = localInnerKey;
      if (innerKey === undefined) {
        settle(null);
        return;
      }

      try {
        const chain = DelegationChain.fromJSON(
          JSON.parse(event.data.chainJson) as Parameters<
            typeof DelegationChain.fromJSON
          >[0],
        );

        if (!isDelegationValid(chain)) {
          settle(null);
          return;
        }

        const identity = DelegationIdentity.fromDelegation(innerKey, chain);

        settle({
          identityNumber: BigInt(event.data.identityNumber),
          identity,
          authMethod: deserializeAuthMethod(event.data.authMethod),
        });
      } catch {
        settle(null);
      }
    };

    window.addEventListener("message", listener);

    const timer = setTimeout(() => settle(null), timeoutMs);

    ECDSAKeyIdentity.generate({ extractable: false })
      .then((innerKey) => {
        if (settled) {
          return;
        }
        localInnerKey = innerKey;
        const derKey = innerKey.getPublicKey().toDer();

        opener.postMessage(
          {
            type: MSG_READY,
            nonce,
            publicKeyDer: toBase64(derKey),
          } satisfies AuthHandoffReady,
          location.origin,
        );
      })
      .catch(() => settle(null));
  });
}
