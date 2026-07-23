import {
  DelegationChain,
  DelegationIdentity,
  ECDSAKeyIdentity,
  isDelegationValid,
} from "@icp-sdk/core/identity";
import type { DerEncodedPublicKey } from "@icp-sdk/core/agent";
import { Principal } from "@icp-sdk/core/principal";
import { z } from "zod";
import type {
  Authenticated,
  AuthenticationResult,
} from "$lib/stores/authentication.store";
import { fromBase64, toBase64 } from "$lib/utils/utils";

const MSG_READY = "ii-handoff:ready";
const MSG_AUTH = "ii-handoff:auth";

const AuthHandoffReadySchema = z.object({
  type: z.literal(MSG_READY),
  publicKeyDer: z.string(),
});
type AuthHandoffReady = z.infer<typeof AuthHandoffReadySchema>;

const SerializedAuthMethodSchema = z.discriminatedUnion("kind", [
  z.object({ kind: z.literal("passkey"), credentialId: z.string() }),
  z.object({ kind: z.literal("openid"), iss: z.string(), sub: z.string() }),
  z.object({ kind: z.literal("recoveryPhrase"), principal: z.string() }),
  z.object({ kind: z.literal("emailRecovery"), principal: z.string() }),
]);
type SerializedAuthMethod = z.infer<typeof SerializedAuthMethodSchema>;

const AuthHandoffPayloadSchema = z.object({
  type: z.literal(MSG_AUTH),
  identityNumber: z.string(),
  chainJson: z.string(),
  authMethod: SerializedAuthMethodSchema,
});
type AuthHandoffPayload = z.infer<typeof AuthHandoffPayloadSchema>;

export const HANDOFF_HASH_KEY = "handoff";

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
  if ("emailRecovery" in method) {
    return {
      kind: "emailRecovery",
      principal: method.emailRecovery.principal.toText(),
    };
  }

  method satisfies never;
  throw new Error("unreachable: unknown auth method variant");
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

function stripHandoffMarkerFromUrl(): void {
  const params = new URLSearchParams(globalThis.location.hash.slice(1));
  if (!params.has(HANDOFF_HASH_KEY)) return;
  params.delete(HANDOFF_HASH_KEY);
  const remaining = params.toString();
  const cleanUrl =
    globalThis.location.pathname +
    globalThis.location.search +
    (remaining.length > 0 ? `#${remaining}` : "");
  globalThis.history.replaceState(null, "", cleanUrl);
}

export function sendAuthToOpenedTab(
  target: Window,
  auth: AuthenticationResult,
  timeoutMs = 2000,
): { cancel: () => void } {
  const controller = new AbortController();
  const { signal } = controller;

  const listener = async (event: MessageEvent) => {
    if (
      event.source !== target ||
      event.origin !== globalThis.location.origin
    ) {
      return;
    }
    const parsed = AuthHandoffReadySchema.safeParse(event.data);
    if (!parsed.success) {
      return;
    }
    clearTimeout(timer);

    try {
      // The manage handoff only ever carries a `DelegationIdentity`.
      if (!(auth.identity instanceof DelegationIdentity)) {
        return;
      }
      const receiverDer = fromBase64(
        parsed.data.publicKeyDer,
      ) as DerEncodedPublicKey;
      const receiverPublicKey = { toDer: () => receiverDer };

      const newChain = await DelegationChain.create(
        auth.identity,
        receiverPublicKey,
        new Date(Date.now() + 30 * 60 * 1000),
        {
          previous: auth.identity.getDelegation(),
        },
      );

      if (signal.aborted) {
        return;
      }

      const payload: AuthHandoffPayload = {
        type: MSG_AUTH,
        identityNumber: auth.identityNumber.toString(),
        chainJson: JSON.stringify(newChain.toJSON()),
        authMethod: serializeAuthMethod(auth.authMethod),
      };
      target.postMessage(payload, globalThis.location.origin);
    } catch (error) {
      // Chain creation failed — receiver will time out and fall back to
      // login. Surface the underlying error so we can debug the rare cases
      // where this happens.
      console.error("ii-handoff: failed to build delegation chain", error);
    }
  };

  window.addEventListener("message", listener, { signal });
  const timer = setTimeout(() => controller.abort(), timeoutMs);

  return {
    cancel: () => controller.abort(),
  };
}

export function receiveAuthFromOpener({
  timeoutMs = 2000,
}: {
  timeoutMs?: number;
} = {}): Promise<AuthenticationResult | null> {
  const opener = window.opener as Window | null;
  if (opener === null || opener.closed) {
    return Promise.resolve(null);
  }

  const params = new URLSearchParams(globalThis.location.hash.slice(1));
  if (!params.has(HANDOFF_HASH_KEY)) {
    return Promise.resolve(null);
  }
  stripHandoffMarkerFromUrl();

  return new Promise((resolve) => {
    const controller = new AbortController();
    const { signal } = controller;
    let localInnerKey: ECDSAKeyIdentity | undefined;

    const settle = (value: AuthenticationResult | null) => {
      if (signal.aborted) return;
      controller.abort();
      clearTimeout(timer);
      resolve(value);
    };

    const listener = (event: MessageEvent) => {
      if (
        event.source !== opener ||
        event.origin !== globalThis.location.origin
      ) {
        return;
      }
      const parsed = AuthHandoffPayloadSchema.safeParse(event.data);
      if (!parsed.success) {
        return;
      }

      const innerKey = localInnerKey;
      if (innerKey === undefined) {
        settle(null);
        return;
      }

      try {
        const chain = DelegationChain.fromJSON(
          JSON.parse(parsed.data.chainJson) as Parameters<
            typeof DelegationChain.fromJSON
          >[0],
        );

        if (!isDelegationValid(chain)) {
          console.warn("ii-handoff: delegation chain from opener is invalid");
          settle(null);
          return;
        }

        const identity = DelegationIdentity.fromDelegation(innerKey, chain);

        settle({
          identityNumber: BigInt(parsed.data.identityNumber),
          identity,
          authMethod: deserializeAuthMethod(parsed.data.authMethod),
        });
      } catch (error) {
        console.warn("ii-handoff: could not consume auth payload", error);
        settle(null);
      }
    };

    window.addEventListener("message", listener, { signal });

    const timer = setTimeout(() => {
      if (!signal.aborted) {
        console.warn("ii-handoff: timed out waiting for auth from opener");
      }
      settle(null);
    }, timeoutMs);

    ECDSAKeyIdentity.generate({ extractable: false })
      .then((innerKey) => {
        if (signal.aborted) {
          return;
        }
        localInnerKey = innerKey;
        const derKey = innerKey.getPublicKey().toDer();

        const ready: AuthHandoffReady = {
          type: MSG_READY,
          publicKeyDer: toBase64(derKey),
        };
        opener.postMessage(ready, globalThis.location.origin);
      })
      .catch(() => settle(null));
  });
}
