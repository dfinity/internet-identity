import { type Readable, derived, writable, get } from "svelte/store";
import { isNullish, nonNullish } from "@dfinity/utils";
import type { SignedDelegation } from "$lib/generated/internet_identity_types";
import {
  authenticationProtocol,
  AuthRequest,
} from "$lib/flows/authorize/postMessageInterface";
import { authenticatedStore } from "$lib/stores/authentication.store";
import { remapToLegacyDomain } from "$lib/utils/iiConnection";
import {
  throwCanisterError,
  transformSignedDelegation,
} from "$lib/utils/utils";

export type AuthorizationContext = {
  authRequest: AuthRequest;
  requestOrigin: string;
};

export type AuthorizationStatus =
  | "init"
  | "orphan"
  | "closed"
  | "waiting"
  | "validating"
  | "invalid"
  | "authenticating"
  | "authorizing"
  | "success"
  | "failure";

type AuthorizationStore = Readable<{
  context?: AuthorizationContext;
  status: AuthorizationStatus;
}> & {
  init: () => Promise<void>;
  authorize: (
    accountNumber: bigint | undefined,
    artificialDelay?: number,
  ) => Promise<void>;
};

const internalStore = writable<{
  context?: AuthorizationContext;
  status: AuthorizationStatus;
}>({ status: "init" });

let authorize: (
  accountNumber: bigint | undefined,
  artificialDelay?: number,
) => Promise<void>;

export const authorizationStore: AuthorizationStore = {
  init: async () => {
    const status = await authenticationProtocol({
      authenticate: (context) => {
        internalStore.set({
          context: {
            authRequest: {
              ...context.authRequest,
              derivationOrigin: nonNullish(context.authRequest.derivationOrigin)
                ? remapToLegacyDomain(context.authRequest.derivationOrigin)
                : undefined,
            },
            requestOrigin: remapToLegacyDomain(context.requestOrigin),
          },
          status: "authenticating",
        });
        return new Promise((resolve) => {
          authorize = async (accountNumber, artificialDelay) => {
            internalStore.set({ context, status: "authorizing" });
            const artificialDelayPromise = new Promise((resolve) =>
              setTimeout(resolve, artificialDelay),
            );
            const { identityNumber, actor } = get(authenticatedStore);
            const derivationOrigin = remapToLegacyDomain(
              context.authRequest.derivationOrigin ?? context.requestOrigin,
            );
            const { user_key, timestamp } = await actor
              .prepare_account_delegation(
                identityNumber,
                derivationOrigin,
                nonNullish(accountNumber) ? [accountNumber] : [],
                context.authRequest.sessionPublicKey,
                nonNullish(context.authRequest.maxTimeToLive)
                  ? [context.authRequest.maxTimeToLive]
                  : [],
              )
              .then(throwCanisterError);
            let delegation: SignedDelegation;
            for (let i = 1; i <= 5; i++) {
              try {
                delegation = await actor
                  .get_account_delegation(
                    identityNumber,
                    derivationOrigin,
                    nonNullish(accountNumber) ? [accountNumber] : [],
                    context.authRequest.sessionPublicKey,
                    timestamp,
                  )
                  .then(throwCanisterError);
                break;
              } catch {
                if (i === 5) {
                  resolve({
                    kind: "failure",
                    text: "Couldn't fetch delegation",
                  });
                  return;
                }
                // Linear backoff
                await new Promise((resolve) => setTimeout(resolve, i * 1000));
              }
            }
            const transformedDelegation = transformSignedDelegation(
              delegation!,
            );
            await artificialDelayPromise;
            resolve({
              kind: "success",
              delegations: [transformedDelegation],
              userPublicKey: new Uint8Array(user_key),
              // This is a authnMethod forwarded to the app that requested authorization.
              // We don't want to leak which authnMethod was used.
              authnMethod: "passkey",
            });
          };
        });
      },
      onProgress: (status) =>
        internalStore.update((value) => ({ ...value, status })),
    });
    internalStore.update((value) => ({ ...value, status }));
  },
  subscribe: (...args) => internalStore.subscribe(...args),
  authorize: (accountNumber, artificialDelay) => {
    if (isNullish(authorize)) {
      throw new Error("Not ready yet for authorization");
    }
    return authorize(accountNumber, artificialDelay);
  },
};

export const authorizationContextStore: Readable<AuthorizationContext> =
  derived(authorizationStore, ({ context }) => {
    if (isNullish(context)) {
      throw new Error("Authorization context is not available yet");
    }
    return context;
  });

export const authorizationStatusStore: Readable<AuthorizationStatus> = derived(
  internalStore,
  ({ status }) => status,
);
