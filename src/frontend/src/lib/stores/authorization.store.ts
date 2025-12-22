import { type Readable, derived, writable, get } from "svelte/store";
import { isNullish, nonNullish } from "@dfinity/utils";
import {
  authenticationProtocol,
  AuthRequest,
} from "$lib/legacy/flows/authorize/postMessageInterface";
import { authenticatedStore } from "$lib/stores/authentication.store";
import { remapToLegacyDomain } from "$lib/utils/iiConnection";
import {
  waitFor,
  throwCanisterError,
  transformSignedDelegation,
  retryFor,
} from "$lib/utils/utils";
import { features } from "$lib/legacy/features";
import { canisterConfig } from "$lib/globals";
import { validateDerivationOrigin } from "$lib/utils/validateDerivationOrigin";
import { rpcAuthenticationProtocol } from "$lib/utils/postMessageInterface";

export type AuthorizationContext = {
  authRequest: AuthRequest; // Additional details e.g. derivation origin
  requestOrigin: string; // Displayed to the user to identify the app
  effectiveOrigin: string; // Used for last used storage and delegations
};

export type AuthorizationStatus =
  // Not handled in `authorize/+layout.svelte`.
  | "init"
  // Sent in postMessageInterface, kept for backwards compatibility.
  // Not handled in `authorize/+layout.svelte`.
  | "waiting"
  // Sent in postMessageInterface, kept for backwards compatibility
  // Not handled in `authorize/+layout.svelte`.
  | "validating"
  // Set on starting the authenticate flow.
  | "authenticating"
  // Set on starting the authorization flow.
  | "authorizing"
  // Set after "success" if the user is still here after 2 seconds.
  | "late-success"
  // All the following are returned by `authenticationProtocol`
  | "invalid"
  | "orphan"
  | "closed"
  | "success"
  | "unverified-origin"
  | "failure";

interface ProtocolOptions {
  legacyProtocol?: boolean;
  allowedOrigin?: string;
}

type AuthorizationStore = Readable<{
  context?: AuthorizationContext;
  status: AuthorizationStatus;
}> & {
  init: (options: ProtocolOptions) => Promise<void>;
  authorize: (
    accountNumber: Promise<bigint | undefined> | bigint | undefined,
    artificialDelay?: number,
  ) => Promise<void>;
};

const internalStore = writable<{
  context?: AuthorizationContext;
  status: AuthorizationStatus;
}>({ status: "init" });

let authorize: (
  accountNumber: Promise<bigint | undefined> | bigint | undefined,
  artificialDelay?: number,
) => Promise<void>;

export const authorizationStore: AuthorizationStore = {
  init: async (options) => {
    const status = await (
      options.legacyProtocol === true
        ? authenticationProtocol
        : rpcAuthenticationProtocol
    )({
      ...options,
      authenticate: async (context) => {
        const effectiveOrigin = remapToLegacyDomain(
          context.authRequest.derivationOrigin ?? context.requestOrigin,
        );
        internalStore.set({
          context: { ...context, effectiveOrigin },
          status: "authenticating",
        });

        const validationResult = await validateDerivationOrigin({
          requestOrigin: context.requestOrigin,
          derivationOrigin: context.authRequest.derivationOrigin,
        });

        if (validationResult.result === "invalid") {
          internalStore.update((value) => ({
            ...value,
            status: "unverified-origin",
          }));
          return {
            kind: "unverified-origin",
            text: `Invalid derivation origin: ${validationResult.message}`,
          };
        }

        return new Promise((resolve) => {
          authorize = async (
            accountNumberMaybePromise,
            artificialDelay = 0,
          ) => {
            internalStore.update((value) => ({
              ...value,
              status: "authorizing",
            }));
            const { identityNumber, actor } = get(authenticatedStore);
            const artificialDelayPromise = waitFor(
              features.DUMMY_AUTH ||
                nonNullish(canisterConfig.dummy_auth[0]?.[0])
                ? 0
                : artificialDelay,
            );
            try {
              const accountNumber = await accountNumberMaybePromise;
              const { user_key, expiration } = await actor
                .prepare_account_delegation(
                  identityNumber,
                  effectiveOrigin,
                  nonNullish(accountNumber) ? [accountNumber] : [],
                  context.authRequest.sessionPublicKey,
                  nonNullish(context.authRequest.maxTimeToLive)
                    ? [context.authRequest.maxTimeToLive]
                    : [],
                )
                .then(throwCanisterError);
              const delegation = await retryFor(5, () =>
                actor
                  .get_account_delegation(
                    identityNumber,
                    effectiveOrigin,
                    nonNullish(accountNumber) ? [accountNumber] : [],
                    context.authRequest.sessionPublicKey,
                    expiration,
                  )
                  .then(throwCanisterError)
                  .then(transformSignedDelegation),
              );
              await artificialDelayPromise;
              resolve({
                kind: "success",
                delegations: [delegation],
                userPublicKey: new Uint8Array(user_key),
                // This is a authnMethod forwarded to the app that requested authorization.
                // We don't want to leak which authnMethod was used.
                authnMethod: "passkey",
              });
            } catch {
              resolve({
                kind: "failure",
                text: "Couldn't fetch delegation",
              });
            }
          };
        });
      },
      onProgress: (status) =>
        internalStore.update((value) => ({ ...value, status })),
    });
    internalStore.update((value) => ({ ...value, status }));
    if (status === "success") {
      const LATE_SUCCESS_MESSAGE_DELAY_MS = 2000;
      // If the user is still here after 2 seconds, show a message
      // "Authentication successful, close page"
      const lateSuccessTimeout = setTimeout(() => {
        internalStore.update((value) => ({
          ...value,
          status: "late-success",
        }));
      }, LATE_SUCCESS_MESSAGE_DELAY_MS);
      internalStore.subscribe(({ status }) => {
        if (status !== "success") {
          clearTimeout(lateSuccessTimeout);
        }
      });
    }
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
    context.requestOrigin = "https://caffeine.ai";
    return context;
  });

export const authorizationStatusStore: Readable<AuthorizationStatus> = derived(
  internalStore,
  ({ status }) => status,
);
