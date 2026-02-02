import { type Readable, derived, writable, get } from "svelte/store";
import { AuthRequest } from "$lib/legacy/flows/authorize/postMessageInterface";
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
import { DelegationChain } from "@icp-sdk/core/identity";
import { DelegationParams } from "$lib/utils/transport/utils";

export type AuthorizationContext = {
  authRequest: AuthRequest; // Additional details e.g. derivation origin
  requestId: string | number; // The ID of the JSON RPC request
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

type AuthorizationStore = Readable<{
  context?: AuthorizationContext;
  status: AuthorizationStatus;
}> & {
  handleRequest: (
    requestOrigin: string,
    requestId: string | number,
    params: DelegationParams,
  ) => Promise<void>;
  authorize: (
    accountNumber: Promise<bigint | undefined> | bigint | undefined,
    artificialDelay?: number,
  ) => Promise<{
    requestId: string | number;
    delegationChain: DelegationChain;
  }>;
};

const internalStore = writable<{
  context?: AuthorizationContext;
  status: AuthorizationStatus;
}>({ status: "init" });

export class UnverifiedOriginError extends Error {
  get message() {
    return `Invalid derivation origin: ${super.message}`;
  }
}

export const authorizationStore: AuthorizationStore = {
  handleRequest: async (requestOrigin, requestId, params) => {
    internalStore.set({
      status: "validating",
    });
    const effectiveOrigin = remapToLegacyDomain(
      params.icrc95DerivationOrigin ?? origin,
    );
    const validationResult = await validateDerivationOrigin({
      requestOrigin,
      derivationOrigin: params.icrc95DerivationOrigin,
    });
    if (validationResult.result === "invalid") {
      throw new UnverifiedOriginError(validationResult.message);
    }
    internalStore.set({
      context: {
        authRequest: {
          kind: "authorize-client",
          sessionPublicKey: params.publicKey.toDer(),
          maxTimeToLive: params.maxTimeToLive,
          derivationOrigin: params.icrc95DerivationOrigin,
        },
        requestId,
        requestOrigin,
        effectiveOrigin,
      },
      status: "authenticating",
    });
  },
  subscribe: (...args) => internalStore.subscribe(...args),
  authorize: async (accountNumberMaybePromise, artificialDelay) => {
    const context = get(authorizationContextStore);
    internalStore.set({
      context,
      status: "authorizing",
    });
    const { identityNumber, actor } = get(authenticatedStore);
    const artificialDelayPromise = waitFor(
      features.DUMMY_AUTH || canisterConfig.dummy_auth[0]?.[0] !== undefined
        ? 0
        : (artificialDelay ?? 0),
    );
    const accountNumber = await accountNumberMaybePromise;
    const { user_key, expiration } = await actor
      .prepare_account_delegation(
        identityNumber,
        context.effectiveOrigin,
        accountNumber !== undefined ? [accountNumber] : [],
        context.authRequest.sessionPublicKey,
        context.authRequest.maxTimeToLive !== undefined
          ? [context.authRequest.maxTimeToLive]
          : [],
      )
      .then(throwCanisterError);
    const delegationChain = await retryFor(5, () =>
      actor
        .get_account_delegation(
          identityNumber,
          context.effectiveOrigin,
          accountNumber !== undefined ? [accountNumber] : [],
          context.authRequest.sessionPublicKey,
          expiration,
        )
        .then(throwCanisterError)
        .then(transformSignedDelegation)
        .then((delegation) =>
          DelegationChain.fromDelegations(
            [delegation],
            new Uint8Array(user_key),
          ),
        ),
    );
    await artificialDelayPromise;
    return { requestId: context.requestId, delegationChain };
  },
};

export const authorizationContextStore: Readable<AuthorizationContext> =
  derived(authorizationStore, ({ context }) => {
    if (context === undefined) {
      throw new Error("Authorization context is not available yet");
    }
    return context;
  });

export const authorizationStatusStore: Readable<AuthorizationStatus> = derived(
  internalStore,
  ({ status }) => status,
);
