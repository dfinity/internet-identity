import { type Readable, derived, writable, get } from "svelte/store";
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
import { AuthRequest, DelegationParams } from "$lib/utils/transport/utils";

export type AuthorizationContext = {
  authRequest: AuthRequest; // Additional details e.g. derivation origin
  requestId: string | number; // The ID of the JSON RPC request
  requestOrigin: string; // Displayed to the user to identify the app
  effectiveOrigin: string; // Used for last used storage and delegations
  isAuthenticating: boolean; // True if user is being redirect back to app
};

type AuthorizationStore = Readable<AuthorizationContext | undefined> & {
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

const internalStore = writable<AuthorizationContext | undefined>();

export const authorizationStore: AuthorizationStore = {
  handleRequest: async (requestOrigin, requestId, params) => {
    const effectiveOrigin = remapToLegacyDomain(
      params.icrc95DerivationOrigin ?? requestOrigin,
    );
    const validationResult = await validateDerivationOrigin({
      requestOrigin,
      derivationOrigin: params.icrc95DerivationOrigin,
    });
    if (validationResult.result === "invalid") {
      throw new Error("Unverified origin");
    }
    internalStore.set({
      authRequest: {
        kind: "authorize-client",
        sessionPublicKey: params.publicKey.toDer(),
        maxTimeToLive: params.maxTimeToLive,
        derivationOrigin: params.icrc95DerivationOrigin,
      },
      requestId,
      requestOrigin,
      effectiveOrigin,
      isAuthenticating: false,
    });
  },
  subscribe: (...args) => internalStore.subscribe(...args),
  authorize: async (accountNumberMaybePromise, artificialDelay) => {
    const context = get(authorizationContextStore);
    internalStore.set({
      ...context,
      isAuthenticating: true,
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
  derived(authorizationStore, (context) => {
    if (context === undefined) {
      throw new Error("Authorization context is not available yet");
    }
    return context;
  });
