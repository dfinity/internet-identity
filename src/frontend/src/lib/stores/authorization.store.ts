import { type Readable, derived, writable, get } from "svelte/store";
import { isNullish } from "@dfinity/utils";
import type { InternetIdentityInit } from "$lib/generated/internet_identity_types";
import { Principal } from "@dfinity/principal";
import {
  authenticationProtocol,
  AuthRequest,
} from "$lib/flows/authorize/postMessageInterface";
import { authenticatedStore } from "$lib/stores/authentication.store";
import { Connection } from "$lib/utils/iiConnection";
import { fetchDelegation } from "$lib/flows/authorize/fetchDelegation";

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
  init: (params: {
    canisterId: Principal;
    canisterConfig: InternetIdentityInit;
  }) => Promise<void>;
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
  init: async ({ canisterId, canisterConfig }) => {
    const status = await authenticationProtocol({
      authenticate: (context) => {
        internalStore.set({ context, status: "authenticating" });
        return new Promise((resolve) => {
          authorize = async (_accountNumber, artificialDelay) => {
            internalStore.set({ context, status: "authorizing" });
            const artificialDelayPromise = new Promise((resolve) =>
              setTimeout(resolve, artificialDelay),
            );
            // TODO: use prepare/get account delegation instead of iiConnection
            const { identityNumber, identity } = get(authenticatedStore);
            const { connection } = await new Connection(
              canisterId.toText(),
              canisterConfig,
            ).fromDelegationIdentity(identityNumber, identity);
            const derivationOrigin =
              context.authRequest.derivationOrigin ?? context.requestOrigin;
            const result = await fetchDelegation({
              connection,
              derivationOrigin,
              publicKey: context.authRequest.sessionPublicKey,
              maxTimeToLive: context.authRequest.maxTimeToLive,
            });
            await artificialDelayPromise;
            if ("error" in result) {
              resolve({ kind: "failure", text: "Couldn't fetch delegation" });
              return;
            }
            const [userKey, parsed_signed_delegation] = result;
            resolve({
              kind: "success",
              delegations: [parsed_signed_delegation],
              userPublicKey: new Uint8Array(userKey),
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
