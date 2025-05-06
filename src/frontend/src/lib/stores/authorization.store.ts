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

export type Authorization = {
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
  | "success"
  | "failure";

type AuthorizationStore = Readable<Authorization> & {
  init: (params: {
    canisterId: Principal;
    canisterConfig: InternetIdentityInit;
  }) => Promise<void>;
  status: Readable<AuthorizationStatus>;
  authorize: (accountNumber: bigint | undefined) => Promise<void>;
};

const createAuthorizationStore = (): AuthorizationStore => {
  const store = writable<{
    context?: Authorization;
    status: AuthorizationStatus;
  }>({ status: "init" });
  let authorize: (accountNumber: bigint | undefined) => Promise<void>;

  return {
    init: async ({ canisterId, canisterConfig }) => {
      const status = await authenticationProtocol({
        authenticate: (context) => {
          store.set({ context: context, status: "authenticating" });
          return new Promise((resolve) => {
            authorize = async (_accountNumber) => {
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
        onProgress: (status) => store.update((value) => ({ ...value, status })),
      });
      store.update((value) => ({ ...value, status }));
    },
    subscribe: derived(store, ({ context }) => {
      if (isNullish(context)) {
        throw new Error("Not ready yet for authentication");
      }
      return context;
    }).subscribe,
    status: derived(store, ({ status }) => status),
    authorize: (accountNumber) => {
      if (isNullish(authorize)) {
        throw new Error("Not ready yet for authorization");
      }
      return authorize(accountNumber);
    },
  };
};

export const authorizationStore = createAuthorizationStore();
