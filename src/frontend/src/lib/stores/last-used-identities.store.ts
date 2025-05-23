import { storeLocalStorageKey } from "$lib/constants/store.constants";
import { derived, Readable } from "svelte/store";
import { writableStored } from "./writable.store";
import { isNullish } from "@dfinity/utils";

export type LastUsedAccount = {
  identityNumber: bigint;
  accountNumber: bigint | undefined;
  origin: string;
  name?: string;
  lastUsedTimestampMillis: number;
};
export type LastUsedAccounts = {
  [origin: string]: {
    [accountNumber: string]: LastUsedAccount;
  };
};
export type LastUsedIdentity = {
  identityNumber: bigint;
  name?: string;
  authMethod:
    | { passkey: { credentialId: Uint8Array } }
    | { openid: { iss: string; sub: string } };
  accounts?: LastUsedAccounts;
  lastUsedTimestampMillis: number;
};
export type LastUsedIdentities = {
  [identityNumber: string]: LastUsedIdentity;
};
type LastUsedIdentitiesStore = Readable<LastUsedIdentities> & {
  addLastUsedIdentity: (
    params: Pick<LastUsedIdentity, "identityNumber" | "name" | "authMethod">,
  ) => void;
  addLastUsedAccount: (
    params: Omit<LastUsedAccount, "lastUsedTimestampMillis">,
  ) => void;
  reset: () => void;
};

export const initLastUsedIdentitiesStore = (): LastUsedIdentitiesStore => {
  const { subscribe, set, update } = writableStored<LastUsedIdentities>({
    key: storeLocalStorageKey.LastUsedIdentities,
    defaultValue: {},
    version: 3,
  });

  return {
    subscribe,
    addLastUsedIdentity: (params) => {
      update((lastUsedIdentities) => {
        const identity = lastUsedIdentities[params.identityNumber.toString()];
        lastUsedIdentities[params.identityNumber.toString()] = {
          accounts: identity?.accounts,
          ...params,
          lastUsedTimestampMillis: Date.now(),
        };
        return lastUsedIdentities;
      });
    },
    addLastUsedAccount: (params) => {
      update((lastUsedIdentities) => {
        const identity = lastUsedIdentities[params.identityNumber.toString()];
        if (isNullish(identity)) {
          return lastUsedIdentities;
        }
        if (isNullish(identity.accounts)) {
          identity.accounts = {};
        }
        if (isNullish(identity.accounts[params.origin])) {
          identity.accounts[params.origin] = {};
        }
        identity.accounts[params.origin][
          isNullish(params.accountNumber)
            ? "primary"
            : params.accountNumber.toString()
        ] = {
          ...params,
          lastUsedTimestampMillis: Date.now(),
        };
        return lastUsedIdentities;
      });
    },
    reset: () => {
      set({});
    },
  };
};

export const lastUsedIdentitiesStore = initLastUsedIdentitiesStore();

export const lastUsedIdentityStore: Readable<LastUsedIdentity | undefined> =
  derived(lastUsedIdentitiesStore, (lastUsedIdentities) => {
    return Object.values(lastUsedIdentities).sort(
      (a, b) => b.lastUsedTimestampMillis - a.lastUsedTimestampMillis,
    )[0];
  });
