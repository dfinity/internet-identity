import { storeLocalStorageKey } from "$lib/constants/store.constants";
import { derived, Readable } from "svelte/store";
import { writableStored } from "./writable.store";

export type LastUsedIdentity = {
  identity: {
    identityNumber: bigint;
    name?: string;
  };
  authMethod:
    | { passkey: { credentialId: Uint8Array } }
    | { openid: { iss: string; sub: string } };
  account: {
    accountNumber?: bigint;
    name?: string;
  };
  lastUsedTimestampMillis: number;
};
export type LastUsedIdentitiesData = {
  [identityNumber: string]: LastUsedIdentity;
};
type LastUsedIdentitiesStore = Readable<LastUsedIdentitiesData> & {
  addLatestUsed: (
    params: Omit<LastUsedIdentity, "lastUsedTimestampMillis">,
  ) => void;
  reset: () => void;
};

export const initLastUsedIdentitiesStore = (): LastUsedIdentitiesStore => {
  const { subscribe, set, update } = writableStored<LastUsedIdentitiesData>({
    key: storeLocalStorageKey.LastUsedIdentities,
    defaultValue: {},
    version: 2,
  });

  return {
    subscribe,
    addLatestUsed: (params) => {
      update((lastUsedIdentities) => {
        lastUsedIdentities[params.identity.identityNumber.toString()] = {
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
