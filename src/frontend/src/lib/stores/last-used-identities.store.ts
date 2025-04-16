import { storeLocalStorageKey } from "$lib/constants/store.constants";
import { derived, Readable } from "svelte/store";
import { writableStored } from "./writable.store";

export type LastUsedIdentity = {
  name: string;
  lastUsedTimestampMillis: number;
  identityNumber: bigint;
};
type LastUsedIdentitiesData = {
  [identityNumber: string]: LastUsedIdentity;
};
type LastUsedIdentitiesStore = Readable<LastUsedIdentitiesData> & {
  addLatestUsed: (identityNumber: bigint, name: string) => void;
  reset: () => void;
};

export const initLastUsedIdentitiesStore = (): LastUsedIdentitiesStore => {
  const { subscribe, set, update } = writableStored<LastUsedIdentitiesData>({
    key: storeLocalStorageKey.LastUsedIdentities,
    defaultValue: {},
    version: 1,
  });

  return {
    subscribe,
    addLatestUsed: (identityNumber: bigint, name: string) => {
      update((lastUsedIdentities) => {
        lastUsedIdentities[identityNumber.toString()] = {
          name,
          lastUsedTimestampMillis: Date.now(),
          identityNumber,
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

export const lastUsedIdentityStore: Readable<LastUsedIdentity> = derived(
  lastUsedIdentitiesStore,
  (lastUsedIdentities) => {
    return Object.values(lastUsedIdentities).sort(
      (a, b) => b.lastUsedTimestampMillis - a.lastUsedTimestampMillis,
    )[0];
  },
);
