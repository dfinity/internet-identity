import { storeLocalStorageKey } from "$lib/constants/store.constants";
import { derived, Readable } from "svelte/store";
import { writableStored } from "./writable.store";

export type LastUsedIdentity = {
  name?: string;
  lastUsedTimestampMillis: number;
  identityNumber: bigint;
  credentialId: Uint8Array | undefined;
  authMethod: "passkey" | "google";
  // In case the auth method is google, this will be the sub (or email)
  sub?: string;
};
export type LastUsedIdentitiesData = {
  [identityNumber: string]: LastUsedIdentity;
};
type LastUsedIdentitiesStore = Readable<LastUsedIdentitiesData> & {
  addLatestUsed: (params: {
    identityNumber: bigint;
    name?: string;
    credentialId: Uint8Array | undefined;
    authMethod: "passkey" | "google";
    sub?: string;
  }) => void;
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
    addLatestUsed: ({
      identityNumber,
      name,
      credentialId,
      authMethod,
      sub,
    }: {
      identityNumber: bigint;
      name?: string;
      credentialId: Uint8Array | undefined;
      authMethod: "passkey" | "google";
      sub?: string;
    }) => {
      update((lastUsedIdentities) => {
        lastUsedIdentities[identityNumber.toString()] = {
          name,
          lastUsedTimestampMillis: Date.now(),
          identityNumber,
          credentialId,
          authMethod,
          sub,
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
