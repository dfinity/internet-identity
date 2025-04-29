export const storeLocalStorageKey = {
  LastUsedIdentities: "ii-last-used-identities",
} as const;

export type StoreLocalStorageKey =
  (typeof storeLocalStorageKey)[keyof typeof storeLocalStorageKey];
