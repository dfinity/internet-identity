export const storeLocalStorageKey = {
  LastUsedIdentities: "ii-last-used-identities",
  Locale: "ii-locale",
  CliAccess: "ii-cli-access",
  LastSharedEmails: "ii-last-shared-emails",
} as const;

export type StoreLocalStorageKey =
  (typeof storeLocalStorageKey)[keyof typeof storeLocalStorageKey];
