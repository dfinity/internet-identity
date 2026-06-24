export const storeLocalStorageKey = {
  LastUsedIdentities: "ii-last-used-identities",
  Locale: "ii-locale",
  CliAccess: "ii-cli-access",
  McpTrustedServers: "ii-mcp-trusted-servers",
} as const;

export type StoreLocalStorageKey =
  (typeof storeLocalStorageKey)[keyof typeof storeLocalStorageKey];
