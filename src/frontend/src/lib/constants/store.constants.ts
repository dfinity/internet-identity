export const storeLocalStorageKey = {
  LastUsedIdentities: "ii-last-used-identities",
  Locale: "ii-locale",
  CliAccess: "ii-cli-access",
  McpAccess: "ii-mcp-access",
  McpTrustedServers: "ii-mcp-trusted-server",
} as const;

export type StoreLocalStorageKey =
  (typeof storeLocalStorageKey)[keyof typeof storeLocalStorageKey];
