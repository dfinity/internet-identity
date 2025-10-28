import { LastUsedIdentity } from "$lib/stores/last-used-identities.store";
import { findConfig } from "./openID";

export const lastUsedIdentityTypeName = (
  lastUsedIdentity: LastUsedIdentity,
) => {
  if ("openid" in lastUsedIdentity.authMethod) {
    const config = findConfig(
      lastUsedIdentity.authMethod.openid.iss,
      lastUsedIdentity.authMethod.openid.metadata ?? [],
    );
    return config?.name ?? "Unknown";
  }
  if ("passkey" in lastUsedIdentity.authMethod) {
    return "Passkey";
  }
  // Not possible, but in case type safety fails at runtime, we don't want to crash.
  console.error(`Unknown auth method ${lastUsedIdentity.authMethod}`);
  return "Unknown";
};
