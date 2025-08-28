import { LastUsedIdentity } from "$lib/stores/last-used-identities.store";
import { isNullish, nonNullish } from "@dfinity/utils";
import { findConfig, isOpenIdConfig } from "./openID";

export const lastUsedIdentityTypeName = (
  lastUsedIdentity: LastUsedIdentity,
) => {
  if ("openid" in lastUsedIdentity.authMethod) {
    const config = findConfig(
      lastUsedIdentity.authMethod.openid.iss,
      lastUsedIdentity.authMethod.openid.metadata,
    );
    if (nonNullish(config) && isOpenIdConfig(config)) {
      return config.name;
    }
    if (isNullish(config)) {
      return "Unknown";
    }
    return "Google";
  }
  if ("passkey" in lastUsedIdentity.authMethod) {
    return "Passkey";
  }
  // Not possible, but in case type safety fails at runtime, we don't want to crash.
  console.error(`Unknown auth method ${lastUsedIdentity.authMethod}`);
  return "Unknown";
};
