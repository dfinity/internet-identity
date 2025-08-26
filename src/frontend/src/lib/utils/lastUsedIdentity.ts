import { LastUsedIdentity } from "$lib/stores/last-used-identities.store";
import { nonNullish } from "@dfinity/utils";
import { findConfig, isOpenIdConfig } from "./openID";

export const lastUsedIdentityTypeName = (
  lastUsedIdentity: LastUsedIdentity,
) => {
  if ("openid" in lastUsedIdentity.authMethod) {
    const config = findConfig(lastUsedIdentity.authMethod.openid.iss);
    if (nonNullish(config) && isOpenIdConfig(config)) {
      return config.name;
    }
    return "Google";
  }
  if ("passkey" in lastUsedIdentity.authMethod) {
    return "Passkey";
  }
};
