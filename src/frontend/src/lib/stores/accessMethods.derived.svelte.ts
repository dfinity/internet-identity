import { getLastUsedAccessMethod } from "$lib/utils/accessMethods";
import { ENABLE_GENERIC_OPEN_ID } from "$lib/state/featureFlags";
import { canisterConfig } from "$lib/globals";
import identityInfo, { IdentityInfo } from "./identity-info.state.svelte";
import { get } from "svelte/store";

const MAX_PASSKEYS = 8;

export class AccessMethodsDerived {
  constructor(
    private readonly identityInfo: IdentityInfo,
    private readonly enableGenericOpenId: boolean,
  ) {
    this.identityInfo = identityInfo;
    this.enableGenericOpenId = enableGenericOpenId;
  }

  lastUsedAccessMethod = $derived(
    getLastUsedAccessMethod(
      this.identityInfo.authnMethods,
      this.identityInfo.openIdCredentials,
    ),
  );

  isMaxOpenIdCredentialsReached = $derived(
    this.enableGenericOpenId
      ? this.identityInfo.openIdCredentials.length >=
          (canisterConfig.openid_configs[0]?.length ?? 0)
      : this.identityInfo.openIdCredentials.length >= 1,
  );

  isMaxPasskeysReached = $derived(
    this.identityInfo.authnMethods.length >= MAX_PASSKEYS,
  );

  accessMethodsMaxReached = $derived(
    this.isMaxOpenIdCredentialsReached && this.isMaxPasskeysReached,
  );
}

// We can't combine Svelte 4 stores with runes like `$derived`.
// This means we won't have reactivity to the flag for this functionality.
// But we can live with that.
export const accessMethods = new AccessMethodsDerived(
  identityInfo,
  get(ENABLE_GENERIC_OPEN_ID),
);
