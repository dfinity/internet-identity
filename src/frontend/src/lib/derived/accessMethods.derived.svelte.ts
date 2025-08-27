import { getLastUsedAccessMethod } from "$lib/utils/accessMethods";
import { ENABLE_GENERIC_OPEN_ID } from "$lib/state/featureFlags";
import { canisterConfig } from "$lib/globals";
import identityInfo from "$lib/stores/identity-info.state.svelte";
import { get } from "svelte/store";

export class AccessMethodsDerived {
  #MAX_PASSKEYS = 8;
  #identityInfo = identityInfo;
  #enableGenericOpenId = false;

  constructor() {
    this.#enableGenericOpenId = get(ENABLE_GENERIC_OPEN_ID);
    ENABLE_GENERIC_OPEN_ID.subscribe((enableGenericOpenId) => {
      this.#enableGenericOpenId = enableGenericOpenId;
    });
  }

  lastUsedAccessMethod = $derived(
    getLastUsedAccessMethod(
      this.#identityInfo.authnMethods,
      this.#identityInfo.openIdCredentials,
    ),
  );

  isMaxOpenIdCredentialsReached = $derived(
    this.#enableGenericOpenId
      ? this.#identityInfo.openIdCredentials.length >=
          (canisterConfig.openid_configs[0]?.length ?? 0)
      : this.#identityInfo.openIdCredentials.length >= 1,
  );

  isMaxPasskeysReached = $derived(
    this.#identityInfo.authnMethods.length >= this.#MAX_PASSKEYS,
  );

  accessMethodsMaxReached = $derived(
    this.isMaxOpenIdCredentialsReached && this.isMaxPasskeysReached,
  );
}

export const accessMethods = new AccessMethodsDerived();
