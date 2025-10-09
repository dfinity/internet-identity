import { getLastUsedAccessMethod } from "$lib/utils/accessMethods";
import { canisterConfig } from "$lib/globals";
import identityInfo from "$lib/stores/identity-info.state.svelte";

export class AccessMethodsDerived {
  #MAX_PASSKEYS = 8;
  #identityInfo = identityInfo;

  lastUsedAccessMethod = $derived(
    getLastUsedAccessMethod(
      this.#identityInfo.authnMethods,
      this.#identityInfo.openIdCredentials,
    ),
  );

  isMaxOpenIdCredentialsReached = $derived(
    this.#identityInfo.openIdCredentials.length >=
      (canisterConfig.openid_configs[0]?.length ?? 0),
  );

  isMaxPasskeysReached = $derived(
    this.#identityInfo.authnMethods.length >= this.#MAX_PASSKEYS,
  );

  accessMethodsMaxReached = $derived(
    this.isMaxOpenIdCredentialsReached && this.isMaxPasskeysReached,
  );
}

export const accessMethods = new AccessMethodsDerived();
