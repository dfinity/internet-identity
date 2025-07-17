import type { PageLoad } from "./$types";
import { authenticatedStore } from "$lib/stores/authentication.store";
import { get } from "svelte/store";
import { authorizationContextStore } from "$lib/stores/authorization.store";
import { throwCanisterError } from "$lib/utils/utils";
import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";

export const load: PageLoad = async () => {
  const { identityNumber, actor } = get(authenticatedStore);
  const { effectiveOrigin } = get(authorizationContextStore);
  let accounts = await actor
    .get_accounts(identityNumber, effectiveOrigin)
    .then(throwCanisterError);
  // TODO: currently makes sure that local last used sorting is applied
  //       to queried accounts since the latter has no timestamps yet.
  //       This can be removed once the canister stores the timestamps.
  accounts = lastUsedIdentitiesStore.syncLastUsedAccounts(
    identityNumber,
    effectiveOrigin,
    accounts,
  );
  return { accounts };
};
