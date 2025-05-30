import type { PageLoad } from "./$types";
import { authenticatedStore } from "$lib/stores/authentication.store";
import { get } from "svelte/store";
import { authorizationContextStore } from "$lib/stores/authorization.store";
import { throwCanisterError } from "$lib/utils/utils";

export const load: PageLoad = async () => {
  const { identityNumber, actor } = get(authenticatedStore);
  const { effectiveOrigin } = get(authorizationContextStore);
  const accounts = await actor
    .get_accounts(identityNumber, effectiveOrigin)
    .then(throwCanisterError);
  return { accounts };
};
