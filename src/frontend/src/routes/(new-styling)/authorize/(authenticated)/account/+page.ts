import type { PageLoad } from "./$types";
import {
  authenticatedStore,
  isAuthenticatedStore,
} from "$lib/stores/authentication.store";
import { get } from "svelte/store";
import { authorizationContextStore } from "$lib/stores/authorization.store";
import { redirect } from "@sveltejs/kit";

export const load: PageLoad = async () => {
  const { identityNumber, actor } = get(authenticatedStore);
  const { authRequest, requestOrigin } = get(authorizationContextStore);
  const accounts = await actor.get_accounts(
    identityNumber,
    authRequest.derivationOrigin ?? requestOrigin,
  );
  return { accounts };
};
