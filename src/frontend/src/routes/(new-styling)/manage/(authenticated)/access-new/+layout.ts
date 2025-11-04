import type { LayoutLoad } from "./$types";
import { redirect } from "@sveltejs/kit";
import { get } from "svelte/store";
import { authenticationStore } from "$lib/stores/authentication.store";
import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
import { isNullish } from "@dfinity/utils";
import { throwCanisterError } from "$lib/utils/utils";

// This load function will later move to /manage root
export const load: LayoutLoad = async ({ url }) => {
  const authentication = get(authenticationStore);
  const selectedIdentity = get(lastUsedIdentitiesStore).selected;
  if (
    isNullish(authentication) ||
    isNullish(selectedIdentity) ||
    authentication.identityNumber !== selectedIdentity.identityNumber
  ) {
    // Add original target URL as next search param,
    // if it's not the default target URL (/manage).
    const next = url.pathname + url.search;
    const location = new URL("/login", url.origin);
    if (next !== "/manage") {
      location.searchParams.set("next", next);
    }
    throw redirect(307, location);
  }

  const identityInfo = await authentication.actor
    .identity_info(selectedIdentity.identityNumber)
    .then(throwCanisterError);

  return { identityInfo, identityNumber: selectedIdentity.identityNumber };
};
