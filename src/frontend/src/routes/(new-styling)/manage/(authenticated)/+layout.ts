import type { LayoutLoad } from "./$types";
import { redirect } from "@sveltejs/kit";
import { get } from "svelte/store";
import { authenticationStore } from "$lib/stores/authentication.store";
import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
import { isNullish } from "@dfinity/utils";
import identityInfoState from "$lib/stores/identity-info.state.svelte";

export const load: LayoutLoad = () => {
  // Go back to / if not authenticated with currently selected identity
  const authentication = get(authenticationStore);
  const selectedIdentity = get(lastUsedIdentitiesStore).selected;
  if (
    isNullish(authentication) ||
    isNullish(selectedIdentity) ||
    authentication.identityNumber !== selectedIdentity.identityNumber
  ) {
    throw redirect(307, "/");
  }

  void identityInfoState.fetch();
};
