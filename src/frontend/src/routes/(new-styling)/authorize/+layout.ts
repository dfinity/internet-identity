import type { PageLoad } from "./$types";
import { get } from "svelte/store";
import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
import { redirect } from "@sveltejs/kit";
import { authenticationState } from "$lib/state/authenticated";
import { nonNullish } from "@dfinity/utils";

export const load: PageLoad = () => {
  if (nonNullish(authenticationState.authenticated)) {
    throw redirect(302, "/authorize/account");
  }
};
