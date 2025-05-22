import type { PageLoad } from "./$types";
import { get } from "svelte/store";
import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
import { redirect } from "@sveltejs/kit";

let redirected = false;

export const load: PageLoad = () => {
  if (!redirected) {
    // Only redirect on first visit
    const lastUsedIdentityAvailable =
      Object.values(get(lastUsedIdentitiesStore)).length > 0;
    if (lastUsedIdentityAvailable) {
      redirected = true;
      throw redirect(302, "/authorize/continue");
    }
  }
};
