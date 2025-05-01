import type { PageLoad } from "./$types";
import { get } from "svelte/store";
import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
import { redirect } from "@sveltejs/kit";

export const load: PageLoad = () => {
  const hasLastUsedIdentity =
    Object.keys(get(lastUsedIdentitiesStore)).length > 0;
  if (hasLastUsedIdentity) {
    throw redirect(302, "./continue");
  }
};
