import { get } from "svelte/store";
import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
import { redirect } from "@sveltejs/kit";
import { PageLoad } from "./$types";

let firstVisit = true;

export const load: PageLoad = ({ url }) => {
  const lastUsedIdentityAvailable =
    get(lastUsedIdentitiesStore).selected !== undefined;

  if (firstVisit) {
    firstVisit = false;

    if (lastUsedIdentityAvailable) {
      // Copy and modify URL path to retain other data e.g. search params
      const next = new URL(url);
      next.pathname = "/authorize/continue";
      throw redirect(307, next);
    }
  }
};
