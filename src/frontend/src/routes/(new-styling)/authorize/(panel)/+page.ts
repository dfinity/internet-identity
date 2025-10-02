import { get } from "svelte/store";
import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
import { redirect } from "@sveltejs/kit";
import { nonNullish } from "@dfinity/utils";
import { PageLoad } from "./$types";

let firstVisit = true;

export const load: PageLoad = ({ url }) => {
  const lastUsedIdentityAvailable = nonNullish(
    get(lastUsedIdentitiesStore).selected,
  );

  if (firstVisit) {
    firstVisit = false;

    if (lastUsedIdentityAvailable) {
      const next = new URL(url);
      next.pathname = "/authorize/continue";
      throw redirect(307, next);
    }
  }
};
