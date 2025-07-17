import type { PageLoad } from "./$types";
import { get } from "svelte/store";
import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
import { redirect } from "@sveltejs/kit";
import { nonNullish } from "@dfinity/utils";

let firstVisit = true;

export const load: PageLoad = () => {
  const lastUsedIdentityAvailable = nonNullish(
    get(lastUsedIdentitiesStore).selected,
  );

  if (firstVisit) {
    firstVisit = false;

    if (lastUsedIdentityAvailable) {
      throw redirect(307, "/authorize/continue");
    }
  }
};
