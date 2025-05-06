import type { PageLoad } from "./$types";
import { get } from "svelte/store";
import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
import { redirect } from "@sveltejs/kit";
import { nonNullish } from "@dfinity/utils";

let redirected = false;

export const load: PageLoad = () => {
  if (!redirected) {
    // Only redirect on first visit
    const lastUsedIdentity = Object.values(get(lastUsedIdentitiesStore)).sort(
      (a, b) => b.lastUsedTimestampMillis - a.lastUsedTimestampMillis,
    )[0];
    if (
      nonNullish(lastUsedIdentity?.accounts) &&
      Object.values(lastUsedIdentity.accounts).length > 0
    ) {
      // Make sure that last used identity also has a last used account
      redirected = true;
      throw redirect(302, "/authorize/continue");
    }
  }
};
