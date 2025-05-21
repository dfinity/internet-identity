import type { PageLoad } from "./$types";
import { get } from "svelte/store";
import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
import { redirect } from "@sveltejs/kit";
import { nonNullish } from "@dfinity/utils";
import { authorizationContextStore } from "$lib/stores/authorization.store";

let redirected = false;

export const load: PageLoad = () => {
  if (!redirected) {
    // Only redirect on first visit
    const context = get(authorizationContextStore);
    const origin =
      context.authRequest.derivationOrigin ?? context.requestOrigin;
    const lastUsedIdentity = Object.values(get(lastUsedIdentitiesStore)).sort(
      (a, b) => b.lastUsedTimestampMillis - a.lastUsedTimestampMillis,
    )[0];
    if (nonNullish(lastUsedIdentity?.accounts?.[origin])) {
      // Make sure that last used identity has a last used account
      // for the current authorization request context origin.
      redirected = true;
      throw redirect(302, "/authorize/continue");
    }
  }
};
