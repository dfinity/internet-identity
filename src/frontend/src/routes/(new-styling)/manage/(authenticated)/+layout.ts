import type { LayoutLoad } from "./$types";
import { redirect } from "@sveltejs/kit";
import { get } from "svelte/store";
import { authenticationStore } from "$lib/stores/authentication.store";
import { mintSession } from "$lib/stores/session-delegation.store";
import { throwCanisterError } from "$lib/utils/utils";
import { receiveAuthFromOpener } from "$lib/utils/auth-handoff";

export const load: LayoutLoad = async ({ url }) => {
  let authentication = get(authenticationStore);

  if (authentication === undefined) {
    const handoff = await receiveAuthFromOpener({ timeoutMs: 2000 });
    if (handoff !== null) {
      await authenticationStore.set(handoff);
      authentication = get(authenticationStore);
      if (authentication !== undefined) {
        void mintSession({
          identityNumber: authentication.identityNumber,
          actor: authentication.actor,
        });
      }
    }
  }

  if (authentication === undefined) {
    const next = url.pathname + url.search;
    const location = new URL("/login", url.origin);
    if (next !== "/manage") {
      location.searchParams.set("next", next);
    }
    throw redirect(307, location);
  }

  const identityInfo = await authentication.actor
    .identity_info(authentication.identityNumber)
    .then(throwCanisterError);

  return { identityInfo, identityNumber: authentication.identityNumber };
};
