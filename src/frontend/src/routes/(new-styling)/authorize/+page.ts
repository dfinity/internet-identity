import type { PageLoad } from "./$types";
import { get } from "svelte/store";
import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
import { redirect } from "@sveltejs/kit";
import {
  AuthenticationV2Events,
  authenticationV2Funnel,
} from "$lib/utils/analytics/authenticationV2Funnel";

let firstVisit = true;

export const load: PageLoad = () => {
  const lastUsedIdentityAvailable =
    Object.values(get(lastUsedIdentitiesStore)).length > 0;

  if (firstVisit) {
    firstVisit = false;

    authenticationV2Funnel.trigger(
      lastUsedIdentityAvailable
        ? AuthenticationV2Events.LastUsedPresent
        : AuthenticationV2Events.LastUsedNotPresent,
    );
    if (lastUsedIdentityAvailable) {
      throw redirect(302, "/authorize/continue");
    }
  }
};
