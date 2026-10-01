import type { PageLoad } from "./$types";
import { get } from "svelte/store";
import { authenticatedStore } from "$lib/stores/authentication.store";
import { throwCanisterError } from "$lib/utils/utils";

export const load: PageLoad = async ({ parent }) => {
  // The layout's load is what makes sure there is an identity to ask about.
  const { identityNumber } = await parent();
  const applications = await get(authenticatedStore)
    .actor.list_applications({ anchor_number: identityNumber })
    .then(throwCanisterError);
  return { applications };
};
