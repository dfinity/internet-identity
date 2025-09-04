import { get } from "svelte/store";
import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
import { redirect } from "@sveltejs/kit";
import { nonNullish } from "@dfinity/utils";
import type { PageLoad } from "./$types";

let firstVisit = true;

export const load: PageLoad = ({ url }) => {
  const authMethod = url.searchParams.get("authMethod");

  // If authMethod is present, redirect to the selected route
  if (nonNullish(authMethod)) {
    console.log("redirecting to selected", authMethod);
    throw redirect(
      307,
      `/authorize/selected?authMethod=${encodeURIComponent(authMethod)}`,
    );
  }

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
