import type { LayoutLoad } from "./$types";
import { redirect } from "@sveltejs/kit";
import { get } from "svelte/store";
import { isAuthenticatedStore } from "$lib/stores/authentication.store";

export const load: LayoutLoad = () => {
  // Go back to /authorize if not authenticated
  if (!get(isAuthenticatedStore)) {
    throw redirect(307, "/authorize");
  }
};
