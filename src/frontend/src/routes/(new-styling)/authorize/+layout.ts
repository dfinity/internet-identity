import type { LayoutLoad } from "./$types";
import { redirect } from "@sveltejs/kit";
import { authenticationState } from "$lib/state/authentication";
import { nonNullish } from "@dfinity/utils";

export const load: LayoutLoad = () => {
  if (nonNullish(authenticationState.authenticated)) {
    throw redirect(302, "/authorize/account");
  }
};
