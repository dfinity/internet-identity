import { redirect } from "@sveltejs/kit";
import type { PageLoad } from "./$types";

export const load: PageLoad = () => {
  // Redirect FAQ to external support site using SvelteKit's redirect
  throw redirect(307, "https://identitysupport.dfinity.org/hc/en-us");
};
