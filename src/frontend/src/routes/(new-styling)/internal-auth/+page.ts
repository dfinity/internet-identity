import type { PageLoad } from "./$types";

export const load: PageLoad = ({ url }) => {
  return { next: url.searchParams.get("next") ?? "/manage" };
};
