import type { PageLoad } from "./$types";
import { toRelative } from "$lib/utils/urlUtils";

export const load: PageLoad = ({ url }) => {
  const param = url.searchParams.get("next");
  if (param === null) {
    return;
  }
  const next = toRelative(param);
  return { next };
};
