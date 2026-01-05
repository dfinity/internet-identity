import type { PageLoad } from "./$types";

export const load: PageLoad = ({ url }) => {
  // Sanitize the next search param, only accept relative paths
  try {
    const param = url.searchParams.get("next");
    if (param === null) {
      return;
    }
    const paramURL = new URL(param, url);
    if (paramURL.origin !== url.origin) {
      return;
    }
    const next = paramURL.pathname + paramURL.search + paramURL.hash;
    return { next };
  } catch {
    return;
  }
};
