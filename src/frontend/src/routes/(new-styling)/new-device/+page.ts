import type { PageLoad } from "./$types";

export const load: PageLoad = ({ url }) => {
  return {
    identityNumber: BigInt(parseInt(url.search.slice(1), 16)),
  };
};
