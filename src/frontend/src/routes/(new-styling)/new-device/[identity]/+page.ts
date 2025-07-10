import type { PageLoad } from "./$types";

export const load: PageLoad = ({ params }) => {
  return {
    identityNumber: BigInt(parseInt(params.identity, 16)),
  };
};
