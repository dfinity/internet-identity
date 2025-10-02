import { LayoutLoad } from "./$types";

export const load: LayoutLoad = ({ url }) => {
  return {
    legacyProtocol: url.searchParams.has("legacyProtocol"),
  };
};
