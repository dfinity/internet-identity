import type { PageLoad } from "./$types";

export const load: PageLoad = ({ url }) => {
  return {
    code: url.searchParams.get("code"),
    idToken: url.searchParams.get("id_token"),
    state: url.searchParams.get("state"),
  };
};
