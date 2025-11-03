import type { PageLoad } from "./$types";
export const load: PageLoad = ({ url }) => {
  return {
    pendingRegistrationId: url.searchParams.get("activate"),
  };
};
