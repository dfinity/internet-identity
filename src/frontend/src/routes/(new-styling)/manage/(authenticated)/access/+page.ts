import type { PageLoad } from "./$types";
export const load: PageLoad = async ({ url }) => {
  return {
    pendingRegistrationId: url.searchParams.get("activate"),
  };
};
