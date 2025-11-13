import type { PageLoad } from "../../../../../../../../../.svelte-kit/types/src/frontend";

export const load: PageLoad = ({ url }) => {
  return { pendingRegistrationId: url.searchParams.get("activate") };
};
