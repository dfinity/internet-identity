import type { PageLoad } from "./$types";
import { derived, get } from "svelte/store";
import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";

export const load: PageLoad = () => {
  const lastUsedIdentities = get(
    derived(lastUsedIdentitiesStore, (lastUsedIdentitiesData) =>
      Object.values(lastUsedIdentitiesData).sort(
        (a, b) => b.lastUsedTimestampMillis - a.lastUsedTimestampMillis,
      ),
    ),
  );

  return {
    lastUsedIdentities,
  };
};
