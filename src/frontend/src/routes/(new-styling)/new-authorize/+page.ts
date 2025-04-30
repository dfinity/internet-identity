import { ECDSAKeyIdentity } from "@dfinity/identity";
import type { PageLoad } from "./$types";
import { createAnonymousNonce } from "$lib/utils/openID";
import { readCanisterConfig, readCanisterId } from "$lib/utils/init";
import { Connection } from "$lib/utils/iiConnection";
import { derived, get } from "svelte/store";
import {
  lastUsedIdentitiesStore,
  lastUsedIdentityStore,
} from "$lib/stores/last-used-identities.store";

export const ssr = false;

export const load: PageLoad = async () => {
  const canisterId = readCanisterId();
  const config = readCanisterConfig();
  const connection = new Connection(canisterId, config);
  const identity = await ECDSAKeyIdentity.generate({
    extractable: false,
  });
  const actor = await connection.createActor(identity);
  const { nonce, salt } = await createAnonymousNonce(identity.getPrincipal());
  const lastUsedIdentity = get(lastUsedIdentityStore);
  const lastUsedIdentities = get(
    derived(lastUsedIdentitiesStore, (lastUsedIdentities) =>
      Object.values(lastUsedIdentities)
        .sort((a, b) => b.lastUsedTimestampMillis - a.lastUsedTimestampMillis)
        .slice(0, 5),
    ),
  );
  return {
    session: { config, actor, identity, nonce, salt },
    lastUsedIdentity,
    lastUsedIdentities,
  };
};
