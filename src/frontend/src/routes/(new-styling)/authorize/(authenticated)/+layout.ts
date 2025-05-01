import type { LayoutLoad } from "./$types";
import { authenticationState } from "$lib/state/authenticated";
import { readCanisterConfig } from "$lib/utils/init";
import { inferHost } from "$lib/utils/iiConnection";
import { HttpAgent } from "@dfinity/agent";
import { features } from "$lib/legacy/features";
import { isNullish } from "@dfinity/utils";
import { redirect } from "@sveltejs/kit";

export const load: LayoutLoad = () => {
  const config = readCanisterConfig();
  const host = inferHost();
  const shouldFetchRootKey =
    features.FETCH_ROOT_KEY || (config.fetch_root_key[0] ?? false);

  const { authenticated } = authenticationState;

  if (isNullish(authenticated)) {
    throw redirect(302, "./");
  }

  // Create authenticated agent, used on e.g. manage accounts
  return {
    authenticated: {
      agent: HttpAgent.createSync({
        identity: authenticated.identity,
        host,
        shouldFetchRootKey,
      }),
      anchorNumber: authenticated.anchorNumber,
    },
  };
};
