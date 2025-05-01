import type { LayoutLoad } from "./$types";
import { readCanisterConfig, readCanisterId } from "$lib/utils/init";
import { inferHost } from "$lib/utils/iiConnection";
import { ECDSAKeyIdentity } from "@dfinity/identity";
import { createAnonymousNonce } from "$lib/utils/openID";
import { HttpAgent } from "@dfinity/agent";
import { features } from "$lib/legacy/features";
import { Principal } from "@dfinity/principal";

export const load: LayoutLoad = async () => {
  const canisterId = Principal.fromText(readCanisterId());
  const config = readCanisterConfig();
  const host = inferHost();
  const shouldFetchRootKey =
    features.FETCH_ROOT_KEY || (config.fetch_root_key[0] ?? false);

  // Create a single anonymous agent, used to fetch public information
  //
  // This single instance is recommended over creating the agent on demand,
  // since multiple agent instances don't share the same watermark state.
  const agent = HttpAgent.createSync({
    host,
    shouldFetchRootKey,
  });

  // Create session, used during e.g. sign-up flows
  //
  // This single agent keeps the same identity across various pages,
  // which is needed during e.g. a sign-up flow with multiple steps.
  const identity = await ECDSAKeyIdentity.generate({
    extractable: false,
  });
  const { nonce, salt } = await createAnonymousNonce(identity.getPrincipal());
  const session = {
    agent: HttpAgent.createSync({
      identity,
      host,
      shouldFetchRootKey,
    }),
    nonce,
    salt,
  };

  return {
    canisterId,
    config,
    agent,
    session,
  };
};
