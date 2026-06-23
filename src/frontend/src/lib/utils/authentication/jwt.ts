import { Principal } from "@icp-sdk/core/principal";
import { Actor } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { idlFactory as internet_identity_idl } from "$lib/generated/internet_identity_idl";
import {
  throwCanisterError,
  transformSignedDelegation,
} from "$lib/utils/utils";
import { MAX_POLL_ATTEMPTS, pollDelay } from "$lib/utils/openidPoll";
import { DelegationChain, DelegationIdentity } from "@icp-sdk/core/identity";
import { Session } from "$lib/stores/session.store";

export const authenticateWithJWT = async ({
  canisterId,
  session,
  jwt,
  discoveryDomain,
}: {
  canisterId: Principal;
  session: Session;
  jwt: string;
  discoveryDomain?: string;
}): Promise<{
  identity: DelegationIdentity;
  identityNumber: bigint;
}> => {
  const actor = Actor.createActor<_SERVICE>(internet_identity_idl, {
    agent: session.agent,
    canisterId,
  });
  const sessionKey = new Uint8Array(session.identity.getPublicKey().toDer());
  const domain: [] | [string] =
    discoveryDomain !== undefined ? [discoveryDomain] : [];

  for (let attempt = 0; attempt < MAX_POLL_ATTEMPTS; attempt++) {
    // `prepare` (update) drives the SSO discovery/JWKS fetch; `get` (query)
    // reads the cached result. Either reports `Pending` while the cache is
    // cold — re-call `prepare` to keep the fetch moving, then retry.
    const prepared = await actor.openid_prepare_delegation(
      jwt,
      session.salt,
      sessionKey,
      domain,
    );
    if ("Pending" in prepared) {
      await pollDelay();
      continue;
    }
    const {
      anchor_number: identityNumber,
      expiration,
      user_key,
    } = await throwCanisterError(prepared);

    const delegation = await actor.openid_get_delegation(
      jwt,
      session.salt,
      sessionKey,
      expiration,
      domain,
    );
    if ("Pending" in delegation) {
      await pollDelay();
      continue;
    }
    const signedDelegation = await throwCanisterError(delegation);
    const transformedDelegation = transformSignedDelegation(signedDelegation);
    const delegationChain = DelegationChain.fromDelegations(
      [transformedDelegation],
      new Uint8Array(user_key),
    );
    const identity = DelegationIdentity.fromDelegation(
      session.identity,
      delegationChain,
    );
    return { identity, identityNumber };
  }

  throw new Error("Timed out waiting for SSO discovery to complete");
};
