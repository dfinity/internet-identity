import { Principal } from "@icp-sdk/core/principal";
import { Actor } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { idlFactory as internet_identity_idl } from "$lib/generated/internet_identity_idl";
import {
  throwCanisterError,
  transformSignedDelegation,
} from "$lib/utils/utils";
import { DelegationChain, DelegationIdentity } from "@icp-sdk/core/identity";
import { Session } from "$lib/stores/session.store";
import { decodeJWT } from "$lib/utils/openID";
import { SignIdentity } from "@dfinity/agent";

export class OpenIdDelegationIdentity extends DelegationIdentity {
  iss?: string;
  sub?: string;

  static fromDelegation(
    key: Pick<SignIdentity, "sign">,
    delegation: DelegationChain,
  ): OpenIdDelegationIdentity {
    return new this(key, delegation);
  }
}

export const authenticateWithJWT = async ({
  canisterId,
  session,
  jwt,
}: {
  canisterId: Principal;
  session: Session;
  jwt: string;
}): Promise<{
  identity: DelegationIdentity;
  identityNumber: bigint;
}> => {
  const actor = Actor.createActor<_SERVICE>(internet_identity_idl, {
    agent: session.agent,
    canisterId,
  });
  const sessionKey = new Uint8Array(session.identity.getPublicKey().toDer());
  const {
    anchor_number: identityNumber,
    expiration,
    user_key,
  } = await actor
    .openid_prepare_delegation(jwt, session.salt, sessionKey)
    .then(throwCanisterError);
  const signedDelegation = await actor
    .openid_get_delegation(jwt, session.salt, sessionKey, expiration)
    .then(throwCanisterError);
  const transformedDelegation = transformSignedDelegation(signedDelegation);
  const delegationChain = DelegationChain.fromDelegations(
    [transformedDelegation],
    new Uint8Array(user_key),
  );
  // Use `OpenIdDelegationIdentity` class so we can add OpenID metadata
  const identity = OpenIdDelegationIdentity.fromDelegation(
    session.identity,
    delegationChain,
  );
  // Add OpenID metadata to delegation identity instance
  const { iss, sub } = decodeJWT(jwt);
  identity.iss = iss;
  identity.sub = sub;
  return { identity, identityNumber };
};
