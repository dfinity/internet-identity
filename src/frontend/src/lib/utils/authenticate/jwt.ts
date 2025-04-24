import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { ActorSubclass, SignIdentity } from "@dfinity/agent";
import { identityFromActor } from "$lib/utils/authenticate/actor";
import {
  throwCanisterError,
  transformSignedDelegation,
} from "$lib/utils/utils";
import { DelegationChain, DelegationIdentity } from "@dfinity/identity";
import { decodeJWT } from "$lib/utils/openID";

export const authenticateWithJWT = async ({
  jwt,
  salt,
  actor,
}: {
  jwt: string;
  salt: Uint8Array;
  actor: ActorSubclass<_SERVICE>;
}): Promise<{
  identity: SignIdentity;
  anchorNumber: bigint;
  sub: string | undefined;
}> => {
  const identity = await identityFromActor(actor);
  const sessionKey = new Uint8Array(identity.getPublicKey().toDer());

  // Decode JWT using jose
  const payload = decodeJWT(jwt);
  const sub = payload?.sub;

  const { anchor_number, expiration, user_key } = await actor
    .openid_prepare_delegation(jwt, salt, sessionKey)
    .then(throwCanisterError);
  const signedDelegation = await actor
    .openid_get_delegation(jwt, salt, sessionKey, expiration)
    .then(throwCanisterError);
  const transformedDelegation = transformSignedDelegation(signedDelegation);
  const delegationChain = DelegationChain.fromDelegations(
    [transformedDelegation],
    new Uint8Array(user_key),
  );
  return {
    identity: DelegationIdentity.fromDelegation(identity, delegationChain),
    anchorNumber: anchor_number,
    sub: typeof sub !== "string" ? undefined : sub,
  };
};
