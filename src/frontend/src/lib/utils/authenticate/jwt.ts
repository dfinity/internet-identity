import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { Actor, HttpAgent, SignIdentity } from "@dfinity/agent";
import {
  throwCanisterError,
  transformSignedDelegation,
} from "$lib/utils/utils";
import { DelegationChain, DelegationIdentity } from "@dfinity/identity";
import { decodeJWT } from "$lib/utils/openID";
import { Principal } from "@dfinity/principal";
import { idlFactory as internet_identity_idl } from "$lib/generated/internet_identity_idl";

export const authenticateWithJWT = async ({
  canisterId,
  agent,
  jwt,
  salt,
}: {
  canisterId: Principal;
  agent: HttpAgent;
  jwt: string;
  salt: Uint8Array;
}): Promise<{
  identity: DelegationIdentity;
  anchorNumber: bigint;
  sub: string;
}> => {
  const identity = await agent.config.identity;
  if (!identity || !(identity instanceof SignIdentity)) {
    throw new Error("Agent with a SignIdentity required");
  }
  const actor = Actor.createActor<_SERVICE>(internet_identity_idl, {
    agent,
    canisterId,
  });
  const sessionKey = new Uint8Array(identity.getPublicKey().toDer());
  const { sub } = decodeJWT(jwt);
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
    sub,
  };
};
