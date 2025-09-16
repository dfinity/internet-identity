import { Principal } from "@dfinity/principal";
import { Actor } from "@dfinity/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { idlFactory as internet_identity_idl } from "$lib/generated/internet_identity_idl";
import {
  throwCanisterError,
  transformSignedDelegation,
} from "$lib/utils/utils";
import { DelegationChain, DelegationIdentity } from "@dfinity/identity";
import { Session } from "$lib/stores/session.store";

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
    new Uint8Array(user_key).buffer,
  );
  const identity = DelegationIdentity.fromDelegation(
    session.identity,
    delegationChain,
  );
  return { identity, identityNumber };
};

export const authenticateIntermediateWithJWT = async ({
  canisterId,
  session,
  jwt,
  intermediateIdentity,
}: {
  canisterId: Principal;
  session: Session;
  jwt: string;
  intermediateIdentity: ECDSAKeyIdentity;
}): Promise<DelegationIdentity> => {
  const actor = Actor.createActor<_SERVICE>(internet_identity_idl, {
    agent: session.agent,
    canisterId,
  });

  const intermedKeyDer = new Uint8Array(
    intermediateIdentity.getPublicKey().toDer(),
  );

  const { expiration, user_key } = await actor
    .openid_prepare_delegation(jwt, session.salt, intermedKeyDer)
    .then(throwCanisterError);

  const signedDelegation = await actor
    .openid_get_delegation(jwt, session.salt, intermedKeyDer, expiration)
    .then(throwCanisterError);

  const transformedDelegation = transformSignedDelegation(signedDelegation);
  const delegationChain = DelegationChain.fromDelegations(
    [transformedDelegation],
    new Uint8Array(user_key),
  );

  return DelegationIdentity.fromDelegation(
    intermediateIdentity,
    delegationChain,
  );
};
