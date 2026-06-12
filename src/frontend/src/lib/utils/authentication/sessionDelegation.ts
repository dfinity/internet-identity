import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import {
  DelegationChain,
  DelegationIdentity,
  ECDSAKeyIdentity,
} from "@icp-sdk/core/identity";
import {
  throwCanisterError,
  transformSignedDelegation,
} from "$lib/utils/utils";

export interface SessionDelegationRecord {
  identityNumber: bigint;
  keyPair: CryptoKeyPair;
  chainJson: string;
  scope: "account_management";
  expiresAtMillis: number;
}

export const mintSessionDelegation = async ({
  identityNumber,
  actor,
}: {
  identityNumber: bigint;
  actor: ActorSubclass<_SERVICE>;
}): Promise<SessionDelegationRecord> => {
  const sessionIdentity = await ECDSAKeyIdentity.generate({
    extractable: false,
  });
  const sessionKey = new Uint8Array(sessionIdentity.getPublicKey().toDer());

  const { user_key, expiration } = await actor
    .prepare_session_delegation(
      identityNumber,
      { account_management: null },
      sessionKey,
      [],
    )
    .then(throwCanisterError);

  const signedDelegation = await actor
    .get_session_delegation(
      identityNumber,
      { account_management: null },
      sessionKey,
      expiration,
    )
    .then(throwCanisterError);

  const transformedDelegation = transformSignedDelegation(signedDelegation);
  const chain = DelegationChain.fromDelegations(
    [transformedDelegation],
    new Uint8Array(user_key),
  );

  const expiresAtMillis = Number(expiration / BigInt(1_000_000));

  return {
    identityNumber,
    keyPair: sessionIdentity.getKeyPair(),
    chainJson: JSON.stringify(chain.toJSON()),
    scope: "account_management",
    expiresAtMillis,
  };
};

export const sessionDelegationIdentity = async (
  keyPair: CryptoKeyPair,
  chainJson: string,
): Promise<DelegationIdentity> => {
  const identity = await ECDSAKeyIdentity.fromKeyPair(keyPair);
  const chain = DelegationChain.fromJSON(JSON.parse(chainJson));
  return DelegationIdentity.fromDelegation(identity, chain);
};
