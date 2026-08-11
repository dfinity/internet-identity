import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { DelegationChain, ECDSAKeyIdentity } from "@icp-sdk/core/identity";
import {
  throwCanisterError,
  transformSignedDelegation,
} from "$lib/utils/utils";

// Everything II's service worker needs to authenticate to a dApp as the user
// when a routing ping arrives: the non-extractable session key, the signed
// chain, when it expires, and the origin it belongs to (the SW's lookup key).
export interface PushDelegationRecord {
  origin: string;
  identityNumber: bigint;
  keyPair: CryptoKeyPair;
  chainJson: string;
  expiresAtMillis: number;
}

// Mint a read-only account delegation for `origin` so the service worker can
// query that dApp as the user and pull the notification content.
//
// `permissions = queries` makes the IC reject any update call through it, so a
// leaked delegation can only read. The principal it produces is the same
// per-origin principal the dApp already sees when the user signs in there, so
// the dApp recognises the caller with no coordination.
export const mintPushDelegation = async ({
  identityNumber,
  origin,
  actor,
}: {
  identityNumber: bigint;
  origin: string;
  actor: ActorSubclass<_SERVICE>;
}): Promise<PushDelegationRecord> => {
  const sessionIdentity = await ECDSAKeyIdentity.generate({
    extractable: false,
  });
  const sessionKey = new Uint8Array(sessionIdentity.getPublicKey().toDer());

  const { user_key, expiration } = await actor
    .prepare_account_delegation(
      identityNumber,
      origin,
      [],
      sessionKey,
      [],
      [{ queries: null }],
    )
    .then(throwCanisterError);

  const signedDelegation = await actor
    .get_account_delegation(
      identityNumber,
      origin,
      [],
      sessionKey,
      expiration,
      [{ queries: null }],
    )
    .then(throwCanisterError);

  const chain = DelegationChain.fromDelegations(
    [transformSignedDelegation(signedDelegation)],
    new Uint8Array(user_key),
  );

  return {
    origin,
    identityNumber,
    keyPair: sessionIdentity.getKeyPair(),
    chainJson: JSON.stringify(chain.toJSON()),
    expiresAtMillis: Number(expiration / BigInt(1_000_000)),
  };
};
