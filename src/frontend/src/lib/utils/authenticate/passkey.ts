import { DelegationChain, DelegationIdentity } from "@dfinity/identity";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import {
  CosePublicKey,
  DiscoverablePasskeyIdentity,
} from "$lib/utils/discoverablePasskeyIdentity";
import { isNullish, nonNullish } from "@dfinity/utils";
import { creationOptions } from "$lib/utils/iiConnection";
import { ActorSubclass } from "@dfinity/agent";
import { identityFromActor } from "$lib/utils/authenticate/actor";

export const authenticateWithPasskey = async ({
  credentialId,
  actor,
}: {
  credentialId?: Uint8Array;
  actor: ActorSubclass<_SERVICE>;
}): Promise<{
  identity: DelegationIdentity;
  anchorNumber: bigint;
  credentialId: Uint8Array;
}> => {
  const identity = await identityFromActor(actor);
  let anchorNumber: bigint;
  const passkeyIdentity = new DiscoverablePasskeyIdentity({
    credentialRequestOptions: {
      publicKey: nonNullish(credentialId)
        ? {
            allowCredentials: [{ type: "public-key", id: credentialId }],
          }
        : creationOptions([], undefined, undefined),
    },
    getPublicKey: async (result) => {
      const lookupResult = (
        await actor.lookup_device_key(new Uint8Array(result.rawId))
      )[0];
      if (isNullish(lookupResult)) {
        throw new Error("Account not migrated yet");
      }
      anchorNumber = lookupResult.anchor_number;
      return CosePublicKey.fromDer(new Uint8Array(lookupResult.pubkey));
    },
  });
  const delegation = await DelegationChain.create(
    passkeyIdentity,
    identity.getPublicKey(),
    new Date(Date.now() + 30 * 60 * 1000),
  );
  const delegationIdentity = DelegationIdentity.fromDelegation(
    identity,
    delegation,
  );
  return {
    identity: delegationIdentity,
    anchorNumber: anchorNumber!,
    credentialId: new Uint8Array(passkeyIdentity.getCredentialId()!),
  };
};
