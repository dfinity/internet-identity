import { DelegationChain, DelegationIdentity } from "@dfinity/identity";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import {
  CosePublicKey,
  DiscoverablePasskeyIdentity,
} from "$lib/utils/discoverablePasskeyIdentity";
import { isNullish, nonNullish } from "@dfinity/utils";
import { creationOptions } from "$lib/utils/iiConnection";
import { Actor, HttpAgent, SignIdentity } from "@dfinity/agent";
import { Principal } from "@dfinity/principal";
import { idlFactory as internet_identity_idl } from "$lib/generated/internet_identity_idl";

export class IdentityNotMigratedError extends Error {
  constructor() {
    super();
    Object.setPrototypeOf(this, IdentityNotMigratedError.prototype);
  }

  message = "Identity has not been migrated.";
}

export const authenticateWithPasskey = async ({
  canisterId,
  agent,
  credentialId,
  expiration = 30 * 60 * 1000,
}: {
  canisterId: Principal;
  agent: HttpAgent;
  credentialId?: Uint8Array;
  expiration?: number;
}): Promise<{
  identity: DelegationIdentity;
  anchorNumber: bigint;
  credentialId: Uint8Array;
}> => {
  const identity = await agent.config.identity;
  if (!identity || !(identity instanceof SignIdentity)) {
    throw new Error("Agent with a SignIdentity required");
  }
  const actor = Actor.createActor<_SERVICE>(internet_identity_idl, {
    agent,
    canisterId,
  });
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
        throw new IdentityNotMigratedError();
      }
      anchorNumber = lookupResult.anchor_number;
      return CosePublicKey.fromDer(new Uint8Array(lookupResult.pubkey));
    },
  });
  const delegation = await DelegationChain.create(
    passkeyIdentity,
    identity.getPublicKey(),
    new Date(Date.now() + expiration),
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
