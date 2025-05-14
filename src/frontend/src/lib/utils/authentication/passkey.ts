import { Principal } from "@dfinity/principal";
import { Actor } from "@dfinity/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { idlFactory as internet_identity_idl } from "$lib/generated/internet_identity_idl";
import {
  CosePublicKey,
  DiscoverablePasskeyIdentity,
} from "$lib/utils/discoverablePasskeyIdentity";
import { isNullish, nonNullish } from "@dfinity/utils";
import { creationOptions } from "$lib/utils/iiConnection";
import { DelegationChain, DelegationIdentity } from "@dfinity/identity";
import { Session } from "$lib/stores/session.store";

export class IdentityNotMigratedError extends Error {
  constructor() {
    super();
    Object.setPrototypeOf(this, IdentityNotMigratedError.prototype);
  }

  message = "Identity has not been migrated.";
}

export const authenticateWithPasskey = async ({
  canisterId,
  session,
  credentialId,
  expiration = 30 * 60 * 1000,
}: {
  canisterId: Principal;
  session: Pick<Session, "identity" | "agent">;
  credentialId?: Uint8Array;
  expiration?: number;
}): Promise<{
  identity: DelegationIdentity;
  identityNumber: bigint;
  credentialId: Uint8Array;
}> => {
  const actor = Actor.createActor<_SERVICE>(internet_identity_idl, {
    agent: session.agent,
    canisterId,
  });
  let identityNumber: bigint;
  const passkeyIdentity = DiscoverablePasskeyIdentity.useExisting({
    credentialId,
    getPublicKey: async (result) => {
      console.log("lookup_device_key", result.rawId);
      const lookupResult = (
        await actor.lookup_device_key(new Uint8Array(result.rawId))
      )[0];
      if (isNullish(lookupResult)) {
        throw new IdentityNotMigratedError();
      }
      console.log("lookupResult", lookupResult);
      identityNumber = lookupResult.anchor_number;
      return CosePublicKey.fromDer(new Uint8Array(lookupResult.pubkey));
    },
  });
  const delegation = await DelegationChain.create(
    passkeyIdentity,
    session.identity.getPublicKey(),
    new Date(Date.now() + expiration),
  );
  if (isNullish(identityNumber!)) {
    throw new Error("Unreachable, identity number should have been set");
  }
  const identity = DelegationIdentity.fromDelegation(
    session.identity,
    delegation,
  );
  return {
    identity,
    identityNumber,
    credentialId: new Uint8Array(passkeyIdentity.getCredentialId()!),
  };
};
