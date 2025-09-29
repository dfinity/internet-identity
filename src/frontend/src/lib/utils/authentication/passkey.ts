import { Principal } from "@dfinity/principal";
import { Actor } from "@dfinity/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { idlFactory as internet_identity_idl } from "$lib/generated/internet_identity_idl";
import {
  CosePublicKey,
  DiscoverablePasskeyIdentity,
} from "$lib/utils/discoverablePasskeyIdentity";
import { isNullish, nonNullish } from "@dfinity/utils";
import { DelegationChain, DelegationIdentity } from "@dfinity/identity";
import { Session } from "$lib/stores/session.store";
import { features } from "$lib/legacy/features";
import { DiscoverableDummyIdentity } from "$lib/utils/discoverableDummyIdentity";
import { canisterConfig } from "$lib/globals";
import { bytesToHex } from "@noble/hashes/utils";

export class CredentialNotFound extends Error {
  constructor() {
    super();
    Object.setPrototypeOf(this, CredentialNotFound.prototype);
  }

  message =
    "Credential not found. Either you removed the passkey from the identity or this identity needs to be migrated.";
}

export const authenticateWithPasskey = async ({
  canisterId,
  session,
  credentialIds,
  expiration = 30 * 60 * 1000,
}: {
  canisterId: Principal;
  session: Pick<Session, "identity" | "agent">;
  credentialIds?: Uint8Array[];
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
  const dummyAuth =
    features.DUMMY_AUTH || nonNullish(canisterConfig.dummy_auth[0]?.[0]);
  let identityNumber: bigint;
  const passkeyIdentity = dummyAuth
    ? DiscoverableDummyIdentity.useExisting()
    : DiscoverablePasskeyIdentity.useExisting({
        credentialIds,
        getPublicKey: async (result) => {
          try {
            const lookupResult = (
              await actor.lookup_device_key(new Uint8Array(result.rawId))
            )[0];
            if (isNullish(lookupResult)) {
              throw new CredentialNotFound();
            }
            identityNumber = lookupResult.anchor_number;
            return CosePublicKey.fromDer(new Uint8Array(lookupResult.pubkey));
          } catch (error: unknown) {
            // To help debug, log the credential id
            console.error(error);
            console.error(
              `Error looking up device key ${bytesToHex(new Uint8Array(result.rawId))}`,
            );
            throw error;
          }
        },
      });
  if (dummyAuth) {
    identityNumber = (
      await actor.lookup_device_key(
        new Uint8Array(passkeyIdentity.getCredentialId()!),
      )
    )[0]!.anchor_number;
  }
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
