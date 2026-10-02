/**
 * Who the storage of this origin can sign for.
 *
 * Notifications are a browser's, not an identity's: what reaches the user is whatever
 * the document they arrive in can prove it is. A browser keeps a key per identity, so
 * it answers one entry per identity that has signed in here; a Home Screen app's
 * partition holds a single key for the single entry it claimed. Both are browser
 * entries to the canister, and both are read the same way.
 *
 * The anchor number comes along with the key because it has to: a browser call names
 * the anchor it is for, and the canister resolves the caller inside it rather than
 * finding it from the key.
 */
import type { SignIdentity } from "@icp-sdk/core/agent";
import { ECDSAKeyIdentity } from "@icp-sdk/core/identity";
import {
  browserKeyIdentity,
  registeredIdentityNumbers,
} from "$lib/stores/browser-key.store";
import { readAppKey } from "./notificationAppKey";

export interface SigningIdentity {
  identityNumber: bigint;
  identity: SignIdentity;
}

const fromBrowserKeys = async (): Promise<SigningIdentity[]> => {
  const identityNumbers = await registeredIdentityNumbers();
  const entries = await Promise.all(
    identityNumbers.map(async (identityNumber) => {
      const identity = await browserKeyIdentity(identityNumber).catch(
        () => undefined,
      );
      return identity === undefined ? [] : [{ identityNumber, identity }];
    }),
  );
  return entries.flat();
};

const fromAppKey = async (): Promise<SigningIdentity[]> => {
  const record = await readAppKey();
  if (record?.identityNumber === undefined) {
    return [];
  }
  return [
    {
      identityNumber: record.identityNumber,
      identity: await ECDSAKeyIdentity.fromKeyPair(record.keyPair),
    },
  ];
};

/**
 * Every entry this storage holds a key for.
 *
 * Empty where nothing has signed in and no app has been claimed, which is a document
 * with nothing to ask the canister rather than an error.
 */
export const signingIdentities = async (): Promise<SigningIdentity[]> => {
  const [browsers, app] = await Promise.all([
    fromBrowserKeys().catch(() => []),
    fromAppKey().catch(() => []),
  ]);
  return [...browsers, ...app];
};
