/** A browser keeps a key per identity signed in here; a Home Screen app's partition holds a single key for the single entry it claimed.
 * The anchor number comes with the key because a browser call names the anchor it is for, and the canister resolves the caller inside it. */
import type { SignIdentity } from "@icp-sdk/core/agent";
import { ECDSAKeyIdentity } from "@icp-sdk/core/identity";
import {
  browserKeyIdentity,
  registeredIdentityNumbers,
} from "$lib/stores/browser-key.store";
import { readAppKey, type AppKeyRecord } from "./notificationAppKey";
import { appKeyActorWith } from "./notificationAppActor";
import { appAnchors } from "./appAnchors";
import { config } from "./workerConfig";
import { Principal } from "@icp-sdk/core/principal";

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

/** The identities pointing at this app, which only the canister can name. Empty for an app
 *  that has never linked, since it has no record to ask about. Rejects where the canister
 *  could not be reached, or where the worker was told nothing to reach. */
const claimedAnchors = async (record: AppKeyRecord): Promise<bigint[]> => {
  if (record.appId === undefined) {
    return [];
  }
  const actor = await appKeyActorWith(record, {
    agentOptions: config.agentOptions,
    canisterId: Principal.fromText(config.canisterId),
  });
  return appAnchors(actor, record.appId);
};

const fromAppKey = async (): Promise<SigningIdentity[]> => {
  const stored = await readAppKey();
  switch (stored.kind) {
    case "no-key":
      return [];
    case "unreadable":
      // Not the same as being no app: the key may be sitting behind a store that would not open, and signing for nobody shows the browser's own "updated in the background" notice.
      throw new Error(`the app key could not be read: ${stored.reason}`);
    case "key": {
      const identity = await ECDSAKeyIdentity.fromKeyPair(
        stored.record.keyPair,
      );
      // The canister is the only party that can name them: a browser can point another identity at this app while it is not running.
      const anchors = await claimedAnchors(stored.record);
      return anchors.map((identityNumber) => ({ identityNumber, identity }));
    }
  }
};

/** Empty where nothing has signed in and no app has been claimed, which is a document with nothing to ask rather than an error.
 * The two keys live in stores of their own, so a failure in one is reported rather than folded into that emptiness. */
export const signingIdentities = async (): Promise<SigningIdentity[]> => {
  const [browsers, app] = await Promise.all([
    fromBrowserKeys().catch((error: unknown) => {
      console.error(error);
      return [];
    }),
    fromAppKey().catch((error: unknown) => {
      console.error(error);
      return [];
    }),
  ]);
  return [...browsers, ...app];
};
