/** The key of the last ICP Alerts install this browser showed the steps for.
 *  The app added from those steps records that key as its installer, and only this browser kept a copy, so a signature by it places this browser on the phone the app is on.
 *  Every install on an iPhone shows its steps in Safari, so a later one, from Safari itself or handed over from another Home Screen app, joins the app already there instead of adding a second icon. */
import { createStore, get as idbGet, set as idbSet } from "idb-keyval";
import { ECDSAKeyIdentity } from "@icp-sdk/core/identity";
import type { JoinNotificationAppError } from "$lib/generated/internet_identity_types";
import { agentOptions, canisterId } from "$lib/globals";
import { isCanisterError, throwCanisterError } from "$lib/utils/utils";
import { importLinkKey } from "./linkKey";
import type { LinkToken } from "./linkToken";
import { linkKeyActorWith } from "./notificationAppActor";
import { startInstall } from "./startInstall";

const INSTALLER_STORE = createStore("ii-notification-installer", "installer");
const INSTALLER_ENTRY = "installer";

/** Must match `NOTIFICATION_APP_JOIN_SIGNATURE_DOMAIN` in `browser_key.rs`. */
const JOIN_SIGNATURE_DOMAIN = new TextEncoder().encode(
  "ii-notification-app-join",
);

interface Installer {
  /** The identity the install was handed out for, whose browsers point at the app. */
  identityNumber: bigint;
  keyPair: CryptoKeyPair;
}

/** Never throws: a browser that cannot keep it adds a second app next time, as it would without it. */
export const rememberInstaller = async (token: LinkToken): Promise<void> => {
  try {
    const installer: Installer = {
      identityNumber: token.identityNumber,
      keyPair: await importLinkKey(token.linkKey),
    };
    await idbSet(INSTALLER_ENTRY, installer, INSTALLER_STORE);
  } catch (error) {
    console.error(error);
  }
};

const readInstaller = async (): Promise<Installer | undefined> => {
  try {
    return await idbGet<Installer>(INSTALLER_ENTRY, INSTALLER_STORE);
  } catch (error) {
    console.error(error);
    return undefined;
  }
};

/** Whether this browser showed an install it could join a later one to. */
export const keepsAnInstaller = async (): Promise<boolean> =>
  (await readInstaller()) !== undefined;

/** Must match `join_token_message` in `browser_key.rs`. */
const joinMessage = (
  identityNumber: bigint,
  caller: Uint8Array,
): Uint8Array<ArrayBuffer> => {
  const message = new Uint8Array(
    JOIN_SIGNATURE_DOMAIN.length + 8 + 1 + caller.length,
  );
  message.set(JOIN_SIGNATURE_DOMAIN);
  new DataView(
    message.buffer,
    message.byteOffset + JOIN_SIGNATURE_DOMAIN.length,
  ).setBigUint64(0, identityNumber, false);
  message[JOIN_SIGNATURE_DOMAIN.length + 8] = caller.length;
  message.set(caller, JOIN_SIGNATURE_DOMAIN.length + 9);
  return message;
};

const describe = (error: unknown): string =>
  isCanisterError<JoinNotificationAppError>(error)
    ? String(error.type)
    : error instanceof Error
      ? error.message
      : String(error);

/**
 * Points the install `linkKey` was handed out for at the app this browser installed
 * before, answering whether it did.
 *
 * `false` covers every way it can fail, which all leave the install to go ahead: no
 * installer kept here, an app since deleted, or a canister that would not answer.
 */
export const joinInstalledApp = async ({
  identityNumber,
  linkKey,
}: {
  identityNumber: bigint;
  linkKey: Uint8Array;
}): Promise<boolean> => {
  const installer = await readInstaller();
  if (installer === undefined) {
    return false;
  }
  try {
    const waiting = await ECDSAKeyIdentity.fromKeyPair(
      await importLinkKey(linkKey),
    );
    const signature = await crypto.subtle.sign(
      { name: "ECDSA", hash: "SHA-256" },
      installer.keyPair.privateKey,
      joinMessage(identityNumber, waiting.getPrincipal().toUint8Array()),
    );
    const installerKey = await crypto.subtle.exportKey(
      "spki",
      installer.keyPair.publicKey,
    );
    const actor = await linkKeyActorWith(linkKey, { agentOptions, canisterId });
    await actor
      .join_notification_app({
        anchor_number: identityNumber,
        installer_anchor_number: installer.identityNumber,
        installer_key: new Uint8Array(installerKey),
        signature: new Uint8Array(signature),
      })
      .then(throwCanisterError);
    return true;
  } catch (error) {
    console.error(`the install did not join the app: ${describe(error)}`);
    return false;
  }
};

/** Points this browser at the app it installed before, with no steps to show: the install is started and joined here.
 *  `false` where nothing was joined, including where this browser never showed an install, which starts nothing. */
export const joinFromThisBrowser = async (
  identityNumber: bigint,
): Promise<boolean> => {
  if (!(await keepsAnInstaller())) {
    return false;
  }
  try {
    const linkKey = await startInstall(identityNumber);
    return (
      linkKey !== undefined &&
      (await joinInstalledApp({ identityNumber, linkKey }))
    );
  } catch (error) {
    console.error(error);
    return false;
  }
};
