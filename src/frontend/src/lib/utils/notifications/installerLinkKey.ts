/** The link key of the last ICP Alerts install this browser showed the steps for.
 *  The app added from those steps records it as the link key it linked with, and only this browser kept a copy, so a signature by it places this browser on the phone the app is on.
 *  Every install on an iPhone shows its steps in Safari, so a later one, from Safari itself or handed over from another Home Screen app, joins the app already there instead of adding a second icon. */
import { createStore, get as idbGet, set as idbSet } from "idb-keyval";
import { ECDSAKeyIdentity } from "@icp-sdk/core/identity";
import type { ActorSubclass, SignIdentity } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { agentOptions, canisterId } from "$lib/globals";
import { browserKeyIdentity } from "$lib/stores/browser-key.store";
import { isCanisterError, throwCanisterError } from "$lib/utils/utils";
import { browserKeyActor } from "./browserActor";
import { importLinkKey } from "./linkKey";
import type { LinkToken } from "./linkToken";
import { actorAs, linkKeyActorWith } from "./notificationAppActor";

const INSTALLER_LINK_KEY_STORE = createStore(
  "ii-notification-installer-link-key",
  "installer-link-key",
);
const INSTALLER_LINK_KEY_ENTRY = "installer-link-key";

/** Must match `NOTIFICATION_APP_JOIN_SIGNATURE_DOMAIN` in `browser_key.rs`. */
const JOIN_SIGNATURE_DOMAIN = new TextEncoder().encode(
  "ii-notification-app-join",
);

interface InstallerLinkKey {
  /** The identity the install was handed out for, whose browsers point at the app. */
  identityNumber: bigint;
  keyPair: CryptoKeyPair;
}

/** Never throws: a browser that cannot keep it adds a second app next time, as it would without it. */
export const rememberInstallerLinkKey = async (
  token: LinkToken,
): Promise<void> => {
  try {
    const installerLinkKey: InstallerLinkKey = {
      identityNumber: token.identityNumber,
      keyPair: await importLinkKey(token.linkKey),
    };
    await idbSet(
      INSTALLER_LINK_KEY_ENTRY,
      installerLinkKey,
      INSTALLER_LINK_KEY_STORE,
    );
  } catch (error) {
    console.error(error);
  }
};

const readInstallerLinkKey = async (): Promise<
  InstallerLinkKey | undefined
> => {
  try {
    return await idbGet<InstallerLinkKey>(
      INSTALLER_LINK_KEY_ENTRY,
      INSTALLER_LINK_KEY_STORE,
    );
  } catch (error) {
    console.error(error);
    return undefined;
  }
};

/** Whether this browser showed an install it could join a later one to. */
export const keepsAnInstallerLinkKey = async (): Promise<boolean> =>
  (await readInstallerLinkKey()) !== undefined;

/** Must match `installer_message` in `browser_key.rs`. */
const joinMessage = (
  identityNumber: bigint,
  caller: Uint8Array,
): Uint8Array<ArrayBuffer> => {
  const domain = JOIN_SIGNATURE_DOMAIN;
  const message = new Uint8Array(domain.length + 8 + 1 + caller.length);
  message.set(domain);
  new DataView(message.buffer, message.byteOffset + domain.length).setBigUint64(
    0,
    identityNumber,
    false,
  );
  message[domain.length + 8] = caller.length;
  message.set(caller, domain.length + 9);
  return message;
};

const describe = (error: unknown): string =>
  isCanisterError(error)
    ? String(error.type)
    : error instanceof Error
      ? error.message
      : String(error);

/** Signs the join for `caller` with the kept link key and makes it as `actor`, answering whether the canister took it. */
const joinAs = async ({
  identityNumber,
  caller,
  actor,
}: {
  identityNumber: bigint;
  caller: SignIdentity;
  actor: ActorSubclass<_SERVICE>;
}): Promise<boolean> => {
  const installerLinkKey = await readInstallerLinkKey();
  if (installerLinkKey === undefined) {
    return false;
  }
  try {
    const signature = await crypto.subtle.sign(
      { name: "ECDSA", hash: "SHA-256" },
      installerLinkKey.keyPair.privateKey,
      joinMessage(identityNumber, caller.getPrincipal().toUint8Array()),
    );
    const installerPublicKey = await crypto.subtle.exportKey(
      "spki",
      installerLinkKey.keyPair.publicKey,
    );
    await actor
      .join_notification_app({
        anchor_number: identityNumber,
        installer_anchor_number: installerLinkKey.identityNumber,
        installer_link_key: new Uint8Array(installerPublicKey),
        signature: new Uint8Array(signature),
      })
      .then(throwCanisterError);
    return true;
  } catch (error) {
    console.error(`the join was refused: ${describe(error)}`);
    return false;
  }
};

/**
 * Points the install `token` was handed out for at the app this browser installed
 * before, answering whether it did.
 *
 * `false` covers every way it can fail, which all leave the install to go ahead: no
 * link key kept here, an app the canister no longer has, or a canister that would not
 * answer.
 */
export const joinInstalledApp = async ({
  identityNumber,
  linkKey,
}: LinkToken): Promise<boolean> => {
  try {
    const [caller, actor] = await Promise.all([
      importLinkKey(linkKey).then((keyPair) =>
        ECDSAKeyIdentity.fromKeyPair(keyPair),
      ),
      linkKeyActorWith(linkKey, { agentOptions, canisterId }),
    ]);
    return await joinAs({ identityNumber, caller, actor });
  } catch (error) {
    console.error(error);
    return false;
  }
};

/** Signed as the kept link key itself, which is what the canister recognises the installer by. */
const installerActor = async (
  installerLinkKey: InstallerLinkKey,
): Promise<ActorSubclass<_SERVICE>> =>
  actorAs(await ECDSAKeyIdentity.fromKeyPair(installerLinkKey.keyPair), {
    agentOptions,
    canisterId,
  });

/** Longer than the canister's window for an answer, so an app it would call gone has had all of it. */
const PROBE_ANSWER_WAIT_MS = 12_000;
const PROBE_POLL_MS = 1_000;

/** Whether the app this browser installed before is still on the phone, which only the
 *  app answering a probe shows: deleting it reports nothing. `"unknown"` where nothing
 *  could be asked, which is no reason to add a second app. */
export type InstalledApp = "answered" | "gone" | "unknown";

/** Asks the app to prove itself and waits for it. One that is there shows its own
 *  notification and calls; one that is gone is forgotten for every entry pointing at it,
 *  so each is asked to install again rather than reading as delivering to nothing. */
export const probeInstalledApp = async (): Promise<InstalledApp> => {
  const installerLinkKey = await readInstallerLinkKey();
  if (installerLinkKey === undefined) {
    return "gone";
  }
  const request = { installer_anchor_number: installerLinkKey.identityNumber };
  try {
    const actor = await installerActor(installerLinkKey);
    const before = (
      await actor.installed_notification_app_last_seen(request)
    )[0];
    if (before === undefined) {
      return "gone";
    }
    const probe = await actor.probe_installed_notification_app(request);
    if ("Err" in probe) {
      return "NoNotificationApp" in probe.Err ? "gone" : "unknown";
    }
    const deadline = Date.now() + PROBE_ANSWER_WAIT_MS;
    while (Date.now() < deadline) {
      await new Promise((resolve) => setTimeout(resolve, PROBE_POLL_MS));
      const seen = (
        await actor.installed_notification_app_last_seen(request)
      )[0];
      if (seen === undefined) {
        return "gone";
      }
      if (seen > before) {
        return "answered";
      }
    }
    const retired = await actor.retire_notification_app(request);
    return "Ok" in retired || "NoNotificationApp" in retired.Err
      ? "gone"
      : "unknown";
  } catch (error) {
    console.error(error);
    return "unknown";
  }
};

/** Points this browser's own entry at the app it installed before, whatever it pointed at until now: an app deleted from the Home Screen is never reported, so an entry left on one would read as delivering.
 *  `false` where nothing was joined, including where this browser never showed an install or no sign-in registered it. */
export const joinFromThisBrowser = async (
  identityNumber: bigint,
): Promise<boolean> => {
  if (!(await keepsAnInstallerLinkKey())) {
    return false;
  }
  try {
    const caller = await browserKeyIdentity(identityNumber);
    if (caller === undefined) {
      return false;
    }
    return await joinAs({
      identityNumber,
      caller,
      actor: await browserKeyActor(identityNumber),
    });
  } catch (error) {
    console.error(error);
    return false;
  }
};
