/** The installer key: one per Safari, made the first time it shows an install and kept for
 *  every one after. Its public half goes in the install URL and the app names it when it
 *  links, so the app has two principals, its own and its installer's. The private half
 *  never leaves this browser, so a signature by it shows a caller is on the phone the app
 *  is on.
 *
 *  Every install on an iPhone shows its steps in Safari, so a later one, from Safari itself
 *  or handed over from another Home Screen app, joins the app already there instead of
 *  adding a second icon. */
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

const INSTALLER_KEY_STORE = createStore(
  "ii-notification-installer-key",
  "installer-key",
);
const INSTALLER_KEY_ENTRY = "installer-key";

/** Must match `NOTIFICATION_APP_JOIN_SIGNATURE_DOMAIN` in `browser_key.rs`. */
const JOIN_SIGNATURE_DOMAIN = new TextEncoder().encode(
  "ii-notification-app-join",
);

interface InstallerKey {
  /** The private half cannot be exported, so it never leaves this browser's storage. */
  keyPair: CryptoKeyPair;
  /** The identity this browser last showed the steps for, whose browsers point at the
   *  app those steps added. Absent until it has shown one. */
  identityNumber?: bigint;
}

const readInstallerKey = async (): Promise<InstallerKey | undefined> => {
  try {
    return await idbGet<InstallerKey>(INSTALLER_KEY_ENTRY, INSTALLER_KEY_STORE);
  } catch (error) {
    console.error(error);
    return undefined;
  }
};

const writeInstallerKey = (installerKey: InstallerKey): Promise<void> =>
  idbSet(INSTALLER_KEY_ENTRY, installerKey, INSTALLER_KEY_STORE);

const ensureInstallerKey = async (): Promise<InstallerKey> => {
  const kept = await readInstallerKey();
  if (kept !== undefined) {
    return kept;
  }
  const keyPair = (await crypto.subtle.generateKey(
    { name: "ECDSA", namedCurve: "P-256" },
    false,
    ["sign", "verify"],
  )) as CryptoKeyPair;
  const installerKey: InstallerKey = { keyPair };
  await writeInstallerKey(installerKey);
  return installerKey;
};

const publicKeyOf = async (installerKey: InstallerKey): Promise<Uint8Array> =>
  new Uint8Array(
    await crypto.subtle.exportKey("spki", installerKey.keyPair.publicKey),
  );

/** The public half to name in the URL whose steps this browser is about to show, made on
 *  first use. Records `identityNumber` as the identity those steps are for. */
export const installerKeyForSteps = async (
  identityNumber: bigint,
): Promise<Uint8Array> => {
  const installerKey = await ensureInstallerKey();
  if (installerKey.identityNumber !== identityNumber) {
    await writeInstallerKey({ ...installerKey, identityNumber });
  }
  return await publicKeyOf(installerKey);
};

/** The installer key, where this browser has shown the steps for an app it could join a
 *  later install to. */
const installerOfAnApp = async (): Promise<
  Required<InstallerKey> | undefined
> => {
  const installerKey = await readInstallerKey();
  if (installerKey?.identityNumber === undefined) {
    return undefined;
  }
  return { ...installerKey, identityNumber: installerKey.identityNumber };
};

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

/** Signs the join for `caller` with the installer key and makes it as `actor`, answering
 *  whether the canister took it. */
const joinAs = async ({
  installer,
  identityNumber,
  caller,
  actor,
}: {
  installer: Required<InstallerKey>;
  identityNumber: bigint;
  caller: SignIdentity;
  actor: ActorSubclass<_SERVICE>;
}): Promise<boolean> => {
  try {
    const signature = await crypto.subtle.sign(
      { name: "ECDSA", hash: "SHA-256" },
      installer.keyPair.privateKey,
      joinMessage(identityNumber, caller.getPrincipal().toUint8Array()),
    );
    await actor
      .join_notification_app({
        anchor_number: identityNumber,
        installer_anchor_number: installer.identityNumber,
        installer_key: await publicKeyOf(installer),
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
 * `false` covers every way it can fail, which all leave the install to go ahead: no app
 * installed from here, an app the canister no longer has, or a canister that would not
 * answer.
 */
export const joinInstalledApp = async ({
  identityNumber,
  linkKey,
}: LinkToken): Promise<boolean> => {
  const installer = await installerOfAnApp();
  if (installer === undefined) {
    return false;
  }
  try {
    const [caller, actor] = await Promise.all([
      importLinkKey(linkKey).then((keyPair) =>
        ECDSAKeyIdentity.fromKeyPair(keyPair),
      ),
      linkKeyActorWith(linkKey, { agentOptions, canisterId }),
    ]);
    return await joinAs({ installer, identityNumber, caller, actor });
  } catch (error) {
    console.error(error);
    return false;
  }
};

/** Points this browser's own entry at the app it installed, whatever it pointed at until
 *  now: an app deleted from the Home Screen is never reported, so an entry left on one
 *  would read as delivering. Its switch stays as the user left it.
 *  `false` where nothing was joined, including where this browser never installed an app
 *  or no sign-in registered it. */
export const joinFromThisBrowser = async (
  identityNumber: bigint,
): Promise<boolean> => {
  const installer = await installerOfAnApp();
  if (installer === undefined) {
    return false;
  }
  try {
    const caller = await browserKeyIdentity(identityNumber);
    if (caller === undefined) {
      return false;
    }
    return await joinAs({
      installer,
      identityNumber,
      caller,
      actor: await browserKeyActor(identityNumber),
    });
  } catch (error) {
    console.error(error);
    return false;
  }
};

/** Longer than the canister's window for an answer, so an app it would call gone has had all of it. */
const PROBE_ANSWER_WAIT_MS = 12_000;
const PROBE_POLL_MS = 1_000;

/** Whether the app this browser installed is still on the phone, which only the app
 *  answering a probe shows: deleting it reports nothing. `"unknown"` where nothing could
 *  be asked, which is no reason to add a second app. */
export type InstalledApp = "answered" | "gone" | "unknown";

/** Asks the app to prove itself and waits for it. One that is there shows its own
 *  notification and calls; one that is gone is forgotten for every entry pointing at it,
 *  so each is asked to install again rather than reading as delivering to nothing. */
export const probeInstalledApp = async (): Promise<InstalledApp> => {
  const installer = await installerOfAnApp();
  if (installer === undefined) {
    return "gone";
  }
  const request = { installer_anchor_number: installer.identityNumber };
  try {
    const actor = actorAs(
      await ECDSAKeyIdentity.fromKeyPair(installer.keyPair),
      { agentOptions, canisterId },
    );
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
