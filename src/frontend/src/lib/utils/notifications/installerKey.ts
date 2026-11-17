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
import { isCanisterError } from "$lib/utils/utils";
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
  /** Every identity this browser showed the steps for or joined, newest first. The
   *  canister finds the app through an identity pointing at it, and any one of these may
   *  have stopped while another still does. */
  identityNumbers?: bigint[];
}

/** Serialises every write: two tabs making the key at once would leave the app naming
 *  one this browser no longer holds. */
const exclusively = async <T>(run: () => Promise<T>): Promise<T> => {
  const locks = navigator.locks;
  if (locks === undefined) {
    return run();
  }
  return await locks.request("ii-notification-installer-key", run);
};

/** Rejects where the store cannot be read: an unreadable key is not an absent one, and
 *  making a new one would orphan the app that names the old. */
const readInstallerKey = (): Promise<InstallerKey | undefined> =>
  idbGet<InstallerKey>(INSTALLER_KEY_ENTRY, INSTALLER_KEY_STORE);

const writeInstallerKey = (installerKey: InstallerKey): Promise<void> =>
  idbSet(INSTALLER_KEY_ENTRY, installerKey, INSTALLER_KEY_STORE);

/** Puts `identityNumber` first among the identities the key is looked up through,
 *  making the key where this browser has none. */
const rememberIdentity = (identityNumber: bigint): Promise<InstallerKey> =>
  exclusively(async () => {
    const kept = await readInstallerKey();
    const keyPair =
      kept?.keyPair ??
      ((await crypto.subtle.generateKey(
        { name: "ECDSA", namedCurve: "P-256" },
        false,
        ["sign", "verify"],
      )) as CryptoKeyPair);
    const others = (kept?.identityNumbers ?? []).filter(
      (known) => known !== identityNumber,
    );
    if (kept !== undefined && kept.identityNumbers?.[0] === identityNumber) {
      return kept;
    }
    const installerKey = {
      keyPair,
      identityNumbers: [identityNumber, ...others],
    };
    await writeInstallerKey(installerKey);
    return installerKey;
  });

const publicKeyOf = async (installerKey: InstallerKey): Promise<Uint8Array> =>
  new Uint8Array(
    await crypto.subtle.exportKey("spki", installerKey.keyPair.publicKey),
  );

/** The public half to name in the URL whose steps this browser is about to show, made on
 *  first use. Rejects where the key cannot be read or kept, since steps without it add
 *  an app no later install can find. */
export const installerKeyForSteps = async (
  identityNumber: bigint,
): Promise<Uint8Array> =>
  await publicKeyOf(await rememberIdentity(identityNumber));

interface Installer {
  keyPair: CryptoKeyPair;
  /** An identity pointing at the app, which is what the canister finds it through. */
  identityNumber: bigint;
  /** When the app last called. */
  lastSeen: bigint;
}

/** The installer key and an identity the canister finds its app through, where this
 *  browser installed an app somebody still points at. */
const installerOfAnApp = async (): Promise<Installer | undefined> => {
  const installerKey = await readInstallerKey();
  if (installerKey === undefined) {
    return undefined;
  }
  const actor = actorAs(
    await ECDSAKeyIdentity.fromKeyPair(installerKey.keyPair),
    {
      agentOptions,
      canisterId,
    },
  );
  for (const identityNumber of installerKey.identityNumbers ?? []) {
    const lastSeen = (
      await actor.installed_notification_app_last_seen({
        installer_anchor_number: identityNumber,
      })
    )[0];
    if (lastSeen !== undefined) {
      return { keyPair: installerKey.keyPair, identityNumber, lastSeen };
    }
  }
  return undefined;
};

/** Whether `identityNumber` points at the app this browser installed. `false` where that
 *  cannot be read. */
export const installedAppServes = async (
  identityNumber: bigint,
): Promise<boolean> => {
  try {
    const installerKey = await readInstallerKey();
    if (installerKey === undefined) {
      return false;
    }
    const actor = actorAs(
      await ECDSAKeyIdentity.fromKeyPair(installerKey.keyPair),
      { agentOptions, canisterId },
    );
    const lastSeen = await actor.installed_notification_app_last_seen({
      installer_anchor_number: identityNumber,
    });
    return lastSeen[0] !== undefined;
  } catch (error) {
    console.error(error);
    return false;
  }
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

/** What a join came to. `gone` is no app for it to join, `expired` a link nothing waits
 *  on any more. */
export type JoinOutcome =
  | { kind: "joined" }
  | { kind: "gone" }
  | { kind: "expired" }
  | { kind: "failed"; reason: string };

/** Signs the join for `caller` with the installer key and makes it as `actor`. */
const joinAs = async ({
  installer,
  identityNumber,
  caller,
  actor,
}: {
  installer: Installer;
  identityNumber: bigint;
  caller: SignIdentity;
  actor: ActorSubclass<_SERVICE>;
}): Promise<JoinOutcome> => {
  try {
    const signature = await crypto.subtle.sign(
      { name: "ECDSA", hash: "SHA-256" },
      installer.keyPair.privateKey,
      joinMessage(identityNumber, caller.getPrincipal().toUint8Array()),
    );
    const joined = await actor.join_notification_app({
      anchor_number: identityNumber,
      installer_anchor_number: installer.identityNumber,
      installer_key: await publicKeyOf(installer),
      signature: new Uint8Array(signature),
    });
    if ("Err" in joined) {
      const error = joined.Err;
      if ("NoNotificationApp" in error) {
        return { kind: "gone" };
      }
      if ("NoPendingLink" in error) {
        return { kind: "expired" };
      }
      if ("InvalidInstallerSignature" in error) {
        return { kind: "failed", reason: "InvalidInstallerSignature" };
      }
      return { kind: "failed", reason: error.InternalCanisterError };
    }
  } catch (error) {
    console.error(error);
    return { kind: "failed", reason: describe(error) };
  }
  // The canister has joined it whatever happens here: this list only orders the lookup.
  await rememberIdentity(identityNumber).catch((error: unknown) => {
    console.error(error);
  });
  return { kind: "joined" };
};

/** Points the identity the install `token` was handed out for at the app this browser
 *  installed before. */
export const joinInstalledApp = async ({
  identityNumber,
  linkKey,
}: LinkToken): Promise<JoinOutcome> => {
  try {
    const installer = await installerOfAnApp();
    if (installer === undefined) {
      return { kind: "gone" };
    }
    const [caller, actor] = await Promise.all([
      importLinkKey(linkKey).then((keyPair) =>
        ECDSAKeyIdentity.fromKeyPair(keyPair),
      ),
      linkKeyActorWith(linkKey, { agentOptions, canisterId }),
    ]);
    return await joinAs({ installer, identityNumber, caller, actor });
  } catch (error) {
    console.error(error);
    return { kind: "failed", reason: describe(error) };
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
  try {
    const installer = await installerOfAnApp();
    if (installer === undefined) {
      return false;
    }
    const caller = await browserKeyIdentity(identityNumber);
    if (caller === undefined) {
      return false;
    }
    const joined = await joinAs({
      installer,
      identityNumber,
      caller,
      actor: await browserKeyActor(identityNumber),
    });
    return joined.kind === "joined";
  } catch (error) {
    console.error(error);
    return false;
  }
};

/** Longer than the canister's window for an answer, so an app it would call gone has had all of it. */
const PROBE_ANSWER_WAIT_MS = 12_000;
/** The canister's window opens when the push is posted, which can be a while after it is
 *  asked, so retiring is retried until this long after the ask. */
const PROBE_GIVE_UP_MS = 45_000;
const PROBE_POLL_MS = 1_000;

/** Whether the app this browser installed is still on the phone, which only the app
 *  answering a probe shows: deleting it reports nothing. `"unknown"` where nothing could
 *  be asked, which is no reason to add a second app. */
export type InstalledApp = "answered" | "gone" | "unknown";

/** Asks the app to prove itself and waits for it. One that is there shows its own
 *  notification and calls; one that is gone is forgotten for every entry pointing at it,
 *  so each is asked to install again rather than reading as delivering to nothing. */
export const probeInstalledApp = async (): Promise<InstalledApp> => {
  try {
    const installer = await installerOfAnApp();
    if (installer === undefined) {
      return "gone";
    }
    const request = { installer_anchor_number: installer.identityNumber };
    const actor = actorAs(
      await ECDSAKeyIdentity.fromKeyPair(installer.keyPair),
      { agentOptions, canisterId },
    );
    const before = installer.lastSeen;
    const probe = await actor.probe_installed_notification_app(request);
    if ("Err" in probe) {
      return "NoNotificationApp" in probe.Err ? "gone" : "unknown";
    }
    const askedAt = Date.now();
    while (Date.now() < askedAt + PROBE_GIVE_UP_MS) {
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
      if (Date.now() < askedAt + PROBE_ANSWER_WAIT_MS) {
        continue;
      }
      const retired = await actor.retire_notification_app(request);
      if ("Ok" in retired || "NoNotificationApp" in retired.Err) {
        return "gone";
      }
      if (!("AppStillAnswering" in retired.Err)) {
        return "unknown";
      }
    }
    return "unknown";
  } catch (error) {
    console.error(error);
    return "unknown";
  }
};
