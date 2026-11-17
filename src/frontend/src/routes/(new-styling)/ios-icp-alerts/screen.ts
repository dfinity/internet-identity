import { isCanisterError } from "$lib/utils/utils";
import { browserAndSystem } from "$lib/utils/describeBrowser";
import { ensureAppKey } from "$lib/utils/notifications/notificationAppKey";
import { registerForAnyAnchor } from "$lib/utils/notifications/subscribeDevice";
import { browserAppStatus } from "$lib/utils/notifications/appStatus";
import { appAnchors } from "$lib/utils/notifications/appAnchors";
import {
  decodeLinkToken,
  type FragmentLinkToken,
  type LinkToken,
} from "$lib/utils/notifications/linkToken";
import { recordAlreadyInstalled } from "$lib/utils/notifications/alreadyInstalled";
import {
  installedAppServes,
  joinInstalledApp,
  probeInstalledApp,
} from "$lib/utils/notifications/installerKey";
import { appKeyActor, linkKeyActor } from "./appKeyActor";
import { linkWithToken, type LinkOutcome } from "./link";
import {
  readAppKey,
  rememberInstalled,
  rememberSpentFragment,
} from "$lib/utils/notifications/notificationAppKey";
import { currentDeviceSubscription } from "$lib/utils/notifications/pushSubscription";

export type Screen =
  | { kind: "working" }
  /** Not a screen: this URL carries nothing to act on, so the page leaves for the host. */
  | { kind: "elsewhere" }
  /** A browser tab, with no app on this device to point at yet. `staleKey` is a tab
   *  whose fragment the canister is no longer waiting for, so the steps need a fresh
   *  URL in the address bar before anyone follows them. */
  | { kind: "install"; replacingApp?: boolean; staleKey?: boolean }
  /** Nothing here reaches an identity, and the host is where that is fixed. */
  | { kind: "unlinked" }
  /** A browser tab the user is done with: `window.close()` did nothing, no script opened it. */
  | { kind: "closed" }
  /** A browser tab where this browser already points at an app; iOS has no link capturing,
   *  so nothing here can open it. `guessed` is the tab that was left and came back, where
   *  nothing proves the app was added at all. `probed` is a tab whose join already had
   *  the app show a notification. */
  | { kind: "open-app"; guessed: boolean; probed?: boolean }
  /** The app, permission still to ask for; iOS raises the prompt only from a gesture. */
  | { kind: "ask" }
  /** The app, with the permission refused. Only iOS Settings can lift it. */
  | { kind: "blocked" }
  /** Delivering; `inTheApp` is which document this is. `probed` is a tab whose join
   *  already had the app show a notification. */
  | { kind: "ready"; inTheApp: boolean; probed?: boolean }
  /** The app, with no identity pointing at it. */
  | { kind: "orphaned" }
  /** The app, carrying a link it could not spend: a refused token, or an unreadable fragment. */
  | { kind: "stale-link" }
  /** A browser tab, told by the user that the app is on their Home Screen, with the install still waiting for it to be opened. */
  | { kind: "open-to-finish" }
  /** A browser tab whose install nothing can finish any more: the key it handed out has expired, and the URL the Home Screen holds is frozen. */
  | { kind: "link-expired" }
  /** A browser tab handed an install by another app on this device, which only that app can renew. */
  | { kind: "handover-expired" }
  /** The app, whose own key the canister will not link; a key is minted once per storage partition, so the way on is a fresh app. */
  | { kind: "unusable-key" }
  | { kind: "failed"; reason: string };

/** `CanisterError` carries no message, so the variant is the whole of what it said. */
export const failed = (error: unknown): Screen => {
  console.error(error);
  return {
    kind: "failed",
    reason: isCanisterError(error)
      ? String(error.type)
      : error instanceof Error
        ? error.message
        : String(error),
  };
};

/** A tab opened for an install holds the placeholder until the minted URL replaces it,
 *  so it waits rather than deciding on a URL that carries no token. A Home Screen entry
 *  added from that placeholder is waiting for nothing, and launches like any other. */
export const holdsTheInstallPlaceholder = ({
  inTheApp,
  search,
}: {
  inTheApp: boolean;
  search: string;
}): boolean => !inTheApp && new URLSearchParams(search).has("installing");

/** `mintedElsewhere` is an install handed over by another browser on this device, whose
 *  entry is the one waiting for it: this browser's own entry says nothing about it. */
export const insideTheBrowser = async (
  hash: string,
  mintedElsewhere: boolean,
): Promise<Screen> => {
  const fragment = decodeLinkToken(hash);
  switch (fragment.kind) {
    // This URL exists to carry a token to an app that was added from it. Reaching it in a
    // tab without one is not a flow with anything to say, so it is not given a screen.
    case "no-token":
      return { kind: "elsewhere" };
    case "unreadable":
      console.error(fragment.reason);
      return { kind: "elsewhere" };
    case "token":
      return await followTheToken(fragment.token, mintedElsewhere);
  }
};

/** An app this browser installed is joined rather than added again, unless a probe found
 *  it gone: nothing reports a deleted Home Screen app, and joining one would
 *  leave the user with nothing installed and no steps to add it. A link nothing waits for
 *  any more is answered without asking the app, whose answer is a notification. */
const followTheToken = async (
  token: LinkToken,
  mintedElsewhere: boolean,
): Promise<Screen> => {
  if (!(await stillWaiting(token))) {
    if (!mintedElsewhere) {
      return await settleTheTab(token);
    }
    // Only the app that handed it over can renew it, so steps on a link that can no
    // longer complete would add an app that never links. A link spent by joining or
    // linking the app this browser installed leaves its identity on that app.
    return (await installedAppServes(token.identityNumber))
      ? { kind: "open-app", guessed: false }
      : { kind: "handover-expired" };
  }
  const installed = await probeInstalledApp();
  if (installed !== "gone") {
    return await joinTheApp(token, installed === "answered", mintedElsewhere);
  }
  return mintedElsewhere ? { kind: "install" } : await settleTheTab(token);
};

/** An app that could not be asked before is asked again once this identity reaches it,
 *  and one silent then is gone, so the steps are what is left. */
const joinTheApp = async (
  token: LinkToken,
  answered: boolean,
  mintedElsewhere: boolean,
): Promise<Screen> => {
  const joined = await joinInstalledApp(token);
  switch (joined.kind) {
    case "joined":
      break;
    case "gone":
      return mintedElsewhere ? { kind: "install" } : await settleTheTab(token);
    case "expired":
      return mintedElsewhere
        ? { kind: "handover-expired" }
        : await settleTheTab(token);
    case "failed":
      return { kind: "failed", reason: joined.reason };
  }
  if (answered) {
    return { kind: "ready", inTheApp: false, probed: true };
  }
  switch (await probeInstalledApp()) {
    case "answered":
      return { kind: "ready", inTheApp: false, probed: true };
    case "unknown":
      return { kind: "open-app", guessed: false, probed: true };
    case "gone":
      // The join spent the link on the app that turned out to be gone.
      return mintedElsewhere
        ? { kind: "handover-expired" }
        : { kind: "install", staleKey: true };
  }
};

/** Whether the install `token` was handed can still complete. A canister that would not
 *  answer is no reason to withhold the steps. */
const stillWaiting = async ({
  identityNumber,
  linkKey,
}: LinkToken): Promise<boolean> => {
  try {
    const actor = await linkKeyActor(linkKey);
    return await actor.notification_app_link_pending({
      anchor_number: identityNumber,
    });
  } catch (error) {
    console.error(error);
    return true;
  }
};

/** What this browser's entry says to do about the identity a readable token named. */
const settleTheTab = async (token: LinkToken): Promise<Screen> => {
  const status = await browserAppStatus(token.identityNumber);
  switch (status.kind) {
    case "unanswered":
      // Offering the install on a query that never happened would add a second app.
      return { kind: "failed", reason: status.reason };
    // An install this tab handed out and nobody has opened still has a key the canister
    // is waiting for, so the steps work as they are.
    case "awaiting-install":
      return { kind: "install" };
    // Nothing pending for this browser: either the install expired or there never was
    // one, and either way the fragment names a key no link can spend.
    case "no-app":
      return { kind: "install", staleKey: true };
    case "app":
      // This tab exists to walk the install, so what it answers is about the app being
      // there. Whether this identity wants it to deliver was recorded where the user
      // said so, and nothing here reads or writes it.
      // The record exists because the app was opened and linked, so it is on the Home
      // Screen and the permission is what is left.
      return status.app.subscribed
        ? { kind: "ready", inTheApp: false }
        : { kind: "open-app", guessed: false };
  }
};

/** `null` rather than `undefined` so that an unhandled arm is a compile error here. */
const spendTheFragment = async (
  fragment: FragmentLinkToken,
  record: Awaited<ReturnType<typeof ensureAppKey>>,
): Promise<LinkOutcome | null> => {
  switch (fragment.kind) {
    case "no-token":
      return null;
    case "unreadable":
      return { status: "token-refused", reason: fragment.reason };
    case "token":
      return await linkWithToken(record, fragment.token);
  }
};

/**
 * Whether this launch has an update call to make, from what this device wrote down.
 *
 * Local reads only: the app record, the stored endpoint, and the subscription the
 * browser holds. Nothing here subscribes and nothing reaches the network, so the page
 * can ask before it paints. Anything it cannot read answers `true`, because a launch
 * that might be about to wait should say so rather than sit on a blank screen.
 */
export const launchWillWrite = async (hash: string): Promise<boolean> => {
  try {
    const stored = await readAppKey();
    if (stored.kind !== "key" || stored.record.appId === undefined) {
      return true;
    }
    const { spentFragment } = stored.record;
    // A fragment this app has not already answered is one it will present.
    if (hash !== "" && spentFragment !== hash) {
      return true;
    }
    // A browser holding no subscription is one that will make and register one.
    return (await currentDeviceSubscription()) === undefined;
  } catch {
    return true;
  }
};

/** `onTokenSpent` runs for a token the canister took, so the page can drop it from the URL. */
export const insideTheApp = async (
  hash: string,
  permission: NotificationPermission,
  onTokenSpent: () => void,
): Promise<Screen> => {
  const record = await ensureAppKey();
  // Only this page knows it is the installed app and what it is running on. The worker
  // cannot ask: an iPad in desktop mode is a Mac to anything reading the user agent
  // alone, and `maxTouchPoints`, which tells them apart, is not on a worker's navigator.
  const { os } = browserAndSystem();
  void rememberInstalled("Ios" in os || "Ipados" in os);
  const actor = await appKeyActor(record);

  // The Home Screen entry replays the URL captured at install, key and all, so every
  // launch after the first presents a key the canister has already spent. Answering it
  // again costs an update call to be told what this app wrote down the first time.
  const replayed = record.spentFragment === hash && record.appId !== undefined;
  const outcome = replayed
    ? null
    : await spendTheFragment(decodeLinkToken(hash), record);
  if (outcome?.status === "failed") {
    // Recorded below and not here: a link that failed for a reason that may pass spent
    // nothing, and an app that already has an `appId` would read the fragment as a
    // replay on its next launch and never present it again. The identity it was adding
    // would be left unlinked with nothing to say so.
    return { kind: "failed", reason: outcome.reason };
  }
  if (!replayed && hash !== "") {
    await rememberSpentFragment(hash);
  }
  if (outcome?.status === "key-refused") {
    // Terminal, and before the anchors are read: only a fresh install gives this app another key.
    return { kind: "unusable-key" };
  }
  if (outcome?.status === "linked") {
    onTokenSpent();
  }

  // The link that just ran is ahead of what the store held when this launch began.
  const appId = outcome?.status === "linked" ? outcome.appId : record.appId;
  // No record means nothing has ever linked this app, which reads as no anchors.
  const anchors = appId === undefined ? [] : await appAnchors(actor, appId);
  if (anchors.length === 0 && outcome?.status === "token-refused") {
    return { kind: "stale-link" };
  }
  return await settle(permission, actor, anchors);
};

/** Where this launch lands, for a permission in whichever state it is in. */
const settle = async (
  permission: NotificationPermission,
  actor: Awaited<ReturnType<typeof appKeyActor>>,
  anchors: bigint[],
): Promise<Screen> => {
  if (anchors.length === 0) {
    // Nothing to register for: what is registered is named for an anchor the app serves.
    return { kind: "orphaned" };
  }
  if (permission === "denied") {
    return { kind: "blocked" };
  }
  if (permission === "default") {
    return { kind: "ask" };
  }
  // Registered on every launch and not only the one that linked: the push service hands out a new endpoint whenever it likes.
  if (!(await registerForAnyAnchor(anchors, actor))) {
    // Every identity pointing at this app has it switched off or is signed out; the app itself is intact.
    return { kind: "unlinked" };
  }
  return { kind: "ready", inTheApp: true };
};

/** Settles against the canister as the app, for a permission the page has just read. */
export const settleWithTheApp = async (
  permission: NotificationPermission,
): Promise<Screen> => {
  const record = await ensureAppKey();
  const actor = await appKeyActor(record);
  const anchors =
    record.appId === undefined ? [] : await appAnchors(actor, record.appId);
  return await settle(permission, actor, anchors);
};

/** iOS Settings is the only place a refused permission can be lifted. */
export const settleFromSettings = (): Promise<Screen> =>
  settleWithTheApp(Notification.permission);

const checkWhatTheySay = async (identityNumber: bigint): Promise<Screen> => {
  const status = await browserAppStatus(identityNumber);
  switch (status.kind) {
    case "awaiting-install":
      return { kind: "open-to-finish" };
    case "no-app":
      return { kind: "link-expired" };
    case "unanswered":
      return { kind: "failed", reason: status.reason };
    case "app":
      await recordAlreadyInstalled(identityNumber);
      return { kind: "closed" };
  }
};

/** What the user says they have is checked against what the canister sees: an app of
 *  their own is the one thing this browser cannot observe, but an install it handed out
 *  and nobody opened is, and taking their word for that one records a device as set up
 *  while nothing delivers. */
export const afterAlreadyAdded = async (hash: string): Promise<Screen> => {
  const fragment = decodeLinkToken(hash);
  switch (fragment.kind) {
    case "token":
      return await checkWhatTheySay(fragment.token.identityNumber);
    case "no-token":
    case "unreadable":
      // Nothing names an identity to record the answer against, and the tab is done either way.
      return { kind: "closed" };
  }
};
