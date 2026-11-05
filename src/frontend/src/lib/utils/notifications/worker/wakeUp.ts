/**
 * What the service worker does with a Web Push wake-up.
 *
 * A wake-up names one entry of one browser's inbox. Showing an app's own text takes a
 * delegation Internet Identity signs for that app and account, so a worker holding none
 * shows a placeholder and replaces it once the mint lands.
 */

import {
  Actor,
  HttpAgent,
  RejectError,
  type ActorSubclass,
  type Identity,
} from "@icp-sdk/core/agent";
import { Principal } from "@icp-sdk/core/principal";
import { idlFactory as internetIdentityIDL } from "$lib/generated/internet_identity_idl";
import type {
  _SERVICE,
  NotificationContentFailure,
  NotificationEvent,
  NotificationToShow,
} from "$lib/generated/internet_identity_types";
import { signingIdentities, type SigningIdentity } from "../signingIdentities";
import { config } from "../workerConfig";
import { fetchAppMetadata, logoAsDataUrl } from "$lib/utils/appMetadata";
import {
  fetchNotificationContent,
  reportNotificationReceived,
} from "./appNotifications";
import { allowedLink, fetchAlternativeOrigins } from "./notificationLink";
import { loadPullIdentity, mintPullIdentity } from "./pullDelegation";
import { dataOf, refOf, tagOf, type NotificationRef } from "./notificationRef";
import { isTheApp } from "./openTarget";
import { recordShown, shownRecently } from "./recentlyShown";
import { identityNotificationText } from "./identityNotification";
import { NOTIFICATION_APP_PATH } from "../appPath";

/** Measured on a phone, the mint took 3.25 seconds; a shorter deadline spends the placeholder on every ordinary notification.
 *  Eight seconds is well inside the budget a push handler gets. */
const MINT_DEADLINE_MS = 8_000;

/** The sweep asks every app that has something on screen whether it still wants it, and
 *  one that cannot answer must not hold up a notification from another. Closing a stale
 *  banner late costs nothing, so this is short. */
const SWEEP_DEADLINE_MS = 2_000;

class DeadlineError extends Error {
  constructor(deadlineMs: number) {
    super(`no answer within ${deadlineMs}ms`);
  }
}

/** The work itself carries on. */
const withDeadline = <T>(work: Promise<T>, deadlineMs: number): Promise<T> =>
  Promise.race([
    work,
    new Promise<never>((_, reject) =>
      setTimeout(() => reject(new DeadlineError(deadlineMs)), deadlineMs),
    ),
  ]);

/** Shown until the delegation that reads the app's own text has been minted. */
const PLACEHOLDER_TITLE = "Internet Identity";
const PLACEHOLDER_TAG = "internet-identity";
const PLACEHOLDER_BODY = "You have a new notification.";

/** What a `userVisibleOnly` subscription owes the user where nothing better can be
 *  shown. */
export const showPlaceholder = async (
  registration: ServiceWorkerRegistration,
): Promise<void> => {
  await closeUnder(registration, [PLACEHOLDER_TAG]);
  await registration.showNotification(PLACEHOLDER_TITLE, {
    body: PLACEHOLDER_BODY,
    tag: PLACEHOLDER_TAG,
  });
  await recordShown();
};

export interface WakeUpContext {
  /** Owns the notifications it shows. */
  registration: ServiceWorkerRegistration;
  // Both injected so a test can answer for them.
  signing?: () => Promise<SigningIdentity[]>;
  internetIdentity?: (
    signing: SigningIdentity,
  ) => Promise<ActorSubclass<_SERVICE>>;
}

/** What the sealed payload carries, which is all a woken worker has to go on. */
export interface InboxEntryId {
  anchorNumber: bigint;
  browserId: number;
  inboxNotificationId: bigint;
}

/** What the push carried, or `undefined` where it carried nothing this worker can use. */
export const entryIdFrom = (
  data: PushMessageData | null,
): InboxEntryId | undefined => {
  try {
    const payload: unknown = data?.json();
    if (payload === null || typeof payload !== "object") {
      return undefined;
    }
    const { anchor_number, browser_id, inbox_notification_id } =
      payload as Record<string, unknown>;
    if (
      typeof anchor_number !== "number" ||
      typeof browser_id !== "number" ||
      typeof inbox_notification_id !== "number"
    ) {
      return undefined;
    }
    return {
      anchorNumber: BigInt(anchor_number),
      browserId: browser_id,
      inboxNotificationId: BigInt(inbox_notification_id),
    };
  } catch {
    return undefined;
  }
};

/** An entry, as the canister answered for it. */
interface InboxEntry {
  actor: ActorSubclass<_SERVICE>;
  identityNumber: bigint;
  notification: NotificationToShow;
  id: InboxEntryId;
}

/** Nothing on screen waits on these: a notification shown a consensus round late is one
 *  iOS holds against the worker. Settled before the handler returns. */
const backgroundCalls = () => {
  const started: Promise<unknown>[] = [];
  return {
    start: (call: Promise<unknown>): void => {
      started.push(call.catch(() => undefined));
    },
    settled: (): Promise<unknown> => Promise.all(started),
  };
};

type BackgroundCalls = ReturnType<typeof backgroundCalls>;

const internetIdentityActor = ({
  identity,
}: SigningIdentity): Promise<ActorSubclass<_SERVICE>> =>
  Promise.resolve(
    Actor.createActor<_SERVICE>(internetIdentityIDL, {
      agent: HttpAgent.createSync({
        ...config.agentOptions,
        identity,
        retryTimes: 0,
      }),
      canisterId: Principal.fromText(config.canisterId),
    }),
  );

/** `failure` says how the app failed to give the content, so Internet Identity counts
 *  the notification as lost rather than shown. */
const removeEntry = (
  actor: ActorSubclass<_SERVICE>,
  id: InboxEntryId,
  failure?: NotificationContentFailure,
): Promise<unknown> =>
  actor.browser_remove_notification({
    anchor_number: id.anchorNumber,
    browser_id: id.browserId,
    inbox_notification_id: id.inboxNotificationId,
    content_failure: failure === undefined ? [] : [failure],
  });

/** A canister that answered with a reject or a trap rejected the call; anything else
 *  never got an answer. */
const contentFailureOf = (error: unknown): NotificationContentFailure =>
  error instanceof RejectError ? { Rejected: null } : { Unreachable: null };

/** A query: nothing the canister remembers having answered. */
const fetchEntry = async (
  context: WakeUpContext,
  signing: SigningIdentity,
  id: InboxEntryId,
): Promise<InboxEntry | undefined> => {
  const actor = await (context.internetIdentity ?? internetIdentityActor)(
    signing,
  );
  const found = await actor.browser_get_notification({
    anchor_number: id.anchorNumber,
    browser_id: id.browserId,
    inbox_notification_id: id.inboxNotificationId,
  });
  if ("Err" in found) {
    return undefined;
  }
  const notification = found.Ok.notification[0];
  if (notification === undefined) {
    return undefined;
  }
  return { actor, identityNumber: signing.identityNumber, notification, id };
};

const refFor = (
  identityNumber: bigint,
  notification: NotificationToShow,
): NotificationRef => ({
  identityNumber,
  // The worker's own key for a shown notification. Internet Identity's own names no
  // app, which an empty origin stands for here and nowhere else.
  origin: notification.origin[0] ?? "",
  accountNumber: notification.account_number[0],
  appCanisterId: notification.canister_id.toText(),
  id: notification.id,
});

/** iOS ignores `tag` entirely, so replacing a notification means closing it by hand, and the tags are compared here rather than by the browser. */
const closeUnder = async (
  registration: ServiceWorkerRegistration,
  tags: string[],
): Promise<void> => {
  for (const existing of await registration.getNotifications()) {
    if (tags.includes(existing.tag)) {
      existing.close();
    }
  }
};

const show = async (
  { registration }: WakeUpContext,
  ref: NotificationRef,
  shown: { title: string; body: string; url: string; icon?: string },
): Promise<void> => {
  const tag = tagOf(ref);
  // The placeholder carries a tag of its own, which nothing else here ever closes.
  await closeUnder(registration, [tag, PLACEHOLDER_TAG]);
  await registration.showNotification(shown.title, {
    body: shown.body,
    icon: shown.icon,
    tag,
    data: dataOf(ref, shown.url, await isTheApp()),
  });
  await recordShown();
};

const closeShown = (
  { registration }: WakeUpContext,
  ref: NotificationRef,
): Promise<void> => closeUnder(registration, [tagOf(ref)]);

/** Who the notification is from, as the app publishes it, falling back to the host
 *  name, which is the part no app can claim for itself. */
const senderOf = async (
  origin: string,
): Promise<{ name: string; icon?: string }> => {
  const hostname = new URL(origin).hostname;
  const metadata = await fetchAppMetadata(origin, logoAsDataUrl).catch(
    () => undefined,
  );
  return { name: metadata?.name ?? hostname, icon: metadata?.logo };
};

/** Internet Identity's own: the words are written here, so there is nothing to mint. */
const showIdentityEvent = async (
  context: WakeUpContext,
  entry: InboxEntry,
  ref: NotificationRef,
  event: NotificationEvent,
  background: BackgroundCalls,
): Promise<boolean> => {
  await show(context, ref, {
    ...identityNotificationText(event),
    url: NOTIFICATION_APP_PATH,
  });
  background.start(removeEntry(entry.actor, entry.id));
  return true;
};

/** What the app published under this entry, or closing what it has dropped. */
const showPublished = async (
  context: WakeUpContext,
  entry: InboxEntry,
  ref: NotificationRef,
  origin: string,
  identity: Identity,
  background: BackgroundCalls,
): Promise<boolean> => {
  const { notification } = entry;
  let content;
  try {
    content = await fetchNotificationContent({
      appCanisterId: notification.canister_id,
      id: notification.id,
      identity,
    });
  } catch (error) {
    // The app said nothing about what is on screen, so that stays. The entry goes with
    // the reason, since no later wake-up names it again.
    background.start(
      removeEntry(entry.actor, entry.id, contentFailureOf(error)),
    );
    throw error;
  }
  if (content === undefined) {
    // The app dismissed it, or it expired: there is nothing to show and nothing left
    // for the canister to hold.
    await closeShown(context, ref);
    background.start(removeEntry(entry.actor, entry.id));
    return false;
  }

  const [sender, alternativeOrigins] = await Promise.all([
    senderOf(origin),
    fetchAlternativeOrigins(origin),
  ]);
  await show(context, ref, {
    // The sender leads the title: an app's own title and body are its text to write,
    // so the one line naming who sent it has to be somewhere the app cannot claim and
    // a platform showing a single body line cannot drop.
    title: `${sender.name} · ${content.title}`,
    body: content.body,
    icon: sender.icon,
    url: allowedLink({ url: content.url, origin, alternativeOrigins }),
  });

  background.start(
    reportNotificationReceived({
      appCanisterId: notification.canister_id,
      id: notification.id,
      identity,
    }),
  );
  background.start(removeEntry(entry.actor, entry.id));
  return true;
};

/** Minting is raced against a deadline, not waited for: a worker that shows nothing is
 *  one the browser answers for. */
const showAppNotification = async (
  context: WakeUpContext,
  entry: InboxEntry,
  ref: NotificationRef,
  origin: string,
  background: BackgroundCalls,
): Promise<boolean> => {
  const target = {
    origin,
    accountNumber: entry.notification.account_number[0],
  };
  const held = await loadPullIdentity({
    identityNumber: entry.identityNumber,
    target,
    nowMillis: Date.now(),
  });
  if (held !== undefined) {
    return await showPublished(context, entry, ref, origin, held, background);
  }

  const minting = mintPullIdentity({
    actor: entry.actor,
    identityNumber: entry.identityNumber,
    target,
  });
  background.start(minting);
  const minted = await withDeadline(minting, MINT_DEADLINE_MS).catch(
    (error: unknown) => {
      console.error(
        error instanceof DeadlineError
          ? `the delegation for ${origin} was still being minted after ${MINT_DEADLINE_MS}ms`
          : error,
      );
      return undefined;
    },
  );
  if (minted !== undefined) {
    return await showPublished(context, entry, ref, origin, minted, background);
  }

  await show(context, ref, {
    title: PLACEHOLDER_TITLE,
    body: PLACEHOLDER_BODY,
    url: origin,
  });
  // Finished under this wake-up rather than the one the canister posts when a
  // delegation is minted: a browser gets one of those per delegation lifetime, and a
  // failed post is not returned.
  const late = await minting.catch(() => undefined);
  if (late === undefined) {
    // Spent anyway: an entry left behind puts the inbox one behind for good.
    background.start(removeEntry(entry.actor, entry.id));
    return true;
  }
  await showPublished(context, entry, ref, origin, late, background);
  return true;
};

/** Answers whether anything was shown, which `userVisibleOnly` obliges the worker to
 *  have done by the time it returns. */
const showEntry = async (
  context: WakeUpContext,
  entry: InboxEntry,
  background: BackgroundCalls,
): Promise<boolean> => {
  const ref = refFor(entry.identityNumber, entry.notification);

  const [event] = entry.notification.event;
  if (event !== undefined) {
    return await showIdentityEvent(context, entry, ref, event, background);
  }

  // Only Internet Identity's own carries no origin, and that returned just above.
  const [origin] = entry.notification.origin;
  if (origin === undefined) {
    return false;
  }
  return await showAppNotification(context, entry, ref, origin, background);
};

/** An app drops a notification's content when it no longer needs attention, which reads
 *  the same as expired and is the same thing to do about. */
const closeDismissed = async (context: WakeUpContext): Promise<void> => {
  const shown = await context.registration.getNotifications();
  await Promise.all(
    shown.map(async (one) => {
      const ref = refOf(one.data);
      if (ref === undefined) {
        return;
      }
      const identity = await loadPullIdentity({
        identityNumber: ref.identityNumber,
        target: { origin: ref.origin, accountNumber: ref.accountNumber },

        nowMillis: Date.now(),
      });
      if (identity === undefined) {
        return;
      }
      // Only the app's own answer closes anything: an app that could not be asked
      // has said nothing about what is on screen.
      let content;
      try {
        content = await fetchNotificationContent({
          appCanisterId: Principal.fromText(ref.appCanisterId),
          id: ref.id,
          identity,
        });
      } catch {
        return;
      }
      if (content === undefined) {
        one.close();
      }
    }),
  );
};

/** One wake-up: show what it named, and clear what no longer matters. */
export const onWakeUp = async (
  context: WakeUpContext,
  /** Absent where the wake-up carried nothing, which leaves nothing to ask for. */
  id: InboxEntryId | undefined,
): Promise<void> => {
  const signing = await (context.signing ?? signingIdentities)();

  // Swept before anything is shown, and never after. Chrome reconciles its own
  // record of what is on screen when the registration is enumerated, and an
  // enumeration that lands before the browser has displayed a notification it was
  // just given drops that notification from the record while the system goes on
  // showing it. What is left is a banner whose click reaches nothing.
  // Enumerated before anything is shown, for the reason above. The answers it waits on
  // are the apps', so they are bounded: past the deadline the sweep goes on closing what
  // it finds while this wake-up gets on with what it was sent for.
  await withDeadline(closeDismissed(context), SWEEP_DEADLINE_MS).catch(
    () => undefined,
  );

  const background = backgroundCalls();
  let shown = false;
  // A browser keeps a key per identity signed in; a Home Screen app's single key is
  // paired with every identity that claimed it.
  const signer =
    id === undefined
      ? undefined
      : signing.find((one) => one.identityNumber === id.anchorNumber);
  if (id !== undefined && signer !== undefined) {
    const entry = await fetchEntry(context, signer, id).catch(() => undefined);
    if (entry !== undefined) {
      shown = await showEntry(context, entry, background).catch(() => false);
    }
  }

  if (!shown) {
    // Our own stamp covers iOS before 17.0, which answered an empty array.
    const onScreen =
      (await context.registration.getNotifications()).length > 0 ||
      (await shownRecently());
    if (!onScreen) {
      // `userVisibleOnly`: a wake-up that left nothing on screen owes the user something.
      await showPlaceholder(context.registration);
    }
  }

  await background.settled();
};
