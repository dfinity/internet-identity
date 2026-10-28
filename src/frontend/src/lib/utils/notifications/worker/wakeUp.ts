/**
 * What the service worker does with a Web Push wake-up.
 *
 * A wake-up carries nothing: the canister queues one per notification and the worker
 * asks what it is for. Showing one takes a delegation Internet Identity signs for that
 * app and account, so the first wake-up for a pair shows a placeholder and mints one,
 * which earns another wake-up that replaces the placeholder with the app's own text.
 *
 * Everything here is driven by the worker's `push` handler; it is a module of its own
 * so it can be read, and tested, without a worker around it.
 */

import {
  Actor,
  HttpAgent,
  type ActorSubclass,
  type Identity,
} from "@icp-sdk/core/agent";
import { Principal } from "@icp-sdk/core/principal";
import { idlFactory as internetIdentityIDL } from "$lib/generated/internet_identity_idl";
import type {
  _SERVICE,
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
import { refillJwtPool } from "./poolRefill";
import { loadPullIdentity, mintPullIdentity } from "./pullDelegation";
import {
  dataOf,
  refOf,
  tagOf,
  type NotificationRef,
} from "./shownNotification";
import { recordShown, shownRecently } from "./shownRecord";
import { identityNotificationText } from "./identityNotification";
import { NOTIFICATION_APP_PATH } from "../appPath";

/** Measured on a phone, the mint took 3.25 seconds; a shorter deadline spends the placeholder on every ordinary notification.
 *  Eight seconds is well inside the budget a push handler gets. */
const MINT_DEADLINE_MS = 8_000;

/** What {@link withDeadline} rejects with, so a caller can tell the deadline running out from the work failing. */
class DeadlineError extends Error {
  constructor(deadlineMs: number) {
    super(`no answer within ${deadlineMs}ms`);
  }
}

/** Rejects where `work` has not answered within `deadlineMs`; the work itself carries on. */
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
  /** The worker's registration, which owns the notifications it shows. */
  registration: ServiceWorkerRegistration;
  /** What this storage can sign for. Injected so a test can answer for it. */
  signing?: () => Promise<SigningIdentity[]>;
  /** The canister, as one of them. Injected so a test can answer for it. */
  internetIdentity?: (
    signing: SigningIdentity,
  ) => Promise<ActorSubclass<_SERVICE>>;
}

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

/** The id names one entry of one inbox, which is all an acknowledgement needs. */
const forgetNamed = (
  actor: ActorSubclass<_SERVICE>,
  named: WakeUpNaming,
): Promise<unknown> =>
  actor.browser_remove_notification({
    anchor_number: named.anchorNumber,
    browser_id: named.browserId,
    inbox_notification_id: named.inboxNotificationId,
  });

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
    data: dataOf(ref, shown.url),
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

/**
 * Update calls the wake-up sets going without waiting for them.
 *
 * Nothing on screen depends on their answers, and a notification that waits for one
 * before it is shown is shown a consensus round late, which is late enough for iOS to
 * hold the push against the worker. They are still settled before the handler returns,
 * so the `waitUntil` around the wake-up keeps the worker alive for them.
 */
const sendAndForget = (
  pending: Promise<unknown>[],
  call: Promise<unknown>,
): void => {
  pending.push(call.catch(() => undefined));
};

interface Queued {
  actor: ActorSubclass<_SERVICE>;
  identityNumber: bigint;
  notification: NotificationToShow;
  named: WakeUpNaming;
}

/** What a wake-up names: the identity and browser whose inbox holds the entry, and the
 *  name that inbox gave it. */
export interface WakeUpNaming {
  anchorNumber: bigint;
  browserId: number;
  inboxNotificationId: bigint;
}

/** What the entry a wake-up names turns out to be. Reads only: a query, and nothing the
 *  canister remembers having answered. */
const namedFor = async (
  context: WakeUpContext,
  signing: SigningIdentity,
  named: WakeUpNaming,
): Promise<Queued | undefined> => {
  const actor = await (context.internetIdentity ?? internetIdentityActor)(
    signing,
  );
  const found = await actor.browser_get_notification({
    anchor_number: named.anchorNumber,
    browser_id: named.browserId,
    inbox_notification_id: named.inboxNotificationId,
  });
  if ("Err" in found) {
    return undefined;
  }
  const notification = found.Ok.notification[0];
  if (notification === undefined) {
    return undefined;
  }
  return { actor, identityNumber: signing.identityNumber, notification, named };
};

/**
 * Show what an identity has waiting, and tell the canister and the app it was shown.
 *
 * Answers whether anything was shown, which is what a `userVisibleOnly` subscription
 * obliges the worker to have done by the time it returns. Everything the answer does
 * not depend on is left in `pending`.
 */
const showQueued = async (
  context: WakeUpContext,
  { actor, identityNumber, notification, named }: Queued,
  {
    pending,
  }: {
    pending: Promise<unknown>[];
  },
): Promise<boolean> => {
  const ref = refFor(identityNumber, notification);

  // Internet Identity's own: no delegation to mint and no app to ask, which is also why it cannot spend a placeholder.
  const [event] = notification.event;
  if (event !== undefined) {
    await show(context, ref, {
      ...identityNotificationText(event),
      // Opens this app rather than anywhere else: what it is about is here.
      url: NOTIFICATION_APP_PATH,
    });
    sendAndForget(pending, forgetNamed(actor, named));
    return true;
  }

  // Only Internet Identity's own carries no origin, and that returned just above.
  const [origin] = notification.origin;
  if (origin === undefined) {
    return false;
  }

  const target = {
    origin,
    accountNumber: notification.account_number[0],
  };
  const held = await loadPullIdentity({
    identityNumber,
    target,

    nowMillis: Date.now(),
  });

  // Minting is an update call, raced against a deadline rather than waited for: a worker that shows nothing is one the browser answers for.
  // The promise is kept so the placeholder path can go on waiting for this same mint rather than starting a second one.
  let minting: Promise<Identity | undefined> | undefined;
  const mintWithin = (): Promise<Identity | undefined> => {
    minting = mintPullIdentity({ actor, identityNumber, target });
    sendAndForget(pending, minting);
    return withDeadline(minting, MINT_DEADLINE_MS).catch((error: unknown) => {
      console.error(
        error instanceof DeadlineError
          ? `the delegation for ${target.origin} was still being minted after ${MINT_DEADLINE_MS}ms`
          : error,
      );
      return undefined;
    });
  };
  const minted = held ?? (await mintWithin());

  /** Spends the entry this wake-up was for. */
  const forget = (): void => sendAndForget(pending, forgetNamed(actor, named));

  /** Shows what the app published under this entry, or closes what it has dropped. */
  const showFromApp = async (identity: Identity): Promise<boolean> => {
    const content = await fetchNotificationContent({
      appCanisterId: notification.canister_id,
      id: notification.id,
      identity,
    });
    if (content === undefined) {
      // The app dismissed it, or it expired: there is nothing to show and nothing left
      // for the canister to hold.
      await closeShown(context, ref);
      forget();
      return false;
    }

    const [sender, alternativeOrigins] = await Promise.all([
      senderOf(origin),
      fetchAlternativeOrigins(origin),
    ]);
    await show(context, ref, {
      // The sender leads the title: an app's own title and body are its text to
      // write, so the one line naming who sent it has to be somewhere the app
      // cannot claim and a platform showing a single body line cannot drop.
      title: `${sender.name} · ${content.title}`,
      body: content.body,
      icon: sender.icon,
      url: allowedLink({
        url: content.url,
        origin,
        alternativeOrigins,
      }),
    });

    sendAndForget(
      pending,
      reportNotificationReceived({
        appCanisterId: notification.canister_id,
        id: notification.id,
        identity,
      }),
    );
    forget();
    return true;
  };

  if (minted === undefined) {
    // What the subscription owes the user, now rather than whenever the mint lands.
    await show(context, ref, {
      title: PLACEHOLDER_TITLE,
      body: PLACEHOLDER_BODY,
      url: origin,
    });
    // The mint is still in flight, and `waitUntil` keeps this worker alive for it, so
    // the rest of the work happens here under the same wake-up. The canister does post
    // one more wake-up when a delegation is minted, but a browser gets only one of
    // those per delegation lifetime and it is not returned when its post fails, so
    // resting correctness on it is what leaves the queue behind the wake-ups.
    const late = await (minting ?? Promise.resolve(undefined)).catch(
      () => undefined,
    );
    if (late === undefined) {
      // A wake-up that leaves its entry queued puts the queue permanently one behind,
      // which costs every notification after this one. One banner carrying no more
      // than the placeholder is the cheaper loss.
      forget();
      return true;
    }
    // Replaces the placeholder: `show` closes this entry's own tag first.
    await showFromApp(late);
    return true;
  }

  return await showFromApp(minted);
};

/** Signs a fresh pool of wake-up authorizations where the stored one is running out.
 *  Nothing depends on it: the notification is already shown. */
const topUpPool = async (
  context: WakeUpContext,
  signing: SigningIdentity,
): Promise<void> => {
  const actor = await (context.internetIdentity ?? internetIdentityActor)(
    signing,
  );
  await refillJwtPool({
    actor,
    identityNumber: signing.identityNumber,
    nowNs: BigInt(Date.now()) * BigInt(1_000_000),
  });
};

/**
 * Close what the apps have dismissed since it was shown.
 *
 * An app drops a notification's content when it no longer needs anyone's attention,
 * and a pull then answers with nothing. That is the same answer as for something
 * expired, and the same thing to do about it.
 */
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

/** One wake-up: show what arrived, and clear what no longer matters. */
export const onWakeUp = async (
  context: WakeUpContext,
  /** Absent where the wake-up carried nothing, which leaves nothing to ask for. */
  named: WakeUpNaming | undefined,
): Promise<void> => {
  const signing = await (context.signing ?? signingIdentities)();

  // Swept before anything is shown, and never after. Chrome reconciles its own
  // record of what is on screen when the registration is enumerated, and an
  // enumeration that lands before the browser has displayed a notification it was
  // just given drops that notification from the record while the system goes on
  // showing it. What is left is a banner whose click reaches nothing.
  await closeDismissed(context).catch(() => undefined);

  const pending: Promise<unknown>[] = [];
  let shown = false;
  // The wake-up names the identity whose inbox holds the entry, so one of these keys
  // signs for it and one call asks. A browser keeps a key per identity signed in; a
  // Home Screen app's single key is paired with every identity that claimed it.
  const signer =
    named === undefined
      ? undefined
      : signing.find((one) => one.identityNumber === named.anchorNumber);
  if (named !== undefined && signer !== undefined) {
    const entry = await namedFor(context, signer, named).catch(() => undefined);
    if (entry !== undefined) {
      shown = await showQueued(context, entry, { pending }).catch(() => false);
    }
  }

  // The pool of signed wake-up authorizations is spent by elapsed time, and a
  // browser whose user never opens the page again would let it run out.
  for (const one of signing) {
    sendAndForget(pending, topUpPool(context, one));
  }

  if (!shown) {
    // Whether this wake-up still owes the user something depends on what is on
    // screen. Nothing was shown under this wake-up, so enumerating here has nothing
    // of its own to drop.
    // The browser is asked first; our own stamp covers iOS before 17.0, which answered an empty array.
    const onScreen =
      (await context.registration.getNotifications()).length > 0 ||
      (await shownRecently());
    if (!onScreen) {
      // The subscription is `userVisibleOnly`: a wake-up that showed nothing and left
      // nothing on screen owes the user something.
      await showPlaceholder(context.registration);
    }
  }

  await Promise.all(pending);
};
