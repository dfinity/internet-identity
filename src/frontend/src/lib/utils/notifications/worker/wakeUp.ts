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
import {
  identityNotificationText,
  NOTIFICATION_APP_PATH,
} from "./identityNotification";

/**
 * How long a wake-up waits for the delegation that reads the app's own text before it
 * shows the placeholder instead.
 *
 * Measured on a phone, the mint took 3.25 seconds: an update call, the query that
 * fetches its chain, and a radio that has just been woken. A deadline under that spends
 * the placeholder on every ordinary notification, and each one then has to be replaced
 * by the wake-up the mint earns — two notifications' worth of work, and two on screen
 * for as long as the replacement takes.
 *
 * Eight seconds is well inside the budget a push handler gets, which is tens of seconds,
 * and leaves the placeholder for a network that genuinely cannot answer.
 */
const MINT_DEADLINE_MS = 8_000;

/** Rejects where `work` has not answered within `deadlineMs`; the work carries on, and
 *  what it earns is a wake-up that finds the delegation already there. */
const withDeadline = <T>(work: Promise<T>, deadlineMs: number): Promise<T> =>
  Promise.race([
    work,
    new Promise<never>((_, reject) =>
      setTimeout(() => reject(new Error("deadline")), deadlineMs),
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
  for (const existing of await shownUnder(registration, PLACEHOLDER_TAG)) {
    existing.close();
  }
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

const refFor = (
  identityNumber: bigint,
  notification: NotificationToShow,
): NotificationRef => ({
  identityNumber,
  origin: notification.origin,
  accountNumber: notification.account_number[0],
  appCanisterId: notification.canister_id.toText(),
  id: notification.id,
});

/**
 * What is on screen under `tag`, matched here rather than by the browser.
 *
 * iOS ignores `tag` entirely — a same-tag notification arrives beside the one it was
 * meant to replace rather than updating it — so replacing means closing by hand. The
 * filtered form of the query is the one engines disagree about, so the tag is compared
 * here instead.
 */
const shownUnder = async (
  registration: ServiceWorkerRegistration,
  tag: string,
): Promise<Notification[]> =>
  (await registration.getNotifications()).filter(
    (existing) => existing.tag === tag,
  );

const show = async (
  { registration }: WakeUpContext,
  ref: NotificationRef,
  shown: { title: string; body: string; url: string; icon?: string },
): Promise<void> => {
  const tag = tagOf(ref);
  for (const existing of await shownUnder(registration, tag)) {
    existing.close();
  }
  await registration.showNotification(shown.title, {
    body: shown.body,
    icon: shown.icon,
    tag,
    data: dataOf(ref, shown.url),
  });
  await recordShown();
};

const closeShown = async (
  { registration }: WakeUpContext,
  ref: NotificationRef,
): Promise<void> => {
  for (const existing of await shownUnder(registration, tagOf(ref))) {
    existing.close();
  }
};

/** Who the notification is from, as the app publishes it, falling back to the host
 *  name — which is the part no app can claim for itself. */
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
 * before it is shown is shown a consensus round late — late enough for iOS to hold the
 * push against the worker. They are still settled before the handler returns, so the
 * `waitUntil` around the wake-up keeps the worker alive for them.
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
}

/** What one identity has at the head of its queue. Reads only: a query, and nothing
 *  the canister remembers having answered. */
const nextFor = async (
  context: WakeUpContext,
  signing: SigningIdentity,
): Promise<Queued | undefined> => {
  const { identityNumber } = signing;
  const actor = await (context.internetIdentity ?? internetIdentityActor)(
    signing,
  );
  const next = await actor.browser_get_next_notification({
    anchor_number: identityNumber,
  });
  if ("Err" in next) {
    return undefined;
  }
  const notification = next.Ok.notification[0];
  if (notification === undefined) {
    return undefined;
  }
  return { actor, identityNumber, notification };
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
  { actor, identityNumber, notification }: Queued,
  {
    pending,
  }: {
    pending: Promise<unknown>[];
  },
): Promise<boolean> => {
  const ref = refFor(identityNumber, notification);

  // Internet Identity's own. It carries what it has to say, so there is no delegation to
  // mint and no app to ask: the worker already signs to this canister, and the words are
  // this bundle's. Shown at once, which is also why it cannot spend a placeholder.
  const [event] = notification.event;
  if (event !== undefined) {
    await show(context, ref, {
      ...identityNotificationText(event),
      // Opens this app rather than anywhere else: what it is about is here.
      url: NOTIFICATION_APP_PATH,
    });
    sendAndForget(
      pending,
      actor.browser_remove_notification({
        anchor_number: identityNumber,
        notification,
      }),
    );
    return true;
  }

  const target = {
    origin: notification.origin,
    accountNumber: notification.account_number[0],
  };
  const held = await loadPullIdentity({
    identityNumber,
    target,

    nowMillis: Date.now(),
  });

  // Minting is an update call, so it is raced against a deadline rather than waited
  // for: a worker that shows nothing is one the browser answers for, with its own
  // "site updated in the background" or, on iOS, by holding the silence against the
  // subscription. Inside the deadline the user sees the app's own text once; outside
  // it they see the placeholder, and the wake-up the mint earns replaces it.
  //
  // The mint goes into `pending` as well as into the race, because what is raced is
  // only how long the screen waits. Past the deadline the mint still has a delegation
  // to cache, and a worker the browser is free to kill never writes it: the next
  // wake-up then mints again and spends another placeholder on it.
  const mintWithin = (): Promise<Identity | undefined> => {
    const mint = mintPullIdentity({ actor, identityNumber, target });
    sendAndForget(pending, mint);
    return withDeadline(mint, MINT_DEADLINE_MS).catch(() => undefined);
  };
  const minted = held ?? (await mintWithin());

  if (minted === undefined) {
    await show(context, ref, {
      title: PLACEHOLDER_TITLE,
      body: PLACEHOLDER_BODY,
      url: notification.origin,
    });
    return true;
  }

  const content = await fetchNotificationContent({
    appCanisterId: notification.canister_id,
    id: notification.id,
    identity: minted,
  });
  if (content === undefined) {
    // The app dismissed it, or it expired: there is nothing to show and nothing left
    // for the canister to hold.
    await closeShown(context, ref);
    sendAndForget(
      pending,
      actor.browser_remove_notification({
        anchor_number: identityNumber,
        notification,
      }),
    );
    return false;
  }

  const [sender, alternativeOrigins] = await Promise.all([
    senderOf(notification.origin),
    fetchAlternativeOrigins(notification.origin),
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
      origin: notification.origin,
      alternativeOrigins,
    }),
  });

  sendAndForget(
    pending,
    reportNotificationReceived({
      appCanisterId: notification.canister_id,
      id: notification.id,
      identity: minted,
    }),
  );
  sendAndForget(
    pending,
    actor.browser_remove_notification({
      anchor_number: identityNumber,
      notification,
    }),
  );
  return true;
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
 * and a pull then answers with nothing — the same answer as for something expired, and
 * the same thing to do about it.
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
export const onWakeUp = async (context: WakeUpContext): Promise<void> => {
  const signing = await (context.signing ?? signingIdentities)();
  // One round trip each, side by side: a wake-up shows one notification, so asking
  // the entries one after another would only make the screen wait on the answers it
  // ends up throwing away.
  const queued = await Promise.all(
    signing.map((one) => nextFor(context, one).catch(() => undefined)),
  );

  const pending: Promise<unknown>[] = [];
  let shown = false;
  for (const entry of queued) {
    if (entry === undefined) {
      continue;
    }
    // The canister queues a wake-up per notification, so the ones passed over here
    // keep their place and their own wake-up is still to come.
    shown = await showQueued(context, entry, {
      pending,
    }).catch(() => false);
    if (shown) {
      break;
    }
  }

  // The pool of signed wake-up authorizations is spent by elapsed time, and a
  // browser whose user never opens the page again would let it run out.
  for (const one of signing) {
    sendAndForget(pending, topUpPool(context, one));
  }

  if (shown) {
    sendAndForget(pending, closeDismissed(context));
  } else {
    // Whether this wake-up still owes the user something depends on what is on
    // screen, and an app may have dismissed what is on screen.
    await closeDismissed(context).catch(() => undefined);
    // The browser is asked first, since it knows the live screen. Our own stamp covers
    // what it cannot: it answered an empty array on iOS before 17.0, and a wake-up that
    // believed the screen empty put a placeholder beside what was already there.
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
