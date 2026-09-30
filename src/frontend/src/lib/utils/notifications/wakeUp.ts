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

import { Actor, HttpAgent, type ActorSubclass } from "@icp-sdk/core/agent";
import { Principal } from "@icp-sdk/core/principal";
import { idlFactory as internetIdentityIDL } from "$lib/generated/internet_identity_idl";
import type {
  _SERVICE,
  NotificationToShow,
} from "$lib/generated/internet_identity_types";
import {
  browserKeyIdentity,
  registeredIdentityNumbers,
} from "$lib/stores/browser-key.store";
import { registrationFrom, type WorkerRegistration } from "./registrationUrl";
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

/** Shown until the delegation that reads the app's own text has been minted. */
const PLACEHOLDER_TITLE = "Internet Identity";
const PLACEHOLDER_BODY = "You have a new notification.";

export interface WakeUpContext {
  /** The worker's registration, which owns the notifications it shows. */
  registration: ServiceWorkerRegistration;
  location: { search: string };
  /** The canister, as this browser. Injected so a test can answer for it. */
  internetIdentity?: (
    identityNumber: bigint,
    worker: WorkerRegistration,
  ) => Promise<ActorSubclass<_SERVICE> | undefined>;
}

const internetIdentityActor = async (
  identityNumber: bigint,
  { canisterId, agentOptions }: WorkerRegistration,
): Promise<ActorSubclass<_SERVICE> | undefined> => {
  const identity = await browserKeyIdentity(identityNumber);
  if (identity === undefined) {
    return undefined;
  }
  return Actor.createActor<_SERVICE>(internetIdentityIDL, {
    agent: HttpAgent.createSync({ ...agentOptions, identity, retryTimes: 0 }),
    canisterId: Principal.fromText(canisterId),
  });
};

const refFor = (
  identityNumber: bigint,
  notification: NotificationToShow,
): NotificationRef => ({
  identityNumber,
  origin: notification.origin,
  accountNumber: notification.account_number[0],
  canisterId: notification.canister_id.toText(),
  id: notification.id,
});

/** Replace by hand rather than by tag: WebKit does not coalesce by tag, so a same-tag
 *  notification arrives beside the one it was meant to replace. */
const show = async (
  { registration }: WakeUpContext,
  ref: NotificationRef,
  shown: { title: string; body: string; url: string; icon?: string },
): Promise<void> => {
  const tag = tagOf(ref);
  for (const existing of await registration.getNotifications({ tag })) {
    existing.close();
  }
  await registration.showNotification(shown.title, {
    body: shown.body,
    icon: shown.icon,
    tag,
    data: dataOf(ref, shown.url),
  });
};

const closeShown = async (
  { registration }: WakeUpContext,
  ref: NotificationRef,
): Promise<void> => {
  for (const existing of await registration.getNotifications({
    tag: tagOf(ref),
  })) {
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
  {
    identityNumber,
    worker,
  }: { identityNumber: bigint; worker: WorkerRegistration },
): Promise<Queued | undefined> => {
  const actor = await (context.internetIdentity ?? internetIdentityActor)(
    identityNumber,
    worker,
  );
  if (actor === undefined) {
    return undefined;
  }
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
    worker,
    pending,
  }: {
    worker: WorkerRegistration;
    pending: Promise<unknown>[];
  },
): Promise<boolean> => {
  const ref = refFor(identityNumber, notification);
  const target = {
    origin: notification.origin,
    accountNumber: notification.account_number[0],
  };
  const held = await loadPullIdentity({
    identityNumber,
    target,
    internetIdentityCanisterId: worker.canisterId,
    nowMillis: Date.now(),
  });

  if (held === undefined) {
    // Nothing to read the content with yet. Say that something arrived, then ask for a
    // delegation — which is what wakes this worker again to replace this with the
    // app's own text.
    await show(context, ref, {
      title: PLACEHOLDER_TITLE,
      body: PLACEHOLDER_BODY,
      url: notification.origin,
    });
    sendAndForget(
      pending,
      mintPullIdentity({
        actor,
        identityNumber,
        target,
        internetIdentityCanisterId: worker.canisterId,
      }),
    );
    return true;
  }

  const content = await fetchNotificationContent({
    canisterId: notification.canister_id,
    id: notification.id,
    identity: held,
    ...worker.agentOptions,
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
      canisterId: notification.canister_id,
      id: notification.id,
      identity: held,
      ...worker.agentOptions,
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
  {
    identityNumber,
    worker,
  }: { identityNumber: bigint; worker: WorkerRegistration },
): Promise<void> => {
  const actor = await (context.internetIdentity ?? internetIdentityActor)(
    identityNumber,
    worker,
  );
  if (actor === undefined) {
    return;
  }
  await refillJwtPool({
    actor,
    identityNumber,
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
const closeDismissed = async (
  context: WakeUpContext,
  { worker }: { worker: WorkerRegistration },
): Promise<void> => {
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
        internetIdentityCanisterId: worker.canisterId,
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
          canisterId: Principal.fromText(ref.canisterId),
          id: ref.id,
          identity,
          ...worker.agentOptions,
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
  const worker = registrationFrom(context.location.search);
  if (worker === undefined) {
    // Nothing can be fetched without knowing which canister to ask.
    await context.registration.showNotification(PLACEHOLDER_TITLE, {
      body: PLACEHOLDER_BODY,
      tag: "internet-identity",
    });
    return;
  }

  const identityNumbers = await registeredIdentityNumbers();
  // One round trip each, side by side: a wake-up shows one notification, so asking
  // the identities one after another would only make the screen wait on the answers
  // it ends up throwing away.
  const queued = await Promise.all(
    identityNumbers.map((identityNumber) =>
      nextFor(context, { identityNumber, worker }).catch(() => undefined),
    ),
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
      worker,
      pending,
    }).catch(() => false);
    if (shown) {
      break;
    }
  }

  // The pool of signed wake-up authorizations is spent by elapsed time, and a
  // browser whose user never opens the page again would let it run out.
  for (const identityNumber of identityNumbers) {
    sendAndForget(pending, topUpPool(context, { identityNumber, worker }));
  }

  if (shown) {
    sendAndForget(pending, closeDismissed(context, { worker }));
  } else {
    // Whether this wake-up still owes the user something depends on what is on
    // screen, and an app may have dismissed what is on screen.
    await closeDismissed(context, { worker }).catch(() => undefined);
    if ((await context.registration.getNotifications()).length === 0) {
      // The subscription is `userVisibleOnly`: a wake-up that showed nothing and left
      // nothing on screen owes the user something.
      await context.registration.showNotification(PLACEHOLDER_TITLE, {
        body: PLACEHOLDER_BODY,
        tag: "internet-identity",
      });
    }
  }

  await Promise.all(pending);
};
