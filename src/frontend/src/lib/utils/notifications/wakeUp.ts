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
import { agentHost, type AgentLocation } from "$lib/utils/agentHost";
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

/**
 * Runs what it is given one at a time, in the order it was given them.
 *
 * `waitUntil` keeps a push handler alive but does not serialise handlers: two pushes
 * arriving together would both take the same queue head, and the second wake-up would
 * be spent showing what the first already showed instead of the entry behind it. A
 * failed wake-up does not hold up the next one.
 */
export const sequencer = (): ((run: () => Promise<void>) => Promise<void>) => {
  let last: Promise<void> = Promise.resolve();
  return (run) => {
    last = last.catch(() => undefined).then(run);
    return last;
  };
};

/** What the page knows and a woken worker cannot ask for: which canister this is, and
 *  whether the deployment fetches the root key. Both ride on the script URL the
 *  registration keeps. */
export interface WorkerDeployment {
  canisterId: string;
  shouldFetchRootKey: boolean;
}

export const deploymentOf = (search: string): WorkerDeployment | undefined => {
  const params = new URLSearchParams(search);
  const canisterId = params.get("canisterId");
  if (canisterId === null) {
    return undefined;
  }
  return { canisterId, shouldFetchRootKey: params.get("fetchRootKey") === "1" };
};

export interface WakeUpContext {
  /** The worker's registration, which owns the notifications it shows. */
  registration: ServiceWorkerRegistration;
  location: AgentLocation & { search: string };
  /** The canister, as this browser. Injected so a test can answer for it. */
  internetIdentity?: (
    identityNumber: bigint,
    deployment: WorkerDeployment,
    host: string,
  ) => Promise<ActorSubclass<_SERVICE> | undefined>;
}

const internetIdentityActor = async (
  identityNumber: bigint,
  { canisterId, shouldFetchRootKey }: WorkerDeployment,
  host: string,
): Promise<ActorSubclass<_SERVICE> | undefined> => {
  const identity = await browserKeyIdentity(identityNumber);
  if (identity === undefined) {
    return undefined;
  }
  return Actor.createActor<_SERVICE>(internetIdentityIDL, {
    agent: HttpAgent.createSync({
      host,
      identity,
      shouldFetchRootKey,
      retryTimes: 0,
    }),
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
 * Show what this identity has waiting, and tell the canister and the app it was shown.
 *
 * Answers whether anything was shown, which is what a `userVisibleOnly` subscription
 * obliges the worker to have done by the time it returns.
 */
const showNextFor = async (
  context: WakeUpContext,
  {
    identityNumber,
    deployment,
    host,
  }: { identityNumber: bigint; deployment: WorkerDeployment; host: string },
): Promise<boolean> => {
  const actor = await (context.internetIdentity ?? internetIdentityActor)(
    identityNumber,
    deployment,
    host,
  );
  if (actor === undefined) {
    return false;
  }
  const next = await actor.browser_get_next_notification({
    anchor_number: identityNumber,
  });
  if ("Err" in next) {
    return false;
  }
  const notification = next.Ok.notification[0];
  if (notification === undefined) {
    return false;
  }

  const ref = refFor(identityNumber, notification);
  const target = {
    origin: notification.origin,
    accountNumber: notification.account_number[0],
  };
  const held = await loadPullIdentity({
    identityNumber,
    target,
    internetIdentityCanisterId: deployment.canisterId,
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
    await mintPullIdentity({
      actor,
      identityNumber,
      target,
      internetIdentityCanisterId: deployment.canisterId,
    });
    return true;
  }

  const content = await fetchNotificationContent({
    canisterId: notification.canister_id,
    id: notification.id,
    identity: held,
    host,
    shouldFetchRootKey: deployment.shouldFetchRootKey,
  });
  if (content === undefined) {
    // The app dismissed it, or it expired: there is nothing to show and nothing left
    // for the canister to hold.
    await closeShown(context, ref);
    await actor.browser_remove_notification({
      anchor_number: identityNumber,
      notification,
    });
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

  await reportNotificationReceived({
    canisterId: notification.canister_id,
    id: notification.id,
    identity: held,
    host,
    shouldFetchRootKey: deployment.shouldFetchRootKey,
  });
  await actor.browser_remove_notification({
    anchor_number: identityNumber,
    notification,
  });
  return true;
};

/** Signs a fresh pool of wake-up authorizations where the stored one is running out.
 *  Nothing depends on it: the notification is already shown. */
const topUpPool = async (
  context: WakeUpContext,
  {
    identityNumber,
    deployment,
    host,
  }: { identityNumber: bigint; deployment: WorkerDeployment; host: string },
): Promise<void> => {
  const actor = await (context.internetIdentity ?? internetIdentityActor)(
    identityNumber,
    deployment,
    host,
  );
  if (actor === undefined) {
    return;
  }
  await refillJwtPool({
    actor,
    identityNumber,
    nowNs: BigInt(Date.now()) * BigInt(1_000_000),
  }).catch(() => false);
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
  { deployment, host }: { deployment: WorkerDeployment; host: string },
): Promise<void> => {
  for (const shown of await context.registration.getNotifications()) {
    const ref = refOf(shown.data);
    if (ref === undefined) {
      continue;
    }
    const identity = await loadPullIdentity({
      identityNumber: ref.identityNumber,
      target: { origin: ref.origin, accountNumber: ref.accountNumber },
      internetIdentityCanisterId: deployment.canisterId,
      nowMillis: Date.now(),
    });
    if (identity === undefined) {
      continue;
    }
    const content = await fetchNotificationContent({
      canisterId: Principal.fromText(ref.canisterId),
      id: ref.id,
      identity,
      host,
      shouldFetchRootKey: deployment.shouldFetchRootKey,
    });
    if (content === undefined) {
      shown.close();
    }
  }
};

/** One wake-up: show what arrived, and clear what no longer matters. */
export const onWakeUp = async (context: WakeUpContext): Promise<void> => {
  const deployment = deploymentOf(context.location.search);
  const host = agentHost(context.location);
  if (deployment === undefined) {
    // Nothing can be fetched without knowing which canister to ask.
    await context.registration.showNotification(PLACEHOLDER_TITLE, {
      body: PLACEHOLDER_BODY,
      tag: "internet-identity",
    });
    return;
  }

  let shown = false;
  for (const identityNumber of await registeredIdentityNumbers()) {
    shown =
      (await showNextFor(context, { identityNumber, deployment, host }).catch(
        () => false,
      )) || shown;
    // The pool of signed wake-up authorizations is spent by elapsed time, and a
    // browser whose user never opens the page again would let it run out.
    await topUpPool(context, { identityNumber, deployment, host });
  }
  await closeDismissed(context, { deployment, host }).catch(() => undefined);

  if (!shown && (await context.registration.getNotifications()).length === 0) {
    // The subscription is `userVisibleOnly`: a wake-up that showed nothing and left
    // nothing on screen owes the user something.
    await context.registration.showNotification(PLACEHOLDER_TITLE, {
      body: PLACEHOLDER_BODY,
      tag: "internet-identity",
    });
  }
};
