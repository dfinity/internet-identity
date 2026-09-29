/**
 * The calls an app canister answers for a notification: the content to show, that a
 * channel showed it, and that somebody acted on it.
 *
 * The interface is declared here rather than generated, because it is the app's and
 * all of it is these two methods. `_internet_identity_notification_content` is a
 * query, so an app whose language cannot read caller info in a query cannot serve it
 * yet.
 */

import { Actor, HttpAgent, type Identity } from "@icp-sdk/core/agent";
import { IDL } from "@icp-sdk/core/candid";
import type { Principal } from "@icp-sdk/core/principal";

export interface NotificationContent {
  title: string;
  body: string;
  /** Where acting on it takes the user, if the app named anywhere. */
  url?: string;
}

/**
 * One call to one app canister.
 *
 * `shouldFetchRootKey` is the deployment's, and arrives by way of the worker's own
 * registration script URL — a woken worker has no page to ask and cannot import the
 * page's agent, which is bootstrapped from injected config. It is not derived from the
 * location either: the e2e serves Internet Identity on a public name against a local
 * replica, so "fetch it only on a loopback host" would be wrong there. Without it a
 * local replica's answers fail verification, which reads the same as an app that
 * dismissed the notification.
 */
export interface AppCall {
  canisterId: Principal;
  id: bigint;
  identity: Identity;
  host: string;
  shouldFetchRootKey: boolean;
}

const Content = IDL.Record({
  title: IDL.Text,
  body: IDL.Text,
  url: IDL.Opt(IDL.Text),
});

const idlFactory = () =>
  IDL.Service({
    _internet_identity_notification_content: IDL.Func(
      [IDL.Nat64],
      [IDL.Opt(Content)],
      ["query"],
    ),
    _internet_identity_notification_received: IDL.Func([IDL.Nat64], [], []),
    _internet_identity_notification_opened: IDL.Func([IDL.Nat64], [], []),
  });

interface AppNotificationService {
  _internet_identity_notification_content: (
    id: bigint,
  ) => Promise<[] | [{ title: string; body: string; url: [] | [string] }]>;
  _internet_identity_notification_received: (id: bigint) => Promise<void>;
  _internet_identity_notification_opened: (id: bigint) => Promise<void>;
}

const actorFor = ({
  canisterId,
  identity,
  host,
  shouldFetchRootKey,
}: AppCall): AppNotificationService =>
  Actor.createActor<AppNotificationService>(idlFactory, {
    agent: HttpAgent.createSync({
      host,
      identity,
      shouldFetchRootKey,
      retryTimes: 0,
    }),
    canisterId,
  });

/**
 * What the app says this notification is about, or `undefined` where the app itself
 * says there is nothing: it dismissed the notification, or the notification expired.
 *
 * Throws where the app could not be asked. A caller acts on `undefined` by dropping
 * the notification, which cannot be undone, so a call that never reached the app must
 * not look like an answer from it.
 */
export const fetchNotificationContent = async (
  call: AppCall,
): Promise<NotificationContent | undefined> => {
  const answer = await actorFor(call)._internet_identity_notification_content(
    call.id,
  );
  const content = answer[0];
  if (content === undefined) {
    return undefined;
  }
  return { title: content.title, body: content.body, url: content.url[0] };
};

/** Tells the app a channel showed it. Nothing depends on the answer: the entry leaves
 *  the browser's queue either way, so that an app which cannot answer cannot hold on
 *  to a place in it. An app that was unreachable counts the notification among the
 *  ones it never saw shown. */
export const reportNotificationReceived = async (
  call: AppCall,
): Promise<void> => {
  try {
    await actorFor(call)._internet_identity_notification_received(call.id);
  } catch {
    // Nothing to do about it here.
  }
};

/** Tells the app somebody acted on it. The app's window is already opening. */
export const reportNotificationOpened = async (
  call: AppCall,
): Promise<void> => {
  try {
    await actorFor(call)._internet_identity_notification_opened(call.id);
  } catch {
    // Acting on a notification must not depend on the app being reachable.
  }
};
