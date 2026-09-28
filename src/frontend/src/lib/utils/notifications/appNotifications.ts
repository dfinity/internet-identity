/**
 * The two calls an app canister answers for a notification: the content to show, and
 * that a channel showed it.
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
  });

interface AppNotificationService {
  _internet_identity_notification_content: (
    id: bigint,
  ) => Promise<[] | [{ title: string; body: string; url: [] | [string] }]>;
  _internet_identity_notification_received: (id: bigint) => Promise<void>;
}

const actorFor = (
  canisterId: Principal,
  identity: Identity,
  host: string,
): AppNotificationService =>
  Actor.createActor<AppNotificationService>(idlFactory, {
    agent: HttpAgent.createSync({ host, identity, retryTimes: 0 }),
    canisterId,
  });

/**
 * What the app says this notification is about, or `undefined` where it says nothing:
 * the app dismissed it, it expired, or the canister refused us.
 */
export const fetchNotificationContent = async ({
  canisterId,
  id,
  identity,
  host,
}: {
  canisterId: Principal;
  id: bigint;
  identity: Identity;
  host: string;
}): Promise<NotificationContent | undefined> => {
  try {
    const answer = await actorFor(
      canisterId,
      identity,
      host,
    )._internet_identity_notification_content(id);
    const content = answer[0];
    if (content === undefined) {
      return undefined;
    }
    return { title: content.title, body: content.body, url: content.url[0] };
  } catch {
    return undefined;
  }
};

/** Tells the app a channel showed it. Nothing depends on the answer. */
export const reportNotificationReceived = async ({
  canisterId,
  id,
  identity,
  host,
}: {
  canisterId: Principal;
  id: bigint;
  identity: Identity;
  host: string;
}): Promise<void> => {
  try {
    await actorFor(
      canisterId,
      identity,
      host,
    )._internet_identity_notification_received(id);
  } catch {
    // The notification is shown either way; the app learns of it on the next one.
  }
};
