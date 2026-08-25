/// <reference types="@sveltejs/kit" />
/// <reference lib="webworker" />

// The push service worker. The browser decrypts II's sealed ping before `push`
// fires, so the payload is just the routing origin (`{"o":"<origin>"}`). The
// worker pulls the real content from the dApp as the user's per-app identity
// and renders it; anything missing (no credential, an expired one, an
// unreachable canister) falls back to a generic notification. It has no
// `fetch` handler, so it never intercepts requests on the auth origin.

import { Actor, HttpAgent, type Identity } from "@icp-sdk/core/agent";
import { IDL } from "@icp-sdk/core/candid";
import { Principal } from "@icp-sdk/core/principal";
import {
  cacheCanister,
  loadCachedCanister,
  loadNotificationCredential,
  notificationIdentity,
} from "$lib/utils/notifications/pullCredential";
import {
  reconcile,
  type PulledNotification,
} from "$lib/utils/notifications/reconcile";

const sw = self as unknown as ServiceWorkerGlobalScope;

// Re-resolve origin -> canister at most this often; a stale entry refreshes
// lazily on the next push (or immediately on a pull failure).
const CANISTER_TTL_MS = 24 * 60 * 60 * 1000;
// Bound resolve + pull so a slow dApp can't blow the push handler's budget; on
// timeout the generic notification still shows.
const PULL_TIMEOUT_MS = 8_000;

interface PullService {
  ii_pending_notifications: () => Promise<PulledNotification[]>;
}
const pullIdl: IDL.InterfaceFactory = ({ IDL }) =>
  IDL.Service({
    ii_pending_notifications: IDL.Func(
      [],
      [
        IDL.Vec(
          IDL.Record({
            id: IDL.Text,
            title: IDL.Text,
            body: IDL.Opt(IDL.Text),
          }),
        ),
      ],
      ["query"],
    ),
  });

sw.addEventListener("install", () => sw.skipWaiting());
sw.addEventListener("activate", (event) => event.waitUntil(sw.clients.claim()));

sw.addEventListener("push", (event) => {
  event.waitUntil(handlePush(event.data?.json()));
});

sw.addEventListener("notificationclick", (event) => {
  const origin = (event.notification.data as { origin?: string } | null)
    ?.origin;
  event.notification.close();
  event.waitUntil(openApp(origin));
});

sw.addEventListener("pushsubscriptionchange", (event) => {
  const change = event as ExtendableEvent & {
    oldSubscription?: PushSubscription | null;
  };
  change.waitUntil(handleSubscriptionChange(change.oldSubscription ?? null));
});

// The browser rotated or expired this device's push subscription. Re-registering
// it needs the user's II identity, which only an authenticated page holds, so the
// worker just clears the dead subscription. The next authenticated page load
// reconciles: it re-subscribes and registers the new endpoint with the canister.
const handleSubscriptionChange = async (
  old: PushSubscription | null,
): Promise<void> => {
  const stale = old ?? (await sw.registration.pushManager.getSubscription());
  await stale?.unsubscribe().catch(() => {});
};

const handlePush = async (payload: unknown): Promise<void> => {
  const origin = originOf(payload);
  const pulled =
    origin === undefined
      ? undefined
      : await withTimeout(pull(origin), PULL_TIMEOUT_MS, undefined);

  // Pull failed (no origin, no credential, timeout, unreachable): the real
  // pending set is unknown, so show a generic notification and touch nothing
  // that is already on screen.
  if (pulled === undefined) {
    await sw.registration.showNotification(appName(origin), {
      body: "You have a new notification.",
      icon: "/favicon.svg",
      tag: "ii-notification",
      data: { origin },
    });
    return;
  }

  // Pull succeeded: the returned set is authoritative for this origin.
  await reconcile(sw.registration, origin as string, pulled);
};

const originOf = (payload: unknown): string | undefined => {
  if (payload !== null && typeof payload === "object" && "o" in payload) {
    const origin = (payload as { o: unknown }).o;
    return typeof origin === "string" ? origin : undefined;
  }
  return undefined;
};

const appName = (origin: string | undefined): string => {
  if (origin === undefined) return "Internet Identity";
  try {
    return new URL(origin).host;
  } catch {
    return "Internet Identity";
  }
};

// Returns the dApp's pending set (possibly empty) on success, or `undefined`
// when the pull can't be made: no credential, an expired one, an unresolvable
// or unreachable canister. The caller treats `undefined` as "state unknown" and
// only reconciles on a real answer.
const pull = async (
  origin: string,
): Promise<PulledNotification[] | undefined> => {
  const record = await loadNotificationCredential(origin);
  if (record === undefined || record.expiresAtMillis <= Date.now()) {
    return undefined;
  }
  const identity = await notificationIdentity(record);
  const canisterId = await resolveCanister(origin, false);
  if (canisterId === undefined) {
    return undefined;
  }
  try {
    return await pullFrom(canisterId, identity, record.host);
  } catch {
    // The cached canister may be stale (the dApp changed its id). Re-resolve
    // from the well-known and retry once, but only if it actually moved.
    const fresh = await resolveCanister(origin, true);
    if (fresh === undefined || fresh.toText() === canisterId.toText()) {
      return undefined;
    }
    return pullFrom(fresh, identity, record.host).catch(() => undefined);
  }
};

const pullFrom = async (
  canisterId: Principal,
  identity: Identity,
  host: string,
): Promise<PulledNotification[]> => {
  const agent = await HttpAgent.create({
    identity,
    host,
    shouldFetchRootKey: isLocalHost(host),
  });
  const actor = Actor.createActor<PullService>(pullIdl, { agent, canisterId });
  return actor.ii_pending_notifications();
};

// The canister comes from the consented origin's own well-known, never from the
// ping (a forged ping can only name an origin II already sealed for). It's cached
// and reused within a TTL; a stale entry is re-resolved here or on a pull failure.
const resolveCanister = async (
  origin: string,
  forceRefresh: boolean,
): Promise<Principal | undefined> => {
  if (!forceRefresh) {
    const cached = await loadCachedCanister(origin);
    if (
      cached !== undefined &&
      cached.resolvedAtMillis + CANISTER_TTL_MS > Date.now()
    ) {
      return principalOf(cached.canisterId);
    }
  }
  const fetched = await fetchSenderCanister(origin);
  if (fetched !== undefined) {
    await cacheCanister(origin, fetched.toText());
  }
  return fetched;
};

const fetchSenderCanister = async (
  origin: string,
): Promise<Principal | undefined> => {
  try {
    const response = await fetch(
      `${origin}/.well-known/ii-notification-senders`,
    );
    if (!response.ok) return undefined;
    const doc: unknown = await response.json();
    const senders =
      doc !== null && typeof doc === "object" && "senders" in doc
        ? (doc as { senders: unknown }).senders
        : undefined;
    const first = Array.isArray(senders) ? senders[0] : undefined;
    return typeof first === "string" ? principalOf(first) : undefined;
  } catch {
    return undefined;
  }
};

const principalOf = (text: string): Principal | undefined => {
  try {
    return Principal.fromText(text);
  } catch {
    return undefined;
  }
};

// Resolves to `fallback` if `promise` rejects or outlives `ms`. A slow or
// failing pull must never leave the push handler without a notification.
const withTimeout = <T>(
  promise: Promise<T>,
  ms: number,
  fallback: T,
): Promise<T> =>
  Promise.race([
    promise.catch(() => fallback),
    new Promise<T>((resolve) => setTimeout(() => resolve(fallback), ms)),
  ]);

const isLocalHost = (host: string): boolean =>
  host.includes("localhost") || host.includes("127.0.0.1");

const openApp = async (origin: string | undefined): Promise<void> => {
  const url = origin ?? "/";
  const windows = await sw.clients.matchAll({ type: "window" });
  const existing = windows.find((client) => client.url.startsWith(url));
  if (existing !== undefined) {
    await existing.focus();
    return;
  }
  await sw.clients.openWindow(url);
};
