/// <reference types="@sveltejs/kit" />
/// <reference lib="webworker" />

// The push service worker. The browser decrypts II's sealed ping before `push`
// fires, so the payload is just the routing origin (`{"o":"<origin>"}`). The
// worker pulls the real content from the dApp as the user's per-app identity and
// renders it. The origin's well-known can authorize several sender canisters, and
// any of them may own content, so the worker pulls each one and reconciles per
// canister; anything missing (no credential, an expired one, an unreachable
// canister) falls back to a generic notification. It has no `fetch` handler, so
// it never intercepts requests on the auth origin.

import { Actor, HttpAgent, type Identity } from "@icp-sdk/core/agent";
import { IDL } from "@icp-sdk/core/candid";
import { Principal } from "@icp-sdk/core/principal";
import {
  addDismissed,
  cacheCanisters,
  loadCachedCanisters,
  loadDismissed,
  loadNotificationCredential,
  notificationIdentity,
  setDismissed,
} from "$lib/utils/notifications/pullCredential";
import {
  reconcile,
  type CanisterPull,
  type PulledNotification,
} from "$lib/utils/notifications/reconcile";
import { MAX_SENDERS, parseSenders } from "$lib/utils/notifications/senders";

const sw = self as unknown as ServiceWorkerGlobalScope;

// Re-resolve origin -> senders at most this often; a stale entry refreshes
// lazily on the next push (or immediately on a pull failure).
const CANISTER_TTL_MS = 24 * 60 * 60 * 1000;
// Bound each canister pull so a slow dApp can't blow the push handler's budget;
// on timeout the canister is treated as unknown and its notifications are kept.
const PULL_TIMEOUT_MS = 8_000;

// The dApp side of `ii_pending_notifications` (see the dApp interface in the
// notification client): `Notification { id: blob; title; body; url; created_at }`.
interface WirePending {
  id: Uint8Array | number[];
  title: string;
  body: string;
  url: [] | [string];
  created_at: bigint;
}
interface PullService {
  ii_pending_notifications: () => Promise<WirePending[]>;
}
const pullIdl: IDL.InterfaceFactory = ({ IDL }) =>
  IDL.Service({
    ii_pending_notifications: IDL.Func(
      [],
      [
        IDL.Vec(
          IDL.Record({
            id: IDL.Vec(IDL.Nat8),
            title: IDL.Text,
            body: IDL.Text,
            url: IDL.Opt(IDL.Text),
            created_at: IDL.Nat64,
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
  const data = event.notification.data as {
    origin?: string;
    url?: string;
  } | null;
  const tag = event.notification.tag;
  event.notification.close();
  event.waitUntil(handleClick(data?.origin, data?.url, tag));
});

// A dismissed notification is not re-shown, so opening one has to record the
// dismissal too, not just close it.
sw.addEventListener("notificationclose", (event) => {
  const origin = (event.notification.data as { origin?: string } | null)
    ?.origin;
  event.waitUntil(rememberDismissed(origin, event.notification.tag));
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
  const results = origin === undefined ? undefined : await pull(origin);

  // No sender answered (no origin, no credential, every sender unreachable or
  // timed out): the real pending set is unknown, so show a generic notification
  // and touch nothing already on screen.
  const anyKnown = (results ?? []).some(
    (result) => result.pulled !== undefined,
  );
  if (!anyKnown) {
    await sw.registration.showNotification(appName(origin), {
      body: "You have a new notification.",
      icon: "/favicon.svg",
      tag: "ii-notification",
      data: { origin },
    });
    return;
  }

  // At least one sender answered: reconcile per canister. Unknown senders in the
  // set are left alone by `reconcile`, which also skips anything the user has
  // dismissed and hands back the dismissals to forget.
  const knownOrigin = origin as string;
  const dismissed = new Set(await loadDismissed(knownOrigin));
  const forget = await reconcile(
    sw.registration,
    knownOrigin,
    results as CanisterPull[],
    dismissed,
  );
  if (forget.length > 0) {
    for (const tag of forget) dismissed.delete(tag);
    await setDismissed(knownOrigin, [...dismissed]);
  }
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

// Returns one result per authorized sender canister, or `undefined` when the
// pull can't be attempted at all: no credential, an expired one, or an origin
// whose well-known names no valid sender. Each result carries that canister's
// pending set, or `undefined` if it could not be reached; the caller reconciles
// the answers and leaves the unknowns alone.
const pull = async (origin: string): Promise<CanisterPull[] | undefined> => {
  const record = await loadNotificationCredential(origin);
  if (record === undefined || record.expiresAtMillis <= Date.now()) {
    return undefined;
  }
  const identity = await notificationIdentity(record);

  const cached = await resolveCanisters(origin, false);
  const canisters =
    cached.length > 0 ? cached : await resolveCanisters(origin, true);
  if (canisters.length === 0) {
    return undefined;
  }

  const results = await pullAll(canisters, identity, record.host);

  // A cached sender that failed may mean the well-known moved (a sender added,
  // removed, or replaced). Re-resolve once; if the set changed, pull the fresh
  // set and close anything a now-absent sender still has on screen.
  const usedCache = cached.length > 0;
  if (usedCache && results.some((result) => result.pulled === undefined)) {
    const fresh = await resolveCanisters(origin, true);
    if (!sameCanisters(fresh, canisters)) {
      const freshResults = await pullAll(fresh, identity, record.host);
      const removed: CanisterPull[] = canisters
        .filter((was) => !fresh.some((now) => now.toText() === was.toText()))
        .map((gone) => ({ canister: gone.toText(), pulled: [] }));
      return [...freshResults, ...removed];
    }
  }
  return results;
};

// Pulls every canister concurrently, each bounded by its own timeout. A pull that
// rejects or outlives the timeout becomes `undefined` (unknown) for that canister
// without affecting the others.
const pullAll = (
  canisters: Principal[],
  identity: Identity,
  host: string,
): Promise<CanisterPull[]> =>
  Promise.all(
    canisters.map(async (canister) => ({
      canister: canister.toText(),
      pulled: await withTimeout<PulledNotification[] | undefined>(
        pullFrom(canister, identity, host),
        PULL_TIMEOUT_MS,
        undefined,
      ),
    })),
  );

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
  const pending = await actor.ii_pending_notifications();
  return pending.map((notification) => ({
    id: toHex(notification.id),
    title: notification.title,
    body: notification.body,
    url: notification.url[0],
  }));
};

// The dApp's blob id rendered as hex, so it can key the tag, the shown-set, and
// the dismissed-set as a string.
const toHex = (bytes: Uint8Array | number[]): string =>
  Array.from(bytes, (byte) => byte.toString(16).padStart(2, "0")).join("");

// The senders come from the consented origin's own well-known, never from the
// ping (a forged ping can only name an origin II already sealed for). They're
// cached and reused within a TTL; a stale set is re-resolved here or on a pull
// failure.
const resolveCanisters = async (
  origin: string,
  forceRefresh: boolean,
): Promise<Principal[]> => {
  if (!forceRefresh) {
    const cached = await loadCachedCanisters(origin);
    if (
      cached !== undefined &&
      cached.resolvedAtMillis + CANISTER_TTL_MS > Date.now()
    ) {
      return cached.canisterIds
        .map(principalOf)
        .filter((canister): canister is Principal => canister !== undefined);
    }
  }
  const fetched = await fetchSenderCanisters(origin);
  if (fetched.length > 0) {
    await cacheCanisters(
      origin,
      fetched.map((canister) => canister.toText()),
    );
  }
  return fetched;
};

// The stored origin is canonicalized to the legacy gateway so the principal
// stays stable, but that gateway does not serve every canister, so the same
// canister is tried on the others before giving up.
const GATEWAYS = [".ic0.app", ".icp0.io", ".icp.net"];

const originCandidates = (origin: string): string[] => {
  const host = origin.startsWith("https://")
    ? origin.slice("https://".length)
    : undefined;
  if (host === undefined || host.includes("/")) return [origin];
  const gateway = GATEWAYS.find((g) => host.endsWith(g));
  if (gateway === undefined) return [origin];
  const subdomain = host.slice(0, -gateway.length);
  if (subdomain === "") return [origin];
  return [
    origin,
    ...GATEWAYS.map((g) => `https://${subdomain}${g}`).filter(
      (candidate) => candidate !== origin,
    ),
  ];
};

const fetchSendersDoc = async (origin: string): Promise<Principal[]> => {
  const response = await fetch(`${origin}/.well-known/ii-notification-senders`);
  if (!response.ok) return [];
  const doc: unknown = await response.json();
  return parseSenders(doc, MAX_SENDERS)
    .map(principalOf)
    .filter((canister): canister is Principal => canister !== undefined);
};

const fetchSenderCanisters = async (origin: string): Promise<Principal[]> => {
  for (const candidate of originCandidates(origin)) {
    try {
      const senders = await fetchSendersDoc(candidate);
      if (senders.length > 0) return senders;
    } catch {
      // Try the next gateway.
    }
  }
  return [];
};

const sameCanisters = (a: Principal[], b: Principal[]): boolean => {
  if (a.length !== b.length) return false;
  const texts = new Set(a.map((canister) => canister.toText()));
  return b.every((canister) => texts.has(canister.toText()));
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

const handleClick = async (
  origin: string | undefined,
  url: string | undefined,
  tag: string,
): Promise<void> => {
  await rememberDismissed(origin, tag);
  await openApp(origin, url);
};

// Records a dismissal for the notifications the reconcile loop tracks. The
// generic fallback (tag `ii-notification`, no canister prefix) is not tracked,
// so it is left out.
const rememberDismissed = async (
  origin: string | undefined,
  tag: string,
): Promise<void> => {
  if (origin !== undefined && tag.includes(" ")) {
    await addDismissed(origin, tag);
  }
};

const openApp = async (
  origin: string | undefined,
  url?: string,
): Promise<void> => {
  const base = origin ?? "/";
  // Only follow a deep link that stays within the consented origin; the url
  // comes from pulled content and must not navigate off it.
  const target =
    url !== undefined && origin !== undefined && url.startsWith(origin)
      ? url
      : base;
  const windows = await sw.clients.matchAll({ type: "window" });
  const existing = windows.find((client) => client.url.startsWith(base));
  if (existing !== undefined) {
    await existing.focus();
    return;
  }
  await sw.clients.openWindow(target);
};
