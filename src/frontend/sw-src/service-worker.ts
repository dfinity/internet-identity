// Source for II's Web Push service worker. Bundled by esbuild into
// static/service-worker.js (see scripts/build-sw.mjs) so it can pull in
// agent-js — the worker now authenticates to the sending dApp and fetches the
// notification content itself, rather than receiving it in the push.
//
// The push carries only routing: {c, o}, and nothing that varies per message
// (so II can cache the sealed bytes). `c` is the dApp canister to query, `o` is
// the origin (the notification label, and the key the delegation is stored
// under). The worker loads a read-only delegation for `o`, queries `c`'s
// ii_pending_notifications as the user, and renders the result — deduping by
// each item's id via the notification tag. If it can't (no delegation, expired,
// dApp slow or down), it shows a generic notification so the permission isn't
// wasted on a silent push.

import { get as idbGet, createStore } from "idb-keyval";
import { Actor, HttpAgent } from "@icp-sdk/core/agent";
import {
  DelegationChain,
  DelegationIdentity,
  ECDSAKeyIdentity,
} from "@icp-sdk/core/identity";

declare const self: ServiceWorkerGlobalScope;

// Wire contract with the app side (push-delegation.store.ts): same DB and store
// names, keyed by origin.
const PUSH_DELEGATION_STORE = createStore("ii-push-delegations", "byOrigin");

interface PushDelegationRecord {
  origin: string;
  identityNumber: bigint;
  keyPair: CryptoKeyPair;
  chainJson: string;
  expiresAtMillis: number;
}

interface Notification {
  id: string;
  title: string;
  body: string;
  url: [] | [string];
  created_at: bigint;
}

// Minimal candid for the dApp-side pull contract. Kept in step with
// docs/ii-notifications-dapp.did.
const notificationIdl = ({ IDL }: { IDL: typeof import("@icp-sdk/core/candid").IDL }) => {
  const Notification = IDL.Record({
    id: IDL.Text,
    title: IDL.Text,
    body: IDL.Text,
    url: IDL.Opt(IDL.Text),
    created_at: IDL.Nat64,
  });
  return IDL.Service({
    ii_pending_notifications: IDL.Func([], [IDL.Vec(Notification)], ["query"]),
    ii_notifications_delivered: IDL.Func([IDL.Vec(IDL.Text)], [], []),
  });
};

const ACTION_MANAGE = "ii-manage";
const SETTINGS_PATH = "/manage/settings";

const DEMO_NAMES: Record<string, string> = {
  "friendship-thinkpad-parents-usd.trycloudflare.com": "MULTI/DEX",
  "frontend.local.localhost:8000": "MULTI/DEX",
  "multidex.ai": "MULTI/DEX",
};

const senderLabel = (origin: string): string => {
  let host: string;
  try {
    const u = new URL(origin);
    host = u.port === "" ? u.hostname : `${u.hostname}:${u.port}`;
  } catch {
    host = origin;
  }
  return DEMO_NAMES[host] ?? host;
};

// Mirror of the frontend's inferHost, using self.location (there is no window
// in a worker). II is served from the same origin the IC API is collocated on
// in local/tunnel deployments; on the official gateways the API is icp-api.io.
const IC_API_DOMAIN = "icp-api.io";
const inferHost = (): string => {
  const loc = self.location;
  const isGateway = (domain: string): boolean =>
    loc.hostname === domain || loc.hostname.endsWith(`.${domain}`);
  if (
    isGateway("icp0.io") ||
    isGateway("ic0.app") ||
    isGateway("icp.net") ||
    isGateway("internetcomputer.org")
  ) {
    return "https://" + IC_API_DOMAIN;
  }
  return loc.protocol + "//" + loc.host;
};

const isMainnet = (host: string): boolean => host.includes(IC_API_DOMAIN);

self.addEventListener("install", () => {
  void self.skipWaiting();
});

self.addEventListener("activate", (event: ExtendableEvent) => {
  event.waitUntil(self.clients.claim());
});

// ── the pull ─────────────────────────────────────────────────────────────────

// Reconstruct the identity from the stored non-extractable key + chain and pull
// the dApp's pending notifications as the user. Returns [] on any failure so the
// caller can fall back to a generic notification.
const pullNotifications = async (
  canisterId: string,
  origin: string,
): Promise<Notification[]> => {
  let record: PushDelegationRecord | undefined;
  try {
    record = await idbGet<PushDelegationRecord>(origin, PUSH_DELEGATION_STORE);
  } catch (err) {
    console.warn("[ii-sw] delegation store unavailable:", err);
    return [];
  }
  if (record === undefined) {
    console.warn("[ii-sw] no delegation stored for", origin);
    return [];
  }
  if (record.expiresAtMillis <= Date.now()) {
    console.warn("[ii-sw] delegation for", origin, "has expired");
    return [];
  }

  try {
    const identity = DelegationIdentity.fromDelegation(
      await ECDSAKeyIdentity.fromKeyPair(record.keyPair),
      DelegationChain.fromJSON(JSON.parse(record.chainJson)),
    );
    const host = inferHost();
    const agent = await HttpAgent.create({ identity, host });
    if (!isMainnet(host)) {
      await agent.fetchRootKey();
    }
    const actor = Actor.createActor<{
      ii_pending_notifications: () => Promise<Notification[]>;
    }>(notificationIdl, { agent, canisterId });
    return await actor.ii_pending_notifications();
  } catch (err) {
    console.warn("[ii-sw] pull failed for", origin, err);
    return [];
  }
};

self.addEventListener("push", (event: PushEvent) => {
  if (!event.data) {
    console.warn("[ii-sw] push event with no data");
    return;
  }

  let routing: { c?: string; o?: string };
  try {
    routing = event.data.json();
  } catch (err) {
    console.error("[ii-sw] push body was not JSON:", err);
    return;
  }

  const origin = routing.o ?? "";
  const canisterId = routing.c ?? "";
  const label = senderLabel(origin || "Internet Identity");

  event.waitUntil(
    (async () => {
      const pending =
        canisterId !== "" && origin !== ""
          ? await pullNotifications(canisterId, origin)
          : [];

      if (pending.length === 0) {
        // Nothing pulled: keep the permission alive with a generic banner. The
        // real content is one tap away once the app is open.
        await self.registration.showNotification(label, {
          body: "You have a new notification",
          data: { origin, url: null },
          actions: [{ action: ACTION_MANAGE, title: "Manage" }],
        });
        return;
      }

      // Show each pending item, deduped on the dApp's own id via the tag so an
      // updated notification replaces rather than stacks.
      await Promise.all(
        pending.map((n) => {
          const url = n.url.length > 0 ? n.url[0] : null;
          return self.registration.showNotification(label, {
            body: n.title !== "" && n.body !== "" ? `${n.title} — ${n.body}` : n.title || n.body,
            tag: n.id,
            data: { origin, url },
            actions: [{ action: ACTION_MANAGE, title: "Manage" }],
          });
        }),
      );
    })(),
  );
});

self.addEventListener("notificationclick", (event: NotificationEvent) => {
  event.notification.close();
  const data = (event.notification.data ?? {}) as {
    origin?: string;
    url?: string | null;
  };

  if (event.action === ACTION_MANAGE) {
    const dest = new URL(SETTINGS_PATH, self.location.origin);
    if (data.origin) {
      dest.searchParams.set("app", data.origin);
    }
    event.waitUntil(self.clients.openWindow(dest.href).then(() => undefined));
    return;
  }

  if (data.url) {
    let target: URL | null;
    try {
      target = new URL(data.url);
    } catch {
      target = null;
    }
    if (
      target !== null &&
      (target.protocol === "https:" || target.protocol === "http:")
    ) {
      event.waitUntil(self.clients.openWindow(target.href).then(() => undefined));
      return;
    }
    console.warn("[ii-sw] ignoring a notification url that is not http(s)");
  }

  const origin = data.origin;
  if (!origin) {
    return;
  }
  const dest = new URL("/notify", self.location.origin);
  dest.searchParams.set("origin", origin);
  event.waitUntil(self.clients.openWindow(dest.href).then(() => undefined));
});
