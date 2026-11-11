import { expect } from "@playwright/test";
import { Actor, AnonymousIdentity, HttpAgent } from "@icp-sdk/core/agent";
import { IDL } from "@icp-sdk/core/candid";
import { Ed25519KeyIdentity } from "@icp-sdk/core/identity";
import { Principal } from "@icp-sdk/core/principal";
import { readCanisterId } from "@dfinity/internet-identity-vite-plugins/utils";

/**
 * The test app's chatroom, as something other than a browser.
 *
 * A scenario about notifications needs somebody to send one, and a sender is never
 * notified of their own message, so the sending happens out here rather than in the page
 * the recipient is signed in on. What the app counted is read the same way.
 */

export const TEST_APP_CANISTER = readCanisterId({ canisterName: "test_app" });

/// The app as the local gateway serves it, which is the one origin both this browser and
/// the canister can reach: Internet Identity reads the app's sender list over an outcall,
/// and `nice-name.com` resolves in the browser alone.
export const APP_URL = `http://${TEST_APP_CANISTER}.localhost:8000`;

/// The replica as this process reaches it. The browser resolves every host to the dev
/// server through a launch flag, which is no help out here.
const REPLICA_URL = process.env.REPLICA_URL ?? "http://localhost:8000";

const Counts = IDL.Record({
  queued: IDL.Nat64,
  accepted: IDL.Nat64,
  received: IDL.Nat64,
  opened: IDL.Nat64,
  dropped: IDL.Nat64,
  deferred: IDL.Nat64,
});

const chatIdl = () =>
  IDL.Service({
    chat_join: IDL.Func([], [IDL.Opt(IDL.Principal)], []),
    chat_send: IDL.Func([IDL.Text], [], []),
    notification_metrics: IDL.Func(
      [],
      [
        IDL.Record({
          hours: IDL.Vec(IDL.Record({ start: IDL.Nat64, counts: Counts })),
          funnels: IDL.Vec(IDL.Record({ name: IDL.Text, counts: Counts })),
          backlog: IDL.Nat64,
          misconfigured: IDL.Opt(IDL.Reserved),
        }),
      ],
      ["query"],
    ),
  });

interface Counts {
  received: bigint;
  opened: bigint;
}

interface ChatService {
  chat_join: () => Promise<[] | [Principal]>;
  chat_send: (text: string) => Promise<void>;
  notification_metrics: () => Promise<{
    hours: { start: bigint; counts: Counts }[];
  }>;
}

export const chatActor = async (
  identity: AnonymousIdentity | Ed25519KeyIdentity,
) => {
  const agent = await HttpAgent.create({
    host: REPLICA_URL,
    identity,
    shouldFetchRootKey: true,
    verifyQuerySignatures: false,
  });
  return Actor.createActor<ChatService>(chatIdl, {
    agent,
    canisterId: Principal.fromText(TEST_APP_CANISTER),
  });
};

/** What the app has counted, summed over the hours it has. */
export const counted = async (): Promise<{
  received: bigint;
  opened: bigint;
}> => {
  const metrics = await (
    await chatActor(new AnonymousIdentity())
  ).notification_metrics();
  return metrics.hours.reduce(
    (total, hour) => ({
      received: total.received + hour.counts.received,
      opened: total.opened + hour.counts.opened,
    }),
    { received: BigInt(0), opened: BigInt(0) },
  );
};

/**
 * Waits until the app has counted at least one more notification as shown than it had
 * at `before`.
 *
 * Measured against a baseline because the app goes on counting across runs, and polled
 * because it is told by a service worker rather than by the scenario. At least, not
 * exactly: these are the canister's own counters and every scenario sharing this room
 * adds to them, so an exact total turns another scenario's notification into this one's
 * failure. What this one showed is asserted on its own notifications, not here.
 */
export const expectOneMoreShown = async (before: {
  received: bigint;
}): Promise<void> => {
  await expect
    .poll(async () => (await counted()).received, {
      message: "the app was never told its notification was shown",
      timeout: 30_000,
    })
    .toBeGreaterThanOrEqual(before.received + BigInt(1));
};

/** Somebody else writes to the room, since nobody is notified of their own message. */
export const someoneElseSends = async (text: string): Promise<void> => {
  const sender = await chatActor(Ed25519KeyIdentity.generate());
  await sender.chat_send(text);
};
