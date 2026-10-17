import { describe, expect, it, vi } from "vitest";
import { Actor } from "@icp-sdk/core/agent";
import { ECDSAKeyIdentity } from "@icp-sdk/core/identity";
import type { AppKeyRecord } from "$lib/utils/notifications/notificationAppKey";
import {
  exportLinkKey,
  generateLinkKey,
} from "$lib/utils/notifications/linkKey";

const { CANISTER_ID } = vi.hoisted(() => ({
  CANISTER_ID: "rwlgt-iiaaa-aaaaa-aaaaa-cai",
}));

/** The globals the page bootstraps, which is what this module exists to bind in. */
vi.mock("$lib/globals", async () => {
  const { Principal } = await import("@icp-sdk/core/principal");
  return {
    agentOptions: { host: "https://icp-api.io" },
    canisterId: Principal.fromText(CANISTER_ID),
  };
});

const { appKeyActor, linkKeyActor } = await import("./appKeyActor");

const appKey = async (): Promise<AppKeyRecord> => ({
  keyPair: (await crypto.subtle.generateKey(
    { name: "ECDSA", namedCurve: "P-256" },
    false,
    ["sign"],
  )) as CryptoKeyPair,
});

describe("appKeyActor", () => {
  it("calls as the key this app holds", async () => {
    const record = await appKey();

    const actor = await appKeyActor(record);

    const expected = await ECDSAKeyIdentity.fromKeyPair(record.keyPair);
    const caller = await Actor.agentOf(actor)!.getPrincipal();
    expect(caller.toText()).toBe(expected.getPrincipal().toText());
  });

  it("calls the canister the page was served by", async () => {
    const actor = await appKeyActor(await appKey());

    expect(Actor.canisterIdOf(actor).toText()).toBe(CANISTER_ID);
  });
});

describe("linkKeyActor", () => {
  it("calls as the key the install URL carried", async () => {
    const keyPair = await generateLinkKey();

    const actor = await linkKeyActor(await exportLinkKey(keyPair));

    const expected = await ECDSAKeyIdentity.fromKeyPair(keyPair);
    const caller = await Actor.agentOf(actor)!.getPrincipal();
    expect(caller.toText()).toBe(expected.getPrincipal().toText());
  });

  it("calls the canister the page was served by", async () => {
    const actor = await linkKeyActor(
      await exportLinkKey(await generateLinkKey()),
    );

    expect(Actor.canisterIdOf(actor).toText()).toBe(CANISTER_ID);
  });
});
