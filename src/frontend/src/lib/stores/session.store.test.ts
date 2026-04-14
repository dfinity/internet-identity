import { get } from "svelte/store";
import { sessionStore } from "$lib/stores/session.store";
import { Principal } from "@icp-sdk/core/principal";
import { expect } from "vitest";
import { Actor } from "@icp-sdk/core/agent";

vi.mock("$app/environment", () => ({
  browser: true,
}));

describe("sessionStore", () => {
  it("initializes session data", async () => {
    await sessionStore.init({
      canisterId: Principal.anonymous(),
      agentOptions: {},
    });
    const session = get(sessionStore);

    // Make sure session identity is not anonymous
    expect(session.identity.getPrincipal().toText()).not.toEqual(
      Principal.anonymous().toText(),
    );

    // Make sure agent and actor are using session identity
    expect(session.identity.getPrincipal().toText()).toEqual(
      (await session.agent?.getPrincipal())?.toText(),
    );
    expect(session.identity.getPrincipal().toText()).toEqual(
      (await Actor.agentOf(session.actor)?.getPrincipal())?.toText(),
    );
  });

  it("resets all session data", async () => {
    await sessionStore.init({
      canisterId: Principal.anonymous(),
      agentOptions: {},
    });
    const session = get(sessionStore);
    await sessionStore.reset();
    const session2 = get(sessionStore);

    // Make sure session identity is not anonymous
    expect(session.identity.getPrincipal().toText()).not.toEqual(
      Principal.anonymous().toText(),
    );

    // Make sure agent and actor identity has been updated with session identity
    expect(session2.identity.getPrincipal().toText()).toEqual(
      (await Actor.agentOf(session2.actor)?.getPrincipal())?.toText(),
    );
    expect(session2.identity.getPrincipal().toText()).toEqual(
      (await session2.agent.getPrincipal())?.toText(),
    );

    // Make sure all data has changed
    expect(session.identity.getPrincipal().toText()).not.toEqual(
      session2.identity.getPrincipal().toText(),
    );
    expect(session.nonce).not.toEqual(session2.nonce);
    expect(session.salt).not.toEqual(session2.salt);
  });
});
