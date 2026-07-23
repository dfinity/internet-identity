import { get } from "svelte/store";
import {
  authorizationContextStore,
  authorizationStore,
  authorizedStore,
  requestedMaxTimeToLiveStore,
} from "./authorization.store";

describe("authorizationStore.authorize", () => {
  it("records the granted access level alongside the account", async () => {
    const accountNumberPromise = Promise.resolve<bigint | undefined>(
      BigInt(42),
    );
    authorizationStore.authorize(accountNumberPromise, "read-only");

    const authorized = get(authorizedStore);
    expect(authorized).toBeDefined();
    expect(authorized?.accessLevel).toBe("read-only");
    await expect(authorized?.accountNumberPromise).resolves.toBe(BigInt(42));
  });

  it("overwrites a previous authorization's access level", () => {
    authorizationStore.authorize(Promise.resolve(undefined), "read-only");
    authorizationStore.authorize(Promise.resolve(undefined), "full-access");
    expect(get(authorizedStore)?.accessLevel).toBe("full-access");
  });

  it("records the chosen session duration when provided", () => {
    const maxTimeToLive = BigInt(3600) * BigInt(1_000_000_000);
    authorizationStore.authorize(
      Promise.resolve(undefined),
      "full-access",
      maxTimeToLive,
    );
    expect(get(authorizedStore)?.maxTimeToLive).toBe(maxTimeToLive);
  });

  it("leaves the session duration unset when omitted", () => {
    authorizationStore.authorize(Promise.resolve(undefined), "full-access");
    expect(get(authorizedStore)?.maxTimeToLive).toBeUndefined();
  });
});

describe("requestedMaxTimeToLiveStore", () => {
  // Reading the requested duration must never throw the way
  // `authorizationContextStore` does before the effective origin is set —
  // the sign-in screen reads it synchronously at mount, and a throw there
  // crashes the page render (regression: a hung attribute-consent flow).
  it("does not throw before the effective origin is set", () => {
    expect(() => get(requestedMaxTimeToLiveStore)).not.toThrow();
    // Sanity check that the throwing store really does throw at this point,
    // so this test is guarding a real difference.
    expect(() => get(authorizationContextStore)).toThrow();
  });

  it("exposes the requested duration once the request context is set", () => {
    const maxTimeToLive = BigInt(3600) * BigInt(1_000_000_000);
    authorizationStore.setRequestContext("https://example.com", maxTimeToLive);
    expect(get(requestedMaxTimeToLiveStore)).toBe(maxTimeToLive);
  });

  it("is undefined when the app didn't request a duration", () => {
    authorizationStore.setRequestContext("https://example.com", undefined);
    expect(get(requestedMaxTimeToLiveStore)).toBeUndefined();
  });
});
