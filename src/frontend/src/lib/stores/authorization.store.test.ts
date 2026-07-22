import { get } from "svelte/store";
import { authorizationStore, authorizedStore } from "./authorization.store";

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
