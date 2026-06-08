import { describe, it, expect, vi, beforeEach } from "vitest";

vi.mock("$app/environment", () => ({ browser: true }));

// Hand-rolled Svelte-store contract so vi.hoisted can construct it
// synchronously before the mock factory below runs.
const authStore = vi.hoisted(() => {
  let value: unknown = undefined;
  const subs = new Set<(v: unknown) => void>();
  return {
    subscribe: (fn: (v: unknown) => void) => {
      subs.add(fn);
      fn(value);
      return () => {
        subs.delete(fn);
      };
    },
    set: (v: unknown) => {
      value = v;
      subs.forEach((fn) => fn(value));
    },
  };
});

vi.mock("$lib/stores/authentication.store", () => ({
  authenticatedStore: authStore,
}));

import { ConfirmAccessMethodFlow } from "./confirmAccessMethodFlow.svelte";

const makeActor = () => ({
  authn_method_registration_mode_exit: vi.fn(() =>
    Promise.resolve({ Ok: null }),
  ),
  // expiration in the past => polling loop never executes
  authn_method_registration_mode_enter: vi.fn(() =>
    Promise.resolve({ Ok: { expiration: BigInt(0) } }),
  ),
  identity_info: vi.fn(() =>
    Promise.resolve({ Ok: { authn_method_registration: [] } }),
  ),
});

describe("ConfirmAccessMethodFlow.enterRegistrationMode", () => {
  beforeEach(() => {
    authStore.set({ actor: makeActor(), identityNumber: BigInt(1) });
  });

  it("generates a fresh /pair#<id> link when called with no registrationId", async () => {
    const flow = new ConfirmAccessMethodFlow();
    await flow.enterRegistrationMode();

    expect(flow.newDeviceLink).toBeDefined();
    expect(flow.newDeviceLink?.pathname).toBe("/pair");
    expect(flow.newDeviceLink?.hash).toMatch(/^#[0-9a-zA-Z]{5}$/);
  });

  it("does not generate a pairing link when called with an existing registrationId", async () => {
    const flow = new ConfirmAccessMethodFlow();
    await flow.enterRegistrationMode("abcde");

    expect(flow.newDeviceLink).toBeUndefined();
  });

  it("regenerates the pairing link on subsequent call with no arg", async () => {
    const flow = new ConfirmAccessMethodFlow();
    await flow.enterRegistrationMode("abcde");
    expect(flow.newDeviceLink).toBeUndefined();

    await flow.enterRegistrationMode();
    const firstHash = flow.newDeviceLink?.hash;
    expect(firstHash).toBeDefined();

    await flow.enterRegistrationMode();
    expect(flow.newDeviceLink).toBeDefined();
    expect(flow.newDeviceLink?.hash).not.toBe(firstHash);
  });
});
