import { beforeEach, describe, expect, it, vi } from "vitest";
import { Principal } from "@icp-sdk/core/principal";

const CANISTER_ID = "rdmx6-jaaaa-aaaaa-aaadq-cai";
const agentOptions = { host: "https://icp-api.io", shouldFetchRootKey: false };

vi.mock("$lib/globals", () => ({
  get agentOptions() {
    return agentOptions;
  },
  get canisterId() {
    return Principal.fromText(CANISTER_ID);
  },
}));

const { subscribeToPush } =
  await import("$lib/utils/notifications/pushSubscription");
const { registrationFrom } =
  await import("$lib/utils/notifications/registrationUrl");

const register = vi.fn((_url: string, _options?: unknown) => Promise.resolve());

/** The registration a subscribe walks through, which is all this needs of one. */
const serviceWorker = () => {
  const registration = {
    pushManager: {
      getSubscription: () => Promise.resolve(undefined),
      subscribe: () => Promise.resolve({ endpoint: "https://relay.example/a" }),
    },
  };
  return { register, ready: Promise.resolve(registration) };
};

/** What the worker will read back out of the URL it was registered with. */
const registeredWith = () =>
  registrationFrom(new URL(register.mock.calls[0][0], "https://id.ai").search);

beforeEach(() => {
  vi.clearAllMocks();
  agentOptions.host = "https://icp-api.io";
  agentOptions.shouldFetchRootKey = false;
  Object.defineProperty(navigator, "serviceWorker", {
    value: serviceWorker(),
    configurable: true,
  });
});

/**
 * A woken worker has no page to ask, so what it needs rides on the URL its
 * registration keeps. The root key is the half that matters: a worker told to fetch
 * it accepts what a local replica signs, so a deployment that did not ask for it
 * must never be handed it.
 */
describe("the worker's registration URL", () => {
  it("carries what the page resolved", async () => {
    await subscribeToPush(new Uint8Array([4]));

    expect(registeredWith()).toEqual({
      canisterId: CANISTER_ID,
      agentOptions: { host: "https://icp-api.io", shouldFetchRootKey: false },
    });
  });

  it("carries the root key flag where the deployment configured it on", async () => {
    agentOptions.host = "http://127.0.0.1:4943";
    agentOptions.shouldFetchRootKey = true;

    await subscribeToPush(new Uint8Array([4]));

    expect(registeredWith()).toEqual({
      canisterId: CANISTER_ID,
      agentOptions: { host: "http://127.0.0.1:4943", shouldFetchRootKey: true },
    });
  });
});
