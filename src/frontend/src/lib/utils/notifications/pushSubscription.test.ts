import { beforeEach, describe, expect, it, vi } from "vitest";

const agentOptions: { shouldFetchRootKey?: boolean } = {};

vi.mock("$lib/globals", () => ({
  get agentOptions() {
    return agentOptions;
  },
}));
vi.mock("$lib/utils/init", () => ({
  readCanisterId: () => "rdmx6-jaaaa-aaaaa-aaadq-cai",
}));

const { subscribeToPush } =
  await import("$lib/utils/notifications/pushSubscription");

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

/** The query the worker is registered with. */
const registeredWith = (): URLSearchParams =>
  new URL(register.mock.calls[0][0], "https://id.ai").searchParams;

beforeEach(() => {
  vi.clearAllMocks();
  delete agentOptions.shouldFetchRootKey;
  Object.defineProperty(navigator, "serviceWorker", {
    value: serviceWorker(),
    configurable: true,
  });
});

/**
 * A woken worker has no page to ask, so what it needs rides on the URL its
 * registration keeps. The root key is the half that matters: a worker told to fetch
 * it accepts what a local replica signs, so a deployment that did not ask for it
 * must never be handed the flag.
 */
describe("the worker's registration URL", () => {
  it("names the canister to ask", async () => {
    await subscribeToPush(new Uint8Array([4]));

    expect(registeredWith().get("canisterId")).toBe(
      "rdmx6-jaaaa-aaaaa-aaadq-cai",
    );
  });

  it("omits the root key where the deployment did not configure it", async () => {
    await subscribeToPush(new Uint8Array([4]));

    expect(registeredWith().has("fetchRootKey")).toBe(false);
  });

  it("omits it where the deployment configured it off", async () => {
    agentOptions.shouldFetchRootKey = false;

    await subscribeToPush(new Uint8Array([4]));

    expect(registeredWith().has("fetchRootKey")).toBe(false);
  });

  it("carries it where the deployment configured it on", async () => {
    agentOptions.shouldFetchRootKey = true;

    await subscribeToPush(new Uint8Array([4]));

    expect(registeredWith().get("fetchRootKey")).toBe("1");
  });
});
