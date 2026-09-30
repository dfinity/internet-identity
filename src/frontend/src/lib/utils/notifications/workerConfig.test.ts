import { describe, expect, it } from "vitest";
import {
  decodeWorkerConfig,
  encodeWorkerConfig,
  type WorkerConfig,
} from "$lib/utils/notifications/workerConfig";

const CONFIG: WorkerConfig = {
  canisterId: "rdmx6-jaaaa-aaaaa-aaadq-cai",
  agentOptions: { host: "http://127.0.0.1:4943", shouldFetchRootKey: true },
};

/**
 * The page writes this and a worker woken long afterwards reads it, with nothing in
 * between to ask. `shouldFetchRootKey` decides whether a local replica's answers are
 * accepted, so what comes back is what the page wrote or nothing at all.
 */
describe("the worker's config", () => {
  it("round trips what the page put on it", () => {
    expect(decodeWorkerConfig(`?${encodeWorkerConfig(CONFIG)}`)).toEqual(
      CONFIG,
    );
  });

  it("reads a deployment that does not fetch the root key", () => {
    const mainnet: WorkerConfig = {
      canisterId: CONFIG.canisterId,
      agentOptions: { host: "https://icp-api.io", shouldFetchRootKey: false },
    };

    expect(decodeWorkerConfig(`?${encodeWorkerConfig(mainnet)}`)).toEqual(
      mainnet,
    );
  });

  it("answers nothing for a search that carries neither", () => {
    expect(decodeWorkerConfig("")).toBeUndefined();
  });

  it("answers nothing where the canister is missing", () => {
    const search = new URLSearchParams({
      agentOptions: JSON.stringify(CONFIG.agentOptions),
    });

    expect(decodeWorkerConfig(`?${search}`)).toBeUndefined();
  });

  it("answers nothing where the options are missing", () => {
    const search = new URLSearchParams({
      canisterId: CONFIG.canisterId,
    });

    expect(decodeWorkerConfig(`?${search}`)).toBeUndefined();
  });

  it("answers nothing where the options are not JSON", () => {
    const search = new URLSearchParams({
      canisterId: CONFIG.canisterId,
      agentOptions: "{not json",
    });

    expect(decodeWorkerConfig(`?${search}`)).toBeUndefined();
  });

  it("answers nothing where a field the agent needs is absent", () => {
    const search = new URLSearchParams({
      canisterId: CONFIG.canisterId,
      agentOptions: JSON.stringify({ host: "https://icp-api.io" }),
    });

    expect(decodeWorkerConfig(`?${search}`)).toBeUndefined();
  });

  it("takes the root key flag only as the boolean it was written as", () => {
    const search = new URLSearchParams({
      canisterId: CONFIG.canisterId,
      agentOptions: JSON.stringify({
        host: "https://icp-api.io",
        shouldFetchRootKey: "true",
      }),
    });

    expect(decodeWorkerConfig(`?${search}`)).toBeUndefined();
  });
});
