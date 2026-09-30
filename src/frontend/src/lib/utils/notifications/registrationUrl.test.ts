import { describe, expect, it } from "vitest";
import {
  registrationFrom,
  registrationSearch,
  type WorkerRegistration,
} from "$lib/utils/notifications/registrationUrl";

const REGISTRATION: WorkerRegistration = {
  canisterId: "rdmx6-jaaaa-aaaaa-aaadq-cai",
  agentOptions: { host: "http://127.0.0.1:4943", shouldFetchRootKey: true },
};

/**
 * The page writes this and a worker woken long afterwards reads it, with nothing in
 * between to ask. `shouldFetchRootKey` decides whether a local replica's answers are
 * accepted, so what comes back is what the page wrote or nothing at all.
 */
describe("the worker's registration URL", () => {
  it("round trips what the page put on it", () => {
    expect(registrationFrom(`?${registrationSearch(REGISTRATION)}`)).toEqual(
      REGISTRATION,
    );
  });

  it("reads a deployment that does not fetch the root key", () => {
    const mainnet: WorkerRegistration = {
      canisterId: REGISTRATION.canisterId,
      agentOptions: { host: "https://icp-api.io", shouldFetchRootKey: false },
    };

    expect(registrationFrom(`?${registrationSearch(mainnet)}`)).toEqual(
      mainnet,
    );
  });

  it("answers nothing for a search that carries neither", () => {
    expect(registrationFrom("")).toBeUndefined();
  });

  it("answers nothing where the canister is missing", () => {
    const search = new URLSearchParams({
      agentOptions: JSON.stringify(REGISTRATION.agentOptions),
    });

    expect(registrationFrom(`?${search}`)).toBeUndefined();
  });

  it("answers nothing where the options are missing", () => {
    const search = new URLSearchParams({
      canisterId: REGISTRATION.canisterId,
    });

    expect(registrationFrom(`?${search}`)).toBeUndefined();
  });

  it("answers nothing where the options are not JSON", () => {
    const search = new URLSearchParams({
      canisterId: REGISTRATION.canisterId,
      agentOptions: "{not json",
    });

    expect(registrationFrom(`?${search}`)).toBeUndefined();
  });

  it("answers nothing where a field the agent needs is absent", () => {
    const search = new URLSearchParams({
      canisterId: REGISTRATION.canisterId,
      agentOptions: JSON.stringify({ host: "https://icp-api.io" }),
    });

    expect(registrationFrom(`?${search}`)).toBeUndefined();
  });

  it("takes the root key flag only as the boolean it was written as", () => {
    const search = new URLSearchParams({
      canisterId: REGISTRATION.canisterId,
      agentOptions: JSON.stringify({
        host: "https://icp-api.io",
        shouldFetchRootKey: "true",
      }),
    });

    expect(registrationFrom(`?${search}`)).toBeUndefined();
  });
});
