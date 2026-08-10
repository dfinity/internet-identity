import { beforeEach, describe, expect, it, vi } from "vitest";
import type { Channel, JsonResponse } from "$lib/utils/transport/utils";
import { GENERIC_ERROR_CODE } from "$lib/utils/transport/utils";
import { handleForgetDelegationRequest } from "./forgetDelegation";

const APP_ORIGIN = "https://docs.example.com";
const DERIVATION_ORIGIN = "https://auth.example.com";

const forgetAppDelegations = vi.hoisted(() => vi.fn());
const validateDerivationOrigin = vi.hoisted(() => vi.fn());

vi.mock("$lib/stores/app-delegation.store", () => ({ forgetAppDelegations }));
vi.mock("$lib/utils/validateDerivationOrigin", () => ({
  validateDerivationOrigin,
}));

const channel = (): Channel & { sent: JsonResponse[] } => {
  const sent: JsonResponse[] = [];
  // The handler is driven directly rather than through a transport, so nothing
  // subscribes here.
  const addEventListener: Channel["addEventListener"] = () => () => {};
  return {
    sent,
    origin: APP_ORIGIN,
    closed: false,
    resumeToken: "test-resume-token",
    addEventListener,
    send: (response: JsonResponse) => {
      sent.push(response);
      return Promise.resolve();
    },
    close: () => Promise.resolve(),
  };
};

const forget = (params?: unknown) => ({
  jsonrpc: "2.0" as const,
  id: 1,
  method: "ii-forget-delegation",
  params: params === undefined ? undefined : { ...Object(params) },
});

beforeEach(() => {
  vi.clearAllMocks();
  validateDerivationOrigin.mockResolvedValue({ result: "valid" });
  forgetAppDelegations.mockResolvedValue(undefined);
});

describe("handleForgetDelegationRequest", () => {
  it("forgets the calling origin when no derivation origin is given", async () => {
    const c = channel();
    await handleForgetDelegationRequest(c)(forget());

    expect(forgetAppDelegations).toHaveBeenCalledWith(APP_ORIGIN);
    expect(c.sent).toEqual([{ jsonrpc: "2.0", id: 1, result: null }]);
  });

  it("forgets the derivation origin, which is where the records live", async () => {
    const c = channel();
    await handleForgetDelegationRequest(c)(
      forget({ icrc95DerivationOrigin: DERIVATION_ORIGIN }),
    );

    // Every alternative origin shares one record, so signing out of any of them
    // has to reach the derivation origin's entry rather than its own.
    expect(forgetAppDelegations).toHaveBeenCalledWith(DERIVATION_ORIGIN);
  });

  it("forgets nothing when the derivation origin does not check out", async () => {
    validateDerivationOrigin.mockResolvedValue({ result: "invalid" });
    const c = channel();

    await handleForgetDelegationRequest(c)(
      forget({ icrc95DerivationOrigin: DERIVATION_ORIGIN }),
    );

    expect(forgetAppDelegations).not.toHaveBeenCalled();
    expect(c.sent[0]).toMatchObject({
      error: { code: GENERIC_ERROR_CODE },
    });
  });

  it("rejects a malformed derivation origin", async () => {
    const c = channel();

    await handleForgetDelegationRequest(c)(
      forget({ icrc95DerivationOrigin: "not an origin" }),
    );

    expect(forgetAppDelegations).not.toHaveBeenCalled();
    expect(c.sent[0]).toHaveProperty("error");
  });

  it("leaves other methods alone", async () => {
    const c = channel();

    await handleForgetDelegationRequest(c)({
      jsonrpc: "2.0",
      id: 1,
      method: "icrc34_delegation",
    });

    expect(forgetAppDelegations).not.toHaveBeenCalled();
    expect(c.sent).toEqual([]);
  });

  it("ignores a notification, which has no id to answer", async () => {
    const c = channel();

    await handleForgetDelegationRequest(c)({
      jsonrpc: "2.0",
      method: "ii-forget-delegation",
    });

    expect(forgetAppDelegations).not.toHaveBeenCalled();
    expect(c.sent).toEqual([]);
  });
});
