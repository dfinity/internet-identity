import { describe, expect, it, vi } from "vitest";
import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { currentBrowserId } from "$lib/stores/browser-key.store";
import { stopRepairChecks } from "./stopRepairChecks";

vi.mock("$lib/stores/browser-key.store", () => ({
  currentBrowserId: vi.fn(),
}));

const IDENTITY_NUMBER = BigInt(10_000);
const THIS_BROWSER_ID = 3;

/** An identity actor that records which calls the decline made. */
const recordingActor = () => {
  const calls: { method: string; request: unknown }[] = [];
  const actor = {
    remove_webpush_subscription: (request: unknown) => {
      calls.push({ method: "remove_webpush_subscription", request });
      return Promise.resolve({ Ok: null });
    },
    notification_revoke_consent: (request: unknown) => {
      calls.push({ method: "notification_revoke_consent", request });
      return Promise.resolve({ Ok: null });
    },
  } as unknown as ActorSubclass<_SERVICE>;
  return { actor, calls };
};

describe("stopRepairChecks", () => {
  it("drops this browser's registration and leaves the app's consent alone", async () => {
    vi.mocked(currentBrowserId).mockResolvedValue(THIS_BROWSER_ID);
    const { actor, calls } = recordingActor();

    await stopRepairChecks({ identityNumber: IDENTITY_NUMBER, actor });

    expect(calls).toEqual([
      {
        method: "remove_webpush_subscription",
        request: {
          anchor_number: IDENTITY_NUMBER,
          browser_id: THIS_BROWSER_ID,
        },
      },
    ]);
  });

  it("calls nothing for a browser with no registration of its own", async () => {
    vi.mocked(currentBrowserId).mockResolvedValue(undefined);
    const { actor, calls } = recordingActor();

    await stopRepairChecks({ identityNumber: IDENTITY_NUMBER, actor });

    expect(calls).toEqual([]);
  });
});
