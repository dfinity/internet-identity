import { afterEach, describe, expect, it } from "vitest";
import { get } from "svelte/store";
import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import {
  notificationConsentSettledStore,
  notificationConsentStore,
  type NotificationConsentContext,
} from "./notificationConsent.store";

const contextFor = (identityNumber: bigint): NotificationConsentContext => ({
  effectiveOrigin: "https://app.example",
  identityNumber,
  actor: {} as ActorSubclass<_SERVICE>,
  device: { source: "app", subscribed: false, enabled: false },
  consented: true,
  installFirst: true,
  isRepair: true,
  disabledInstalledApps: undefined,
});

describe("notificationConsentStore", () => {
  afterEach(() => {
    notificationConsentStore.clear();
  });

  it("settles nothing for a screen another one has replaced", () => {
    const first = contextFor(BigInt(10_000));
    const second = contextFor(BigInt(10_001));
    notificationConsentStore.setContext(first);
    notificationConsentStore.setContext(second);

    notificationConsentStore.settle(first);
    expect(get(notificationConsentSettledStore)).toBeUndefined();

    notificationConsentStore.settle(second);
    expect(get(notificationConsentSettledStore)).toBe(true);
  });
});
