import type { NotificationAppInfo } from "$lib/generated/internet_identity_types";
import { browserAppStatus } from "$lib/utils/notifications/appStatus";
import { claimDeviceApp } from "$lib/utils/notifications/deviceApp";
import { installUrlOrNone } from "$lib/utils/notifications/installUrl";

/** Notifications reach this identity on this device now, or the install is what is left and this is where it lives. */
export type AppSwitchOutcome =
  | { delivering: true }
  | { delivering: false; installUrl: string | undefined };

/** A handle is offered on the strength of the donor browser pointing at an app, which does not mean the app has a subscription, so what the canister says after the claim is what answers. */
const claimedAppDelivers = async (identityNumber: bigint): Promise<boolean> => {
  // Only a claim that happened says this device has an app: "no donor" and "nothing was
  // asked" both leave the switch with nothing to report.
  if ((await claimDeviceApp(identityNumber)) !== "claimed") {
    return false;
  }
  const status = await browserAppStatus(identityNumber);
  return status.kind === "app" && status.app.subscribed;
};

/** The install URL is minted here rather than kept from the render: the key it carries is good for half an hour and a settings page sits open for longer. */
export const turnOnWithApp = async ({
  identityNumber,
  ownApp,
}: {
  identityNumber: bigint;
  /** What this browser's entry says about the app it points at. */
  ownApp: NotificationAppInfo | undefined;
}): Promise<AppSwitchOutcome> => {
  // An app another identity on this browser can hand over is pointed at instead of installed again, and only where this browser points at nothing.
  if (ownApp === undefined && (await claimedAppDelivers(identityNumber))) {
    return { delivering: true };
  }
  return {
    delivering: false,
    installUrl: await installUrlOrNone(identityNumber),
  };
};
