import type { ActorSubclass } from "@icp-sdk/core/agent";
import { purgeAppSessions } from "$lib/stores/app-session.store";
import { currentDeviceId } from "$lib/stores/browser-key.store";
import type {
  _SERVICE,
  SessionDeviceInfo,
} from "$lib/generated/internet_identity_types";
import { nanosToMillis } from "$lib/utils/time";

export interface SessionDevice {
  id: number;
  name: string;
  createdAtMillis: number;
  lastUsedMillis: number;
  /** Several browsers report the same name, so the list marks the one being read from. */
  isCurrent: boolean;
}

export const fromCanisterSessionDevices = (
  devices: [] | [SessionDeviceInfo[]],
  currentDeviceId?: number,
): SessionDevice[] =>
  (devices[0] ?? [])
    .map((device) => ({
      id: device.id,
      name: device.name,
      createdAtMillis: nanosToMillis(device.created_at),
      lastUsedMillis: nanosToMillis(device.last_used),
      isCurrent: device.id === currentDeviceId,
    }))
    .sort((a, b) => b.lastUsedMillis - a.lastUsedMillis);

/**
 * Ends every session this browser holds, across every app it is signed into.
 *
 * The device record itself survives, so a browser that has been signed out is still one
 * the user recognises and signing back in from it reuses the same entry.
 *
 * Signing *this* browser out also discards the session chains it holds locally. The
 * canister has already stopped honouring them, and leaving them would have the next
 * silent request offer a chain that cannot mint.
 *
 * Which browser this is comes from the key record here rather than from a caller: the
 * list renders that flag from a promise, so a click landing before it resolves would
 * otherwise pass `false` for the user's own browser and leave exactly those chains
 * behind.
 */
export const signOutSessionDevice = async (
  actor: ActorSubclass<_SERVICE>,
  identityNumber: bigint,
  deviceId: number,
): Promise<void> => {
  const result = await actor.revoke_device_sessions({
    identity_number: identityNumber,
    device_id: deviceId,
  });
  // The only refusal the canister can report. A storage failure traps, which arrives as
  // a rejected call rather than as an `Err`, so there is nothing else to name here.
  if ("Err" in result) {
    throw new Error("Not authorized to end this browser's sessions");
  }
  if ((await currentDeviceId(identityNumber)) === deviceId) {
    await purgeAppSessions(identityNumber);
  }
};
