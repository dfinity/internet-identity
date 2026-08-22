import type { ActorSubclass } from "@icp-sdk/core/agent";
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
  if ("Err" in result) {
    throw new Error(
      "Unauthorized" in result.Err
        ? "Not authorized to end this browser's sessions"
        : result.Err.InternalCanisterError,
    );
  }
};
