import { DeviceData } from "../../generated/internet_identity_types";

export type RecoveryDevice = Omit<DeviceData, "alias"> & {
  purpose: { recovery: null };
};

export const recoveryDeviceToLabel = (device: RecoveryDevice): string => {
  if ("seed_phrase" in device.key_type) {
    return "Recovery Phrase";
  }
  return "External Hardware";
};
export const isRecoveryDevice = (
  device: Pick<DeviceData, "purpose">
): device is RecoveryDevice => "recovery" in device.purpose;

// Whether the user has a recovery phrase or not
export const hasRecoveryPhrase = (
  devices: Pick<DeviceData, "key_type">[]
): boolean => devices.some(isRecoveryPhrase);

// Whether the user has a recovery key (i.e. recovery but not phrase) or not
export const hasRecoveryKey = (
  devices: Pick<DeviceData, "key_type" | "purpose">[]
): boolean =>
  devices.some(
    (device) => isRecoveryDevice(device) && !isRecoveryPhrase(device)
  );

export const isProtected = (device: DeviceData): boolean =>
  "protected" in device.protection;

export const isRecoveryPhrase = ({
  key_type,
}: Pick<DeviceData, "key_type">): boolean => "seed_phrase" in key_type;
