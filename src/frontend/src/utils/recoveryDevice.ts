import { DeviceData } from "$generated/internet_identity_types";

export type RecoveryDevice = Omit<DeviceData, "alias"> & {
  purpose: { recovery: null };
};

export type RecoveryPhrase = RecoveryDevice & {
  key_type: { seed_phrase: null };
};

export const recoveryDeviceToLabel = (device: RecoveryDevice): string => {
  if ("seed_phrase" in device.key_type) {
    return recoveryPhraseLabel;
  }
  return recoveryKeyLabel;
};
export const recoveryPhraseLabel = "Recovery Phrase";
export const recoveryKeyLabel = "Recovery Device";

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

export const isRecoveryPhrase = (
  device: Pick<DeviceData, "key_type">
): device is RecoveryPhrase => "seed_phrase" in device.key_type;
