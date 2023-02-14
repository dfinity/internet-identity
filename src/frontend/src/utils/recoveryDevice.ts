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
  device: Omit<DeviceData, "alias">
): device is RecoveryDevice => "recovery" in device.purpose;
