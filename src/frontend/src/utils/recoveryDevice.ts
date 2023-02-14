import { DeviceData, Purpose } from "../../generated/internet_identity_types";

export type RecoveryDevice = Omit<DeviceData, "alias"> & {
  purpose: Extract<Purpose, { recovery: null }>;
};

export const recoveryDeviceToLabel = (
  device: Omit<DeviceData, "alias"> & RecoveryDevice
): string => {
  if ("seed_phrase" in device.key_type) {
    return "Recovery phrase";
  }
  return "External Hardware";
};
export const isRecoveryDevice = (
  device: Omit<DeviceData, "alias">
): device is RecoveryDevice => "recovery" in device.purpose;
