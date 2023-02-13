import { DeviceData } from "../../generated/internet_identity_types";

export const recoveryDeviceToLabel = (
  device: Omit<DeviceData, "alias">
): string => {
  if (!("recovery" in device.purpose)) {
    throw new Error(`${device} is not a recovery device`);
  }

  if ("seed_phrase" in device.key_type) {
    return "Recovery phrase";
  }
  return "Recovery key";
};
