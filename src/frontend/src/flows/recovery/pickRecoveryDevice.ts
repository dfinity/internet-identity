import { DeviceData } from "../../../generated/internet_identity_types";

// TODO
export const pickRecoveryDevice = (
  devices: DeviceData[]
): Promise<DeviceData> =>
  new Promise((resolve) => {
    resolve(devices[0]);
  });
