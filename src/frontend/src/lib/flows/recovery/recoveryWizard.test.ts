import type { DeviceData } from "$lib/generated/internet_identity_types";
import { PinIdentityMaterial } from "../pin/idb";
import { getDevicesStatus } from "./recoveryWizard";

const ONE_WEEK_MILLIS = 7 * 24 * 60 * 60 * 1000;
const nowInMillis = 1722259851155;
const moreThanAWeekAgo = nowInMillis - ONE_WEEK_MILLIS - 1;
const lessThanAWeekAgo = nowInMillis - 1;

const pinIdentityMaterial: PinIdentityMaterial =
  {} as unknown as PinIdentityMaterial;

const noCredentials: Omit<DeviceData, "alias">[] = [];

const recoveryPhrase: Omit<DeviceData, "alias"> = {
  origin: [],
  protection: { unprotected: null },
  // eslint-disable-next-line
  pubkey: undefined as any,
  key_type: { seed_phrase: null },
  purpose: { recovery: null },
  credential_id: [],
  metadata: [],
};

const device: Omit<DeviceData, "alias"> = {
  origin: [],
  protection: { unprotected: null },
  // eslint-disable-next-line
  pubkey: undefined as any,
  key_type: { unknown: null },
  purpose: { authentication: null },
  credential_id: [],
  metadata: [],
};

const recoveryDevice: Omit<DeviceData, "alias"> = {
  metadata: [],
  origin: [],
  protection: { protected: null },
  pubkey: new Uint8Array(),
  key_type: { cross_platform: null },
  purpose: { recovery: null },
  credential_id: [Uint8Array.from([0, 0, 0, 0, 0])],
};

const oneDeviceOnly: Omit<DeviceData, "alias">[] = [device];

const oneRecoveryDeviceOnly: Omit<DeviceData, "alias">[] = [recoveryDevice];

const oneDeviceAndPhrase: Omit<DeviceData, "alias">[] = [
  device,
  recoveryPhrase,
];

const twoDevices: Omit<DeviceData, "alias">[] = [
  device,
  { ...device },
  recoveryPhrase,
];

const threeDevices: Omit<DeviceData, "alias">[] = [
  device,
  { ...device },
  { ...device },
  recoveryPhrase,
];

const oneNormalOneRecovery: Omit<DeviceData, "alias">[] = [
  device,
  recoveryDevice,
  recoveryPhrase,
];

test("getDevicesStatus returns 'pin-only' for user with pin and has seen recovery longer than a week ago", () => {
  expect(
    getDevicesStatus({
      credentials: noCredentials,
      identityMetadata: {
        recoveryPageShownTimestampMillis: moreThanAWeekAgo,
      },
      pinIdentityMaterial,
      nowInMillis,
    }),
  ).toBe("pin-only");
});

test("getDevicesStatus returns 'one-device' for user with one passkey and has seen recovery longer than a week ago", () => {
  expect(
    getDevicesStatus({
      credentials: oneDeviceOnly,
      identityMetadata: {
        recoveryPageShownTimestampMillis: moreThanAWeekAgo,
      },
      pinIdentityMaterial: undefined,
      nowInMillis,
    }),
  ).toBe("one-device");
});

test("getDevicesStatus returns true for user with one passkey and empty identity metadata", () => {
  expect(
    getDevicesStatus({
      credentials: oneDeviceOnly,
      identityMetadata: {},
      pinIdentityMaterial: undefined,
      nowInMillis,
    }),
  ).toBe("one-device");
});

test("getDevicesStatus returns 'one-device' for user with one recovery device and has seen recovery longer than a week ago", () => {
  expect(
    getDevicesStatus({
      credentials: oneRecoveryDeviceOnly,
      identityMetadata: {
        recoveryPageShownTimestampMillis: moreThanAWeekAgo,
      },
      pinIdentityMaterial: undefined,
      nowInMillis,
    }),
  ).toBe("one-device");
});

test("getDevicesStatus returns 'one-device' for user with one device and a recovery phrase", () => {
  expect(
    getDevicesStatus({
      credentials: oneDeviceAndPhrase,
      identityMetadata: {
        recoveryPageShownTimestampMillis: moreThanAWeekAgo,
      },
      pinIdentityMaterial: undefined,
      nowInMillis,
    }),
  ).toBe("one-device");
});

test("getDevicesStatus returns 'no-warning' for user with pin that has disabled the warning", () => {
  expect(
    getDevicesStatus({
      credentials: noCredentials,
      identityMetadata: {
        recoveryPageShownTimestampMillis: moreThanAWeekAgo,
        doNotShowRecoveryPageRequestTimestampMillis: moreThanAWeekAgo,
      },
      pinIdentityMaterial,
      nowInMillis,
    }),
  ).toBe("no-warning");
});

test("getDevicesStatus returns 'no-warning' for user with one device that has disabled the warning", () => {
  expect(
    getDevicesStatus({
      credentials: oneDeviceOnly,
      identityMetadata: {
        recoveryPageShownTimestampMillis: moreThanAWeekAgo,
        doNotShowRecoveryPageRequestTimestampMillis: moreThanAWeekAgo,
      },
      pinIdentityMaterial: undefined,
      nowInMillis,
    }),
  ).toBe("no-warning");
});

test("getDevicesStatus returns 'no-warning' for user with two devices", () => {
  expect(
    getDevicesStatus({
      credentials: twoDevices,
      identityMetadata: {
        recoveryPageShownTimestampMillis: moreThanAWeekAgo,
      },
      pinIdentityMaterial: undefined,
      nowInMillis,
    }),
  ).toBe("no-warning");
});

test("getDevicesStatus returns 'no-warning' for user with one normal device and a recovery device", () => {
  expect(
    getDevicesStatus({
      credentials: oneNormalOneRecovery,
      identityMetadata: {
        recoveryPageShownTimestampMillis: moreThanAWeekAgo,
      },
      pinIdentityMaterial: undefined,
      nowInMillis,
    }),
  ).toBe("no-warning");
});

test("getDevicesStatus returns 'no-warning' for user with more than two devices and empty identity metadata", () => {
  expect(
    getDevicesStatus({
      credentials: threeDevices,
      identityMetadata: {},
      pinIdentityMaterial: undefined,
      nowInMillis,
    }),
  ).toBe("no-warning");
});

test("getDevicesStatus returns 'no-warning' for user with pin and has seen recovery less than a week ago", () => {
  expect(
    getDevicesStatus({
      credentials: noCredentials,
      identityMetadata: {
        recoveryPageShownTimestampMillis: lessThanAWeekAgo,
      },
      pinIdentityMaterial,
      nowInMillis,
    }),
  ).toBe("no-warning");
});
