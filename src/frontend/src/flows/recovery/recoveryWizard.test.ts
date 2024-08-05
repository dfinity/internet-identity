import {
  AnchorCredentials,
  CredentialId,
  PublicKey,
  WebAuthnCredential,
} from "$generated/internet_identity_types";
import { PinIdentityMaterial } from "../pin/idb";
import { getDevicesStatus } from "./recoveryWizard";

const ONE_WEEK_MILLIS = 7 * 24 * 60 * 60 * 1000;
const nowInMillis = 1722259851155;
const moreThanAWeekAgo = nowInMillis - ONE_WEEK_MILLIS - 1;
const lessThanAWeekAgo = nowInMillis - 1;

const pinIdentityMaterial: PinIdentityMaterial =
  {} as unknown as PinIdentityMaterial;

const noCredentials: AnchorCredentials = {
  credentials: [],
  recovery_credentials: [],
  recovery_phrases: [],
};

const device: WebAuthnCredential = {
  pubkey: [] as PublicKey,
  credential_id: [] as CredentialId,
};

const oneDeviceOnly: AnchorCredentials = {
  credentials: [device],
  recovery_credentials: [],
  recovery_phrases: [],
};

const oneRecoveryDeviceOnly: AnchorCredentials = {
  credentials: [],
  recovery_credentials: [device],
  recovery_phrases: [],
};

const oneDeviceAndPhrase: AnchorCredentials = {
  credentials: [device],
  recovery_credentials: [],
  recovery_phrases: [[] as PublicKey],
};

const twoDevices: AnchorCredentials = {
  credentials: [device, { ...device }],
  recovery_credentials: [],
  recovery_phrases: [[] as PublicKey],
};

const threeDevices: AnchorCredentials = {
  credentials: [device, { ...device }, { ...device }],
  recovery_credentials: [],
  recovery_phrases: [[] as PublicKey],
};

const oneNormalOneRecovery: AnchorCredentials = {
  credentials: [device],
  recovery_credentials: [device],
  recovery_phrases: [[] as PublicKey],
};

test("getDevicesStatus returns 'pin-only' for user with pin and has seen recovery longer than a week ago", () => {
  expect(
    getDevicesStatus({
      credentials: noCredentials,
      identityMetadata: {
        recoveryPageShownTimestampMillis: moreThanAWeekAgo,
      },
      pinIdentityMaterial,
      nowInMillis,
    })
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
    })
  ).toBe("one-device");
});

test("getDevicesStatus returns true for user with one passkey and empty identity metadata", () => {
  expect(
    getDevicesStatus({
      credentials: oneDeviceOnly,
      identityMetadata: {},
      pinIdentityMaterial: undefined,
      nowInMillis,
    })
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
    })
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
    })
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
    })
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
    })
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
    })
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
    })
  ).toBe("no-warning");
});

test("getDevicesStatus returns 'no-warning' for user with more than two devices and empty identity metadata", () => {
  expect(
    getDevicesStatus({
      credentials: threeDevices,
      identityMetadata: {},
      pinIdentityMaterial: undefined,
      nowInMillis,
    })
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
    })
  ).toBe("no-warning");
});
