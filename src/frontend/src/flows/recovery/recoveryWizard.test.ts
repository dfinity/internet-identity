import {
  AnchorCredentials,
  CredentialId,
  PublicKey,
  WebAuthnCredential,
} from "$generated/internet_identity_types";
import { shouldShowRecoveryWarning } from "./recoveryWizard";

const ONE_WEEK_MILLIS = 7 * 24 * 60 * 60 * 1000;
const nowInMillis = 1722259851155;
const moreThanAWeekAgo = nowInMillis - ONE_WEEK_MILLIS - 1;
const lessThanAWeekAgo = nowInMillis - 1;

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

test("shouldShowRecoveryWarning returns true for user with pin and has seen recovery longer than a week ago", () => {
  expect(
    shouldShowRecoveryWarning({
      credentials: noCredentials,
      identityMetadata: {
        recoveryPageShownTimestampMillis: moreThanAWeekAgo,
      },
      nowInMillis,
    })
  ).toBe(true);
});

test("shouldShowRecoveryWarning returns true for user with one passkey and has seen recovery longer than a week ago", () => {
  expect(
    shouldShowRecoveryWarning({
      credentials: oneDeviceOnly,
      identityMetadata: {
        recoveryPageShownTimestampMillis: moreThanAWeekAgo,
      },
      nowInMillis,
    })
  ).toBe(true);
});

test("shouldShowRecoveryWarning returns true for user with one passkey and empty identity metadata", () => {
  expect(
    shouldShowRecoveryWarning({
      credentials: oneDeviceOnly,
      identityMetadata: {},
      nowInMillis,
    })
  ).toBe(true);
});

test("shouldShowRecoveryWarning returns true for user with one recovery device and has seen recovery longer than a week ago", () => {
  expect(
    shouldShowRecoveryWarning({
      credentials: oneRecoveryDeviceOnly,
      identityMetadata: {
        recoveryPageShownTimestampMillis: moreThanAWeekAgo,
      },
      nowInMillis,
    })
  ).toBe(true);
});

test("shouldShowRecoveryWarning doesn't count phrase as one device", () => {
  expect(
    shouldShowRecoveryWarning({
      credentials: oneDeviceAndPhrase,
      identityMetadata: {
        recoveryPageShownTimestampMillis: moreThanAWeekAgo,
      },
      nowInMillis,
    })
  ).toBe(true);
});

test("shouldShowRecoveryWarning returns false for user with pin that has disabled the warning", () => {
  expect(
    shouldShowRecoveryWarning({
      credentials: noCredentials,
      identityMetadata: {
        recoveryPageShownTimestampMillis: moreThanAWeekAgo,
        doNotShowRecoveryPageRequestTimestampMillis: moreThanAWeekAgo,
      },
      nowInMillis,
    })
  ).toBe(false);
});

test("shouldShowRecoveryWarning returns false for user with one device that has disabled the warning", () => {
  expect(
    shouldShowRecoveryWarning({
      credentials: oneDeviceOnly,
      identityMetadata: {
        recoveryPageShownTimestampMillis: moreThanAWeekAgo,
        doNotShowRecoveryPageRequestTimestampMillis: moreThanAWeekAgo,
      },
      nowInMillis,
    })
  ).toBe(false);
});

test("shouldShowRecoveryWarning returns false for user with two devices", () => {
  expect(
    shouldShowRecoveryWarning({
      credentials: twoDevices,
      identityMetadata: {
        recoveryPageShownTimestampMillis: moreThanAWeekAgo,
      },
      nowInMillis,
    })
  ).toBe(false);
});

test("shouldShowRecoveryWarning returns false for user with one normal device and a recovery device", () => {
  expect(
    shouldShowRecoveryWarning({
      credentials: oneNormalOneRecovery,
      identityMetadata: {
        recoveryPageShownTimestampMillis: moreThanAWeekAgo,
      },
      nowInMillis,
    })
  ).toBe(false);
});

test("shouldShowRecoveryWarning returns false for user with more than two devices and empty identity metadata", () => {
  expect(
    shouldShowRecoveryWarning({
      credentials: threeDevices,
      identityMetadata: {},
      nowInMillis,
    })
  ).toBe(false);
});

test("shouldShowRecoveryWarning returns false for user with pin and has seen recovery less than a week ago", () => {
  expect(
    shouldShowRecoveryWarning({
      credentials: noCredentials,
      identityMetadata: {
        recoveryPageShownTimestampMillis: lessThanAWeekAgo,
      },
      nowInMillis,
    })
  ).toBe(false);
});
