import type { MetadataMapV2 } from "$lib/generated/internet_identity_types";
import {
  DO_NOT_SHOW_RECOVERY_PAGE_REQUEST_TIMESTAMP_MILLIS,
  IdentityMetadata,
  IdentityMetadataRepository,
  RECOVERY_PAGE_SHOW_TIMESTAMP_MILLIS,
} from "./identityMetadata";

const recoveryPageShownTimestampMillis = 1234567890;
const doNotShowRecoveryPageRequestTimestampMillis = 3456789012;
const mockRawMetadata: MetadataMapV2 = [
  [
    RECOVERY_PAGE_SHOW_TIMESTAMP_MILLIS,
    { String: String(recoveryPageShownTimestampMillis) },
  ],
  [
    DO_NOT_SHOW_RECOVERY_PAGE_REQUEST_TIMESTAMP_MILLIS,
    { String: String(doNotShowRecoveryPageRequestTimestampMillis) },
  ],
];
const mockIdentityMetadata: IdentityMetadata = {
  recoveryPageShownTimestampMillis,
  doNotShowRecoveryPageRequestTimestampMillis,
};

const getterMockSuccess = vi.fn().mockResolvedValue(mockRawMetadata);
const getterMockError = vi.fn().mockImplementation(() => {
  return Promise.reject(Error("test error"));
});
const setterMockSuccess = vi.fn();
const setterMockError = vi.fn().mockRejectedValue("test error");

beforeEach(() => {
  vi.clearAllMocks();
  vi.spyOn(console, "warn").mockImplementation(() => {});
});

test("IdentityMetadataRepository loads data on init in the background", async () => {
  const instance = IdentityMetadataRepository.init({
    getter: getterMockSuccess,
    setter: setterMockSuccess,
  });

  expect(getterMockSuccess).toHaveBeenCalledTimes(1);
  expect(await instance.getMetadata()).toEqual(mockIdentityMetadata);
});

test("getMetadata waits until metadata is loaded", async () => {
  const slowGetter = vi.fn().mockImplementation(async () => {
    await new Promise((resolve) => setTimeout(resolve, 1_000));
    return mockRawMetadata;
  });
  const instance = IdentityMetadataRepository.init({
    getter: slowGetter,
    setter: setterMockSuccess,
  });

  expect(await instance.getMetadata()).toEqual(mockIdentityMetadata);
});

test("IdentityMetadataRepository returns undefined without raising an error if fetching fails", async () => {
  const instance = IdentityMetadataRepository.init({
    getter: getterMockError,
    setter: setterMockSuccess,
  });

  expect(getterMockError).toHaveBeenCalledTimes(1);

  expect(await instance.getMetadata()).toEqual(undefined);
  expect(getterMockError).toHaveBeenCalledTimes(1);
  // Error is not thrown, but warnings is logged.
  expect(console.warn).toHaveBeenCalledTimes(1);
});

test("IdentityMetadataRepository changes partial data in memory", async () => {
  const instance = IdentityMetadataRepository.init({
    getter: getterMockSuccess,
    setter: setterMockSuccess,
  });

  const newRecoveryPageShownTimestampMillis = 9876543210;
  instance.updateMetadata({
    recoveryPageShownTimestampMillis: newRecoveryPageShownTimestampMillis,
  });

  expect(await instance.getMetadata()).toEqual({
    recoveryPageShownTimestampMillis: newRecoveryPageShownTimestampMillis,
    doNotShowRecoveryPageRequestTimestampMillis:
      doNotShowRecoveryPageRequestTimestampMillis,
  });
});

test("IdentityMetadataRepository changes data in memory", async () => {
  const instance = IdentityMetadataRepository.init({
    getter: getterMockSuccess,
    setter: setterMockSuccess,
  });

  const newRecoveryPageShownTimestampMillis = 9876543210;
  const newDoNotShowRecoveryPageRequestTimestampMillis = 1234567890;
  instance.updateMetadata({
    recoveryPageShownTimestampMillis: newRecoveryPageShownTimestampMillis,
    doNotShowRecoveryPageRequestTimestampMillis:
      newDoNotShowRecoveryPageRequestTimestampMillis,
  });

  expect(await instance.getMetadata()).toEqual({
    recoveryPageShownTimestampMillis: newRecoveryPageShownTimestampMillis,
    doNotShowRecoveryPageRequestTimestampMillis:
      newDoNotShowRecoveryPageRequestTimestampMillis,
  });
});

test("IdentityMetadataRepository sets data from partial data in memory", async () => {
  const partialMetadata: MetadataMapV2 = [
    [
      RECOVERY_PAGE_SHOW_TIMESTAMP_MILLIS,
      { String: String(recoveryPageShownTimestampMillis) },
    ],
  ];
  const instance = IdentityMetadataRepository.init({
    getter: vi.fn().mockResolvedValue(partialMetadata),
    setter: setterMockSuccess,
  });

  expect(await instance.getMetadata()).toEqual({
    recoveryPageShownTimestampMillis: recoveryPageShownTimestampMillis,
  });

  const newDoNotShowRecoveryPageRequestTimestampMillis = 1234567890;
  instance.updateMetadata({
    doNotShowRecoveryPageRequestTimestampMillis:
      newDoNotShowRecoveryPageRequestTimestampMillis,
  });

  expect(await instance.getMetadata()).toEqual({
    recoveryPageShownTimestampMillis: recoveryPageShownTimestampMillis,
    doNotShowRecoveryPageRequestTimestampMillis:
      newDoNotShowRecoveryPageRequestTimestampMillis,
  });
});

test("IdentityMetadataRepository sets partial data in memory", async () => {
  const noMetadata: MetadataMapV2 = [];
  const instance = IdentityMetadataRepository.init({
    getter: vi.fn().mockResolvedValue(noMetadata),
    setter: setterMockSuccess,
  });

  const newRecoveryPageShownTimestampMillis = 9876543210;
  instance.updateMetadata({
    recoveryPageShownTimestampMillis: newRecoveryPageShownTimestampMillis,
  });

  expect(await instance.getMetadata()).toEqual({
    recoveryPageShownTimestampMillis: newRecoveryPageShownTimestampMillis,
  });
});

test("IdentityMetadataRepository sets data in memory", async () => {
  const noMetadata: MetadataMapV2 = [];
  const instance = IdentityMetadataRepository.init({
    getter: vi.fn().mockResolvedValue(noMetadata),
    setter: setterMockSuccess,
  });

  const newRecoveryPageShownTimestampMillis = 9876543210;
  const newDoNotShowRecoveryPageRequestTimestampMillis = 1234567890;
  instance.updateMetadata({
    recoveryPageShownTimestampMillis: newRecoveryPageShownTimestampMillis,
    doNotShowRecoveryPageRequestTimestampMillis:
      newDoNotShowRecoveryPageRequestTimestampMillis,
  });

  expect(await instance.getMetadata()).toEqual({
    recoveryPageShownTimestampMillis: newRecoveryPageShownTimestampMillis,
    doNotShowRecoveryPageRequestTimestampMillis:
      newDoNotShowRecoveryPageRequestTimestampMillis,
  });
});

test("IdentityMetadataRepository commits updated metadata to canister", async () => {
  const instance = IdentityMetadataRepository.init({
    getter: getterMockSuccess,
    setter: setterMockSuccess,
  });

  const newRecoveryPageShownTimestampMillis = 9876543210;
  const newDoNotShowRecoveryPageRequestTimestampMillis = 1234567890;
  instance.updateMetadata({
    recoveryPageShownTimestampMillis: newRecoveryPageShownTimestampMillis,
    doNotShowRecoveryPageRequestTimestampMillis:
      newDoNotShowRecoveryPageRequestTimestampMillis,
  });

  expect(setterMockSuccess).not.toHaveBeenCalled();
  await instance.commitMetadata();

  expect(setterMockSuccess).toHaveBeenCalledTimes(1);
  expect(setterMockSuccess).toHaveBeenCalledWith([
    [
      RECOVERY_PAGE_SHOW_TIMESTAMP_MILLIS,
      { String: String(newRecoveryPageShownTimestampMillis) },
    ],
    [
      DO_NOT_SHOW_RECOVERY_PAGE_REQUEST_TIMESTAMP_MILLIS,
      { String: String(newDoNotShowRecoveryPageRequestTimestampMillis) },
    ],
  ]);
});

test("IdentityMetadataRepository doesn't commit to canister without changes", async () => {
  const instance = IdentityMetadataRepository.init({
    getter: getterMockSuccess,
    setter: setterMockSuccess,
  });

  expect(setterMockSuccess).not.toHaveBeenCalled();
  await instance.commitMetadata();

  expect(setterMockSuccess).not.toHaveBeenCalled();
});

test("IdentityMetadataRepository doesn't raise an error if committing fails", async () => {
  const instance = IdentityMetadataRepository.init({
    getter: getterMockSuccess,
    setter: setterMockError,
  });

  const newRecoveryPageShownTimestampMillis = 9876543210;
  const newDoNotShowRecoveryPageRequestTimestampMillis = 1234567890;
  const newMetadata = {
    recoveryPageShownTimestampMillis: newRecoveryPageShownTimestampMillis,
    doNotShowRecoveryPageRequestTimestampMillis:
      newDoNotShowRecoveryPageRequestTimestampMillis,
  };
  instance.updateMetadata(newMetadata);

  expect(setterMockError).not.toHaveBeenCalled();
  const committed = await instance.commitMetadata();

  expect(committed).toBe(false);
  expect(setterMockError).toHaveBeenCalledTimes(1);
  expect(setterMockError).toHaveBeenCalledWith([
    [
      RECOVERY_PAGE_SHOW_TIMESTAMP_MILLIS,
      { String: String(newRecoveryPageShownTimestampMillis) },
    ],
    [
      DO_NOT_SHOW_RECOVERY_PAGE_REQUEST_TIMESTAMP_MILLIS,
      { String: String(newDoNotShowRecoveryPageRequestTimestampMillis) },
    ],
  ]);

  // But the value in memory is not lost.
  expect(await instance.getMetadata()).toEqual(newMetadata);
});

test("IdentityMetadataRepository commits additional metadata to canister after update", async () => {
  const anotherMetadataEntry: [string, { String: string }] = [
    "otherKey",
    { String: "otherValue" },
  ];
  const mockMoreRawMetadata: MetadataMapV2 = [
    [
      RECOVERY_PAGE_SHOW_TIMESTAMP_MILLIS,
      { String: String(recoveryPageShownTimestampMillis) },
    ],
    anotherMetadataEntry,
  ];
  const getterMock = vi.fn().mockResolvedValue(mockMoreRawMetadata);
  const instance = IdentityMetadataRepository.init({
    getter: getterMock,
    setter: setterMockSuccess,
  });

  const newRecoveryPageShownTimestampMillis = 9876543210;
  instance.updateMetadata({
    recoveryPageShownTimestampMillis: newRecoveryPageShownTimestampMillis,
  });

  expect(setterMockSuccess).not.toHaveBeenCalled();
  await instance.commitMetadata();

  expect(setterMockSuccess).toHaveBeenCalledTimes(1);
  expect(setterMockSuccess).toHaveBeenCalledWith([
    [
      RECOVERY_PAGE_SHOW_TIMESTAMP_MILLIS,
      { String: String(newRecoveryPageShownTimestampMillis) },
    ],
    anotherMetadataEntry,
  ]);
});

test("IdentityMetadataRepository commits from initial partial data after adding more partial data", async () => {
  const partialMetadata: MetadataMapV2 = [
    [
      RECOVERY_PAGE_SHOW_TIMESTAMP_MILLIS,
      { String: String(recoveryPageShownTimestampMillis) },
    ],
  ];
  const instance = IdentityMetadataRepository.init({
    getter: vi.fn().mockResolvedValue(partialMetadata),
    setter: setterMockSuccess,
  });

  expect(await instance.getMetadata()).toEqual({
    recoveryPageShownTimestampMillis: recoveryPageShownTimestampMillis,
  });

  const newDoNotShowRecoveryPageRequestTimestampMillis = 1234567890;
  instance.updateMetadata({
    doNotShowRecoveryPageRequestTimestampMillis:
      newDoNotShowRecoveryPageRequestTimestampMillis,
  });

  expect(await instance.getMetadata()).toEqual({
    recoveryPageShownTimestampMillis: recoveryPageShownTimestampMillis,
    doNotShowRecoveryPageRequestTimestampMillis:
      newDoNotShowRecoveryPageRequestTimestampMillis,
  });

  expect(setterMockSuccess).not.toHaveBeenCalled();
  await instance.commitMetadata();

  expect(setterMockSuccess).toHaveBeenCalledTimes(1);
  expect(setterMockSuccess).toHaveBeenCalledWith([
    [
      RECOVERY_PAGE_SHOW_TIMESTAMP_MILLIS,
      { String: String(recoveryPageShownTimestampMillis) },
    ],
    [
      DO_NOT_SHOW_RECOVERY_PAGE_REQUEST_TIMESTAMP_MILLIS,
      { String: String(newDoNotShowRecoveryPageRequestTimestampMillis) },
    ],
  ]);
});
