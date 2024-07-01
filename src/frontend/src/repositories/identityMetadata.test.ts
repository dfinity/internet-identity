import { MetadataMapV2 } from "$generated/internet_identity_types";
import {
  IdentityMetadata,
  IdentityMetadataRepository,
  RECOVERY_PAGE_SHOW_TIMESTAMP_MILLIS,
} from "./identityMetadata";

const recoveryPageShownTimestampMillis = 1234567890;
const mockRawMetadata: MetadataMapV2 = [
  [
    RECOVERY_PAGE_SHOW_TIMESTAMP_MILLIS,
    { String: String(recoveryPageShownTimestampMillis) },
  ],
];
const mockIdentityMetadata: IdentityMetadata = {
  recoveryPageShownTimestampMillis,
};

// Used to await that the getter has resolved.
let getterResponse: MetadataMapV2 | null | undefined = null;
const getterMockSuccess = vi.fn().mockImplementation(() => {
  getterResponse = mockRawMetadata;
  return mockRawMetadata;
});
const getterMockError = vi.fn().mockImplementation(() => {
  getterResponse = null;
  throw new Error("test error");
});
const setterMockSuccess = vi.fn();
const setterMockError = vi.fn().mockRejectedValue("test error");

beforeEach(() => {
  getterResponse = undefined;
  vi.clearAllMocks();
  vi.spyOn(console, "warn").mockImplementation(() => {});
});

test("IdentityMetadataRepository loads data on init in the background", async () => {
  const instance = IdentityMetadataRepository.init({
    getter: getterMockSuccess,
    setter: setterMockSuccess,
  });

  await vi.waitFor(() => expect(getterResponse).toEqual(mockRawMetadata));

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

  // Wait for the first getter to fail.
  await vi.waitFor(() => expect(getterResponse).toEqual(null));

  // Error is not thrown, but warnings is logged.
  expect(console.warn).toHaveBeenCalledTimes(1);
  expect(getterMockError).toHaveBeenCalledTimes(1);

  expect(await instance.getMetadata()).toEqual(undefined);
  expect(getterMockError).toHaveBeenCalledTimes(1);
  expect(console.warn).toHaveBeenCalledTimes(1);
});

test("IdentityMetadataRepository changes data in memory", async () => {
  const instance = IdentityMetadataRepository.init({
    getter: getterMockSuccess,
    setter: setterMockSuccess,
  });

  await vi.waitFor(() => expect(getterResponse).toEqual(mockRawMetadata));

  const newRecoveryPageShownTimestampMillis = 9876543210;
  await instance.updateMetadata({
    recoveryPageShownTimestampMillis: newRecoveryPageShownTimestampMillis,
  });

  expect(await instance.getMetadata()).toEqual({
    recoveryPageShownTimestampMillis: newRecoveryPageShownTimestampMillis,
  });
});

test("IdentityMetadataRepository commits updated metadata to canister", async () => {
  const instance = IdentityMetadataRepository.init({
    getter: getterMockSuccess,
    setter: setterMockSuccess,
  });

  await vi.waitFor(() => expect(getterResponse).toEqual(mockRawMetadata));

  const newRecoveryPageShownTimestampMillis = 9876543210;
  await instance.updateMetadata({
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
  ]);
});

test("IdentityMetadataRepository doesn't commit to canister without changes", async () => {
  const instance = IdentityMetadataRepository.init({
    getter: getterMockSuccess,
    setter: setterMockSuccess,
  });

  await vi.waitFor(() => expect(getterResponse).toEqual(mockRawMetadata));

  expect(setterMockSuccess).not.toHaveBeenCalled();
  await instance.commitMetadata();

  expect(setterMockSuccess).not.toHaveBeenCalled();
});

test("IdentityMetadataRepository doesn't raise an error if committing fails", async () => {
  const instance = IdentityMetadataRepository.init({
    getter: getterMockSuccess,
    setter: setterMockError,
  });

  await vi.waitFor(() => expect(getterResponse).toEqual(mockRawMetadata));

  const newRecoveryPageShownTimestampMillis = 9876543210;
  const newMetadata = {
    recoveryPageShownTimestampMillis: newRecoveryPageShownTimestampMillis,
  };
  await instance.updateMetadata(newMetadata);

  expect(setterMockError).not.toHaveBeenCalled();
  const committed = await instance.commitMetadata();

  expect(committed).toBe(false);
  expect(setterMockError).toHaveBeenCalledTimes(1);
  expect(setterMockError).toHaveBeenCalledWith([
    [
      RECOVERY_PAGE_SHOW_TIMESTAMP_MILLIS,
      { String: String(newRecoveryPageShownTimestampMillis) },
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
  const getterMock = vi.fn().mockImplementation(async () => {
    // The `await` is necessary to make sure that the `getterResponse` is set before the test continues.
    getterResponse = await mockMoreRawMetadata;
    return mockMoreRawMetadata;
  });
  const instance = IdentityMetadataRepository.init({
    getter: getterMock,
    setter: setterMockSuccess,
  });

  await vi.waitFor(() => expect(getterResponse).toEqual(mockMoreRawMetadata));

  const newRecoveryPageShownTimestampMillis = 9876543210;
  await instance.updateMetadata({
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
