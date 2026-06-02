import { get } from "svelte/store";
import { beforeEach, expect, test, vi } from "vitest";

const { mockGetConfiguredFeatureFlag, mockGetPrimaryOrigin } = vi.hoisted(
  () => ({
    mockGetConfiguredFeatureFlag:
      vi.fn<(name: string) => boolean | undefined>(),
    mockGetPrimaryOrigin: vi.fn<() => string | undefined>(() => undefined),
  }),
);

vi.mock("$lib/globals", () => ({
  getConfiguredFeatureFlag: mockGetConfiguredFeatureFlag,
  getPrimaryOrigin: mockGetPrimaryOrigin,
}));

// Imported after the mock so the store factory picks up the mocked globals.
const { DISCOVERABLE_PASSKEY_FLOW, MIN_GUIDED_UPGRADE } =
  await import("$lib/state/featureFlags");

beforeEach(() => {
  window.localStorage.clear();
  mockGetConfiguredFeatureFlag.mockReset();
  mockGetConfiguredFeatureFlag.mockReturnValue(undefined);
  // Clear any value left on the stores by a previous test.
  MIN_GUIDED_UPGRADE.getFeatureFlag()?.reset();
  DISCOVERABLE_PASSKEY_FLOW.getFeatureFlag()?.reset();
});

test("keeps the compile-time default when nothing is configured", () => {
  MIN_GUIDED_UPGRADE.initialize();
  expect(get(MIN_GUIDED_UPGRADE)).toEqual(false);

  DISCOVERABLE_PASSKEY_FLOW.initialize();
  expect(get(DISCOVERABLE_PASSKEY_FLOW)).toEqual(true);
});

test("canister deploy arg overrides the compile-time default", () => {
  mockGetConfiguredFeatureFlag.mockImplementation((name) =>
    name === "MIN_GUIDED_UPGRADE" ? true : undefined,
  );
  MIN_GUIDED_UPGRADE.initialize();
  expect(get(MIN_GUIDED_UPGRADE)).toEqual(true);

  mockGetConfiguredFeatureFlag.mockImplementation((name) =>
    name === "DISCOVERABLE_PASSKEY_FLOW" ? false : undefined,
  );
  DISCOVERABLE_PASSKEY_FLOW.initialize();
  expect(get(DISCOVERABLE_PASSKEY_FLOW)).toEqual(false);
});

test("localStorage takes precedence over the canister deploy arg", () => {
  // Persist a user choice that disagrees with the configured value.
  MIN_GUIDED_UPGRADE.getFeatureFlag()?.set(false);
  mockGetConfiguredFeatureFlag.mockReturnValue(true);

  MIN_GUIDED_UPGRADE.initialize();

  expect(get(MIN_GUIDED_UPGRADE)).toEqual(false);
  // The configured value isn't even consulted once a stored value exists.
  expect(mockGetConfiguredFeatureFlag).not.toHaveBeenCalled();
});
