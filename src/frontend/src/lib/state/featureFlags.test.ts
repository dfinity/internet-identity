import { get } from "svelte/store";
import { afterEach, beforeEach, expect, test, vi } from "vitest";

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
const {
  DISCOVERABLE_PASSKEY_FLOW,
  MIN_GUIDED_UPGRADE,
  EMAIL_RECOVERY,
  EMAIL_RECOVERY_SETUP,
  MCP,
} = await import("$lib/state/featureFlags");

// `window.location` is read-only, so swap it for a writable stand-in we can
// point at the host under test. Capture the original property descriptor (not
// just the value) so `afterEach` can restore `location` exactly as it was,
// rather than leaving a plain data property behind for the rest of the worker.
const originalLocation = window.location;
const originalLocationDescriptor = Object.getOwnPropertyDescriptor(
  window,
  "location",
);
const setHostname = (hostname: string) => {
  Object.defineProperty(window, "location", {
    configurable: true,
    value: { ...originalLocation, hostname },
  });
};

beforeEach(() => {
  window.localStorage.clear();
  mockGetConfiguredFeatureFlag.mockReset();
  mockGetConfiguredFeatureFlag.mockReturnValue(undefined);
  // Clear any value left on the stores by a previous test.
  MIN_GUIDED_UPGRADE.getFeatureFlag()?.reset();
  DISCOVERABLE_PASSKEY_FLOW.getFeatureFlag()?.reset();
  EMAIL_RECOVERY.getFeatureFlag()?.reset();
  EMAIL_RECOVERY_SETUP.getFeatureFlag()?.reset();
  MCP.getFeatureFlag()?.reset();
});

afterEach(() => {
  if (originalLocationDescriptor !== undefined) {
    Object.defineProperty(window, "location", originalLocationDescriptor);
  } else {
    // `location` was inherited rather than an own property — drop our override
    // so lookups fall back to the prototype again.
    delete (window as { location?: unknown }).location;
  }
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

test("email-recovery flags default on for id.ai and beta.id.ai", () => {
  for (const hostname of ["id.ai", "beta.id.ai"]) {
    setHostname(hostname);
    EMAIL_RECOVERY.getFeatureFlag()?.reset();
    EMAIL_RECOVERY_SETUP.getFeatureFlag()?.reset();

    EMAIL_RECOVERY.initialize();
    EMAIL_RECOVERY_SETUP.initialize();

    expect(get(EMAIL_RECOVERY), hostname).toEqual(true);
    expect(get(EMAIL_RECOVERY_SETUP), hostname).toEqual(true);
  }
});

test("email-recovery flags stay off on other domains when unconfigured", () => {
  setHostname("example.com");

  EMAIL_RECOVERY.initialize();
  EMAIL_RECOVERY_SETUP.initialize();

  expect(get(EMAIL_RECOVERY)).toEqual(false);
  expect(get(EMAIL_RECOVERY_SETUP)).toEqual(false);
});

test("configured false keeps email-recovery flags off even on id.ai", () => {
  setHostname("id.ai");
  mockGetConfiguredFeatureFlag.mockReturnValue(false);

  EMAIL_RECOVERY.initialize();
  EMAIL_RECOVERY_SETUP.initialize();

  expect(get(EMAIL_RECOVERY)).toEqual(false);
  expect(get(EMAIL_RECOVERY_SETUP)).toEqual(false);
});

test("configured true turns email-recovery flags on off-domain", () => {
  setHostname("example.com");
  mockGetConfiguredFeatureFlag.mockReturnValue(true);

  EMAIL_RECOVERY.initialize();
  EMAIL_RECOVERY_SETUP.initialize();

  expect(get(EMAIL_RECOVERY)).toEqual(true);
  expect(get(EMAIL_RECOVERY_SETUP)).toEqual(true);
});

test("MCP flag defaults on for id.ai and beta.id.ai", () => {
  for (const hostname of ["id.ai", "beta.id.ai"]) {
    setHostname(hostname);
    MCP.getFeatureFlag()?.reset();

    MCP.initialize();

    expect(get(MCP), hostname).toEqual(true);
  }
});

test("MCP flag stays off on other domains when unconfigured", () => {
  setHostname("example.com");

  MCP.initialize();

  expect(get(MCP)).toEqual(false);
});

test("configured false keeps MCP flag off even on id.ai", () => {
  setHostname("id.ai");
  mockGetConfiguredFeatureFlag.mockReturnValue(false);

  MCP.initialize();

  expect(get(MCP)).toEqual(false);
});

test("configured true turns MCP flag on off-domain", () => {
  setHostname("example.com");
  mockGetConfiguredFeatureFlag.mockImplementation((name) =>
    name === "MCP" ? true : undefined,
  );

  MCP.initialize();

  expect(get(MCP)).toEqual(true);
});

test("the two email-recovery flags resolve independently from config", () => {
  setHostname("example.com");
  mockGetConfiguredFeatureFlag.mockImplementation((name) =>
    name === "EMAIL_RECOVERY_SETUP" ? true : undefined,
  );

  EMAIL_RECOVERY.initialize();
  EMAIL_RECOVERY_SETUP.initialize();

  // Only the set-up flag is configured on; the recovery flow flag stays at its
  // off-domain default.
  expect(get(EMAIL_RECOVERY)).toEqual(false);
  expect(get(EMAIL_RECOVERY_SETUP)).toEqual(true);
});
