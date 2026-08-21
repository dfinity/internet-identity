import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

const { mockGetPrimaryOrigin } = vi.hoisted(() => ({
  mockGetPrimaryOrigin: vi.fn<() => string | undefined>(),
}));

vi.mock("$lib/globals", () => ({
  getPrimaryOrigin: mockGetPrimaryOrigin,
}));

const PRIMARY_ORIGIN = "https://id.ai";
const LEGACY_ORIGIN = "https://identity.ic0.app";

// `window.location` is read-only, so swap it for a stand-in pointed at the URL
// under test. Capture the original property descriptor (not just the value) so
// `afterEach` can restore `location` exactly as it was.
const originalLocationDescriptor = Object.getOwnPropertyDescriptor(
  window,
  "location",
);
const replace = vi.fn();

const setLocation = (href: string) => {
  const { origin, pathname, search, hash } = new URL(href);
  Object.defineProperty(window, "location", {
    configurable: true,
    value: { origin, pathname, search, hash, replace },
  });
};

// The redirect is latched in module state, so every case needs a fresh module.
const loadPrimaryOrigin = () => {
  vi.resetModules();
  return import("./primaryOrigin");
};

const legacyRedirectHash = (sourceOrigin: string) => {
  const params = new URLSearchParams();
  params.set(
    "redirect_message",
    JSON.stringify({ origin: "https://dapp.example", data: {} }),
  );
  params.set("redirect_origin", sourceOrigin);
  return `#${params.toString()}`;
};

beforeEach(() => {
  replace.mockClear();
  mockGetPrimaryOrigin.mockReturnValue(PRIMARY_ORIGIN);
});

afterEach(() => {
  if (originalLocationDescriptor !== undefined) {
    Object.defineProperty(window, "location", originalLocationDescriptor);
  } else {
    delete (window as { location?: unknown }).location;
  }
});

describe("redirectToPrimaryOrigin", () => {
  it("redirects an authorize page loaded from a related origin", async () => {
    setLocation(`${LEGACY_ORIGIN}/authorize?foo=bar`);
    const { redirectToPrimaryOrigin, isRedirectingToPrimaryOrigin } =
      await loadPrimaryOrigin();

    expect(redirectToPrimaryOrigin()).toBe(true);
    expect(replace).toHaveBeenCalledWith(`${PRIMARY_ORIGIN}/authorize?foo=bar`);
    expect(isRedirectingToPrimaryOrigin()).toBe(true);
  });

  it("keeps a legacy response bounced back from the primary origin", async () => {
    setLocation(
      `${LEGACY_ORIGIN}/authorize${legacyRedirectHash(PRIMARY_ORIGIN)}`,
    );
    const { redirectToPrimaryOrigin, isRedirectingToPrimaryOrigin } =
      await loadPrimaryOrigin();

    expect(redirectToPrimaryOrigin()).toBe(false);
    expect(replace).not.toHaveBeenCalled();
    expect(isRedirectingToPrimaryOrigin()).toBe(false);
  });

  it("redirects when the redirect message comes from another origin", async () => {
    setLocation(
      `${LEGACY_ORIGIN}/authorize${legacyRedirectHash("https://attacker.example")}`,
    );
    const { redirectToPrimaryOrigin } = await loadPrimaryOrigin();

    expect(redirectToPrimaryOrigin()).toBe(true);
    expect(replace).toHaveBeenCalled();
  });
});
