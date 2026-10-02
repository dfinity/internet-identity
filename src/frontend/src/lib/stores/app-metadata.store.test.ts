import { get } from "svelte/store";
import { beforeEach, expect, test, vi } from "vitest";
import {
  getAppMetadataStore,
  resetAppMetadataStores,
} from "$lib/stores/app-metadata.store";
import { fetchAppMetadata, type AppMetadata } from "$lib/utils/appMetadata";

vi.mock("$lib/utils/appMetadata", () => ({
  fetchAppMetadata: vi.fn(),
  logoAsObjectUrl: (blob: Blob) => Promise.resolve(`blob:${blob.size}`),
}));

/// The cache the store keeps of what a visit resolved. jsdom has no IndexedDB, and
/// `idb-keyval` throws rather than rejects without one, which is its own scenario
/// below.
const idb = new Map<string, unknown>();
let idbAvailable = true;
vi.mock("idb-keyval", () => ({
  createStore: () => "store",
  get: (key: string) => {
    if (!idbAvailable) {
      throw new Error("IndexedDB is not available");
    }
    return Promise.resolve(idb.get(key));
  },
  set: (key: string, value: unknown) => {
    if (!idbAvailable) {
      throw new Error("IndexedDB is not available");
    }
    idb.set(key, value);
    return Promise.resolve();
  },
}));

// The curated dapps list reads canister config that is only available in the
// browser; stub it with a single known dapp.
vi.mock("$lib/legacy/flows/dappsExplorer/dapps", () => ({
  getDapps: () => [
    {
      hasOrigin: (origin: string) => origin === "https://known.example.com",
      name: "Known App",
      oneLiner: "A curated app",
      logoSrc: "/known-logo.png",
    },
  ],
}));

const fetchAppMetadataMock = vi.mocked(fetchAppMetadata);

const pending = (): Promise<AppMetadata | undefined> => new Promise(() => {});

beforeEach(() => {
  resetAppMetadataStores();
  fetchAppMetadataMock.mockReset();
  idb.clear();
  idbAvailable = true;
  vi.stubGlobal("URL", {
    ...URL,
    createObjectURL: (blob: Blob) => `blob:${blob.size}`,
    revokeObjectURL: vi.fn(),
  });
});

test("should fall back to the curated dapps list while fetching", () => {
  fetchAppMetadataMock.mockReturnValue(pending());

  const store = getAppMetadataStore("https://known.example.com");

  expect(get(store)).toEqual({
    name: "Known App",
    description: "A curated app",
    logo: "/known-logo.png",
  });
});

test("should fetch from the derivation origin, not the displayed one", () => {
  fetchAppMetadataMock.mockReturnValue(pending());

  getAppMetadataStore(
    "https://derivation.example.com",
    "https://displayed.example.com",
  );

  // The app publishes once, on the origin its identity is derived for; its
  // alternative origins are presented from that same document.
  expect(fetchAppMetadataMock).toHaveBeenCalledExactlyOnceWith(
    "https://derivation.example.com",
    expect.any(Function),
  );
});

test("should match the curated fallback on the displayed origin too", () => {
  // A curated entry lists the origins an app signs in from, which needn't
  // include the origin it derives from -- so while apps migrate to the
  // well-known file, the displayed origin still resolves the entry.
  fetchAppMetadataMock.mockReturnValue(pending());

  const store = getAppMetadataStore(
    "https://derivation.example.com",
    "https://known.example.com",
  );

  expect(get(store)).toEqual({
    name: "Known App",
    description: "A curated app",
    logo: "/known-logo.png",
  });
});

test("should fall back to empty metadata for unknown origins", () => {
  fetchAppMetadataMock.mockReturnValue(pending());

  const store = getAppMetadataStore("https://unknown.example.com");

  expect(get(store)).toEqual({});
});

test("should replace the fallback wholesale once metadata is fetched", async () => {
  fetchAppMetadataMock.mockResolvedValue({ name: "Self-Published App" });

  const store = getAppMetadataStore("https://known.example.com");

  await vi.waitFor(() =>
    expect(get(store)).toEqual({ name: "Self-Published App" }),
  );
  expect(fetchAppMetadataMock).toHaveBeenCalledExactlyOnceWith(
    "https://known.example.com",
    expect.any(Function),
  );
});

test("should keep the fallback when the origin serves no metadata", async () => {
  fetchAppMetadataMock.mockResolvedValue(undefined);

  const store = getAppMetadataStore("https://known.example.com");

  // Give the resolved promise a chance to (incorrectly) overwrite the value.
  await new Promise((resolve) => setTimeout(resolve));
  expect(get(store)).toEqual({
    name: "Known App",
    description: "A curated app",
    logo: "/known-logo.png",
  });
});

test("should fetch once per origin and share the store", () => {
  fetchAppMetadataMock.mockReturnValue(pending());

  const first = getAppMetadataStore("https://app.example.com");
  const second = getAppMetadataStore("https://app.example.com");
  const other = getAppMetadataStore("https://other.example.com");

  expect(first).toBe(second);
  expect(other).not.toBe(first);
  expect(fetchAppMetadataMock).toHaveBeenCalledTimes(2);
  expect(fetchAppMetadataMock).toHaveBeenCalledWith(
    "https://app.example.com",
    expect.any(Function),
  );
  expect(fetchAppMetadataMock).toHaveBeenCalledWith(
    "https://other.example.com",
    expect.any(Function),
  );
});

test("should update subscribers that subscribed before the fetch resolved", async () => {
  let resolveFetch: (metadata: AppMetadata | undefined) => void = () => {};
  fetchAppMetadataMock.mockReturnValue(
    new Promise((resolve) => (resolveFetch = resolve)),
  );

  const store = getAppMetadataStore("https://app.example.com");
  const seen: AppMetadata[] = [];
  const unsubscribe = store.subscribe((value) => seen.push(value));

  resolveFetch({ name: "Late App" });
  await vi.waitFor(() => expect(seen).toHaveLength(2));
  expect(seen[0]).toEqual({});
  expect(seen[1]).toEqual({ name: "Late App" });
  unsubscribe();
});

test("should fetch again after the cache is reset", () => {
  fetchAppMetadataMock.mockResolvedValue({ name: "App" });

  getAppMetadataStore("https://app.example.com");
  resetAppMetadataStores();
  getAppMetadataStore("https://app.example.com");

  expect(fetchAppMetadataMock).toHaveBeenCalledTimes(2);
});

test("should open on what the last visit resolved, not the curated entry", async () => {
  idb.set("https://known.example.com", {
    name: "Self-Published App",
    logo: new Blob(["logo"]),
    storedAtMillis: Date.now(),
  });
  fetchAppMetadataMock.mockReturnValue(pending());

  const store = getAppMetadataStore("https://known.example.com");

  await vi.waitFor(() =>
    expect(get(store)).toEqual({ name: "Self-Published App", logo: "blob:4" }),
  );
});

/** An entry is rewritten on every visit, so a stale one means an app that has not
 *  been signed in to for a month, and its branding is the one thing we no longer
 *  have any reason to believe. */
test("should ignore a cached entry past its age", async () => {
  idb.set("https://known.example.com", {
    name: "Self-Published App",
    storedAtMillis: Date.now() - 31 * 24 * 60 * 60 * 1000,
  });
  fetchAppMetadataMock.mockReturnValue(pending());

  const store = getAppMetadataStore("https://known.example.com");

  await new Promise((resolve) => setTimeout(resolve));
  expect(get(store)).toEqual({
    name: "Known App",
    description: "A curated app",
    logo: "/known-logo.png",
  });
});

/** The cache only ever stands in for a fetch that has not landed. One that has
 *  already answered is the answer. */
test("should not let a slow cache read replace what was fetched", async () => {
  idb.set("https://known.example.com", {
    name: "Stale App",
    storedAtMillis: Date.now(),
  });
  fetchAppMetadataMock.mockResolvedValue({ name: "Self-Published App" });

  const store = getAppMetadataStore("https://known.example.com");

  await new Promise((resolve) => setTimeout(resolve));
  expect(get(store)).toEqual({ name: "Self-Published App" });
});

test("should keep the logo as a blob, which is what survives the page", async () => {
  const logo = new Blob(["logo"]);
  fetchAppMetadataMock.mockImplementation(async (_origin, addressLogo) => ({
    name: "Self-Published App",
    logo: await addressLogo!(logo),
  }));

  const store = getAppMetadataStore("https://known.example.com");

  await vi.waitFor(() =>
    expect(get(store)).toEqual({ name: "Self-Published App", logo: "blob:4" }),
  );
  await vi.waitFor(() =>
    expect(idb.get("https://known.example.com")).toEqual({
      name: "Self-Published App",
      // The live value addresses it as a URL; what is kept is the blob itself.
      logo,
      storedAtMillis: expect.any(Number),
    }),
  );
});

/** Metadata is a display nicety: a private window has no IndexedDB and must still
 *  sign in. */
test("should resolve without a cache at all", async () => {
  idbAvailable = false;
  fetchAppMetadataMock.mockResolvedValue({ name: "Self-Published App" });

  const store = getAppMetadataStore("https://known.example.com");

  expect(get(store)).toEqual({
    name: "Known App",
    description: "A curated app",
    logo: "/known-logo.png",
  });
  await vi.waitFor(() =>
    expect(get(store)).toEqual({ name: "Self-Published App" }),
  );
});
