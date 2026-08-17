import { get } from "svelte/store";
import { beforeEach, expect, test, vi } from "vitest";
import {
  getAppMetadataStore,
  resetAppMetadataStores,
} from "$lib/stores/app-metadata.store";
import { fetchAppMetadata, type AppMetadata } from "$lib/utils/appMetadata";

vi.mock("$lib/utils/appMetadata", () => ({
  fetchAppMetadata: vi.fn(),
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
  expect(fetchAppMetadataMock).toHaveBeenCalledWith("https://app.example.com");
  expect(fetchAppMetadataMock).toHaveBeenCalledWith(
    "https://other.example.com",
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
