import { storeLocalStorageKey } from "$lib/constants/store.constants";
import { writableStored } from "$lib/stores/writable.store";
import { get } from "svelte/store";
import type { LastUsedIdentity } from "$lib/stores/last-used-identities.store";
import { jsonReplacer } from "@dfinity/utils";

vi.mock("$app/environment", () => ({
  browser: true, // Or false, depending on the test case
}));

// Mock data based on LastUsedIdentity type
const mockIdentity1: LastUsedIdentity = {
  name: "Test Identity 1",
  lastUsedTimestampMillis: 1678886400000, // Example timestamp
  identityNumber: BigInt("10001"),
  authMethod: { passkey: { credentialId: new Uint8Array() } },
};

const mockIdentity2: LastUsedIdentity = {
  name: "Test Identity 2",
  lastUsedTimestampMillis: 1678887400000, // Slightly later timestamp
  identityNumber: BigInt("10002"),
  authMethod: { passkey: { credentialId: new Uint8Array() } },
};

/* eslint-disable-next-line @typescript-eslint/no-explicit-any */
const stringifyJson = (data: any) => JSON.stringify(data, jsonReplacer);

describe("writableStored", () => {
  beforeEach(() => {
    localStorage.clear();
  });

  it("writes to local storage when state changes", () => {
    const store = writableStored<LastUsedIdentity>({
      key: storeLocalStorageKey.LastUsedIdentities,
      defaultValue: mockIdentity1,
    });

    const newState = mockIdentity2;
    store.set(newState);

    expect(
      window.localStorage.getItem(storeLocalStorageKey.LastUsedIdentities),
    ).toEqual(stringifyJson(newState));
  });

  it("loads initial value from local storage if present", () => {
    const storedState = mockIdentity2;
    window.localStorage.setItem(
      storeLocalStorageKey.LastUsedIdentities,
      stringifyJson(storedState),
    );
    const store = writableStored<LastUsedIdentity>({
      key: storeLocalStorageKey.LastUsedIdentities,
      defaultValue: mockIdentity1,
    });

    expect(get(store)).toEqual(storedState);
  });

  it("loads default value if no value in local storage", () => {
    const defaultValue = mockIdentity1;
    const store = writableStored<LastUsedIdentity>({
      key: storeLocalStorageKey.LastUsedIdentities,
      defaultValue,
    });

    expect(get(store)).toEqual(defaultValue);
  });

  it("should serialize bigint values", () => {
    const defaultValue =
      BigInt(1000000000000000000000000000000000000000000000000000000);
    const store = writableStored({
      key: storeLocalStorageKey.LastUsedIdentities,
      defaultValue,
    });
    expect(get(store)).toEqual(defaultValue);

    const store2 = writableStored({
      key: storeLocalStorageKey.LastUsedIdentities,
      defaultValue: 1,
    });
    expect(get(store2)).toEqual(defaultValue);
  });

  describe("version upgrade", () => {
    it("should replace value from local storage when it has not the same version", () => {
      const storedState = "old-data-string"; // Representing arbitrary old data
      const defaultValue = mockIdentity1;
      window.localStorage.setItem(
        storeLocalStorageKey.LastUsedIdentities,
        stringifyJson({ data: storedState, version: 0 }),
      );
      const store = writableStored<LastUsedIdentity>({
        key: storeLocalStorageKey.LastUsedIdentities,
        defaultValue,
        version: 1,
      });

      expect(get(store)).toEqual(defaultValue);
    });

    it("should replace value from local storage even when it has newer version", () => {
      const storedState = mockIdentity2;
      const defaultValue = mockIdentity1;
      window.localStorage.setItem(
        storeLocalStorageKey.LastUsedIdentities,
        stringifyJson({ data: storedState, version: 2 }),
      );
      const store = writableStored<LastUsedIdentity>({
        key: storeLocalStorageKey.LastUsedIdentities,
        defaultValue,
        version: 1,
      });

      expect(get(store)).toEqual(defaultValue);
      expect(
        localStorage.getItem(storeLocalStorageKey.LastUsedIdentities),
      ).toEqual(
        stringifyJson({
          data: defaultValue,
          version: 1,
        }),
      );
    });

    it("should replace value from local storage even when the default has no version", () => {
      const storedState = mockIdentity2;
      const defaultValue = mockIdentity1;
      window.localStorage.setItem(
        storeLocalStorageKey.LastUsedIdentities,
        stringifyJson({ data: storedState, version: 2 }), // Stored data has a version
      );
      const store = writableStored<LastUsedIdentity>({
        // New store definition has no version
        key: storeLocalStorageKey.LastUsedIdentities,
        defaultValue,
      });

      expect(get(store)).toEqual(defaultValue);
      expect(
        localStorage.getItem(storeLocalStorageKey.LastUsedIdentities),
      ).toEqual(stringifyJson(defaultValue)); // Expect default value because stored had version, new didn't
    });

    it("should not replace value in local storage when no versions provided", () => {
      const storedState = mockIdentity2;
      const defaultValue = mockIdentity1;
      window.localStorage.setItem(
        storeLocalStorageKey.LastUsedIdentities,
        stringifyJson(storedState), // No version in stored data
      );
      const store = writableStored<LastUsedIdentity>({
        // No version in store definition
        key: storeLocalStorageKey.LastUsedIdentities,
        defaultValue,
      });

      expect(get(store)).toEqual(storedState); // Expect stored state because no versions involved
    });

    it("should not replace default value in local storage when same version", () => {
      const storedState = mockIdentity2;
      const defaultValue = mockIdentity1;
      window.localStorage.setItem(
        storeLocalStorageKey.LastUsedIdentities,
        stringifyJson({ data: storedState, version: 5 }),
      );
      const store = writableStored<LastUsedIdentity>({
        key: storeLocalStorageKey.LastUsedIdentities,
        defaultValue,
        version: 5,
      });

      expect(get(store)).toEqual(storedState);
    });
  });

  describe("unsubscribeStorage", () => {
    it("unsubscribes storing in local storage", () => {
      const defaultValue = mockIdentity1;
      const store = writableStored<LastUsedIdentity>({
        key: storeLocalStorageKey.LastUsedIdentities,
        defaultValue,
      });
      const newState = mockIdentity2;
      store.set(newState);
      expect(
        window.localStorage.getItem(storeLocalStorageKey.LastUsedIdentities),
      ).toEqual(stringifyJson(newState));

      store.unsubscribeStorage();
      store.set(defaultValue);
      expect(
        window.localStorage.getItem(storeLocalStorageKey.LastUsedIdentities),
      ).toEqual(stringifyJson(newState));
    });
  });

  it("should resetForTesting", () => {
    const defaultState = mockIdentity1;

    const store = writableStored<LastUsedIdentity>({
      key: storeLocalStorageKey.LastUsedIdentities,
      defaultValue: defaultState,
    });

    expect(get(store)).toEqual(defaultState);

    const newState = mockIdentity2;

    store.set(newState);
    expect(get(store)).toEqual(newState);

    store.resetForTesting();
    expect(get(store)).toEqual(defaultState);
  });
});
