import { browser } from "$app/environment";
import { jsonReplacer, jsonReviver, nonNullish } from "@dfinity/utils";
import { writable, type Unsubscriber, type Writable } from "svelte/store";
import { type StoreLocalStorageKey } from "$lib/constants/store.constants";

type WritableStored<T> = Writable<T> & {
  unsubscribeStorage: Unsubscriber;
  resetForTesting: () => void;
};

type VersionedData<T> = { data: T | undefined; version: number | undefined };

/** Returns true when the value has numeric version and data fields. */
const isVersionedData = <T>(
  data: T | VersionedData<T>,
): data is VersionedData<T> =>
  nonNullish(data) &&
  typeof data === "object" &&
  getVersion(data) !== undefined &&
  "data" in data;

/** Writes the state (w/ or w/o the version) to local storage. */
const writeData = <T>({
  key,
  data,
  version,
}: {
  key: StoreLocalStorageKey;
  data: T;
  version?: number;
}) => {
  // Do not break UI if local storage fails
  try {
    // Data structure  `{ data, version }` vs `data` depends on version absence.
    const storeData: T | VersionedData<T> = nonNullish(version)
      ? { data, version }
      : data;
    localStorage.setItem(key, JSON.stringify(storeData, jsonReplacer));
  } catch (error: unknown) {
    console.error(error);
  }
};

/** Returns the version field of the value if it has one, otherwise undefined. */
const getVersion = <T>(data: T | VersionedData<T>): number | undefined => {
  if (typeof data === "object" && data !== null && "version" in data) {
    const version = Number(
      (data as unknown as { version: number | string }).version,
    );
    if (!isNaN(version)) {
      return version;
    }
  }
  // default
  return undefined;
};

/** Reads the state (w/ or w/o the version) from local storage and returns the versioned state. */
const readData = <T>(key: StoreLocalStorageKey): VersionedData<T> => {
  // Do not break UI if local storage fails
  try {
    const storedValue = localStorage.getItem(key);
    // `theme` stored "undefined" string which is not a valid JSON string.
    if (
      storedValue !== null &&
      storedValue !== undefined &&
      storedValue !== "undefined"
    ) {
      const parsedValue = JSON.parse(storedValue, jsonReviver);

      if (isVersionedData(parsedValue)) {
        return parsedValue;
      }

      return { data: parsedValue, version: undefined };
    }
  } catch (error: unknown) {
    console.error(error);
  }

  return { data: undefined, version: undefined };
};

export const writableStored = <T>({
  key,
  defaultValue,
  version: defaultVersion,
}: {
  key: StoreLocalStorageKey;
  defaultValue: T;
  version?: number;
}): WritableStored<T> => {
  const getInitialValue = (): VersionedData<T> => {
    if (!browser) {
      return { data: defaultValue, version: defaultVersion };
    }

    const storedValue = readData<T>(key);
    // Use always the default value when the versions do not match.
    if (
      storedValue.data === undefined ||
      defaultVersion !== storedValue.version
    ) {
      return { data: defaultValue, version: defaultVersion };
    }

    return storedValue;
  };

  const initialValue = getInitialValue();
  const store = writable<T>(initialValue.data);

  const unsubscribeStorage = store.subscribe((store: T) => {
    if (!browser) {
      return;
    }

    writeData({ key, data: store, version: initialValue.version });
  });

  return {
    ...store,
    unsubscribeStorage,
    resetForTesting: () => {
      store.set(defaultValue);
    },
  };
};
