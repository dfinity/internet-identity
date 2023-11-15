import { nonNullish } from "@dfinity/utils";
import { IDBFactory } from "fake-indexeddb";
import {
  clear as idbClear,
  get as idbGet,
  keys as idbKeys,
  set as idbSet,
} from "idb-keyval";
import { MAX_SAVED_ANCHORS, getAnchors, setAnchorUsed } from ".";

beforeAll(() => {
  // Initialize the IndexedDB global
  global.indexedDB = new IDBFactory();
});

test("anchors default to nothing", async () => {
  expect(await getAnchors()).toStrictEqual([]);
});

test(
  "old userNumber is recovered",
  withStorage(
    async () => {
      expect(await getAnchors()).toStrictEqual([BigInt(123456)]);
    },
    { localStorage: { before: { userNumber: "123456" } } }
  )
);

test(
  "old userNumber is not deleted",
  withStorage(
    async () => {
      await getAnchors();
    },
    {
      localStorage: {
        before: { userNumber: "123456" },
        after: (storage) => {
          expect(storage["userNumber"]).toBe("123456");
        },
      },
    }
  )
);

test(
  "old local storage anchors are not deleted",
  withStorage(
    async () => {
      expect(await getAnchors()).toContain(BigInt("123456"));
    },
    {
      localStorage: {
        before: {
          anchors: JSON.stringify({ "123456": { lastUsedTimestamp: 10 } }),
        },
        after: (storage) => {
          const value = storage["anchors"];
          expect(value).toBeDefined();
          const anchors = JSON.parse(value);
          expect(anchors).toBeTypeOf("object");
          expect(anchors["123456"]).toBeDefined();
        },
      },
    }
  )
);

test(
  "reading old userNumber migrates anchors",
  withStorage(
    async () => {
      await getAnchors();
    },
    {
      localStorage: {
        before: { userNumber: "123456" },
        after: (storage) => {
          const value = storage["anchors"];
          expect(value).toBeDefined();
          const anchors = JSON.parse(value);
          expect(anchors).toBeTypeOf("object");
          expect(anchors["123456"]).toBeDefined();
        },
      },
      indexeddb: {
        after: (storage) => {
          // eslint-disable-next-line @typescript-eslint/no-explicit-any
          const anchors: any = storage["anchors"];
          expect(anchors).toBeTypeOf("object");
          expect(anchors["123456"]).toBeDefined();
        },
      },
    }
  )
);

test(
  "one anchor can be stored",
  withStorage(async () => {
    await setAnchorUsed(BigInt(10000));
    expect(await getAnchors()).toStrictEqual([BigInt(10000)]);
  })
);

test(
  "multiple anchors can be stored",
  withStorage(async () => {
    await setAnchorUsed(BigInt(10000));
    await setAnchorUsed(BigInt(10001));
    await setAnchorUsed(BigInt(10003));
    expect(await getAnchors()).toContain(BigInt(10000));
    expect(await getAnchors()).toContain(BigInt(10001));
    expect(await getAnchors()).toContain(BigInt(10003));
  })
);

test(
  "anchors are also written to localstorage",
  withStorage(
    async () => {
      await setAnchorUsed(BigInt(10000));
      await setAnchorUsed(BigInt(10001));
      await setAnchorUsed(BigInt(10003));
    },
    {
      localStorage: {
        after: (storage) => {
          const value = storage["anchors"];
          expect(value).toBeDefined();
          const anchors = JSON.parse(value);
          expect(anchors).toBeTypeOf("object");
          expect(anchors["10000"]).toBeDefined();
          expect(anchors["10001"]).toBeDefined();
          expect(anchors["10003"]).toBeDefined();
        },
      },
    }
  )
);

test(
  "anchors are sorted",
  withStorage(async () => {
    const anchors = [BigInt(10400), BigInt(10001), BigInt(1011003)];
    for (const anchor of anchors) {
      await setAnchorUsed(anchor);
    }
    anchors.sort();
    expect(await getAnchors()).toStrictEqual(anchors);
  })
);

test(
  "only N anchors are stored",
  withStorage(async () => {
    for (let i = 0; i < MAX_SAVED_ANCHORS + 5; i++) {
      await setAnchorUsed(BigInt(i));
    }
    expect((await getAnchors()).length).toStrictEqual(MAX_SAVED_ANCHORS);
  })
);

test(
  "old anchors are dropped",
  withStorage(async () => {
    vi.useFakeTimers().setSystemTime(new Date(0));
    await setAnchorUsed(BigInt(10000));
    vi.useFakeTimers().setSystemTime(new Date(1));
    await setAnchorUsed(BigInt(203000));
    vi.useFakeTimers().setSystemTime(new Date(2));
    for (let i = 0; i < MAX_SAVED_ANCHORS; i++) {
      await setAnchorUsed(BigInt(i));
    }
    expect(await getAnchors()).not.toContain(BigInt(10000));
    expect(await getAnchors()).not.toContain(BigInt(203000));
    vi.useRealTimers();
  })
);

test(
  "unknown fields are not dropped",
  withStorage(
    async () => {
      vi.useFakeTimers().setSystemTime(new Date(20));
      await setAnchorUsed(BigInt(10000));
      vi.useRealTimers();
    },
    {
      indexeddb: {
        before: {
          anchors: {
            "10000": { lastUsedTimestamp: 10, hello: "world" },
          },
        },
        after: {
          anchors: {
            "10000": { lastUsedTimestamp: 20, hello: "world" },
          },
        },
      },
    }
  )
);

/** Test storage usage. Storage is cleared after the callback has returned.
 * If `before` is specified, storage is populated with its content before the test is run.
 * If `after` is specified, the content of storage are checked against `after` after the
 * test is run and before storage is cleared.
 * If `after` is a function, the function is called with the content of the storage.
 */
function withStorage(
  fn: () => void | Promise<void>,
  opts?: {
    localStorage?: {
      before?: LocalStorage;
      after?: LocalStorage | ((storage: LocalStorage) => void);
    };
    indexeddb?: {
      before?: IndexedDB;
      after?: IndexedDB | ((storage: IndexedDB) => void);
    };
  }
): () => Promise<void> {
  return async () => {
    localStorage.clear();
    const lsBefore = opts?.localStorage?.before;
    if (nonNullish(lsBefore)) {
      setLocalStorage(lsBefore);
    }

    await idbClear();
    const idbBefore = opts?.indexeddb?.before;
    if (nonNullish(idbBefore)) {
      await setIndexedDB(idbBefore);
    }

    await fn();

    // Check the IndexedDB "after"

    const idbAfter = opts?.indexeddb?.after;
    if (nonNullish(idbAfter)) {
      const actual: IndexedDB = await readIndexedDB();

      if (typeof idbAfter === "function") {
        idbAfter(actual);
      } else {
        const expected: IndexedDB = idbAfter;
        expect(actual).toStrictEqual(expected);
      }
    }

    // Remove all entries
    // (cannot just reset global.indexeddb because idb-keyval stores a pointer to the DB)
    await idbClear();

    // Check the localStorage "after"

    const lsAfter = opts?.localStorage?.after;
    if (nonNullish(lsAfter)) {
      const actual: LocalStorage = readLocalStorage();

      if (typeof lsAfter === "function") {
        lsAfter(actual);
      } else {
        const expected: LocalStorage = lsAfter;
        expect(actual).toStrictEqual(expected);
      }
    }
    localStorage.clear();
  };
}

/// Type representing the whole localStorage, used for tests.
type LocalStorage = Record<string, string>;

/// Set the entire localStorage (does NOT clear localStorage)
function setLocalStorage(ls: LocalStorage) {
  for (const key in ls) {
    localStorage.setItem(key, ls[key]);
  }
}

function readLocalStorage(): LocalStorage {
  const ls: LocalStorage = {};
  for (let i = 0, len = localStorage.length; i < len; ++i) {
    const key = localStorage.key(i);
    if (key !== null) {
      ls[key] = localStorage.getItem(key) as string;
    }
  }

  return ls;
}

/** Indexed DB */

type IndexedDB = Record<string, unknown>;

const setIndexedDB = async (db: IndexedDB) => {
  for (const key in db) {
    await idbSet(key, db[key]);
  }
};

const readIndexedDB = async (): Promise<IndexedDB> => {
  const db: IndexedDB = {};

  for (const k of await idbKeys()) {
    if (typeof k !== "string") {
      throw new Error("Bad type");
    }
    db[k] = await idbGet(k);
  }
  return db;
};
