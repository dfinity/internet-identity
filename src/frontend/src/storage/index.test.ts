import { nonNullish } from "@dfinity/utils";
<<<<<<< HEAD
import { vi } from "vitest";
import { getAnchors, MAX_SAVED_ANCHORS, setAnchorUsed } from ".";

testStorage("anchors default to nothing", () => {
  expect(getAnchors()).toStrictEqual([]);
=======
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
>>>>>>> main
});

testStorage(
  "old userNumber is recovered",
<<<<<<< HEAD
  () => {
    expect(getAnchors()).toStrictEqual([BigInt(123456)]);
  },
  { localStorage: { before: { userNumber: "123456" } } }
);

testStorage(
  "reading old userNumber migrates anchors",
  () => {
    vi.useFakeTimers().setSystemTime(new Date(20));
    getAnchors();
    vi.useRealTimers();
  },
  {
    localStorage: {
      before: { userNumber: "123456" },
      after: {
        anchors: JSON.stringify({
          "123456": { lastUsedTimestamp: 20 },
        }),
        userNumber: "123456",
      },
    },
  }
);

testStorage("one anchor can be stored", () => {
  setAnchorUsed(BigInt(10000));
  expect(getAnchors()).toStrictEqual([BigInt(10000)]);
});

testStorage("multiple anchors can be stored", () => {
  setAnchorUsed(BigInt(10000));
  setAnchorUsed(BigInt(10001));
  setAnchorUsed(BigInt(10003));
  expect(getAnchors()).toContain(BigInt(10000));
  expect(getAnchors()).toContain(BigInt(10001));
  expect(getAnchors()).toContain(BigInt(10003));
});

testStorage("anchors are sorted", () => {
  const anchors = [BigInt(10400), BigInt(10001), BigInt(1011003)];
  for (const anchor of anchors) {
    setAnchorUsed(anchor);
  }
  anchors.sort();
  expect(getAnchors()).toStrictEqual(anchors);
});

testStorage("only N anchors are stored", () => {
  for (let i = 0; i < MAX_SAVED_ANCHORS + 5; i++) {
    setAnchorUsed(BigInt(i));
  }
  expect(getAnchors().length).toStrictEqual(MAX_SAVED_ANCHORS);
});

testStorage("old anchors are dropped", () => {
  vi.useFakeTimers().setSystemTime(new Date(0));
  setAnchorUsed(BigInt(10000));
  vi.useFakeTimers().setSystemTime(new Date(1));
  setAnchorUsed(BigInt(203000));
  vi.useFakeTimers().setSystemTime(new Date(2));
  for (let i = 0; i < MAX_SAVED_ANCHORS; i++) {
    setAnchorUsed(BigInt(i));
  }
  expect(getAnchors()).not.toContain(BigInt(10000));
  expect(getAnchors()).not.toContain(BigInt(203000));
  vi.useRealTimers();
});
=======
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
>>>>>>> main

testStorage(
  "unknown fields are not dropped",
<<<<<<< HEAD
  () => {
    vi.useFakeTimers().setSystemTime(new Date(20));
    setAnchorUsed(BigInt(10000));
    vi.useRealTimers();
  },
  {
    localStorage: {
      before: {
        anchors: JSON.stringify({
          "10000": { lastUsedTimestamp: 10, hello: "world" },
        }),
=======
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
>>>>>>> main
      },
      after: {
        anchors: JSON.stringify({
          "10000": { lastUsedTimestamp: 20, hello: "world" },
        }),
      },
    },
  }
);

/** Run a test that makes use of localStorage. Local storage is always cleared after
 * the test was run.
 * If `before` is specified, local storage is populated with its content before the test is run.
 * If `after` is specified, the content of local storage are checked against `after` after the
 * test is run and before local storage is cleared.
 */
<<<<<<< HEAD
function testStorage(
  name: string,
  fn: () => void,
  opts?: { localStorage?: { before?: LocalStorage; after?: LocalStorage } }
) {
  test(name, () => {
=======
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
>>>>>>> main
    localStorage.clear();
    const lsBefore = opts?.localStorage?.before;
    if (nonNullish(lsBefore)) {
      setLocalStorage(lsBefore);
    }
<<<<<<< HEAD
    fn();
    const after = opts?.localStorage?.after;
    if (nonNullish(after)) {
      const actual: LocalStorage = readLocalStorage();
      const expected: LocalStorage = after;
      expect(actual).toStrictEqual(expected);
=======

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
>>>>>>> main
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
  });
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
