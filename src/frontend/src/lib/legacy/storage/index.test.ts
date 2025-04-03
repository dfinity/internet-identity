import { Principal } from "@dfinity/principal";
import { nonNullish } from "@dfinity/utils";
import { IDBFactory } from "fake-indexeddb";
import {
  clear as idbClear,
  get as idbGet,
  keys as idbKeys,
  set as idbSet,
} from "idb-keyval";
import { expect } from "vitest";
import {
  MAX_SAVED_ANCHORS,
  MAX_SAVED_PRINCIPALS,
  getAnchorByPrincipal,
  getAnchorIfLastUsed,
  getAnchors,
  setAnchorUsed,
  setKnownPrincipal,
} from "./index";

beforeAll(() => {
  // Initialize the IndexedDB global
  global.indexedDB = new IDBFactory();
});

test("anchors default to nothing", async () => {
  expect(await getAnchors()).toStrictEqual([]);
});

test(
  "old userNumber V0 is recovered",
  withStorage(
    async () => {
      expect(await getAnchors()).toStrictEqual([BigInt(123456)]);
    },
    { localStorage: { before: { userNumber: "123456" } } },
  ),
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
    },
  ),
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
    },
  ),
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
      },
      indexeddb: {
        after: (storage) => {
          // Written to V4
          // eslint-disable-next-line @typescript-eslint/no-explicit-any
          const storageV4: any = storage["ii-storage-v4"];
          expect(storageV4).toBeTypeOf("object");

          // eslint-disable-next-line @typescript-eslint/no-explicit-any
          const anchorsV4: any = storageV4["anchors"];
          expect(anchorsV4).toBeTypeOf("object");
          expect(anchorsV4["123456"]).toBeDefined();
        },
      },
    },
  ),
);

test(
  "one anchor can be stored",
  withStorage(async () => {
    await setAnchorUsed(BigInt(10000));
    expect(await getAnchors()).toStrictEqual([BigInt(10000)]);
  }),
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
  }),
);

test(
  "V2 anchors are migrated",
  withStorage(
    async () => {
      expect(await getAnchors()).toContain(BigInt(10000));
      expect(await getAnchors()).toContain(BigInt(10001));
      expect(await getAnchors()).toContain(BigInt(10003));
    },
    {
      indexeddb: {
        before: {
          /* V2 layout */
          anchors: {
            "10000": { lastUsedTimestamp: 0 },
            "10001": { lastUsedTimestamp: 0 },
            "10003": { lastUsedTimestamp: 0 },
          },
        },
      },
    },
  ),
);

test(
  "V3 anchors are migrated",
  withStorage(
    async () => {
      expect(await getAnchors()).toContain(BigInt(10000));
      expect(await getAnchors()).toContain(BigInt(10001));
      expect(await getAnchors()).toContain(BigInt(10003));
    },
    {
      indexeddb: {
        before: {
          /* V3 layout */
          "ii-storage-v3": {
            anchors: {
              "10000": { lastUsedTimestamp: 0, knownPrincipals: [] },
              "10001": { lastUsedTimestamp: 0, knownPrincipals: [] },
              "10003": { lastUsedTimestamp: 0, knownPrincipals: [] },
            },
            hasher: await crypto.subtle.generateKey(
              { name: "HMAC", hash: "SHA-512" },
              false /* not extractable */,
              ["sign"] /* only used to "sign" (e.g. produce a digest ) */,
            ),
          },
        },
        after: (storage) => {
          // Written to V4
          // eslint-disable-next-line @typescript-eslint/no-explicit-any
          const storageV4: any = storage["ii-storage-v4"];
          expect(storageV4).toBeTypeOf("object");
          expect(storageV4.anchors["10000"]).toBeDefined();
          expect(storageV4.anchors["10001"]).toBeDefined();
          expect(storageV4.anchors["10003"]).toBeDefined();
        },
      },
    },
  ),
);

test(
  "anchors are also written to V3",
  withStorage(
    async () => {
      await setAnchorUsed(BigInt(10000));
      await setAnchorUsed(BigInt(10001));
      await setAnchorUsed(BigInt(10003));
    },
    {
      indexeddb: {
        after: (storage) => {
          // Written to V3
          // eslint-disable-next-line @typescript-eslint/no-explicit-any
          const storageV3: any = storage["ii-storage-v3"];
          expect(storageV3).toBeTypeOf("object");
          expect(storageV3.anchors["10000"]).toBeDefined();
          expect(storageV3.anchors["10001"]).toBeDefined();
          expect(storageV3.anchors["10003"]).toBeDefined();
        },
      },
    },
  ),
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
  }),
);

test(
  "only N anchors are stored",
  withStorage(async () => {
    for (let i = 0; i < MAX_SAVED_ANCHORS + 5; i++) {
      await setAnchorUsed(BigInt(i));
    }
    expect((await getAnchors()).length).toStrictEqual(MAX_SAVED_ANCHORS);
  }),
);

test(
  "principal digests are stored",
  withStorage(async () => {
    const origin = "https://example.com";
    const principal = Principal.fromText("2vxsx-fae");
    await setKnownPrincipal({
      userNumber: BigInt(10000),
      origin,
      principal,
    });

    expect(await getAnchorByPrincipal({ principal })).toBe(BigInt(10000));
  }),
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
  }),
);

test(
  "old principals are dropped",
  withStorage(async () => {
    const userNumber = BigInt(10000);
    const principal = Principal.fromText(
      "hawxh-fq2bo-p5sh7-mmgol-l3vtr-f72w2-q335t-dcbni-2n25p-xhusp-fqe",
    );
    const oldPrincipal = Principal.fromText(
      "lrf2i-zba54-pygwt-tbi75-zvlz4-7gfhh-ylcrq-2zh73-6brgn-45jy5-cae",
    );
    const veryOldPrincipal = Principal.fromText(
      "t6s4t-dzahw-w2cqi-csimw-vn4xf-fjte4-doj3g-gyjxt-6v2uk-cjeft-eae",
    );
    const origin = "https://origin.com";
    vi.useFakeTimers().setSystemTime(new Date(0));
    await setKnownPrincipal({
      userNumber,
      principal: veryOldPrincipal,
      origin,
    });
    vi.useFakeTimers().setSystemTime(new Date(1));
    await setKnownPrincipal({ userNumber, principal: oldPrincipal, origin });
    let date = 2;
    vi.useFakeTimers().setSystemTime(new Date(date));
    for (let i = 0; i < MAX_SAVED_PRINCIPALS; i++) {
      date++;
      vi.useFakeTimers().setSystemTime(new Date(date));
      await setKnownPrincipal({
        userNumber,
        principal: Principal.fromUint8Array(
          window.crypto.getRandomValues(new Uint8Array(29)),
        ),
        origin: `https://new${i}.com`,
      });
    }
    date++;
    vi.useFakeTimers().setSystemTime(new Date(date));
    await setKnownPrincipal({ userNumber, principal, origin });
    expect(
      await getAnchorByPrincipal({ principal: veryOldPrincipal }),
    ).not.toBeDefined();
    expect(
      await getAnchorByPrincipal({ principal: oldPrincipal }),
    ).not.toBeDefined();
    expect(await getAnchorByPrincipal({ principal })).toBeDefined();
    vi.useRealTimers();
  }),
);

test(
  "should retrieve last anchor",
  withStorage(async () => {
    const origin = "https://example.com";
    const principal = Principal.fromText("2vxsx-fae");
    await setKnownPrincipal({
      userNumber: BigInt(10000),
      origin,
      principal,
    });

    expect(await getAnchorIfLastUsed({ principal, origin })).toBe(
      BigInt(10000),
    );
  }),
);

test(
  "should not retrieve anchor if not most recent",
  withStorage(async () => {
    const origin = "https://example.com";
    const oldPrincipal = Principal.fromText(
      "hawxh-fq2bo-p5sh7-mmgol-l3vtr-f72w2-q335t-dcbni-2n25p-xhusp-fqe",
    );
    vi.useFakeTimers().setSystemTime(new Date(0));
    const oldIdentity = BigInt(10000);
    await setKnownPrincipal({
      userNumber: oldIdentity,
      origin,
      principal: oldPrincipal,
    });

    vi.useFakeTimers().setSystemTime(new Date(1));
    const mostRecentPrincipal = Principal.fromText(
      "lrf2i-zba54-pygwt-tbi75-zvlz4-7gfhh-ylcrq-2zh73-6brgn-45jy5-cae",
    );
    const mostRecentIdentity = BigInt(10001);
    await setKnownPrincipal({
      userNumber: mostRecentIdentity,
      origin,
      principal: mostRecentPrincipal,
    });

    expect(
      await getAnchorIfLastUsed({ principal: oldPrincipal, origin }),
    ).not.toBeDefined();
    // most recent principal still works
    expect(
      await getAnchorIfLastUsed({ principal: mostRecentPrincipal, origin }),
    ).toBe(mostRecentIdentity);
  }),
);

test(
  "latest principals on different origins can be retrieved",
  withStorage(async () => {
    const origin1 = "https://example1.com";
    const origin2 = "https://example2.com";
    const principal1 = Principal.fromText(
      "hawxh-fq2bo-p5sh7-mmgol-l3vtr-f72w2-q335t-dcbni-2n25p-xhusp-fqe",
    );
    vi.useFakeTimers().setSystemTime(new Date(0));
    const identity1 = BigInt(10000);
    await setKnownPrincipal({
      userNumber: identity1,
      origin: origin1,
      principal: principal1,
    });

    vi.useFakeTimers().setSystemTime(new Date(1));
    const principal2 = Principal.fromText(
      "lrf2i-zba54-pygwt-tbi75-zvlz4-7gfhh-ylcrq-2zh73-6brgn-45jy5-cae",
    );
    const identity2 = BigInt(10001);
    await setKnownPrincipal({
      userNumber: identity2,
      origin: origin2,
      principal: principal2,
    });

    expect(
      await getAnchorIfLastUsed({ principal: principal1, origin: origin1 }),
    ).toBe(identity1);
    expect(
      await getAnchorIfLastUsed({ principal: principal2, origin: origin2 }),
    ).toBe(identity2);
  }),
);

test(
  "should not retrieve unknown principal",
  withStorage(async () => {
    const origin = "https://example.com";
    const principal = Principal.fromText(
      "hawxh-fq2bo-p5sh7-mmgol-l3vtr-f72w2-q335t-dcbni-2n25p-xhusp-fqe",
    );

    expect(await getAnchorIfLastUsed({ principal, origin })).not.toBeDefined();
  }),
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
  },
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
