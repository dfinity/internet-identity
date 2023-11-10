import { nonNullish } from "@dfinity/utils";
import { MAX_SAVED_ANCHORS, getAnchors, setAnchorUsed } from ".";

test("anchors default to nothing", () => {
  expect(getAnchors()).toStrictEqual([]);
});

test(
  "old userNumber is recovered",
  withStorage(
    () => {
      expect(getAnchors()).toStrictEqual([BigInt(123456)]);
    },
    { localStorage: { before: { userNumber: "123456" } } }
  )
);

test(
  "old userNumber is not deleted",
  withStorage(
    () => {
      getAnchors();
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
  "reading old userNumber migrates anchors",
  withStorage(
    () => {
      getAnchors();
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
    }
  )
);

test(
  "one anchor can be stored",
  withStorage(() => {
    setAnchorUsed(BigInt(10000));
    expect(getAnchors()).toStrictEqual([BigInt(10000)]);
  })
);

test(
  "multiple anchors can be stored",
  withStorage(() => {
    setAnchorUsed(BigInt(10000));
    setAnchorUsed(BigInt(10001));
    setAnchorUsed(BigInt(10003));
    expect(getAnchors()).toContain(BigInt(10000));
    expect(getAnchors()).toContain(BigInt(10001));
    expect(getAnchors()).toContain(BigInt(10003));
  })
);

test(
  "anchors are sorted",
  withStorage(() => {
    const anchors = [BigInt(10400), BigInt(10001), BigInt(1011003)];
    for (const anchor of anchors) {
      setAnchorUsed(anchor);
    }
    anchors.sort();
    expect(getAnchors()).toStrictEqual(anchors);
  })
);

test(
  "only N anchors are stored",
  withStorage(() => {
    for (let i = 0; i < MAX_SAVED_ANCHORS + 5; i++) {
      setAnchorUsed(BigInt(i));
    }
    expect(getAnchors().length).toStrictEqual(MAX_SAVED_ANCHORS);
  })
);

test(
  "old anchors are dropped",
  withStorage(() => {
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
  })
);

test(
  "unknown fields are not dropped",
  withStorage(
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
        },
        after: {
          anchors: JSON.stringify({
            "10000": { lastUsedTimestamp: 20, hello: "world" },
          }),
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
  fn: () => void,
  opts?: {
    localStorage?: {
      before?: LocalStorage;
      after?: LocalStorage | ((storage: LocalStorage) => void);
    };
  }
): () => void {
  return () => {
    localStorage.clear();
    const before = opts?.localStorage?.before;
    if (nonNullish(before)) {
      setLocalStorage(before);
    }
    fn();
    const after = opts?.localStorage?.after;
    if (nonNullish(after)) {
      const actual: LocalStorage = readLocalStorage();

      if (typeof after === "function") {
        after(actual);
      } else {
        const expected: LocalStorage = after;
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
  let ls: LocalStorage = {};
  for (let i = 0, len = localStorage.length; i < len; ++i) {
    const key = localStorage.key(i);
    if (key !== null) {
      ls[key] = localStorage.getItem(key) as string;
    }
  }

  return ls;
}
