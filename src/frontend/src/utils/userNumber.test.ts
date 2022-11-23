import { AnchorStore, anchorStore, MAX_SAVED_ANCHORS } from "./userNumber";

testLocalStorage("anchors default to nothing", (store) => {
  expect(store.getAnchors()).toStrictEqual([]);
});

testLocalStorage(
  "old userNumber is recovered",
  (store) => {
    expect(store.getAnchors()).toStrictEqual([BigInt(123456)]);
  },
  { before: { userNumber: "123456" } }
);

testLocalStorage("one anchor can be stored", (store) => {
  store.setAnchorUsed(BigInt(10000));
  expect(store.getAnchors()).toStrictEqual([BigInt(10000)]);
});

testLocalStorage("multiple anchors can be stored", (store) => {
  store.setAnchorUsed(BigInt(10000));
  store.setAnchorUsed(BigInt(10001));
  store.setAnchorUsed(BigInt(10003));
  expect(store.getAnchors()).toContain(BigInt(10000));
  expect(store.getAnchors()).toContain(BigInt(10001));
  expect(store.getAnchors()).toContain(BigInt(10003));
});

testLocalStorage("anchors are sorted", (store) => {
  const anchors = [BigInt(10400), BigInt(10001), BigInt(1011003)];
  for (const anchor of anchors) {
    store.setAnchorUsed(anchor);
  }
  anchors.sort();
  expect(store.getAnchors()).toStrictEqual(anchors);
});

testLocalStorage("only N anchors are stored", (store) => {
  for (let i = 0; i < MAX_SAVED_ANCHORS + 5; i++) {
    store.setAnchorUsed(BigInt(i));
  }
  expect(store.getAnchors().length).toStrictEqual(MAX_SAVED_ANCHORS);
});

testLocalStorage("old anchors are dropped", (store) => {
  store.setAnchorUsed(BigInt(10000));
  store.setAnchorUsed(BigInt(203000));
  for (let i = 0; i < MAX_SAVED_ANCHORS + 2; i++) {
    store.setAnchorUsed(BigInt(i));
  }
  expect(store.getAnchors()).not.toContain(BigInt(10000));
  expect(store.getAnchors()).not.toContain(BigInt(203000));
});

testLocalStorage(
  "unknown fields are not dropped",
  (store) => {
    store.setAnchorUsed(BigInt(10000));
  },
  {
    before: {
      anchors: JSON.stringify({ "10000": { unusedCount: 10, hello: "world" } }),
    },
    after: {
      anchors: JSON.stringify({ "10000": { unusedCount: 0, hello: "world" } }),
    },
  }
);

/** Run a test that makes use of localStorage. Local storage is always cleared after
 * the test was run.
 * If `before` is specified, local storage is populated with its content before the test is run.
 * If `after` is specified, the content of local storage are checked against `after` after the
 * test is run and before local storage is cleared.
 */
function testLocalStorage(
  name: string,
  fn: (store: AnchorStore) => void,
  opts?: { before?: LocalStorage; after?: LocalStorage }
) {
  test(name, () => {
    localStorage.clear();
    if (opts?.before !== undefined) {
      setLocalStorage(opts.before);
    }
    fn(anchorStore());
    if (opts?.after !== undefined) {
      const actual: LocalStorage = readLocalStorage();
      const expected: LocalStorage = opts.after;
      expect(actual).toStrictEqual(expected);
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
  let ls: LocalStorage = {};
  for (let i = 0, len = localStorage.length; i < len; ++i) {
    const key = localStorage.key(i);
    if (key !== null) {
      ls[key] = localStorage.getItem(key) as string;
    }
  }

  return ls;
}
