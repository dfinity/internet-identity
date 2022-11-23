import { getAnchors, setAnchorUsed, MAX_SAVED_ANCHORS } from "./userNumber";

testLocalStorage("anchors default to nothing", () => {
  expect(getAnchors()).toStrictEqual([]);
});

testLocalStorage(
  "old userNumber is recovered",
  () => {
    expect(getAnchors()).toStrictEqual([BigInt(123456)]);
  },
  { before: { userNumber: "123456" } }
);

testLocalStorage("one anchor can be stored", () => {
  setAnchorUsed(BigInt(10000));
  expect(getAnchors()).toStrictEqual([BigInt(10000)]);
});

testLocalStorage("multiple anchors can be stored", () => {
  setAnchorUsed(BigInt(10000));
  setAnchorUsed(BigInt(10001));
  setAnchorUsed(BigInt(10003));
  expect(getAnchors()).toContain(BigInt(10000));
  expect(getAnchors()).toContain(BigInt(10001));
  expect(getAnchors()).toContain(BigInt(10003));
});

testLocalStorage("anchors are sorted", () => {
  const anchors = [BigInt(10400), BigInt(10001), BigInt(1011003)];
  for (const anchor of anchors) {
    setAnchorUsed(anchor);
  }
  anchors.sort();
  expect(getAnchors()).toStrictEqual(anchors);
});

testLocalStorage("only N anchors are stored", () => {
  for (let i = 0; i < MAX_SAVED_ANCHORS + 5; i++) {
    setAnchorUsed(BigInt(i));
  }
  expect(getAnchors().length).toStrictEqual(MAX_SAVED_ANCHORS);
});

testLocalStorage("old anchors are dropped", () => {
  setAnchorUsed(BigInt(10000));
  setAnchorUsed(BigInt(203000));
  for (let i = 0; i < MAX_SAVED_ANCHORS + 2; i++) {
    setAnchorUsed(BigInt(i));
  }
  expect(getAnchors()).not.toContain(BigInt(10000));
  expect(getAnchors()).not.toContain(BigInt(203000));
});

testLocalStorage(
  "unknown fields are not dropped",
  () => {
    setAnchorUsed(BigInt(10000));
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
  fn: () => void,
  opts?: { before?: LocalStorage; after?: LocalStorage }
) {
  test(name, () => {
    localStorage.clear();
    if (opts?.before !== undefined) {
      setLocalStorage(opts.before);
    }
    fn();
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
