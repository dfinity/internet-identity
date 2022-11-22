import { getUserNumber, setUserNumber } from "./userNumber";

testLocalStorage("user number can be set", () => {
  expect(getUserNumber()).toBe(undefined);
  setUserNumber(BigInt(123456));
  expect(getUserNumber()).toBe(BigInt(123456));
});

testLocalStorage("user number can be cleared", () => {
  setUserNumber(BigInt(123456));
  expect(getUserNumber()).toBe(BigInt(123456));
  setUserNumber(undefined);
  expect(getUserNumber()).toBe(undefined);
});

testLocalStorage(
  "user number localStorage format",
  () => {
    setUserNumber(BigInt(123456));
  },
  { after: { userNumber: "123456" } }
);

testLocalStorage(
  "user number read from known localStorage format",
  () => {
    expect(getUserNumber()).toStrictEqual(BigInt(123456));
  },
  { before: { userNumber: "123456" } }
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
