import { FeatureFlag } from "$lib/utils/featureFlags/index";

class MockStorage {
  #data: Record<string, string> = {};

  getItem(key: string): string | null {
    return this.#data[key] ?? null;
  }

  setItem(key: string, value: string): void {
    this.#data[key] = value;
  }

  removeItem(key: string): void {
    delete this.#data[key];
  }
}

const createStore = (initialValue: boolean) => {
  const store = {
    value: initialValue,
    set: function (newVal: boolean) {
      this.value = newVal;
    },
    get: function () {
      return this.value;
    },
  };
  return store;
};

test("feature flag to be initialized", () => {
  const storage = new MockStorage();
  storage.setItem("c", "true");
  storage.setItem("d", "false");

  const aStore = createStore(true);
  const bStore = createStore(false);
  const cStore = createStore(false);
  const dStore = createStore(true);

  const enabledFlag = new FeatureFlag(
    storage,
    "a",
    true,
    aStore.set,
    aStore.get,
  );

  const disabledFlag = new FeatureFlag(
    storage,
    "b",
    false,
    bStore.set,
    bStore.get,
  );

  const storedOverrideFlag = new FeatureFlag(
    storage,
    "c",
    false,
    cStore.set,
    cStore.get,
  );

  const storedDisabledFlag = new FeatureFlag(
    storage,
    "d",
    true,
    dStore.set,
    dStore.get,
  );

  expect(enabledFlag.isEnabled()).toEqual(true);
  expect(disabledFlag.isEnabled()).toEqual(false);
  expect(storedOverrideFlag.isEnabled()).toEqual(true);
  expect(storedDisabledFlag.isEnabled()).toEqual(false);
});

test("feature flag to be set", () => {
  const storage = new MockStorage();
  const aStore = createStore(true);
  const bStore = createStore(false);

  const enabledFlag = new FeatureFlag(
    storage,
    "a",
    true,
    aStore.set,
    aStore.get,
  );

  const disabledFlag = new FeatureFlag(
    storage,
    "b",
    false,
    bStore.set,
    bStore.get,
  );

  enabledFlag.set(false);
  disabledFlag.set(true);

  expect(enabledFlag.isEnabled()).toEqual(false);
  expect(disabledFlag.isEnabled()).toEqual(true);
  expect(storage.getItem("a")).toEqual("false");
  expect(storage.getItem("b")).toEqual("true");
});

test("feature flag to be reset", () => {
  const storage = new MockStorage();
  const aStore = createStore(true);
  const bStore = createStore(false);

  const enabledFlag = new FeatureFlag(
    storage,
    "a",
    true,
    aStore.set,
    aStore.get,
  );

  const disabledFlag = new FeatureFlag(
    storage,
    "b",
    false,
    bStore.set,
    bStore.get,
  );

  enabledFlag.set(false);
  disabledFlag.set(true);
  enabledFlag.reset();
  disabledFlag.reset();

  expect(enabledFlag.isEnabled()).toEqual(true);
  expect(disabledFlag.isEnabled()).toEqual(false);
  expect(storage.getItem("a")).toEqual(null);
  expect(storage.getItem("b")).toEqual(null);
});
