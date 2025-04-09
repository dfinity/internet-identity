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

test("feature flag to be initialized", () => {
  const storage = new MockStorage();
  storage.setItem("c", "true");
  storage.setItem("d", "false");

  // Create store values and setter/getter functions
  let aValue = true;
  let bValue = false;
  let cValue = false;
  let dValue = true;

  const createSetterGetter = (value: boolean) => {
    return {
      set: (newVal: boolean) => {
        value = newVal;
      },
      get: () => value,
    };
  };

  const enabledFlag = new FeatureFlag(
    storage,
    "a",
    true,
    createSetterGetter(aValue).set,
    createSetterGetter(aValue).get,
  );

  const disabledFlag = new FeatureFlag(
    storage,
    "b",
    false,
    createSetterGetter(bValue).set,
    createSetterGetter(bValue).get,
  );

  const storedOverrideFlag = new FeatureFlag(
    storage,
    "c",
    false,
    createSetterGetter(cValue).set,
    createSetterGetter(cValue).get,
  );

  const storedDisabledFlag = new FeatureFlag(
    storage,
    "d",
    true,
    createSetterGetter(dValue).set,
    createSetterGetter(dValue).get,
  );

  expect(enabledFlag.isEnabled()).toEqual(true);
  expect(disabledFlag.isEnabled()).toEqual(false);
  expect(storedOverrideFlag.isEnabled()).toEqual(true);
  expect(storedDisabledFlag.isEnabled()).toEqual(false);
});

test("feature flag to be set", () => {
  const storage = new MockStorage();
  let aValue = true;
  let bValue = false;

  const createSetterGetter = (value: boolean) => {
    return {
      set: (newVal: boolean) => {
        value = newVal;
      },
      get: () => value,
    };
  };

  const enabledFlag = new FeatureFlag(
    storage,
    "a",
    true,
    createSetterGetter(aValue).set,
    createSetterGetter(aValue).get,
  );

  const disabledFlag = new FeatureFlag(
    storage,
    "b",
    false,
    createSetterGetter(bValue).set,
    createSetterGetter(bValue).get,
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
  let aValue = true;
  let bValue = false;

  const createSetterGetter = (value: boolean) => {
    return {
      set: (newVal: boolean) => {
        value = newVal;
      },
      get: () => value,
    };
  };

  const enabledFlag = new FeatureFlag(
    storage,
    "a",
    true,
    createSetterGetter(aValue).set,
    createSetterGetter(aValue).get,
  );

  const disabledFlag = new FeatureFlag(
    storage,
    "b",
    false,
    createSetterGetter(bValue).set,
    createSetterGetter(bValue).get,
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
