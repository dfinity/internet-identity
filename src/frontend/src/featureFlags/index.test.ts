import { FeatureFlag } from "$src/featureFlags/index";

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

  const enabledFlag = new FeatureFlag(storage, "a", true);
  const disabledFlag = new FeatureFlag(storage, "b", false);
  const storedOverrideFlag = new FeatureFlag(storage, "c", false);
  const storedDisabledFlag = new FeatureFlag(storage, "d", true);

  expect(enabledFlag.isEnabled()).toEqual(true);
  expect(disabledFlag.isEnabled()).toEqual(false);
  expect(storedOverrideFlag.isEnabled()).toEqual(true);
  expect(storedDisabledFlag.isEnabled()).toEqual(false);
});

test("feature flag to be set", () => {
  const storage = new MockStorage();
  const enabledFlag = new FeatureFlag(storage, "a", true);
  const disabledFlag = new FeatureFlag(storage, "b", false);

  enabledFlag.set(false);
  disabledFlag.set(true);

  expect(enabledFlag.isEnabled()).toEqual(false);
  expect(disabledFlag.isEnabled()).toEqual(true);
  expect(storage.getItem("a")).toEqual("false");
  expect(storage.getItem("b")).toEqual("true");
});

test("feature flag to be reset", () => {
  const storage = new MockStorage();
  const enabledFlag = new FeatureFlag(storage, "a", true);
  const disabledFlag = new FeatureFlag(storage, "b", false);

  enabledFlag.set(false);
  disabledFlag.set(true);
  enabledFlag.reset();
  disabledFlag.reset();

  expect(enabledFlag.isEnabled()).toEqual(true);
  expect(disabledFlag.isEnabled()).toEqual(false);
  expect(storage.getItem("a")).toEqual(null);
  expect(storage.getItem("b")).toEqual(null);
});
