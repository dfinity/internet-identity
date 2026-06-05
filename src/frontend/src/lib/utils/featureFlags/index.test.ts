import { FeatureFlag } from "$lib/utils/featureFlags/index";
import { writable } from "svelte/store";

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

  const aStore = writable(true);
  const bStore = writable(false);
  const cStore = writable(false);
  const dStore = writable(true);

  const enabledFlag = new FeatureFlag(storage, "a", true, aStore);

  const disabledFlag = new FeatureFlag(storage, "b", false, bStore);

  const storedOverrideFlag = new FeatureFlag(storage, "c", false, cStore);

  const storedDisabledFlag = new FeatureFlag(storage, "d", true, dStore);

  expect(enabledFlag.isEnabled()).toEqual(true);
  expect(disabledFlag.isEnabled()).toEqual(false);
  expect(storedOverrideFlag.isEnabled()).toEqual(true);
  expect(storedDisabledFlag.isEnabled()).toEqual(false);
});

test("feature flag to be set", () => {
  const storage = new MockStorage();
  const aStore = writable(true);
  const bStore = writable(false);

  const enabledFlag = new FeatureFlag(storage, "a", true, aStore);

  const disabledFlag = new FeatureFlag(storage, "b", false, bStore);

  enabledFlag.set(false);
  disabledFlag.set(true);

  expect(enabledFlag.isEnabled()).toEqual(false);
  expect(disabledFlag.isEnabled()).toEqual(true);
  expect(storage.getItem("a")).toEqual("false");
  expect(storage.getItem("b")).toEqual("true");
});

test("feature flag to be reset", () => {
  const storage = new MockStorage();
  const aStore = writable(true);
  const bStore = writable(false);

  const enabledFlag = new FeatureFlag(storage, "a", true, aStore);

  const disabledFlag = new FeatureFlag(storage, "b", false, bStore);

  enabledFlag.set(false);
  disabledFlag.set(true);
  enabledFlag.reset();
  disabledFlag.reset();

  expect(enabledFlag.isEnabled()).toEqual(true);
  expect(disabledFlag.isEnabled()).toEqual(false);
  expect(storage.getItem("a")).toEqual(null);
  expect(storage.getItem("b")).toEqual(null);
});

test("temporary override changes value without persisting", () => {
  const storage = new MockStorage();
  const aStore = writable(false);

  const flag = new FeatureFlag(storage, "a", false, aStore);

  flag.temporaryOverride(true);

  expect(flag.isEnabled()).toEqual(true);
  // Not persisted, so storage stays empty and `isSet` keeps reporting false.
  expect(flag.isSet()).toEqual(false);
  expect(storage.getItem("a")).toEqual(null);
});

test("set persists over a previous temporary override", () => {
  const storage = new MockStorage();
  const aStore = writable(false);

  const flag = new FeatureFlag(storage, "a", false, aStore);

  flag.temporaryOverride(true);
  flag.set(false);

  expect(flag.isEnabled()).toEqual(false);
  expect(flag.isSet()).toEqual(true);
  expect(storage.getItem("a")).toEqual("false");
});
