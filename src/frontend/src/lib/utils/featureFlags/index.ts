import { type Writable, get } from "svelte/store";

export class FeatureFlag {
  readonly #storage: Pick<Storage, "getItem" | "setItem" | "removeItem">;
  readonly #key: string;
  readonly #defaultValue: boolean;
  readonly #store: Writable<boolean>;

  constructor(
    storage: Pick<Storage, "getItem" | "setItem" | "removeItem">,
    key: string,
    defaultValue: boolean,
    store: Writable<boolean>,
  ) {
    this.#storage = storage;
    this.#key = key;
    this.#defaultValue = defaultValue;
    this.#store = store;

    const storedValue = this.#storage.getItem(this.#key);
    try {
      this.#store.set(
        storedValue === null
          ? this.#defaultValue
          : Boolean(JSON.parse(storedValue)),
      );
    } catch {
      this.#store.set(this.#defaultValue);
    }
  }

  isEnabled(): boolean {
    return get(this.#store);
  }

  set(value: boolean) {
    this.#store.set(Boolean(value));
    this.#storage.setItem(this.#key, JSON.stringify(get(this.#store)));
  }

  reset(): void {
    this.#store.set(this.#defaultValue);
    this.#storage.removeItem(this.#key);
  }

  temporaryOverride(value: boolean) {
    this.#store.set(Boolean(value));
  }
}
