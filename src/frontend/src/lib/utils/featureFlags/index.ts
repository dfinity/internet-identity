export class FeatureFlag {
  readonly #storage: Pick<Storage, "getItem" | "setItem" | "removeItem">;
  readonly #key: string;
  readonly #defaultValue: boolean;
  #value: boolean;

  constructor(
    storage: Pick<Storage, "getItem" | "setItem" | "removeItem">,
    key: string,
    defaultValue: boolean,
  ) {
    this.#storage = storage;
    this.#key = key;
    this.#defaultValue = defaultValue;
    const storedValue = this.#storage.getItem(this.#key);
    try {
      this.#value =
        storedValue === null
          ? this.#defaultValue
          : Boolean(JSON.parse(storedValue));
    } catch {
      this.#value = this.#defaultValue;
    }
  }

  isEnabled(): boolean {
    return this.#value;
  }

  set(value: boolean) {
    this.#value = Boolean(value);
    this.#storage.setItem(this.#key, JSON.stringify(this.#value));
  }

  reset(): void {
    this.#value = this.#defaultValue;
    this.#storage.removeItem(this.#key);
  }

  temporaryOverride(value: boolean) {
    this.#value = Boolean(value);
  }
}
