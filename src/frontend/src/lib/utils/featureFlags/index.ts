export class FeatureFlag {
  readonly #storage: Pick<Storage, "getItem" | "setItem" | "removeItem">;
  readonly #key: string;
  readonly #defaultValue: boolean;
  readonly #set: (setArg: boolean) => void;
  readonly #get: () => boolean;

  constructor(
    storage: Pick<Storage, "getItem" | "setItem" | "removeItem">,
    key: string,
    defaultValue: boolean,
    set: (setArg: boolean) => void,
    get: () => boolean,
  ) {
    this.#storage = storage;
    this.#key = key;
    this.#defaultValue = defaultValue;
    this.#set = set;
    this.#get = get;

    const storedValue = this.#storage.getItem(this.#key);
    try {
      set(
        storedValue === null
          ? this.#defaultValue
          : Boolean(JSON.parse(storedValue)),
      );
    } catch {
      set(this.#defaultValue);
    }
  }

  isEnabled(): boolean {
    return this.#get();
  }

  set(value: boolean) {
    this.#set(Boolean(value));
    this.#storage.setItem(this.#key, JSON.stringify(this.#get()));
  }

  reset(): void {
    this.#set(this.#defaultValue);
    this.#storage.removeItem(this.#key);
  }

  temporaryOverride(value: boolean) {
    this.#set(Boolean(value));
  }
}
