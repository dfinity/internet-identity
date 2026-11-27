import { type Writable, get } from "svelte/store";

export class FeatureFlag {
  readonly #storage: Pick<Storage, "getItem" | "setItem" | "removeItem">;
  // Holds a value set for this tab only, e.g. from a `?feature_flag_*` URL
  // param, so it survives same-tab redirects (an OpenID provider round trip)
  // without leaking into other tabs or later visits. Takes precedence over
  // `#storage`.
  readonly #sessionStorage: Pick<Storage, "getItem" | "setItem" | "removeItem">;
  readonly #key: string;
  readonly #defaultValue: boolean;
  readonly #store: Writable<boolean>;

  constructor(
    storage: Pick<Storage, "getItem" | "setItem" | "removeItem">,
    sessionStorage: Pick<Storage, "getItem" | "setItem" | "removeItem">,
    key: string,
    defaultValue: boolean,
    store: Writable<boolean>,
  ) {
    this.#storage = storage;
    this.#sessionStorage = sessionStorage;
    this.#key = key;
    this.#defaultValue = defaultValue;
    this.#store = store;

    const storedValue =
      this.#sessionStorage.getItem(this.#key) ??
      this.#storage.getItem(this.#key);
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

  isSet(): boolean {
    return (
      this.#sessionStorage.getItem(this.#key) !== null ||
      this.#storage.getItem(this.#key) !== null
    );
  }

  set(value: boolean) {
    this.#store.set(Boolean(value));
    this.#storage.setItem(this.#key, JSON.stringify(get(this.#store)));
  }

  setForSession(value: boolean) {
    this.#store.set(Boolean(value));
    this.#sessionStorage.setItem(this.#key, JSON.stringify(get(this.#store)));
  }

  reset(): void {
    this.#store.set(this.#defaultValue);
    this.#storage.removeItem(this.#key);
    this.#sessionStorage.removeItem(this.#key);
  }

  temporaryOverride(value: boolean) {
    this.#store.set(Boolean(value));
  }
}
