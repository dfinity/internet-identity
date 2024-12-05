// Feature flags with default values
const FEATURE_FLAGS_WITH_DEFAULTS = {
  DOMAIN_COMPATIBILITY: false,
} as const satisfies Record<string, boolean>;

const LOCALSTORAGE_FEATURE_FLAGS_PREFIX = "ii-localstorage-feature-flags__";

export class FeatureFlag {
  readonly #storage: Pick<Storage, "getItem" | "setItem" | "removeItem">;
  readonly #key: string;
  readonly #defaultValue: boolean;
  #value: boolean;

  constructor(
    storage: Pick<Storage, "getItem" | "setItem" | "removeItem">,
    key: string,
    defaultValue: boolean
  ) {
    this.#storage = storage;
    this.#key = key;
    this.#defaultValue = defaultValue;
    const storedValue = this.#storage.getItem(this.#key);
    this.#value =
      storedValue === null
        ? this.#defaultValue
        : Boolean(JSON.parse(storedValue));
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
}

// Initialize feature flags with values from localstorage
const initializedFeatureFlags = Object.fromEntries(
  Object.entries(FEATURE_FLAGS_WITH_DEFAULTS).map(([key, defaultValue]) => [
    key,
    new FeatureFlag(
      window.localStorage,
      LOCALSTORAGE_FEATURE_FLAGS_PREFIX + key,
      defaultValue
    ),
  ])
);

// Make feature flags configurable from browser console
Object.assign(window, "__featureFlags", initializedFeatureFlags);

// Export initialized feature flags as named exports
export const { DOMAIN_COMPATIBILITY } = initializedFeatureFlags;
