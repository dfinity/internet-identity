import { storeLocalStorageKey } from "$lib/constants/store.constants";
import { derived, get, Readable } from "svelte/store";
import { writableStored } from "./writable.store";
import { building } from "$app/environment";
import { locale as linguiLocaleStore } from "svelte-i18n-lingui";

export const availableLocales = ["en"];
export const browserLocales = building
  ? [availableLocales[0]] // Fallback during SSG
  : (navigator.languages ?? [navigator.language ?? "en"]);
export const availableBrowserLocale =
  // Exact match
  browserLocales.find((ul) => availableLocales.includes(ul)) ??
  // Language-only match
  availableLocales.find((al) =>
    browserLocales.some((ul) => ul.split("-")[0] === al.split("-")[0]),
  ) ??
  // Fallback
  availableLocales[0];

type LocaleStore = Readable<string> & {
  init: () => Promise<void>;
  set: (locale: string) => Promise<void>;
  reset: () => Promise<void>;
};

const internalStore = writableStored<string | null>({
  key: storeLocalStorageKey.Locale,
  defaultValue: null,
  version: 1,
});

export const localeStore: LocaleStore = {
  init: async () => {
    const locale = get(internalStore) ?? availableBrowserLocale;
    if (availableLocales[0] !== locale && availableLocales.includes(locale)) {
      // Load locale if not default and it's available
      const { messages } = await import(`$lib/locales/${locale}.po`);
      linguiLocaleStore.set(locale, messages);
    }
  },
  subscribe: derived(
    [internalStore],
    ([locale]) => locale ?? availableBrowserLocale,
  ).subscribe,
  set: async (locale: string) => {
    if (!availableLocales.includes(locale)) {
      // Return if locale isn't available
      return;
    }
    internalStore.set(locale);
    const { messages } = await import(`$lib/locales/${locale}.po`);
    linguiLocaleStore.set(locale, messages);
  },
  reset: async () => {
    internalStore.set(null);
    const { messages } = await import(
      `$lib/locales/${availableBrowserLocale}.po`
    );
    linguiLocaleStore.set(availableBrowserLocale, messages);
  },
};
