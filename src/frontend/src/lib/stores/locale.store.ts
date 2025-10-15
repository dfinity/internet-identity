import { storeLocalStorageKey } from "$lib/constants/store.constants";
import { derived, get, Readable } from "svelte/store";
import { writableStored } from "./writable.store";
import { building } from "$app/environment";
import { i18n } from "@lingui/core";
import { MacroMessageDescriptor, ChoiceOptions } from "@lingui/core/macro";
import {
  availableLocales,
  enabledLocales,
} from "$lib/constants/locale.constants";
import { ENABLE_ALL_LOCALES } from "$lib/state/featureFlags";

export const locales = derived(ENABLE_ALL_LOCALES, () =>
  get(ENABLE_ALL_LOCALES) ? availableLocales : enabledLocales,
);

export const browserLocales = building
  ? [get(locales)[0]] // Fallback during SSG
  : (navigator.languages ?? [navigator.language ?? availableLocales[0]]);
export const availableBrowserLocale =
  // Exact match
  browserLocales.find((ul) => get(locales).includes(ul)) ??
  // Language-only match
  get(locales).find((al) =>
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
    const validLocale = availableLocales.includes(locale)
      ? locale
      : availableLocales[0];
    const { messages } = await import(`$lib/locales/${validLocale}.po`);
    i18n.loadAndActivate({ locale: locale, messages });
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
    const { messages } = await import(`$lib/locales/${locale}.po`);
    i18n.loadAndActivate({ locale: locale, messages });
    internalStore.set(locale);
  },
  reset: async () => {
    const { messages } = await import(
      `$lib/locales/${availableBrowserLocale}.po`
    );
    i18n.loadAndActivate({ locale: availableBrowserLocale, messages });
    internalStore.set(null);
  },
};

// Derives based on localeStore so that translations update on language change
export const t = derived(
  localeStore,
  () =>
    function (
      _descriptor: string | MacroMessageDescriptor | TemplateStringsArray,
      ..._parts: unknown[]
    ) {
      // eslint-disable-next-line prefer-rest-params
      return i18n.t(arguments[0]);
    },
);
export const plural = derived(
  localeStore,
  () =>
    function (
      _value: number,
      _options: ChoiceOptions & { [digit: `=${number}`]: string },
    ) {
      // eslint-disable-next-line prefer-rest-params
      return i18n.t(arguments[0]);
    },
);
