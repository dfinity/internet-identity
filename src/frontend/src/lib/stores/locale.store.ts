import { storeLocalStorageKey } from "$lib/constants/store.constants";
import { derived, get, readable, Readable } from "svelte/store";
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
  : "navigator" in globalThis
    ? (navigator.languages ?? [navigator.language ?? availableLocales[0]])
    : [get(locales)[0]]; // Fallback if no navigator available
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
  setOrReset: (local: string) => Promise<void>;
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
  setOrReset: async (locale: string) => {
    // Switch back to locale auto-detection if locale matches browser
    if (locale === availableBrowserLocale) {
      await localeStore.reset();
    } else {
      await localeStore.set(locale);
    }
  },
};

/**
 * Usage: <span>{$t`Hello ${planet}`}</span>
 */
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

/**
 * Usage:
 * <p>
 *   {$plural(numBooks, {
 *     one: `One ${genre} book`,
 *     other: `# ${genre} books`
 *   })}
 * </p>
 */
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

/**
 * Usage: <span>{$formatDate(dateObj, { dateStyle: "short" })}</span>
 */
export const formatDate = derived(
  localeStore,
  () => (value: string | Date, format?: Intl.DateTimeFormatOptions) =>
    i18n.date(value, format),
);

/**
 * Usage: <span>{$formatNumber(12345.678)}</span>
 */
export const formatNumber = derived(
  localeStore,
  () => (value: number, format?: Intl.NumberFormatOptions) =>
    i18n.number(value, format),
);

// Current time, used in below `formatRelative`
const now = readable(Date.now(), (set) => {
  const interval = setInterval(() => set(Date.now()), 1000);
  return () => clearInterval(interval);
});

/**
 * Usage: <span>{$formatRelative(dateObj, { style: "long" })}</span>
 */
export const formatRelative = derived(
  [localeStore, now],
  ([locale, now]) =>
    (
      value: string | Date,
      format?: Intl.RelativeTimeFormatOptions,
      {
        units = [
          { unit: "year", ms: 1000 * 60 * 60 * 24 * 365 },
          { unit: "month", ms: 1000 * 60 * 60 * 24 * 30 },
          { unit: "day", ms: 1000 * 60 * 60 * 24 },
          { unit: "hour", ms: 1000 * 60 * 60 },
          { unit: "minute", ms: 1000 * 60 },
          { unit: "second", ms: 1000 },
        ],
      } = {},
    ) => {
      const diffMs = new Date(value).getTime() - now;
      const rtf = new Intl.RelativeTimeFormat(locale, format);

      // Pick unit within threshold
      for (const { unit, ms } of units) {
        if (Math.abs(diffMs) >= ms || unit === "second") {
          const value = Math.round(diffMs / ms);
          return rtf.format(value, unit as Intl.RelativeTimeFormatUnit);
        }
      }

      // Fallback
      return i18n.date(value);
    },
);
