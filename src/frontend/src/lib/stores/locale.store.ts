import { storeLocalStorageKey } from "$lib/constants/store.constants";
import { derived, get, Readable } from "svelte/store";
import { writableStored } from "./writable.store";
import { building } from "$app/environment";
import { generateMessageId } from "../../../../lingui-svelte/generateMessageId";
import { i18n } from "@lingui/core";
import { MacroMessageDescriptor } from "@lingui/core/macro";
import { nonNullish } from "@dfinity/utils";
import { availableLocales } from "$lib/constants/locale.constants";

export const browserLocales = building
  ? [availableLocales[0]] // Fallback during SSG
  : (navigator.languages ?? [navigator.language ?? availableLocales[0]]);
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
  reset: () => void;
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
      i18n.loadAndActivate({ locale: locale, messages });
    } else {
      // Else use fallback (which are the "en" default)
      i18n.loadAndActivate({ locale: availableLocales[0], messages: {} });
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
    const { messages } = await import(`$lib/locales/${locale}.po`);
    i18n.loadAndActivate({ locale: locale, messages });
    internalStore.set(locale);
  },
  reset: () => {
    i18n.loadAndActivate({ locale: availableLocales[0], messages: {} });
    internalStore.set(null);
  },
};

// Copy from: https://github.com/HenryLie/svelte-i18n-lingui/blob/main/src/lib/index.js#L50
const processTaggedLiteral = (
  descriptor: string | MacroMessageDescriptor | TemplateStringsArray,
  ...args: Array<string | number>
) => {
  // string
  if (typeof descriptor === "string") {
    const id = generateMessageId(descriptor);
    return i18n.t({ id, message: descriptor });
  }

  // MacroMessageDescriptor
  if (
    typeof descriptor === "object" &&
    "message" in descriptor &&
    nonNullish(descriptor.message)
  ) {
    const id = generateMessageId(descriptor.message, descriptor.context);
    return i18n.t({ id, ...descriptor });
  }

  // TemplateStringsArray
  if (Array.isArray(descriptor)) {
    let message = descriptor[0];
    args.forEach((_arg, i) => {
      message += `{${i}}` + descriptor[i + 1];
    });
    const id = generateMessageId(message);
    const values = { ...args };

    return i18n.t({ id, message, values });
  }

  throw new Error("Unknown descriptor");
};
const processPlural = (num: number, variations: Record<string, string>) => {
  let pluralOptions = "";
  Object.entries(variations).forEach(([key, value]) => {
    pluralOptions += ` ${key} {${value}}`;
  });
  const message = `{num, plural,${pluralOptions}}`;
  const id = generateMessageId(message);
  return i18n.t({ id, message, values: { num } });
};

// Derives based on localeStore so that translations update when language updates
export const t = derived(localeStore, () => processTaggedLiteral);
export const plural = derived(localeStore, () => processPlural);
