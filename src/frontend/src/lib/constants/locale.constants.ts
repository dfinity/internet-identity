// First locale is default directly defined in svelte components
export const availableLocales = [
  "en",
  "de",
  "es",
  "fr",
  "nl",
  "id",
  "it",
  "pl",
  "ru",
  "uk",
  "ur",
];

// List of languages that are actually enabled
export const enabledLocales = [
  "en",
  "de",
  "es",
  "fr",
  "nl",
  "id",
  "it",
  "pl",
  "ru",
  "uk",
  "ur",
];

const rtlLocales = ["ur"];

export const getLocaleDirection = (locale: string): "ltr" | "rtl" =>
  rtlLocales.includes(locale) ? "rtl" : "ltr";
