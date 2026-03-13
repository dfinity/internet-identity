// First locale is default directly defined in svelte components
export const availableLocales = [
  "en",
  "de",
  "es",
  "fr",
  "id",
  "it",
  "pl",
  "ur",
];

// List of languages that are actually enabled
export const enabledLocales = ["en", "de", "es", "fr", "id", "it", "pl", "ur"];

const rtlLocales = ["ur"];

export const getLocaleDirection = (locale: string): "ltr" | "rtl" =>
  rtlLocales.includes(locale) ? "rtl" : "ltr";
