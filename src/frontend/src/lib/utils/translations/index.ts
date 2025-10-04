import i18n from "sveltekit-i18n";
import type { Config } from "sveltekit-i18n";
import lang from "./lang.json";

const config: Config = {
  translations: {
    en: { lang },
    es: { lang },
    id: { lang },
    de: { lang },
  },
  loaders: [
    {
      locale: "en",
      key: "t",
      loader: async () => (await import("./en.json")).default,
    },
    {
      locale: "es",
      key: "t",
      loader: async () => (await import("./es.json")).default,
    },
    {
      locale: "id",
      key: "t",
      loader: async () => (await import("./id.json")).default,
    },
    {
      locale: "de",
      key: "t",
      loader: async () => (await import("./de.json")).default,
    },
  ],
};

export const { t, locale, locales, loading, loadTranslations } = new i18n(
  config,
);

loading.subscribe(
  ($loading) => $loading && console.log("Loading translations..."),
);
