import { defineConfig } from "@lingui/cli";
import { jstsExtractor, svelteExtractor } from "svelte-i18n-lingui/extractor";

export default defineConfig({
  locales: ["en"],
  sourceLocale: "en",
  catalogs: [
    {
      path: "src/frontend/src/lib/locales/{locale}",
      include: ["src/frontend/src/lib", "src/frontend/src/routes"],
    },
  ],
  extractors: [jstsExtractor, svelteExtractor],
});
