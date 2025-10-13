import { defineConfig } from "@lingui/cli";
import { svelteExtractor } from "./src/lingui-svelte";
import { availableLocales } from "./src/frontend/src/lib/constants/locale.constants";

export default defineConfig({
  locales: availableLocales,
  sourceLocale: availableLocales[0],
  catalogs: [
    {
      path: "src/frontend/src/lib/locales/{locale}",
      include: ["src/frontend/src/lib", "src/frontend/src/routes"],
    },
  ],
  extractors: [svelteExtractor],
});
