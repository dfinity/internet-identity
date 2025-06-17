import adapter from "@sveltejs/adapter-static";
import { vitePreprocess } from "@sveltejs/vite-plugin-svelte";
import packagejson from "./package.json" with { type: "json" };

/** @type {import("@sveltejs/kit").Config} */
const config = {
  // Consult https://svelte.dev/docs/kit/integrations for more information about preprocessors
  preprocess: vitePreprocess(),
  kit: {
    // See https://svelte.dev/docs/kit/adapters for more information about adapters.
    adapter: adapter({
      pages: "dist",
      assets: "dist",
      fallback: "index.html",
      precompress: true,
    }),
    files: {
      appTemplate: "src/frontend/src/app.html",
      lib: "src/frontend/src/lib",
      routes: "src/frontend/src/routes",
      assets: "src/frontend/static",
      hooks: {
        client: "src/frontend/src/hooks.client",
        server: "src/frontend/src/hooks.server",
        universal: "src/frontend/src/hooks",
      },
    },
    version: { name: packagejson.version },
    output: {
      bundleStrategy: "single",
    },
  },
};

export default config;
