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
      fallback: "200.html",
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
    // The OpenID provider's `response_mode=form_post` callback is a
    // cross-origin form POST to /callback, which SvelteKit's CSRF origin
    // check would reject with a 403 before the server hook that translates
    // it in dev can run. The check only ever applies to the dev server —
    // adapter-static ships no server in production — and the dev server
    // holds no cookie-backed state a forged POST could abuse.
    csrf: { checkOrigin: false },
    version: { name: packagejson.version },
  },
};

export default config;
