import adapter from "@sveltejs/adapter-static";
import { vitePreprocess } from "@sveltejs/vite-plugin-svelte";

/** @type {import("@sveltejs/kit").Config} */
const config = {
  preprocess: vitePreprocess(),
  compilerOptions: {
    customElement: true,
  },
  kit: {
    adapter: adapter(),
    files: {
      appTemplate: "src/frontend/app.html",
      lib: "src/frontend/lib",
      routes: "src/frontend/routes",
      assets: "src/frontend/assets",
      hooks: {
        client: "src/frontend/hooks.client",
        server: "src/frontend/hooks.server",
        universal: "src/frontend/hooks",
      },
    },
  },
};

export default config;
