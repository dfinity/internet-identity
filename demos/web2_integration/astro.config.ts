import { defineConfig } from "astro/config";

// https://astro.build/config
export default defineConfig({
  output: "server",
  vite: {
    server: {
      watch: {
        ignored: [".idea"], // don't hot reload on IDE file changes
      },
    },
  },
});
