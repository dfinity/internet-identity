import { defineConfig } from "astro/config";

const ROOT = "../../";

// https://astro.build/config
export default defineConfig({
  publicDir: ROOT + "src/frontend/assets",
  outDir: ROOT + "dist-showcase",
  server: {
    port: 5174,
  },
  vite: {
    optimizeDeps: {
      esbuildOptions: {
        define: {
          global: "globalThis",
        },
      },
    },
  },
});
