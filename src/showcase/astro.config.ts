import { defineConfig } from "astro/config";
import path from "path";

const ROOT = "../..";

// https://astro.build/config
export default defineConfig({
  publicDir: path.join(ROOT, "src/frontend/assets"),
  outDir: path.join(ROOT, "dist-showcase"),
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
